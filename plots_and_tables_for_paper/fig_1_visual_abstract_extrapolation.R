# Scaling laws: how well can we extrapolate performance at larger sample sizes, 
# based on performance at smaller sample sizes?

if (!isTRUE(getOption("run_all_loaded_packages"))) {
  library(groundhog)
  packages <- c("tidyverse", "scales", "rlang", "nlsr",  "minpack.lm", "readxl")
  invisible(suppressMessages(
    groundhog.library(packages, "2025-11-15")
  ))
}


#### SETTINGS ####

# Choose whether to test on evaluation set or holdout set
# "evaluation_test_50_percent_split" for validation set 
# "official_holdout_set" for holdout set
if (!exists("target_test_set", inherits = TRUE)) {
  target_test_set <- "evaluation_test_50_percent_split"
}

# Choose an axis type
#axis_type <- "linear"
axis_type <- "log10"

# Chose a metric
#my_metric <- "LogLoss"
#my_metric <- "MSE"
#my_metric <- "In_Sample_R2"
my_metric <- "R2_Holdout"
#my_metric <- "AUC"
#my_metric <- "F1_Score"
#my_metric <- "Accuracy"

abs_gaps_all_seeds <- tibble()

##### NOTE #################################################
## This file recycles the code that loops over all 5 seeds, 
## but we only need 1 seed for the abstract. 
############################################################

for(main_seed in c(1)) {

#### DATA PREP #### 
  
# Make seed into a string
main_seed_string <- paste0(main_seed)

# Read results for training points (n <= 10k) & bind them together
lower_n_results <- switch(
  main_seed_string,
  "1" = read_excel(here("exports/export_2025-09-15_mark/results_extrapolation_seed_1_2025-08.xlsx")),
  "2" = read_excel(here("exports/export_2025-09-15_mark/results_extrapolation_seeds_2_2025-08.xlsx")),
  "3" = read_excel(here("exports/export_2025-09-15_mark/results_extrapolation_seeds_3_2025-08.xlsx")),
  "4" = read_excel(here("exports/export_2025-09-15_mark/results_extrapolation_seeds_4-5_2025-08.xlsx")) %>%
    filter(str_starts(sampling_file, "dms_samples_seed_4")),
  "5" = read_excel(here("exports/export_2025-09-15_mark/results_extrapolation_seeds_4-5_2025-08.xlsx")) %>%
    filter(str_starts(sampling_file, "dms_samples_seed_5"))
)

# Make column with sample sizes
lower_n_results <- lower_n_results %>%
  mutate(n_training_set = as.numeric(str_extract(training_set, "\\d+$")), 
         n_training_set = ifelse(training_set == "training_set", 3696041, n_training_set))

# Read results for test points (n > 10k)
higher_n_results <- if (main_seed == "1") {
  read_excel(here("exports/export_2025-09-15_mark/results_learning_curves_seed_1_2025-08.xlsx"))
} else if (main_seed_string %in% c("2", "3", "4", "5")) {
  read_excel(here("exports/export_2025-09-15_mark/results_learning_curves_seeds_2-5_2025-08.xlsx")) %>%
    filter(str_starts(sampling_file, paste0("dms_samples_seed_", main_seed)))
} else {
  stop("main_seed must be 1, 2, 3, 4, or 5")
}
  
# Filter to just n > 10k
higher_n_results <- higher_n_results %>%
  mutate(n_training_set = as.numeric(str_extract(training_set, "\\d+$")), 
         n_training_set = ifelse(training_set == "training_set", 3696041, n_training_set)) %>%
  filter(n_training_set > 10000) 
  

# Combine all results
results <- bind_rows(lower_n_results, higher_n_results)
rm(lower_n_results, higher_n_results)

# Filter to just three feature sets (we are holding the fourth back as an informal "holdout")
results <- results %>%
  filter(feature_set %in% c("without_leakage", "prefer_official_train", "ego_AS"))

# Build column names dynamically
estimate_col <- paste0("estimate_", my_metric)
ci_lower_col <- paste0("ci_lower_", my_metric)
ci_upper_col <- paste0("ci_upper_", my_metric)
  
# Select desired columns & create column of training sample sizes
results <- results %>%
  filter(evaluation_set == target_test_set) %>%
  dplyr::select(training_set, feature_set, 
         !!estimate_col := all_of(estimate_col),
         !!ci_lower_col := all_of(ci_lower_col),
         !!ci_upper_col := all_of(ci_upper_col)) %>%
  rename(estimate_metric = !!estimate_col,
         ci_lower_metric = !!ci_lower_col,
         ci_upper_metric = !!ci_upper_col) %>%
  # TODO: This duplicates n_training_set code above; improve this to avoid duplication
  mutate(n_training_set = as.numeric(str_extract(training_set, "\\d+$")), 
         n_training_set = ifelse(training_set == "training_set", 3696041, n_training_set)) 

#### PLOTS ####

# Define x-axis scale based on axis_type
x_axis_scale <- if (axis_type == "log10") {
  scale_x_log10(labels = trans_format("log10", math_format(10^.x)), 
                guide = guide_axis_logticks())
} else {
  scale_x_continuous()
}

# Define x-axis label based on axis_type
x_axis_label <- if (axis_type == "log10") {
  "Sample Size (Log Scale)"
} else {
  "Sample Size"
}

# Define y axis label based on my_metric
# Note: Even though we called it R2_holdout in the code, we are
# actually testing on the validation set when we use evaluation_test_50_percent_split, 
# so the plot label should be "R^2_Validation"
y_axis_label <- switch(
  my_metric,
  "LogLoss" = "Log Loss",
  "MSE" = "Mean Squared Error",
  "R2_Holdout" = if (target_test_set == "evaluation_test_50_percent_split") {
    bquote(R[Validation]^2)
  } else {
    bquote(R[Holdout]^2)
  },
  "F1_Score" = "F1 Score",
  "Accuracy" = "Accuracy",
  my_metric  # fallback
)

# Define feature sets to compare
feature_sets_to_plot <- c("without_leakage")

# Initialize an empty dataframe
data_for_plot <- tibble()

# Loop through each feature set
for (feature_i in feature_sets_to_plot) {
  # Filter results for the current feature set
  feature_data <- results %>%
    filter(feature_set == feature_i)

  # Create 4 additional copies of 10k results so that all points ≤ 10k are weighted equally
  # since all the smaller sample sizes appear 5 times but 10k results only appear 1 time
  feature_data <- feature_data %>%
    bind_rows(
      feature_data %>%
        filter(n_training_set == 10000) %>%
        slice_sample(n = 4, replace = TRUE)
    )
  
  # Select only points where training sample size ≤ 10000 for fitting
  training_data <- feature_data %>% 
    filter(n_training_set <= 10000) 

    feature_data <- feature_data %>% 
    mutate(split = ifelse(n_training_set <= 10000, "train", "test"))
  
  # Fit scaling law
  scaling_formula <- estimate_metric ~ a * n_training_set^(-b) + c
  starting_points <- switch(my_metric,
                            "LogLoss" = c(a = 0.5, b = 0.5, c = 0.5),
                            "MSE" =     c(a = 0.5, b = 0.5, c = 0.1),
                            "R2_Holdout" = c(a = -0.5, b = 0.5, c = 0.5),
                            "In_Sample_R2" = c(a = -0.5, b = 0.5, c = 0.5),
                            "AUC" =     c(a = -0.5, b = 0.5, c = 0.8),
                            "F1_Score" = c(a = -0.5, b = 0.5, c = 0.5),
                            "Accuracy" = c(a = -0.5, b = 0.5, c = 0.85))
  
  # Fit the learning curve
  fit_nlsLM <- try(nlsLM(scaling_formula,
                         start = starting_points,
                         data = training_data,
                         control = nls.lm.control(maxiter = 500)))

  # Predict for all points
  feature_data$predicted <- predict(fit_nlsLM, newdata = feature_data)
  
  # Append to combined dataframe
  data_for_plot <- bind_rows(data_for_plot, feature_data)
}

# Set the order for the legend
data_for_plot <- data_for_plot %>%
  mutate(split = factor(split, levels = c("train", "test"))) 

# Define y limits: for seed 1 for the main paper, let ggplot choose automatically; 
# for the other seeds, standardize the ylims so the plots are comparable 
y_limits <- if (main_seed == 1) {
  NULL  # Let ggplot choose automatically
} else {
  c(-0.125, 0.375)
}

# Create function to remove leading 0 from decimal numbers
strip_leading_zero <- function(x, digits = 1) {
  gsub("^(-?)0\\.", "\\1.", formatC(x, format = "f", digits = digits))
}

# Plot
p <- ggplot(data_for_plot, aes(x = n_training_set, y = estimate_metric)) +
  geom_point(aes(shape = split), size = 2, alpha = 0.6) +
  scale_shape_manual(
    values = c("train" = 16, "test" = 1),
    labels = c(
      "train" = "Data points used \nto fit curve\n",
      "test"  = "Data points not \nobserved when \nfitting curve"
    )
  ) +
  geom_line(
    aes(y = predicted, linetype = "Estimated learning curve")
  ) +
  scale_linetype_manual(
    values = c("Estimated learning curve" = "dashed")
  ) +
  x_axis_scale +
  labs(
    shape = NULL,
    x = x_axis_label,
    y = y_axis_label,
    #title = "Out-of-Sample Extrapolation to Estimate Performance at Larger Sample Sizes",
  ) +
  guides(
    shape = guide_legend(order = 1),
    linetype = guide_legend(order = 2, title = NULL)
  ) + 
  theme_bw() + 
  theme(
    axis.title = element_text(size = 20),  
    axis.text = element_text(size = 20),  
    legend.title = element_text(size = 20), 
    legend.text = element_text(size = 20) , 
    legend.justification = c("left", "bottom"),
    legend.box.just = "left",
  ) + 
  scale_y_continuous(labels = strip_leading_zero)
  #ggtitle(paste0("Extrapolation with main seed = ", main_seed)) 

}

# Create target test set label for the file name
eval_set_label_for_saved_file <- 
  if (target_test_set == "evaluation_test_50_percent_split") {
    "_validation_set"
  } else if (target_test_set == "official_holdout_set") {
    "_holdout_set"
  } else {
    stop("Unknown target_test_set value: ", target_test_set) 
    # Note: we also have results for the selection set but I don't 
    # expect to ever use it for these plots
  }

# Save the figure
ggsave(
  filename = paste0(
    "plots_and_tables_output/fig_1_visual_abstract_extrapolation",
    eval_set_label_for_saved_file, 
    ".png"
  ),
  width = 10,
  height = 6, 
  dpi = 300, 
  bg = "white"
)

