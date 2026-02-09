if (!isTRUE(getOption("run_all_loaded_packages"))) {
  library(groundhog)
  packages <- c("tidyverse", "ggtext", "readxl", "here")
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

#### LOAD AND PREP DATA ####

# Read learning curve results, from which I will extract a subset of baseline results & winning model results
results_learning_curves_seed_1 <- read_excel(here("results/results_learning_curves_seed_1_2025-08.xlsx"))
results_learning_curves_seeds_2_5 <- read_excel(here("results/results_learning_curves_seeds_2-5_2025-08.xlsx"))
results_learning_curves <- bind_rows(results_learning_curves_seed_1, results_learning_curves_seeds_2_5) %>%
  filter(evaluation_set == target_test_set) %>%
  mutate(n_training_set = as.numeric(str_extract(training_set, "\\d+$")),
         n_training_set = ifelse(training_set == "training_set", 3696041, n_training_set)) %>%
  filter(n_training_set %in% c(1000, 10000, 100000, 1000000, 3696041))

# Baseline results (age, sex, family structure only)
results_AS_FS <- results_learning_curves %>%
  filter(feature_set == "AS_FS") %>%
  # Select the median performance across 5 seeds used to generate training sets
  group_by(n_training_set) %>%
  filter(estimate_R2_Holdout == median(estimate_R2_Holdout)) %>%
  slice_sample(n = 1) %>% # The full training set has just one result, so choose one copy of it
  ungroup()

# Results for Stork Oracle winning model
# Note: "Without leakage" refers to a version of the winning model that omits a feature that 
# introduced leakage; this feature had no meaningful impact on performance.
results_winning_model <- results_learning_curves %>%
  filter(feature_set == "without_leakage") %>%
  mutate(feature_set = "winning_model") %>%
  # Select the median performance across 5 seeds used to generate training sets
  group_by(n_training_set) %>%
  filter(estimate_R2_Holdout == median(estimate_R2_Holdout)) %>%
  slice_sample(n = 1) %>% # The full training set has just one result, so choose one copy of it
  ungroup()

# Bonus topic results (baseline + one additional topic)
results_bonus_topic_seed_1 <- read_excel(here("results/results_baseline_plus_topics_seed_1_2025-08.xlsx"))
results_bonus_topic_seeds_2_5 <- read_excel(here("results/results_baseline_plus_topics_seeds_2-5_2025-08.xlsx"))
results_bonus_topic <- bind_rows(results_bonus_topic_seed_1, results_bonus_topic_seeds_2_5) %>%
  filter(evaluation_set == "evaluation_test_50_percent_split") %>%
  mutate(n_training_set = as.numeric(str_extract(training_set, "\\d+$")),
         n_training_set = ifelse(training_set == "training_set", 3696041, n_training_set)) %>%
  # Select the median performance across 5 seeds used to generate training sets
  group_by(n_training_set, feature_set) %>%
  filter(estimate_R2_Holdout == median(estimate_R2_Holdout)) %>%
  slice_sample(n = 1) %>% # The full training set has just one result, so choose one copy of it
  ungroup()

# Combine results that will be used for bars
results_combined <- bind_rows(results_bonus_topic, results_winning_model)

# Remove dataframes that are no longer needed
rm(results_learning_curves_seed_1,
   results_learning_curves_seeds_2_5,
   results_bonus_topic_seed_1,
   results_bonus_topic_seeds_2_5,
   results_winning_model
   )
gc()

# Feature set label mapping with bolded topics
feature_set_labels <- c(
  "AS_FS_childcare" = "Baseline + <b>Childcare</b>",
  "AS_FS_education" = "Baseline + <b>Education</b>",
  "AS_FS_employment" = "Baseline + <b>Employment</b>",
  "AS_FS_housing" = "Baseline + <b>Housing</b>",
  "AS_FS_immigration" = "Baseline + <b>Immigration</b>",
  "AS_FS_income" = "Baseline + <b>Income</b>",
  "winning_model" = "<b>Winning Model</b><br>(All Topics)"
)

# Determine feature set order based on performance at n = 3696041
# To keep the order consistent across plots, we'll use the R^2 performance order 
# regardless of what metric is in the plot.
feature_order <- c(
  "winning_model",
  results_bonus_topic %>%
    filter(n_training_set == 3696041) %>%
    arrange(desc(estimate_R2_Holdout)) %>%
    pull(feature_set)
)

# Apply this order to the combined data
results_combined <- results_combined %>%
  mutate(feature_set = fct_relevel(feature_set, feature_order))

#### PLOT ####

# Create function to remove leading 0 from decimal numbers
strip_leading_zero <- function(x, digits = 3) {
  if (length(x) == 0) return(x)
  
  out <- formatC(x, format = "f", digits = digits)
  out <- gsub("^(-?)0\\.", "\\1.", out)
  
  # Preserve NA values explicitly
  out[is.na(x)] <- NA_character_
  out
}

# Define 5 values for viridis - skipping the brightest yellow
viridis <- hcl.colors(6, "Viridis")[1:5]

# Fake data for legend-only line and point
legend_line <- data.frame(y = NA_real_)
legend_point <- legend_point <- data.frame(
  feature_set = levels(results_combined$feature_set)[1],
  y = NA_real_
)

# Function to get column names for a given metric
metric_cols <- function(my_metric) {
  list(
    estimate = paste0("estimate_", my_metric),
    ci_lower = paste0("ci_lower_", my_metric),
    ci_upper = paste0("ci_upper_", my_metric)
  )
}

# Create vector of metric names
metrics_to_run <- c("R2_Holdout", "Accuracy", "F1_Score", "LogLoss", "MSE")

# Loop over the metrics
for (my_metric in metrics_to_run) {
  
  # Print progress message
  if (my_metric == "R2_Holdout") { 
    message("Running Figure 5 (``baseline plus topic'') for metric: ", my_metric)
  } else { 
    message("Running ``baseline plus topic'' plot for metric: ", my_metric)
  }
  
  cols <- metric_cols(my_metric)
  
  # Define y axis label based on my_metric
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
    my_metric
  )
  
  p <- ggplot(
    results_combined,
    aes(
      x = feature_set,
      y = .data[[cols$estimate]],
      fill = factor(n_training_set)
    )
  ) +
    geom_point(
      aes(fill = factor(n_training_set)),
      shape = 21,                  # fillable shape
      size = 3,
      color = "black",
      position = position_dodge(width = 0.8)
    ) + 
    geom_errorbar(
      aes(
        ymin = .data[[cols$ci_lower]],
        ymax = .data[[cols$ci_upper]]
      ),
      width = 0.2,
      position = position_dodge(width = 0.8)
    ) +
    
    # Baseline dashed lines (no legend)
    geom_hline(
      data = results_AS_FS,
      aes(
        yintercept = .data[[cols$estimate]],
        color = factor(n_training_set)
      ),
      linetype = "dashed",
      linewidth = 0.9,
      show.legend = FALSE
    ) +
    scale_color_manual(values = viridis, guide = "none") +
    
    # Fake line to force the desired line to appear in the legend 
    # (this will not actually appear on the plot)
    geom_hline(
      data = legend_line,
      aes(
        yintercept = y,
        linetype = "Baseline:\nage, sex, &\nfamily structure"
      ),
      color = "black",
      linewidth = 0.9,
      na.rm = TRUE
    ) +
    scale_linetype_manual(
      name = "",
      values = c("Baseline:\nage, sex, &\nfamily structure" = "dashed")
    ) +
    
    # Fake point to force the desired point to appear in the legend 
    # (this will not actually appear on the plot)
    geom_point(
      data = legend_point,
      aes(
        x = feature_set,
        y = y,
        shape = "Baseline + \ntopic area"
      ),
      color = "black",
      size = 3,
      show.legend = TRUE,
      na.rm = TRUE,
      inherit.aes = FALSE
    ) + 
    scale_shape_manual(
      name = "",
      values = c(
        "Baseline + \ntopic area" = 1  # hollow circle
      )
    ) +
    
    scale_x_discrete(labels = feature_set_labels) +
    xlab("Feature Set") +
    ylab(y_axis_label) +
    
    scale_fill_manual(
      values = viridis,
      name = "Training Size",
      labels = c(
        expression(10^3),
        expression(10^4),
        expression(10^5),
        expression(10^6),
        expression(3.7 %*% 10^6)
      )
    ) +
    
    guides(
      fill = guide_legend(
        order = 1,
        override.aes = list(
          shape = 22,
          size  = 5,
          color = NA
        )
      ),
      linetype = guide_legend(
        order = 2,
        override.aes = list(
          shape = NA,   
          linewidth = 0.9
        )
      ),
      color = "none"
    ) +
    
    theme_bw() +
    theme(
      axis.text.x = element_markdown(angle = 45, hjust = 1),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 13),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 11),
      panel.border = element_rect(color = "black", size = .75)
    ) + 
    
    scale_y_continuous(
      labels = function(x) strip_leading_zero(x)
    )
  
  # Create labels for the file name
  # If R2_Holdout, just call it R2 (eval_set_label_for_saved_file will indicate validation or holdout)
  metric_label_for_saved_file <- ifelse(my_metric == "R2_Holdout", "R2", my_metric)
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
  
  # Save the file
  ggsave(
    filename = paste0(
      here("analysis/plots_and_tables_output/appendix_baseline_plus_topics_"),
      metric_label_for_saved_file, 
      eval_set_label_for_saved_file, 
      ".png"
    ),
    plot = p,
    width = 8,
    height = 6
  )
}

  


