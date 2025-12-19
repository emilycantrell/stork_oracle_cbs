if (!isTRUE(getOption("run_all_loaded_packages"))) {
  library(groundhog)
  packages <- c("tidyverse", "ggtext", "here", "readxl")
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

# Baseline results (age, sex, family structure only)
results_AS_FS <- read_excel(here("exports/export_2025-09-15_mark/results_learning_curves_seed_1_2025-08.xlsx")) %>%
  filter(feature_set == "AS_FS",
         evaluation_set == target_test_set) %>%
  mutate(n_training_set = as.numeric(str_extract(training_set, "\\d+$")),
         n_training_set = ifelse(training_set == "training_set", 3696041, n_training_set)) %>%
  filter(n_training_set %in% c(1000, 10000, 100000, 1000000, 3696041))

# Results for Stork Oracle winning model
results_winning_model <- read_excel(here("exports/export_2025-09-15_mark/results_learning_curves_seed_1_2025-08.xlsx")) %>%
  filter(feature_set == "without_leakage",
         evaluation_set == target_test_set) %>%
  mutate(n_training_set = as.numeric(str_extract(training_set, "\\d+$")),
         n_training_set = ifelse(training_set == "training_set", 3696041, n_training_set),
         feature_set = "winning_model") %>%
  filter(n_training_set %in% c(1000, 10000, 100000, 1000000, 3696041))

# Combine all results
results <- bind_rows(results_AS_FS, results_winning_model) %>%
  filter(feature_set %in% c("AS_FS", "winning_model"), 
         training_set == "training_set")

#### PLOT ####

# Create function to remove leading 0 from decimal numbers
strip_leading_zero <- function(x, digits = 1) {
  gsub("^(-?)0\\.", "\\1.", formatC(x, format = "f", digits = digits))
}

# Define y axis label
# Note: Even though we called it R2_holdout in the code, we are
# actually testing on the validation set when we use evaluation_test_50_percent_split, 
# so the plot label should be "R^2_Validation"
y_axis_label <- if (target_test_set == "evaluation_test_50_percent_split") {
  bquote(R[Validation]^2)
} else {
  bquote(R[Holdout]^2)
}

# Label the feature sets
feature_set_labels <- c(
  "AS_FS" = "Age, Sex, & \nFamily Structure \nBaseline",
  "winning_model" = "Winning \nModel"
)

# Bar plot with one bar for AS_FS and one for winning model
results %>%
  ggplot(aes(x = feature_set, y = estimate_R2_Holdout)) + 
  geom_bar(stat = "identity", position = "dodge", fill = NA, color = "black") + 
  geom_errorbar(aes(ymin = ci_lower_R2_Holdout, ymax = ci_upper_R2_Holdout),
                width = 0.2,
                position = position_dodge(width = 0.8)) + 
  scale_y_continuous(
    limits = c(0, 0.35),
    breaks  = seq(0, 0.35, by = 0.1),
    labels  = function(x) strip_leading_zero(x, digits = 1)) + 
  scale_x_discrete(labels = feature_set_labels) + 
  ylab(y_axis_label) + 
  xlab(NULL) +
  theme_bw() + 
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20), 
    panel.grid = element_blank()
  )

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
    "plots_and_tables_output/fig_1_visual_abstract_feature_sets",
    eval_set_label_for_saved_file, 
    ".png"
  ),
  width = 6,
  height = 6, 
  dpi = 300, 
  bg = "white"
)