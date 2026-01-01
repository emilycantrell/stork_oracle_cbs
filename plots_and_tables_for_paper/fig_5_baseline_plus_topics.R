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
results_learning_curves_seed_1 <- read_excel(here("exports/export_2025-09-15_mark/results_learning_curves_seed_1_2025-08.xlsx"))
results_learning_curves_seeds_2_5 <- read_excel(here("exports/export_2025-09-15_mark/results_learning_curves_seeds_2-5_2025-08.xlsx"))
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
results_bonus_topic_seed_1 <- read_excel("~/Documents/GitHub/stork_oracle_cbs/exports/export_2025-09-15_mark/results_baseline_plus_topics_seed_1_2025-08.xlsx")
results_bonus_topic_seeds_2_5 <- read_excel("~/Documents/GitHub/stork_oracle_cbs/exports/export_2025-09-15_mark/results_baseline_plus_topics_seeds_2-5_2025-08.xlsx")
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
strip_leading_zero <- function(x, digits = 2) {
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

# Define 5 values for viridis - skipping the brightest yellow
viridis <- hcl.colors(6, "Viridis")[1:5]

# Fake data for legend-only line
legend_line <- data.frame(y = -10)

ggplot(results_combined, aes(x = feature_set, y = estimate_R2_Holdout, fill = factor(n_training_set))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower_R2_Holdout, ymax = ci_upper_R2_Holdout),
                width = 0.2,
                position = position_dodge(width = 0.8)) +
  
  # Actual dashed baseline lines (colored by training size, no legend)
  geom_hline(
    data = results_AS_FS,
    aes(yintercept = estimate_R2_Holdout, color = factor(n_training_set)),
    linetype = "dashed", linewidth = 0.9, show.legend = FALSE
  ) +
  scale_color_manual(values = viridis, guide = "none") + 
  
  # Fake line for black dashed line in legend
  geom_hline(
    data = legend_line,
    aes(yintercept = y, linetype = "Baseline:\nAge, Sex, &\nFamily Structure"),
    color = "black", linewidth = 0.9
  ) +
  
  scale_linetype_manual(
    name = "", values = c("Baseline:\nAge, Sex, &\nFamily Structure" = "dashed")
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
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2),
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
    limits = c(0, 0.35),
    breaks  = seq(0, 0.35, by = 0.1),
    labels  = function(x) strip_leading_zero(x, digits = 1),
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
    "plots_and_tables_output/fig_5_baseline_plus_topics",
    eval_set_label_for_saved_file, 
    ".png"
  ),
  width = 8,
  height = 6
)