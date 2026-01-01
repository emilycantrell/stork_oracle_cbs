if (!isTRUE(getOption("run_all_loaded_packages"))) {
  library(groundhog)
  packages <- c("tidyverse", "readxl", "ggtext", "pBrackets", "png", "here")
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
results_bonus_topic_seed_1 <- read_excel(here("exports/export_2025-09-15_mark/results_baseline_plus_topics_seed_1_2025-08.xlsx"))
results_bonus_topic_seeds_2_5 <- read_excel(here("exports/export_2025-09-15_mark/results_baseline_plus_topics_seeds_2-5_2025-08.xlsx"))
results_bonus_topic <- bind_rows(results_bonus_topic_seed_1, results_bonus_topic_seeds_2_5) %>%
  filter(evaluation_set == "evaluation_test_50_percent_split") %>%
  mutate(n_training_set = as.numeric(str_extract(training_set, "\\d+$")),
         n_training_set = ifelse(training_set == "training_set", 3696041, n_training_set)) %>%
  # Select the median performance across 5 seeds used to generate training sets
  group_by(n_training_set, feature_set) %>%
  filter(estimate_R2_Holdout == median(estimate_R2_Holdout)) %>%
  slice_sample(n = 1) %>% # The full training set has just one result, so choose one copy of it
  ungroup()

# Combine results on bonus topic and winnning model (since these are what we compare to the baseline)
results_combined <- bind_rows(results_bonus_topic, results_winning_model)

# Feature set label mapping with bolded topics
feature_set_labels <- c(
  "AS_FS_childcare" = "Baseline + <b>Childcare</b>",
  "AS_FS_education" = "Baseline + <b>Education</b>",
  "AS_FS_employment" = "Baseline + <b>Employment</b>",
  "AS_FS_housing" = "Baseline + <b>Housing</b>",
  "AS_FS_immigration" = "Baseline + <b>Immigration</b>",
  "AS_FS_income" = "Baseline + <b>Income</b>",
  "winning_model" = "<b>Winning Model</b>"
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
strip_leading_zero <- function(x, digits = 3) {
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

# Extract baseline + winning model data only
gap_data <- bind_rows(
  results_AS_FS %>% mutate(model_type = "Baseline"),
  results_winning_model %>% mutate(model_type = "Winning Model")
) %>%
  select(n_training_set, estimate_R2_Holdout, ci_lower_R2_Holdout, ci_upper_R2_Holdout, model_type) %>%
  mutate(model_type = factor(model_type, levels = c("Winning Model", "Baseline")))

# Plot with gap labels
plot_without_brackets <- ggplot(gap_data, aes(x = n_training_set, y = estimate_R2_Holdout, color = model_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = ci_lower_R2_Holdout, ymax = ci_upper_R2_Holdout), width = 0.05) +
  scale_x_log10(
    breaks = c(1e3, 1e4, 1e5, 1e6, 3696041),
    labels = c(expression(10^3), expression(10^4), expression(10^5), expression(10^6), expression(3.7 %*% 10^6))
  ) +
  scale_y_continuous(
    labels  = function(x) strip_leading_zero(x, digits = 3)) + 
  # What we called r2_holdout in the code was actually calculated on the validation
  # set for the data we are currently using 
  ylab(y_axis_label) +
  xlab("Training Set Size (Log Scale)") +
  scale_color_manual(
    name = "Feature Set",
    values = c("Baseline" = "black", "Winning Model" = "magenta4"),
    # The spacing is weird in the lines below in order to add vertical space between legend items
    labels = c("Winning Model" = " 
Winning Model
",
               "Baseline" = "Baseline:\nAge, Sex, &\nFamily Structure")
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    panel.border = element_rect(color = "black", size = .75)
  )

print(plot_without_brackets)

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
    "plots_and_tables_output/fig_6_sample_size_feature_set_interdependence",
    eval_set_label_for_saved_file, 
    ".png"
  ),
  width = 8,
  height = 6
)

#### CHECK ON SAMPLE SIZE/FEATURE SET INTERDEPENDENCE ####

## This section of code provides the number to fill in the blank in the following text: ##
# Third, the gap between the “baseline” feature set and larger feature sets usually 
# (but not always) increased as sample size increased, because at larger sample sizes, 
# the model was better able to learn from the additional features. 
# For example, Figure 5 contains 21 instances where we incremented the sample size 
# by exactly one order of magnitude: for each of the seven feature sets that were 
# compared to the baseline, we increased the sample size from 1,000 to 10,000, from 
# 10,000 to 100,000, and from 100,000 to 1,000,000 for both the feature set and the 
# baseline. Out of these 21 increment exercises, in _X_ cases the performance gap between 
# the larger feature set and the baseline was larger at the higher sample size than at 
# the lower one.

# Get just baseline R^2 by training size
baseline_r2 <- results_AS_FS %>%
  select(n_training_set, estimate_R2_Holdout) %>%
  rename(baseline_R2 = estimate_R2_Holdout)

# Feature sets to compare against the baseline
feature_sets_to_check <- c(
  "AS_FS_childcare",
  "AS_FS_education",
  "AS_FS_employment",
  "AS_FS_housing",
  "AS_FS_immigration",
  "AS_FS_income",
  "winning_model"
)

# Loop through each feature set and compute the performance gap
all_comparisons <- map_dfr(feature_sets_to_check, function(feature_set_name) {
  feature_set_data <- results_combined %>%
    filter(feature_set == feature_set_name) %>%
    select(n_training_set, estimate_R2_Holdout) %>%
    rename(feature_set_R2 = estimate_R2_Holdout)
  
  feature_set_vs_baseline_comparison <- feature_set_data %>%
    left_join(baseline_r2, by = "n_training_set") %>%
    mutate(gap = feature_set_R2 - baseline_R2, 
           feature_set = feature_set_name) %>%
    arrange(n_training_set) %>%
    group_by(feature_set) %>%
    mutate(gap_increased = gap > lag(gap)) %>%
    ungroup()
  
  return(feature_set_vs_baseline_comparison)
})

# In how many cases did the gap increased as sample size increased?
times_the_gap_increased <- all_comparisons %>%
  # Filter out 3.7 million since we are only looking at increases that are exactly 1 order of magnitude
  filter(n_training_set %in% c(1000, 10000, 100000, 1000000)) %>% 
  summarise(times_the_gap_increased = sum(gap_increased, na.rm = TRUE)) %>%
  pull(times_the_gap_increased)

print(paste0(
"Text for section 3.3, ``Interplay between feature sets and sample size'': Out of these 21 increment exercises, in ",
times_the_gap_increased,
" cases the performance gap between the larger feature set and the baseline was larger at the higher sample size than at the lower one."))

#### CALCULATE THE NUMBERS FOR THE CURLY BRACKETS TO MANUALLY ADD TO FIGURE 6 ####
curly_bracket_for_winning_model_at_1000 <- all_comparisons %>%
  filter(feature_set == "winning_model", n_training_set == 1000) %>%
  pull(gap)
curly_bracket_for_winning_model_at_3696041 <- all_comparisons %>%
  filter(feature_set == "winning_model", n_training_set == 3696041) %>%
  pull(gap)
print(paste0("The bracket showing the gap between curves at n = 1000 to be added to Figure 6 should read: ", 
             round(curly_bracket_for_winning_model_at_1000, 3)))
print(paste0("The bracket showing the gap between curves at n = 3,696,041 to be added to Figure 6 should read: ", 
             round(curly_bracket_for_winning_model_at_3696041, 3)))