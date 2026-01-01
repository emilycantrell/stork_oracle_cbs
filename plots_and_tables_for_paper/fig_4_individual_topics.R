if (!isTRUE(getOption("run_all_loaded_packages"))) {
  library(groundhog)
  packages <- c("tidyverse", "readxl", "forcats", "here")
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

# Read in results on individual topic areas
# These results come from a few different jobs because some parts are the same as other plots in the paper
results_six_topics_seed_1 <- read_excel(here("exports/export_2025-09-15_mark/results_individual_topics_seed_1_2025-08.xlsx"))
results_six_topics_seeds_2_to_5 <- read_excel(here("exports/export_2025-09-15_mark/results_individual_topics_seeds_2-5_2025-08.xlsx"))
training_sets_for_this_plot <- unique(results_six_topics_seed_1$training_set)
results_winning_and_asfs_seed_1 <- read_excel(here("exports/export_2025-09-15_mark/results_learning_curves_seed_1_2025-08.xlsx")) %>%
  filter(training_set %in% training_sets_for_this_plot,
         feature_set %in% c("without_leakage", "AS_FS"))
results_winning_and_asfs_seeds_2_to_5 <- read_excel(here("exports/export_2025-09-15_mark/results_learning_curves_seeds_2-5_2025-08.xlsx")) %>%
  filter(training_set %in% training_sets_for_this_plot,
         feature_set %in% c("without_leakage", "AS_FS"))

# Combine all results for this plot
results <- rbind.data.frame(results_six_topics_seed_1, 
                            results_six_topics_seeds_2_to_5, 
                            results_winning_and_asfs_seed_1, 
                            results_winning_and_asfs_seeds_2_to_5)
rm(results_six_topics_seed_1, 
   results_six_topics_seeds_2_to_5, 
   results_winning_and_asfs_seed_1, 
   results_winning_and_asfs_seeds_2_to_5)

# Rename "without leakage" as "winning model" 
# Note: This version of the winning model "without leakage" refers to the removal 
# of a feature that leaked future information into the training data
results <- results %>%
  mutate(feature_set = ifelse(feature_set == "without_leakage", "winning_model", feature_set))

# Filter to desired evaluation set
results <- results %>%
  filter(evaluation_set == target_test_set)

# Make column with sample sizes
results <- results %>%
  mutate(n_training_set = as.numeric(str_extract(training_set, "\\d+$")), 
         n_training_set = ifelse(training_set == "training_set", 3696041, n_training_set)) 

# Label the feature sets
feature_set_labels <- c(
  "childcare" = "Childcare Proximity",
  "education" = "Education",
  "employment" = "Employment",
  "AS_FS" = "Age, Sex, and\nFamily Structure",
  "housing" = "Housing", 
  "immigration" = "Immigration",
  "income" = "Income",
  "winning_model" = "Winning Model\n(All Topics)")

#### PLOT WITH BARS ####

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

# Ensure consistent factor conversion
results_prepped <- results %>%
  mutate(
    n_training_set = factor(n_training_set, levels = sort(unique(n_training_set)))
  )

# Compute medians & order feature sets by performance at n = 3.7 million
results_medians <- results_prepped %>%
  group_by(feature_set, n_training_set) %>%
  filter(estimate_R2_Holdout == median(estimate_R2_Holdout)) %>%
  slice_sample(n = 1) %>% # The full training set has just one result, so choose one copy of it
  ungroup() %>%
  rename(median_R2_Holdout = estimate_R2_Holdout) %>%
  # reorder feature_set by performance at n = 3.7 million
  group_by(feature_set) %>%
  mutate(performance_at_max = median_R2_Holdout[n_training_set == 3696041]) %>%
  ungroup() %>%
  mutate(feature_set = fct_reorder(feature_set, performance_at_max, .desc = TRUE)) %>%
  select(-performance_at_max)

# apply the same reordering to the raw points
results_prepped <- results_prepped %>%
  left_join(dplyr::distinct(results_medians, feature_set), by = "feature_set") %>%
  mutate(feature_set = fct_relevel(feature_set, levels(results_medians$feature_set)))

# Define 5 values for viridis - skipping the brightest yellow
viridis <- hcl.colors(6, "Viridis")[1:5]

# plot
ggplot() +
  # MEDIAN BARS (outlined, empty fill)
  geom_col(
    data = results_medians,
    aes(
      x = feature_set,
      y = median_R2_Holdout,
      color = n_training_set,   # outline color encodes training size
      group = n_training_set,
    ),
    fill = NA,              
    position = position_dodge(width = 0.8),
    width = 0.6,
    alpha = 1,                  # solid outline
    linewidth = 0.8,
    show.legend = TRUE
  ) +
  
  # SMALL POINTS for individual seeds (same color as outline)
  geom_point(
    data = results_prepped,
    aes(
      x = feature_set, 
      y = estimate_R2_Holdout, 
      color = n_training_set
    ),
    position = position_dodge(width = 0.8),
    size = 1.2,
    alpha = 0.9,
    show.legend = TRUE
  ) +
  
  # BLACK ERROR BARS for the median (kept out of legend)
  geom_errorbar(
    data = results_medians,
    aes(
      x = feature_set,
      ymin = ci_lower_R2_Holdout,
      ymax = ci_upper_R2_Holdout,
      group = n_training_set,
    ),
    position = position_dodge(width = 0.8),
    width = 0.2,
    color = "black",
    linewidth = 0.6,
    show.legend = FALSE
  ) +
  
  scale_x_discrete(labels = feature_set_labels) +
  xlab("Feature Set") +
  ylab(y_axis_label) +
  theme_bw() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.title   = element_text(size = 14),
    axis.text    = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 11),
    panel.border = element_rect(color = "black", size = .75)
  ) +
  # Color legend = training size
  scale_color_manual(
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
  
  scale_y_continuous(
    labels = function(x) strip_leading_zero(x)
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
    "plots_and_tables_output/fig_4_individual_topics",
    eval_set_label_for_saved_file, 
    ".png"
  ),
  width = 8,
  height = 6
)
