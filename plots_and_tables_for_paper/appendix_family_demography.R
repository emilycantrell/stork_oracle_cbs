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
results <- read_excel(here("exports/export_2025-05-05_mark/results_plot6_2025-04-23.xlsx"))

# Filter to desired rows
results <- results %>%
  # Filter to desired evaluation set
  filter(evaluation_set == target_test_set) %>%
  # Remove sanity check results (these were just to confirm things work as expected)
  filter(!str_detect(feature_set, "sanity_check")) %>%
  filter(feature_set != "demography_101") %>%
  # Filter to just n = 3.7 million 
  # Note: the other results in this file using an older method of downsampling, which we 
  # changed in later version of the code. Due to computation time constraints, we did not
  # regenerate the results for this plot using the new downsampling method, so we will just
  # present results for the full training set.
  filter(training_set == "training_set")

feature_set_labels <- c(
  "ego_age" = "Ego's Age",
  "ego_sex" = "Ego's Sex",
  "has_partner" = "Relationship Status",
  "hh_child_ages" = "Children's Ages",
  "hh_child_sexes" = "Children's Sexes", 
  "parity" = "Number of Children",
  "partner_age" = "Partner's Age",
  "partner_sex" = "Partner's Sex")

# Group ego, partner, and child features into three groups
results <- results %>%
  mutate(feature_group = case_when(
    feature_set %in% c("ego_age", "ego_sex") ~ "Ego",
    feature_set %in% c("hh_child_ages", "hh_child_sexes", "parity") ~ "Children",
    feature_set %in% c("partner_age", "partner_sex", "has_partner") ~ "Partner",
    TRUE ~ "Other"
  )) %>%
  mutate(feature_group = factor(
    feature_group,
    levels = c("Ego", "Partner", "Children")
  ))

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

results %>%
  mutate(feature_set = fct_reorder(feature_set, estimate_R2_Holdout, .desc = TRUE)) %>%
  ggplot(aes(x = feature_set, y = estimate_R2_Holdout)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = ci_lower_R2_Holdout, ymax = ci_upper_R2_Holdout),
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  scale_x_discrete(labels = feature_set_labels) +
  xlab("Feature Set") +
  ylab(y_axis_label) +
  facet_grid(. ~ feature_group, scales = "free_x", space = "free_x") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 13, face = "bold"),
    panel.border = element_rect(color = "black", size = .75)
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
    "plots_and_tables_output/appendix_family_demography",
    eval_set_label_for_saved_file, 
    ".png"
  ),
  width = 8,
  height = 6
)