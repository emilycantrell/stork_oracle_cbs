library(groundhog)

#### CHOOSE TEST SET ####

# The default in all scripts is evaluation_test_50_percent_split. Also, by
# default, feature_sets_to_plot_for_sample_size does not include "AS_FS". To 
# use the official holdout results instead, uncomment the appropriate line 
# below and it will override the default in all scripts.
# Note: we have not tested the official_holdout_set and AS_FS option yet since 
# we will not examine holdout results until after reviewer feedback.
target_test_set <- "evaluation_test_50_percent_split"
feature_sets_to_plot_for_sample_size <- 
  c("without_leakage", "prefer_official_train", "ego_AS")
# or
# target_test_set <- "official_holdout_set"
# feature_sets_to_plot_for_sample_size <- 
#   c("without_leakage", "prefer_official_train", "AS_FS", "ego_AS")

stopifnot(
  target_test_set %in% c(
    "evaluation_test_50_percent_split",
    "official_holdout_set"
  ) &
    identical(
      feature_sets_to_plot_for_sample_size,
      c("without_leakage", "prefer_official_train", "ego_AS")
    ) |
    identical(
      feature_sets_to_plot_for_sample_size,
      c("without_leakage", "prefer_official_train", "AS_FS", "ego_AS")
    )
)

#### LOAD PACKAGES ####
options(run_all_loaded_packages = TRUE) # This will override package loading in individual scripts

packages <- c(
  "tidyverse",
  "ggplot2",
  "scales",
  "forcats",
  "ggtext",
  "here",
  "readxl",
  "png",
  "rlang",
  "nlsr",
  "minpack.lm",
  "patchwork",
  "cowplot",
  "knitr",
  "kableExtra",
  "pBrackets"
)

groundhog.library(packages, "2025-11-15")
here() %>%
  paste0("/analysis") %>%
  setwd()

#### CREATE OUTPUT DIRECTORY ####

# Create plots_and_tables_output directory if it does not exist
output_dir <- here("analysis/plots_and_tables_output")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

#### RUN SCRIPTS ####

#### FILE LIST ####

file_names <- c(
  "fig_1_visual_abstract_extrapolation.R",
  "fig_1_visual_abstract_feature_sets.R",
  "fig_2_and_table_1_learning_curves.R",
  "fig_3_and_table_2_and_appendix_extrapolation.R",
  "fig_4_individual_topics.R",
  "fig_5_baseline_plus_topics.R",
  "fig_6_sample_size_feature_set_interdependence.R", 
  "appendix_feature_description.R",
  "appendix_baseline_plus_topics.R",
  "appendix_family_demography.R"
)

#### RUN ALL FILES ####

for (file in file_names) {
  
  message("--------------------------------------------------")
  message("Running: ", file)
  message("--------------------------------------------------")
  
  source(file, local = FALSE)
  
  # Clear everything except global controls
  rm(list = setdiff(
    ls(envir = .GlobalEnv),
    c("target_test_set", "feature_sets_to_plot_for_sample_size", "output_dir")
  ))
  
  gc()
}

message("All files completed successfully.")