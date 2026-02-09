# This is a configuration file specifying settings for parallelizaton, feature
# choices, train-test splits, modeling options, and results output

# Set up
library(tidyverse)

# Parallelization settings
seed_job <- 0
seed_worker <- 0
workers_grid_row <- 3
workers_metric_for_selecting_pipelines <- 3
#n_thread_within_worker <- -1

# Feature choices
# NB: make sure to use equal signs here, not arrows
feature_set_settings <- list(
  without_leakage = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS_without_leakage", "prefer_official_train", "FAMILIENETWERKTAB", "live_in_partner"),
  prefer_official_train = c("prefer_official_train"),
  AS_FS = c("family_age_and_sex_from_prefer_submission", "family_structure"),
  ego_AS = c("ego_AS")
)

# Train-test splits 
sampling_files <- c("dms_samples_seed_4_subsamples_1.csv",
                    "dms_samples_seed_4_subsamples_2.csv",
                    "dms_samples_seed_4_subsamples_3.csv",
                    "dms_samples_seed_4_subsamples_4.csv",
                    "dms_samples_seed_4_subsamples_5.csv",
                    "dms_samples_seed_5_subsamples_1.csv",
                    "dms_samples_seed_5_subsamples_2.csv",
                    "dms_samples_seed_5_subsamples_3.csv",
                    "dms_samples_seed_5_subsamples_4.csv",
                    "dms_samples_seed_5_subsamples_5.csv")
data_splits <- bind_rows(
  expand_grid(
    training_sets = c("train_sample_n_100",
                      "train_sample_n_200",
                      "train_sample_n_300",
                      "train_sample_n_400",
                      "train_sample_n_500",
                      "train_sample_n_600",
                      "train_sample_n_700",
                      "train_sample_n_800",
                      "train_sample_n_900",
                      "train_sample_n_1000",
                      "train_sample_n_2000",
                      "train_sample_n_3000",
                      "train_sample_n_4000",
                      "train_sample_n_5000",
                      "train_sample_n_6000",
                      "train_sample_n_7000",
                      "train_sample_n_8000",
                      "train_sample_n_9000",
                      "train_sample_n_10000"),
    selection_sets = c("evaluation_selection_50_percent_split"), # Evaluation sets we use to select the best hyperparameter combination and classification threshold (special issue paper has no hyperparameter tuning)
    test_sets = c("evaluation_test_50_percent_split", "official_holdout_set") # Evaluation sets we use for producing evaluation metrics for the paper.
  )
)

# Modeling options
model_settings <- list(
  # These are default catboost settings, and these are the only model-
  # hyperparameter combination we have time to explore, but our code supports
  # tuning other models and hyperparameter options, as commented out below
  catboost = list(tibble(grid_row = pmap(expand_grid(
    learning_rate = c(NA),
    subsample = c(.8),
    depth = c(6)
  ),
  list)),
  steps = c(1000)) # how many trees in catboost?
  # catboost = list(tibble(grid_row = pmap(expand_grid(
  #   learning_rate = c(.009, .03, .09, .3, .9, NA),
  #   subsample = c(.2, .5, .8, 1),
  #   depth = c(1, 2, 4, 6, 8, 10)
  # ),
  # list)),
  # steps = c(200, 400, 600, 800, 1000)),
  # xgboost = list(tibble(grid_row = pmap(expand_grid(
  #   eta = c(.009, .03, .09, .3, .9),
  #   subsample = c(.2, .5, .8, 1),
  #   max_depth = c(1, 2, 4, 6, 8, 10)
  # ),
  # list)),
  # steps = c(200, 400, 600, 800, 1000)),
  # elastic_net = list(tibble(grid_row = pmap(bind_rows(expand_grid(
  #   alpha = c(0, .15, .3, .5, .7, .85, 1),
  #   lambda = c(NA)
  # ),
  # tibble(alpha = 1, lambda = 0)),
  # list)),
  # steps = c(1, 2, 3, 4, 5))
  )
n_grid_row <- 1 # how many hyperparameter combinations to sample from expanded
# grid?

# Performance metrics
metrics_for_all_pipelines <- c("LogLoss", "MSE", "R2_Holdout") 
metrics_for_selecting_pipelines <- c("LogLoss") # We use these metrics to select the best hyperparameter combination, as defined by "grid_row" and "steps" above, for each model saved
metrics_for_winning_pipelines <- c("F1_Score", "Accuracy")
threshold_increment <- .01
n_bootstrap <- 2 

# Save results
save_only_winning_hyperparameter_draw_results <- FALSE
results_path <- "results/results_extrapolation_seeds_4-5_2025-08.csv"