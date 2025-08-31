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
sampling_files <- c("dms_samples_seed_2.csv")
data_splits <- bind_rows(
  expand_grid(
    training_sets = c("train_sample_n_100",
                      "train_sample_n_200",
                      "train_sample_n_500",
                      "train_sample_n_1000",
                      "train_sample_n_2000",
                      "train_sample_n_5000",
                      "train_sample_n_10000",
                      "train_sample_n_20000",
                      "train_sample_n_50000",
                      "train_sample_n_100000",
                      "train_sample_n_200000",
                      "train_sample_n_500000",
                      "train_sample_n_1000000",
                      "train_sample_n_2000000",
                      "training_set"),
    selection_sets = c("evaluation_selection_50_percent_split"), # Evaluation sets we use to select the best pipelines
    test_sets = c("evaluation_test_50_percent_split", "official_holdout_set") # Evaluation sets we use for holdout evaluations.
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
metrics_for_all_pipelines <- c("LogLoss", "MSE", "R2_Holdout") # Deciles_for_Calibration
metrics_for_selecting_pipelines <- c("LogLoss")
metrics_for_winning_pipelines <- c("F1_Score", "Accuracy") # F1_Score
threshold_increment <- .01
n_bootstrap <- 1000 

save_only_winning_hyperparameter_draw_results <- FALSE
results_path <- "results_learning_curves_seed_2_125_2025-08.csv"