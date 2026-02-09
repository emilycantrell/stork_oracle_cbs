library(tidyverse)

# This file creates a data frame where each row defines a feature_set-sampling_
# file-training_set-model-hyperparameter_grid_row combination. A grid_row
# refers to one row in the hyperparameter tuning grid as defined in the
# jobfile. The grids include all hyperparameters except what we call "step"
# hyperparameters, which include the number of trees for catboost and xgboost
# as well as lambda for glmnet. Tuning these hyperparameters do not require
# training a model from scratch, so they are handled somewhat separately in
# run_grid_row.R as well as run_step.R

feature_set <- names(feature_set_settings)
training_set <- unique(data_splits$training_sets)
models <-names(model_settings)
data_settings_df <- expand_grid(feature_set, sampling_files, training_set, models) %>%
  rename(sampling_file = sampling_files, model = models)
model_settings_df <-map(models, ~mutate(model_settings[[.x]][[1]], model = .x)) %>%
  list_rbind()
grid_rows <- full_join(data_settings_df, model_settings_df, by = "model", relationship = "many-to-many") %>%
  group_by(feature_set, sampling_file, training_set, model) %>%
  slice_sample(n = n_grid_row) %>%
  ungroup()
