# Once models are fitted and selection metrics are evaluated in selection sets,
# the function run_metric_for_selecting_pipelines(), defined here, selects the
# best pipeline. This file runs the function for each feature_set-sampling_
# file-training_set-model-selection_set combination. Then the file calls 
# run_grid_row() again on the best pipelines to get classification thresholds 
# and prediction metrics not calculated first time around. The code then saves 
# all of the results
library(tidyverse)

# For each feature_set-sampling_file-training_set-model-selection_set
# combination, this function picks the best hyperparameter combination. (For
# the special issue paper, there is no hyperparameter tunng
run_metric_for_selecting_pipelines <- function(selection_metric) {
  estimate_selection_metric <- sym(paste0("estimate_", selection_metric))
  if (selection_metric %in% c("LogLoss", "MSE")) {
    best <- min
  } else {
    best <- max
  }
  run_selection_metric_output <- group_by(run_grid_row_outputs_for_selection, feature_set, sampling_file, training_set, model, selection_set) %>%
    filter(!!estimate_selection_metric == best(!!estimate_selection_metric)) %>%
    slice_sample()
  tibble(selection_metric = selection_metric, run_selection_metric_output = run_selection_metric_output)
}
# Produce a dataframe run_grid_row_outputs_for_selection with one row for each
# For each feature_set-sampling_file-training_set-model-selection_set 
# combination
run_grid_row_outputs_for_selection <- rename(run_grid_row_outputs, selection_set = evaluation_set)
if (!save_only_winning_hyperparameter_draw_results) { # Always TRUE for special issue paper
  run_grid_row_outputs_for_selection <- distinct(data_splits, training_sets, selection_sets) %>%
    rename(training_set = training_sets, selection_set = selection_sets) %>%
    left_join(run_grid_row_outputs_for_selection, by = c("training_set", "selection_set"), relationship = "one-to-many")
}
set.seed(0)
# Run the function to pick the best hyperparameter combination
run_selection_metric_outputs <- map(metrics_for_selecting_pipelines, run_metric_for_selecting_pipelines) %>%
  list_rbind() %>%
  unnest(run_selection_metric_output)

# Run run_grid_row for the second round to get metrics that require a
# classification threshold. See run_grid_row.R.
plan(multisession, workers = workers_metric_for_selecting_pipelines)
run_winning_grid_rows_outputs <- select(run_selection_metric_outputs, feature_set, sampling_file, training_set, model, grid_row, selection_set) %>%
  future_pmap(~run_grid_row(..., metrics_for_all_pipelines = metrics_for_all_pipelines, metrics_for_winning_pipelines = metrics_for_winning_pipelines, n_bootstrap = n_bootstrap, threshold_increment = threshold_increment), .options = furrr_options(seed = TRUE)) %>%
  list_rbind() %>%
  select(-grid_row) %>%
  rename(grid_row = grid_row_text) %>%
  unnest(run_grid_row_output) %>%
  unnest(run_step_output)
run_grid_row_outputs <- select(run_grid_row_outputs, -grid_row) %>%
  rename(grid_row = grid_row_text)
if(save_only_winning_hyperparameter_draw_results) { # Never TRUE for the special issues paper
  old_rows <- filter(run_winning_grid_rows_outputs, new_row == 0) %>%
    select(-ends_with(metrics_for_all_pipelines))
} else {
  old_rows <- run_winning_grid_rows_outputs
}
results <- full_join(run_grid_row_outputs, old_rows, by = c("feature_set", "sampling_file", "training_set", "model", "grid_row", "step", "evaluation_set"), relationship = "many-to-many")
if(save_only_winning_hyperparameter_draw_results) {
  new_rows <- filter(run_winning_grid_rows_outputs, new_row == 1)
  results <- filter(results, !is.na(selection_set)) %>%
    bind_rows(new_rows) %>%
    select(-new_row)
} 
fwrite(results, results_path)
