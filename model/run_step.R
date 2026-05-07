# For each step option as specified in the jobfile, or for the best step
# option if we already found the best hyperparameter combination, we apply 
# run_step to make predictions for each relevant evaluation set. We then 
# evaluate the predictions.
# In addition, if we have already picked a best hyperparameter combination 
# using the selection set, we can now determine the best classification 
# threshold for each classification metric.
library(catboost)
library(xgboost)
library(glmnet)
library(tidyverse)

run_step <- function(step) {
  source("run_threshold.R", local = TRUE)
  source("run_evaluation_set.R", local = TRUE)
  
  # Make predictions for the evaluation sets, starting with a placeholder
  evaluation_sets_predictions <- NULL
  
  # Prediction function for catboost
  predict_for_catboost <- function() {
    evaluation_sets_predictions <<- catboost.predict(model_fit, evaluation_sets_data, prediction_type = "Probability", ntree_end = step, thread_count = 1)
  }
  
  # Prediction function for xgboost, not relevant to special issue paper
  predict_for_xgboost <- function() {
    evaluation_sets_predictions <<- predict(model_fit, evaluation_sets_data, iterationrange = c(1, step + 1))
  }
  
  # Prediction function for xgboost, not relevant to special issue paper
  predict_for_elastic_net <- function() {
    evaluation_sets_predictions <<- predict(model_fit, evaluation_sets_data, s = model_fit$lambda[[step]], type = "response")
  }
  
  # Make predictions for evaluation sets using the function corresponding to
  # the model
  get(paste0("predict_for_", model))()
  
  # If we have already picked a best hyperparameter combination using the 
  # selection set, we calculate classification metrics at multiple
  # classification thresholds as defined in the jobfile, and pick the best
  # threshold on the selection set. See run_threshold.R.
  if (!is.null(selection_set)) {
    selection_set_predictions <- evaluation_sets_predictions[evaluation_sets_rinpersoon %in% selection_set_rinpersoon]
    thresholds <- seq(min(selection_set_predictions), max(selection_set_predictions) + threshold_increment, threshold_increment)
    threshold_reference <- map(thresholds, run_threshold) %>%
      list_rbind() %>%
      unnest(run_threshold_output) %>%
      group_by(metric) %>%
      summarize(winning_threshold = sample(threshold[value == max(value)], 1))
  }
  
  # Calculate metrics for each evaluation set. See run_evaluation_set.R
  run_step_output <- map(evaluation_sets, run_evaluation_set)
  tibble(step = step, run_step_output = run_step_output)
}