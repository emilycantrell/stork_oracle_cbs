# For each possible value of classification threshold, get performance measures
# in terms of classification metrics.

library(tidyverse)

run_threshold <- function(threshold) {
 
  source("run_metric.R", local = TRUE)
  
  selection_set_label_predictions <- ifelse(selection_set_predictions >= threshold, 1, 0)
  # See run_metric.R
  run_threshold_output <- map(metrics_for_winning_pipelines, ~run_metric(.x, selection_set_label_predictions, selection_set_outcomes, FALSE))
  tibble(threshold = threshold, run_threshold_output = run_threshold_output)
}