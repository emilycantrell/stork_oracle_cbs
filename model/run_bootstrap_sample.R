# For each original/bootstrap sample, calculate metrics
library(tidyverse)
run_bootstrap_sample <- function(bootstrap_sample) {
  source("run_metric.R", local = TRUE)
  # bootstrap_sample No. 1 is always the non-bootstrapped estimate
  if (bootstrap_sample == 1) {
    bootstrapped_evaluation_set_predictions <- evaluation_set_predictions
    bootstrapped_evaluation_set_outcomes <- evaluation_set_outcomes
  } else {
    bootstrapped_indices <- sample(bootstrapping_indices, n_evaluation_set, replace = TRUE)
    bootstrapped_evaluation_set_predictions <- evaluation_set_predictions[bootstrapped_indices]
    bootstrapped_evaluation_set_outcomes <- evaluation_set_outcomes[bootstrapped_indices]
  }
  # See run_metric.R
  run_bootstrap_sample_output <- map(metrics, ~run_metric(.x, bootstrapped_evaluation_set_predictions, bootstrapped_evaluation_set_outcomes, check_threshold_reference_if_applicable))
  tibble(bootstrap_sample = bootstrap_sample, run_bootstrap_sample_output = run_bootstrap_sample_output)
}