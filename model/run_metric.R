# This function produces evaluation metrics given predictions and actual outcomes
library(MLmetrics)
library(tidyverse)
library(pROC)

run_metric <- function(metric, predictions, outcomes, check_threshold_reference_if_applicable) {
  run_metric_LogLoss <- function() {
    MLmetrics::LogLoss(predictions_internal, outcomes)
  }
  run_metric_MSE <- function() {
    MLmetrics::MSE(predictions_internal, outcomes)
  }
  run_metric_In_Sample_R2 <- function() {
    output <- MLmetrics::R2_Score(predictions_internal, outcomes)
    ifelse(is.na(output), -Inf, output)
  }
  run_metric_R2_Holdout <- function() { 
    sum_of_squares_predictions <- sum((outcomes - predictions_internal)^2)
    sum_of_squares_baseline <- sum((outcomes - training_set_outcome_mean)^2)
    output <- 1 - (sum_of_squares_predictions/sum_of_squares_baseline)
    ifelse(is.na(output), -Inf, output)
  }
  run_metric_AUC <- function() {
    # Note: We are using pROC for AUC because MLMetrics sometimes had an overflow error when calculating AUC.
    # Note: in pROC, the order of arguments is flipped compared to MLmetrics: labels first, predictions second
    output <- pROC::auc(outcomes, predictions_internal)
    ifelse(is.na(output), 0, output)
  }
  run_metric_F1_Score <- function() {
    p <- mean(outcomes[predictions_internal == 1])
    r <- mean(predictions_internal[outcomes == 1])
    output <- 2 * (p * r) / (p + r)
    ifelse(is.na(output), 0, output)
  }
  run_metric_Precision <- function() {
    output <- mean(outcomes[predictions_internal == 1])
    ifelse(is.na(output), 0, output)
  }
  run_metric_Recall <- function() {
    output <- mean(predictions_internal[outcomes == 1])
    ifelse(is.na(output), 0, output)
  }
  run_metric_Accuracy <- function() {
    value <- MLmetrics::Accuracy(predictions_internal, outcomes)
    ifelse(is.na(value), 0, value)
  }
  
  # Apply a classification threshold for metrics that require it. The 
  # thresholds are determined in run_step.R.
  if(check_threshold_reference_if_applicable & metric %in% c("F1_Score", "Recall", "Precision", "Accuracy")) {
    winning_threshold <- threshold_reference$winning_threshold[threshold_reference$metric == metric]
    predictions_internal <- ifelse(predictions >= winning_threshold, 1, 0)
    tibble(metric = metric, value = get(paste0("run_metric_", metric))(), threshold = winning_threshold)
  } else {
    predictions_internal <- predictions
    tibble(metric = metric, value = get(paste0("run_metric_", metric))())
  }
  
  
}