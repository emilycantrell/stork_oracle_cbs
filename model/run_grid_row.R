# Set up
library(tidymodels)
library(catboost)
library(xgboost)
library(glmnet)
library(tidyverse)

# This function in this file is a worker function that can get parallelized 
# across multiple machines. Iterations of this function are run for two 
# separate rounds, as will be explained below.
# A grid_row refers to one row in the hyperparameter tuning grid as defined in
# the jobfile. The grids include all hyperparameters except what we call "step"
# hyperparameters, which include the number of trees for catboost and xgboost
# as well as automatically generated lambdas in glmnet. Tuning these 
# hyperparameters do not require training a model from scratch, so they are 
# handled somewhat separately in this file as well as run_step.R.


# On Line 291 of this file, the function run_grid_row() is run for a first 
# round for each feature_set-sampling_file-training_set-grid_row combination 
# (each row in output of chunking.R for the special issue paper) to get 
# bootstrapped performance metrics for each evaluation set. The list of 
# evaluation sets is defined on Lines 82-119 and ultimately draws from jobfile
# settings.

# On Line 43 of run_metric_for_selecting_pipelines.R, this function is run for
# a second round for each 
# feature_set-sampling_file-training_set-grid_row-selection_set combination
# to get additional bootstrapped performance metrics for each evaluation set.
# The main goal of this second round for the special issue paper is to compute
# metrics that require a classification threshold. We calculate these metrics
# only for the hyperparameter combination that performed the best according to 
# metrics_for_selecting_pipelines as defined in the jobfile. (We decided not to 
# calculate a threshold for all hyperparameter combinations to save compute). 
# We couldn't get these measures the first time around because if we were to
# have hyperparameter tuning for the special issue paper, we couldn't decide 
# what the best hyperparameter combination is without analyzing the performance 
# in a selection set.
run_grid_row <- function(feature_set, sampling_file, training_set, model, grid_row, step, selection_set, metrics_for_all_pipelines, metrics_for_winning_pipelines, n_bootstrap, threshold_increment) {
  print(Sys.getpid())
  set.seed(seed_worker)
  bootstrap_samples <- 1:(n_bootstrap+1)
  this_feature_set <- feature_set
  this_sampling_file <- sampling_file
  this_training_set <- training_set
  this_model <- model
  this_selection_set <- selection_set
  source("run_step.R", local = TRUE)
  
  # Subset to the right columns
  names_of_features_in_the_selected_feature_set <- metadata %>%
    filter(rowSums(select(., all_of(paste0("feature_set_", feature_set_settings[[feature_set]])))) > 0) %>%
    pull(variable_name)
  feature_set_data <- data %>%
    select(RINPERSOON, all_of(names_of_features_in_the_selected_feature_set))
  
  # Subset to the right rows for the training set
  sampling_file_path <- paste0(sampling_files_path, sampling_file)
  sampling_file_content <- fread(sampling_file_path, colClasses = c(RINPERSOON = "character"))
  # This function gets rinpersoons for a named subset of the data, as defined
  # in the sampling file
  get_rinpersoon <- function(sets) {
    filter(sampling_file_content, rowSums(select(sampling_file_content, all_of(sets))) > 0) %>%
      pull(RINPERSOON)
  }
  training_set_rinpersoon <- get_rinpersoon(training_set)
  training_set_data <- feature_set_data %>%
    filter(RINPERSOON %in% training_set_rinpersoon)
  # Remove columns with zero variance in training set
  zv <- recipe(training_set_data) %>%
    step_zv() %>%
    prep(training_set_data, strings_as_factors = "FALSE")
  # Save the remove_zero_variance recipe. The saved file is never actually 
  # used.
  saveRDS(zv, "data/remove_zero_variance.RDS")
  # Apply recipe for removing zero variance
  training_set_data <- bake(zv, training_set_data)
  training_set_outcomes <- outcome_data$outcome[outcome_data$RINPERSOON %in% training_set_rinpersoon]
  # Save the mean of the training set outcome for use in R2_Holdout calculation (see run_metric file)
  training_set_outcome_mean <- mean(training_set_outcomes)
  
  # Subset to the right rows for the evaluation sets
  data_splits_related_to_training_set <- 
    filter(data_splits, training_sets == training_set)
  # Selection sets are evaluation sets we use to select the best hyperparameter 
  # combination and/or a best classification threshold (For the special issue 
  # paper, there is no hyperparameter tuning, so we would just be picking the 
  # best the best thresholds for F1 and accuracy, as in run_step.R). Selection 
  # sets were defined in the jobfile.
  selection_sets <- pull(data_splits_related_to_training_set, selection_sets) %>%
    unique()
  # For the special issue paper, this first if condition is always FALSE
  if (save_only_winning_hyperparameter_draw_results | !"test_sets" %in% colnames(data_splits_related_to_training_set)) {
    first_round_evaluation_sets <- selection_sets
  } else {
    # Test sets are evaluation sets we use to producing evaluation metrics for 
    # the paper. They were also defined in the jobfile.
    test_sets <- pull(data_splits_related_to_training_set, test_sets)
    # For the special issue paper, evaluation sets for the first round contains 
    # both the selection set and all test sets
    first_round_evaluation_sets <- unique(c(selection_sets, test_sets))
  }
  if(is.null(selection_set)) {
    evaluation_sets <- first_round_evaluation_sets
  } else {
    # For the special issue paper, this if condition is always FALSE
    if (!"test_sets" %in% colnames(data_splits_related_to_training_set)) {
      evaluation_sets <- selection_set
    } else {
      # For the special issue paper, evaluation sets for the second round 
      # contains both the selection set and the test sets specific to the 
      # selection_set in question
      test_sets <- filter(data_splits, training_sets == training_set, selection_sets == selection_set) %>%
        pull(test_sets)
      evaluation_sets <- unique(c(selection_set, test_sets))
    }
    selection_set_rinpersoon <- get_rinpersoon(selection_set)
    selection_set_outcomes <- outcome_data$outcome[outcome_data$RINPERSOON %in% selection_set_rinpersoon]
  }
  evaluation_sets_rinpersoon <- get_rinpersoon(evaluation_sets)
  evaluation_sets_data <- filter(feature_set_data, RINPERSOON %in% evaluation_sets_rinpersoon)
  evaluation_sets_data <- bake(zv, evaluation_sets_data)
  
  # This function conducts some final preprocessing steps for catboost, like getting rid of RINPERSOON from the data
  preprocess_data_for_catboost <- function() {
    recipe_for_model <- recipe(training_set_data) %>%
      step_rm(RINPERSOON, any_of(binary_one_hot_variables)) %>% # binary_one_hot_variables == NULL for speical issue paper
      prep(training_set_data)
    # Generate the pools
    training_set_data <<- catboost.load_pool(data = bake(recipe_for_model, training_set_data),
                                             label = training_set_outcomes)
    evaluation_sets_data <<- catboost.load_pool(data = bake(recipe_for_model, evaluation_sets_data))
  }

  # This function conducts some final preprocessing steps for xgboost, not relevant to the special issue paper
  preprocess_data_for_xgboost <- function() {
    recipe_for_model <- recipe(training_set_data) %>%
      step_rm(RINPERSOON, any_of(categorical_variables)) %>%
      prep(training_set_data)
    # Generate the pools
    training_set_data <<- xgb.DMatrix(data = as.matrix(bake(recipe_for_model, training_set_data)),
                                      label = training_set_outcomes)
    evaluation_sets_data <<- xgb.DMatrix(data = as.matrix(bake(recipe_for_model, evaluation_sets_data)))
  }
  
  # This function conducts some final preprocessing steps for elastic net, not relevant to the special issue paper
  preprocess_data_for_elastic_net <- function() {
    recipe_for_model <- recipe(training_set_data) %>%
      step_rm(RINPERSOON, any_of(categorical_variables)) %>%
      step_impute_mean(any_of(continuous_variables)) %>%
      prep(training_set_data)
    training_set_data <<- bake(recipe_for_model, training_set_data) %>%
      as.matrix()
    evaluation_sets_data <<- bake(recipe_for_model, evaluation_sets_data) %>%
        as.matrix()
  }
  
  # Run the preprocessing function depending on what the model is
  get(paste0("preprocess_data_for_", model))() 
  
  # Placeholders
  steps <- NULL
  model_fit <- NULL
  
  # Construct a string that uniquely identify a 
  # feature_set-sampling_file-training_set-grid_row, we will save the resulting
  # modeling file so we don't have to refit the model during the second round
  grid_row_text <- paste(names(grid_row), grid_row, sep = "=", collapse = ",")
  model_path <- paste(feature_set, sampling_file, training_set, model, grid_row_text, sep = "_")
  model_path <- paste0("data/", model_path)
  
  # Get model for catboost
  get_model_for_catboost <- function(learning_rate, subsample, depth) {
    # During the first round, we actually fit the model
    if (is.null(selection_set)) {
      steps <<- model_settings[[model]]$steps # Always 1000 for special issue
      if (is.na(learning_rate)) { # Always TRUE for special issue
        learning_rate_internal <- NULL # Catboost determines learning rate automatically
        steps_internal <- max(model_settings[[model]]$steps) # Always 1000 for special issue
      } else {
        learning_rate_internal <- learning_rate
        steps_internal <- max(steps)
      }
      # Train the model
      fit_start <- Sys.time()
      print(paste("Fitting started for", model_path))
      print(fit_start)
      model_fit <<- catboost.train(training_set_data, params = list(
        loss_function = "Logloss",
        iterations = steps_internal,
        learning_rate = learning_rate_internal,
        subsample = subsample,
        depth = depth,
        # thread_count = n_thread_within_worker, # this is commented out because it raised the error: Catboost can't parse parameter "thread_count" with value: -1
        logging_level = "Silent"
      ))
      fit_end <- Sys.time()
      print(paste("Fitting ended for", model_path))
      print(fit_end)
      print(paste("Fitting time for", model_path))
      print(fit_end - fit_start)
      # Save the model
      catboost.save_model(model_fit, model_path)
    } else {
      # During the second round, we read the saved model
      steps <<- filter(run_selection_metric_outputs, run_selection_metric_outputs$feature_set == this_feature_set, run_selection_metric_outputs$sampling_file == this_sampling_file, run_selection_metric_outputs$training_set == this_training_set, run_selection_metric_outputs$model == this_model, run_selection_metric_outputs$selection_set == this_selection_set) %>%
        pull(step) # Always 1000 for special issue
      model_fit <<- catboost.load_model(model_path)
    }
  }
  
  # Get model for xgboost, not relevant for the special issue paper
  get_model_for_xgboost <- function(eta, subsample, max_depth) {
    if (is.null(selection_set)) {
      # Train the model
      steps <<- model_settings[[model]]$steps
      model_fit <<- xgb.train(
        params = list(
          objective = "binary:logistic", 
          eta = eta,
          subsample = subsample,
          max_depth = max_depth,
          nthread = 1
        ), 
        data = training_set_data,
        nrounds = max(steps)
      )
      xgb.save(model_fit, model_path)
    } else {
      steps <<- filter(run_selection_metric_outputs, run_selection_metric_outputs$feature_set == this_feature_set, run_selection_metric_outputs$sampling_file == this_sampling_file, run_selection_metric_outputs$training_set == this_training_set, run_selection_metric_outputs$model == this_model, run_selection_metric_outputs$selection_set == this_selection_set) %>%
        pull(step)
      model_fit <<- xgb.load(model_path)
    }
    
  }
  
  # Get model for elastic net, not relevant for the special issue paper
  get_model_for_elastic_net <- function(alpha, lambda) {
    if (is.null(selection_set)) {
      if (is.na(lambda)) {
        lambda_internal <- NULL
        steps_internal <- max(model_settings[[model]]$steps)
        steps <<- model_settings[[model]]$steps
      } else {
        lambda_internal <- lambda
        steps_internal <- 1
        steps <<- 1
      }
      
      model_fit <<- glmnet(training_set_data, training_set_outcomes,
                           family = "binomial",
                           alpha = alpha,
                           lambda = lambda_internal,
                           nlambda = steps_internal)
      saveRDS(model_fit, model_path)
    } else {
      steps <<- filter(run_selection_metric_outputs, run_selection_metric_outputs$feature_set == this_feature_set, run_selection_metric_outputs$sampling_file == this_sampling_file, run_selection_metric_outputs$training_set == this_training_set, run_selection_metric_outputs$model == this_model, run_selection_metric_outputs$selection_set == this_selection_set) %>%
        pull(step)
      model_fit <<- readRDS(model_path)
    }
  }
  
  # Run the preprocessing function depending on what the model is
  do.call(get(paste0("get_model_for_", model)), grid_row)
  
  # For each model, produce evaluation metrics at each step value, which refers
  # to the number of trees in a catboost or xgboost model or an automatically
  # generated lambda in glmnet. See run_step.R. For the special issue paper,
  # this is not really iterated because we only look at one step value, 
  # which corresponds to 1000 trees for catboost.
  steps_start <- Sys.time()
  print(paste("run_steps started for", model_path))
  print(steps_start)
  run_grid_row_output <- map(steps, run_step)
  steps_end <- Sys.time()
  print(paste("run_steps ended for", model_path))
  print(steps_end)
  print(paste("Time for run_steps to run for", model_path))
  print(steps_end - steps_start)
  run_grid_row_output <- tibble(feature_set = feature_set, sampling_file = sampling_file, training_set = training_set, model = model, grid_row = list(grid_row), grid_row_text = grid_row_text, run_grid_row_output = run_grid_row_output)
  if (is.null(selection_set)) {
    run_grid_row_output
  } else {
    mutate(run_grid_row_output, selection_set = selection_set)
  }
}

# Run the function in parallel
options(future.globals.maxSize = +Inf)
plan(multisession, workers = workers_grid_row)
run_grid_row_outputs <- future_pmap(grid_rows, ~run_grid_row(..., selection_set = NULL, metrics_for_all_pipelines = metrics_for_all_pipelines, metrics_for_winning_pipelines = metrics_for_winning_pipelines, n_bootstrap = n_bootstrap, threshold_increment = NULL), .options = furrr_options(seed = TRUE, scheduling = Inf)) %>%
  list_rbind() %>%
  unnest(run_grid_row_output) %>%
  unnest(run_step_output)
select(run_grid_row_outputs, -grid_row) %>%
  rename(grid_row = grid_row_text) %>%
  fwrite(sub("/", "/intermediate_", results_path))
