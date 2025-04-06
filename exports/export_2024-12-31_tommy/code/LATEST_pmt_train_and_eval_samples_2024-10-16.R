library(data.table) # 1.15.4 on CBS server
library(tidyverse) # 2.0.0 on CBS server

train_path <- "H:/stork_oracle/data/train.csv"
holdout_path <- "H:/stork_oracle/data/holdout_final_leaderboard.csv"
prefer_train_and_eval <- fread(train_path, colClasses = c(RINPERSOON = "character"))
holdout <- fread(holdout_path, colClasses = c(RINPERSOON = "character"))

# NOTE: The official PreFer file, "train.csv", has a column "evaluation_set" 
# indicating 10% of cases that can be used as evaluation data. 
# Therefore, we will refer to the data from this file "prefer_train_and_eval". 
# We will use "train" to refer to the 90% of cases in train.csv that are not part of the eval set.

#### FILTER OUT PEOPLE WITH MISSING OUTCOMES, AND SELECT JUST THE COLUMNS WE NEED TO CREATE SAMPLES ####
prefer_train_and_eval <- prefer_train_and_eval %>%
  filter(!is.na(children_post2021)) %>%
  select(RINPERSOON, evaluation_set)

#### FUNCTION TO GENERATE SAMPLES AND CREATE COLUMNS INDICATING WHO IS IN EACH SAMPLE ####
create_sample_indicators <- function(df, vector_of_sample_sizes, sample_from) {
 
  # Create prefix and filter the data to either the train or eval rows
  # and there is not need to worry about a 50% train set
  if(sample_from == "train") { 
    df <- mutate(df, training_and_evaluation_set = 1) # This is just a column that is 1 for everyone, for when we want to try on the whole train and eval set
    df <- mutate(df, training_set = ifelse(evaluation_set == 0, 1, 0))
    sample_prefix <- "train_sample_n_"
    sampling_df <- df %>% 
      filter(evaluation_set == 0)
    n_evaluation_test_50_percent_split <- NA
  } else if (sample_from == "evaluation") { 
    sample_prefix <- "evaluation_sample_n_"
    sampling_df <- df %>% 
      filter(evaluation_set == 1)
    # Calculate the size of the evaluation_test_50_percent_split set if we are in the evaluation set
    n_evaluation_test_50_percent_split <- nrow(sampling_df) %/% 2
    vector_of_sample_sizes <- c(vector_of_sample_sizes, n_evaluation_test_50_percent_split)
  } else { 
    stop("sample_from must be either `train` or `evaluation`")
  }
  
  # Put the sample sizes in decreasing order
  vector_of_sample_sizes <- sort(vector_of_sample_sizes, decreasing = TRUE)
  
  # Remove any sample sizes that are larger tha nthe sample size of the full data
  if (any(vector_of_sample_sizes > nrow(sampling_df))) {
    vector_of_sample_sizes <- vector_of_sample_sizes[vector_of_sample_sizes <= nrow(sampling_df)]
    warning("One or more sample sizes is larger than the available data, and has been automatically removed from the vector of sample sizes.")
  }
  
  # Take the largest sample, then take progressively smaller samples from within each previous sample
  evaluation_test_50_percent_split_generated <- FALSE # This allows us to prevent bugs when n_eval_test
  # is in the pre-specified set of sample sizes
  for(n in vector_of_sample_sizes) { 
    # Create the new column name 
    if (sample_from == "evaluation" & n == n_evaluation_test_50_percent_split & !evaluation_test_50_percent_split_generated) {
      new_col <- "evaluation_test_50_percent_split"
      evaluation_test_50_percent_split_generated <- TRUE
    } else {
      new_col <- paste0(sample_prefix, format(n, scientific = FALSE))
    }
    
    # Sample n rows from the previously sampled df
    sampling_df <- sampling_df %>%
      slice_sample(n = n) 
    # Create new column indicating which rows are part of the sample
    df <- df %>%
      mutate(!!new_col := ifelse(RINPERSOON %in% sampling_df$RINPERSOON, 1, 0))
  }
  
  # Generate column for eval_selection_50_percent_split if applicable
  if (sample_from == "evaluation") {
    df <- df %>%
      mutate(evaluation_selection_50_percent_split = ifelse(evaluation_set == 0, 0, 1 - evaluation_test_50_percent_split))
  }
  
  return(df)
}

#### FUNCTION TO TEST THE OUTPUT OF CREATE_SAMPLE_INDICATORS ####
check_output_of_create_sample_indicators <- function(df) {
  # There should be no NAs in the file
  NA_values_exist <- any(is.na(df))
  if(NA_values_exist) { 
    stop("There are NA values in the dataframe.")
  } else { 
    print("Check passed: there are no NAs.")
  }
  
  # Other than the RINPERSOON column, all values should be either 0 or 1
  all_0_and_1 <- df %>%
    select(-RINPERSOON) %>%
    summarise(across(everything(), ~ all(. %in% c(0, 1)))) %>%
    unlist() %>%
    all()
  if(!all_0_and_1) {
    stop("Some values outside the RINPERSOON column are not 0 and 1.")
  } else {
    print("Check passed: all values other than RINPERSOON are 0/1")
  }
  
  # If the column name includes a sample size, then the sum of values matches the label 
  for(col in colnames(df)) {
    if(grepl("sample_n", col)) { 
      expected_sample_size <- as.numeric((sub(".*sample_n_", "", col)))
      actual_sum <- sum(df[[col]])
      if(actual_sum != expected_sample_size) { 
        stop(paste("Column", col, "has a sum of", actual_sum, "which is not the expected sample size."))
      } else { 
        print("Check passed: column sums match the expected sample sizes.")
      }
    }
  }
  
  # RINPERSOON values should all be unique
  all_RINPERSOON_values_are_unique <- length(unique(df$RINPERSOON)) == nrow(df)
  if(!all_RINPERSOON_values_are_unique) { 
    stop("RINPERSOON column contains duplicate values.")
  } else { 
    print("Check passed: all RINPERSOON values are unique.")
  }
  
  # All cases in train_sample_n_* should have evaluation_set == 0 
  train_sample_columns <- df %>%
    select(matches("^train_sample_n_"))
  eval_rows <- df$evaluation_set == 1
  train_samples_mistakenly_came_from_evaluation_rows <- any(train_sample_columns[eval_rows] == 1)
  if(train_samples_mistakenly_came_from_evaluation_rows) { 
    stop("Some train samples came from the evaluation set.")
  } else { 
    print("Check passed: All cases train samples come from the train set.")
  }
  
  # All cases in evaluation_sample_n_* should have evaluation_set == 1
  eval_sample_columns <- df %>%
    select(matches("^evaluation_sample_n_"))
  train_rows <- df$evaluation_set == 0
  eval_samples_mistakenly_came_from_train_rows <- any(eval_sample_columns[train_rows] == 1)
  if(eval_samples_mistakenly_came_from_train_rows) { 
    stop("Some evaluation samples came from the train set.")
  } else { 
    print("Check passed: All cases in evaluation samples come from the evaluation set.")
  }
  
  # All cases in train_sample_n_[X] should have train_sample_n_[X+1] == 1 
  # (where X + 1 is the next largest sample size)
  train_col_names <- colnames(train_sample_columns)
  for(i in 1:(length(train_col_names)-1)) { 
    larger_sample_column <- train_col_names[i]
    smaller_sample_column <- train_col_names[i+1]
    row_in_smaller_sample_but_not_in_larger_sample <- 
      any(df[[smaller_sample_column]] == 1 & 
            df[[larger_sample_column]] == 0)
    if(row_in_smaller_sample_but_not_in_larger_sample) { 
      stop("One or more rows in a smaller train sample is not nested within a larger train sample.")
    } else { 
      print("Check passed: train samples are properly nested.")  
    }
  }
  
  # All cases in evaluation_sample_n_[X] should have evaluation_sample_n_[X+1] == 1 
  # (where X + 1 is the next largest sample size)
  eval_sample_columns_and_test_split_column <- df %>%
    select(matches("^evaluation_sample_n_|evaluation_test_50_percent_split"))
  eval_sample_columns_and_test_split_column_names <- colnames(eval_sample_columns_and_test_split_column)
  for(i in 1:(length(eval_sample_columns_and_test_split_column_names)-1)) { 
    larger_sample_column <- eval_sample_columns_and_test_split_column_names[i]
    smaller_sample_column <- eval_sample_columns_and_test_split_column_names[i+1]
    row_in_smaller_sample_but_not_in_larger_sample <- 
      any(df[[smaller_sample_column]] == 1 & 
            df[[larger_sample_column]] == 0)
    if(row_in_smaller_sample_but_not_in_larger_sample) { 
      stop("One or more rows in a smaller evaluation sample is not nested within a larger evaluation sample.")
    } else { 
      print("Check passed: evaluation samples are properly nested.")
    }
  }
  
  # Check that selection and test splits are a partition of the evaluation set,
  # by testing that they always sum to 1 for rows where evaluation_set == 1
  row_sum_of_selection_and_test_set_indicators_within_eval_set <- df %>%
    filter(evaluation_set == 1) %>%
    mutate(row_sum_of_selection_and_test_set_indicators = evaluation_selection_50_percent_split + evaluation_test_50_percent_split) %>%
    pull(row_sum_of_selection_and_test_set_indicators)
  if(!all(row_sum_of_selection_and_test_set_indicators_within_eval_set == 1)) {
    stop("Selection and test columns do not sum to 1 for all rows. They are not a partition of the evaluation set.") 
  } else {
    print("Check passed: Selection and test sets are a partition of the evaluation set.")
  }
  
  # Check that within the training set, evaluation_selection_50_percent_split and evaluation_test_50_percent_split are always 0
  row_sum_of_selection_and_test_set_indicators_within_train_set <- df %>%
    filter(evaluation_set == 0) %>%
    mutate(row_sum_of_selection_and_test_set_indicators = evaluation_selection_50_percent_split + evaluation_test_50_percent_split) %>%
    pull(row_sum_of_selection_and_test_set_indicators)
  if(!all(row_sum_of_selection_and_test_set_indicators_within_train_set == 0)) {
    stop("Some rows sampled for the selection or test set(s) come from the training set") 
  } else {
    print("Check passed: Rows sampled for the selection and test sets never come from the training set.")
  }
  
  # Check that the sizes of the selection set and test set differ by no more than 1 row
  # (their size could differ by 1 row if the evaluation set has an odd number of rows)
  selection_set_size <- sum(df$evaluation_selection_50_percent_split)
  test_set_size <- sum(df$evaluation_test_50_percent_split)
  if(abs(selection_set_size - test_set_size) > 1) {
    stop("The sizes of selection set and test set differ by more than 1 row.")
  } else {
    print("Check passed: The sizes of selection set and test set differ by 1 row or less.")
  }
  
}

#### FUNCTION TO EDIT, TEST, AND SAVE THE SAMPLING DATAFRAME FOR A GIVEN SEED ####
edit_and_save_sample_df <- function(seed) { 
  set.seed(seed)
  
  # Edit and test the dataframe
  pmt_train_samples <- create_sample_indicators(df = prefer_train_and_eval,
                                                vector_of_sample_sizes = train_sample_sizes, 
                                                sample_from = "train")
  
  sum_check <- as.numeric(colSums(dplyr::select(pmt_train_samples, contains("^train_sample_"))))
  assertthat::assert_that(all(sum_check[order(sum_check)] == train_sample_sizes[order(train_sample_sizes)]),
                          msg = paste0("The train sample sizes do not match the expected values for seed ", seed))
  
  pmt_train_and_eval_samples <- create_sample_indicators(df = pmt_train_samples,
                                                         vector_of_sample_sizes = eval_sample_sizes, 
                                                         sample_from = "evaluation")
  
  sum_check <- as.numeric(colSums(dplyr::select(pmt_train_and_eval_samples, contains("^evaluation_sample_"))))
  assertthat::assert_that(all(sum_check[order(sum_check)] == eval_sample_sizes[order(eval_sample_sizes)]),
                          msg = paste0("The evaluation sample sizes do not match the expected values for seed ", seed))
  
  check_output_of_create_sample_indicators(pmt_train_and_eval_samples)
  
  # Add holdout set
  assertthat::assert_that(!any(holdout$RINPERSOON %in% pmt_train_and_eval_samples$RINPERSOON),
                          msg = paste0("The holdout set contains RINPERSOON values that are also in the training and evaluation set for seed ", seed))
  
  assertthat::assert_that(!any(is.na(pmt_train_and_eval_samples)),
                          msg = paste0("There are NAs in the train and eval samples", seed))
  
  # Combine pmt_train_and_eval_samples and holdout with only RINPERSOON column, filling rest with NAs
  pmt_train_and_eval_samples <- pmt_train_and_eval_samples %>%
    mutate(official_holdout_set = 0)
  
  holdout <- holdout %>%
    mutate(official_holdout_set = 1)
  
  pmt_train_and_eval_samples <- bind_rows(
    pmt_train_and_eval_samples, select(holdout, RINPERSOON, official_holdout_set))
  
  # Check that we indeed filled NAs exactly equal to the holdout set size
  assertthat::assert_that(sum(is.na(pmt_train_and_eval_samples)) ==
                          (nrow(holdout) * (ncol(pmt_train_and_eval_samples) - 2)),
                          msg = paste0("The number of NAs in the train and eval samples does not match the size of the binded holdout set for seed ", seed))
  
  pmt_train_and_eval_samples[is.na(pmt_train_and_eval_samples)] <- 0
  
  # Save results 
  file_name <- paste0("pmt_train_and_evaluation_samples_seed_", seed, "_241016_with_holdout.csv")
  pmt_samples_path_to_write <- paste0("H:/pmt/eval/train_and_eval_samples/", file_name)
  fwrite(pmt_train_and_eval_samples, pmt_samples_path_to_write)
  print(paste0("Data file with samples generated from seed ", seed, " has been saved."))
}

#### VECTORS LISTING THE DESIRED SAMPLE SIZES ####
train_sample_sizes <- c(100,
                        200,
                        300,
                        400,
                        500,
                        600,
                        700,
                        800,
                        900,
                        1000,
                        2000, 
                        3000, 
                        4000, 
                        5000, 
                        6000,
                        7000,
                        8000,
                        9000,
                        10000,
                        20000,
                        30000,
                        40000,
                        50000,
                        60000,
                        70000,
                        80000,
                        90000,
                        100000,
                        200000,
                        300000,
                        400000,
                        500000,
                        600000,
                        700000,
                        800000,
                        900000,
                        1000000,
                        1500000,
                        2000000,
                        2500000,
                        3000000,
                        3500000
                        )

eval_sample_sizes <- c(100,
                       500,
                       1000,
                       2000, 
                       3000, 
                       4000, 
                       5000, 
                       10000, 
                       100000)

#### RUN THE FINAL FUNCTION WITH DIFFERENT SEEDS ####
seeds <- c(1:10)
map(seeds, edit_and_save_sample_df)
