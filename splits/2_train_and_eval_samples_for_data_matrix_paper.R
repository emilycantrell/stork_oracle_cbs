library(data.table) 
library(tidyverse)

# This file creates new sampling files for our paper on data matrix size with the Stork Oracle model.
# The goals are:
# (1) create new sampling files so that we have multiple seeds for each training set size; 
# these samples will NOT be nested within each other (whereas previously we had nested samples).
# (2) create new sampling files with multiple subsamples at each <10k sample size nested within 
# the 10k sample, for use in our learning curve extrapolation.

# Please start by setting the working directory to the main directory, the one 
# containing folders `data`, `results`, and others. Not that the directory gets
# set to `data` on Line 183

# For efficiency, this file primarily uses data.table syntax instead of dplyr.

# Dependency versions cannot be pinned because the CBS environment only allows us 
# to use whatever package versions are currently installed.

#### LOAD TRAINING & HOLDOUT FILES FROM LISA ####

# This path should go to the train.csv file provided by Lisa
train_path <- "data/train.csv"
prefer_train_and_eval <- fread(train_path, colClasses = c(RINPERSOON = "character"))

# NOTE: The official PreFer file, "train.csv", has a column "evaluation_set" 
# indicating 10% of cases that can be used as evaluation data. 
# Therefore, we will refer to the data from this file "prefer_train_and_eval". 
# We will use "train" to refer to the 90% of cases in train.csv that are not part of the eval set.

# Filter out people with missing outcomes, and select just the columns we need to create samples
prefer_train_and_eval <- prefer_train_and_eval[
  !is.na(children_post2021), 
  .(RINPERSOON, evaluation_set)
]

gc()

# This path should go to holdout_final_leaderboard.csv from Lisa
holdout_path <- "data/holdout_final_leaderboard.csv" 
prefer_official_holdout <- fread(holdout_path, colClasses = c(RINPERSOON = "character"))

# Select just the ID column
prefer_official_holdout <- prefer_official_holdout[ ,.(RINPERSOON)]

# Add a column called "official_holdout_set"
prefer_official_holdout[, official_holdout_set := 1]

gc()

#### CREATE TRAINING_SET COLUMN ####
# training_set is the 3.7 million training set, i.e., rows of the "Lisa file" that are not in the eval set
prefer_train_and_eval[, training_set := as.integer(evaluation_set == 0)]

#### GET EVAL SPLITS FROM OUR ORIGINAL SAMPLING FILE ####

# This path should go to pmt_train_and_evaluation_samples_seed_1_241016.csv
original_sampling_file_path <- "data/train_and_evaluation_samples_seed_1_241016.csv"
original_sampling_file <- fread(original_sampling_file_path, colClasses = c(RINPERSOON = "character"))

# Extract the evaluation splits
eval_split_cols <- original_sampling_file[, .(RINPERSOON, 
                                              evaluation_selection_50_percent_split, 
                                              evaluation_test_50_percent_split)]

# Remove the original sampling file to save memory
rm(original_sampling_file) 
gc() 

#### CREATE VECTOR LISTING THE DESIRED SAMPLE SIZES ####
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
                        2000000,
                        3000000)

#### GENERATE SAMPLES FOR ALL SAMPLE SIZES ####
# This section generates sampling files with random (non-nested) samples for all sample sizes

# Create function
generate_main_samples <- function(seed){ 
  # Filter training pool
  training_pool <- prefer_train_and_eval[training_set == 1, .(RINPERSOON)]
  
  # Number of available training rows
  n_total <- nrow(training_pool)
  
  # Loop over sizes and create binary columns
  set.seed(seed)
  for (n in train_sample_sizes) {
    colname <- paste0("train_sample_n_", format(n, scientific = FALSE))
    if (n > n_total) {
      stop("Sample size exceeds available training rows")
    } else {
      # Randomly select n row indices from the training pool
      selected_rows <- sample.int(n_total, n)
      # Create an integer vector of 0s (length = total number of training rows)
      indicator <- integer(n_total)
      # Set the positions of the sampled rows to 1
      indicator[selected_rows] <- 1
      # Add a new column to the data.table with 1s for selected rows and 0s elsewhere
      training_pool[, (colname) := indicator]
    }
  }
  
  # Merge the new samples with the train and eval data from Lisa
  samples_df <- merge(
    prefer_train_and_eval,
    training_pool,
    by = "RINPERSOON",
    all.x = TRUE,
    sort = FALSE
  )
  
  # Merge in the evaluation splits from our original sampling file
  samples_df <- merge(
    samples_df,
    eval_split_cols,
    by = "RINPERSOON",
    all.x = TRUE,
    sort = FALSE
  )
  
  # Combine with the official holdout set
  samples_df <- rbindlist(
    list(samples_df, prefer_official_holdout),
    fill = TRUE,
    use.names = TRUE
  )
  
  # Fill NA train_sample_n_* columns with 0 
  # Note: NA values were introduced when merging the training pool with prefer_train_and_eval, which as more rows, 
  # and when appending the holdout set
  for (col in names(samples_df)) {
    set(samples_df, i = which(is.na(samples_df[[col]])), j = col, value = 0L)
  }
  
  # Save the file
  file_name <- sprintf("dms_samples_seed_%d.csv", seed)
  fwrite(samples_df, file = file_name)
  
  # Record the file name in order to read it back in later
  vector_of_main_sampling_file_names <<- c(vector_of_main_sampling_file_names, file_name)
  
  gc()
  }

# Set working directory to save the output files
setwd("data")
# setwd() # Mark will need to fill in the correct path here

# Run the "generate_main_samples" function with 5 seeds
vector_of_main_sampling_file_names <- c() # Empty vector to store file names
number_of_seeds_for_main_samples <- 5
# Note: using lapply in the lines below yields nearly identical system.time output
for(seed_i in 1:number_of_seeds_for_main_samples) {
  generate_main_samples(seed_i)
  }


#### FOR EACH MAIN FILE, CREATE ADDITIONAL FILES WITH SAMPLES NESTED WITH THE 10K SAMPLE ####

# Make vector of sample sizes under 10k
train_sample_sizes_under_10k <- train_sample_sizes[train_sample_sizes < 10000]

# Generate function that will create the subsamples: 
# Among rows where train_sample_n_10000 == 1, the function generates random samples at smaller sample sizes.
generate_subsamples <- function(seed, main_sampling_file, main_sampling_file_name) {
  # Create df with just the 10k sampling column and eval splits columns
  df_with_10k_sample_column <- main_sampling_file[, .(RINPERSOON, 
                                          train_sample_n_10000,
                                          evaluation_set, 
                                          evaluation_selection_50_percent_split,
                                          evaluation_test_50_percent_split, 
                                          official_holdout_set)]
  rm(main_sampling_file)
  gc()

  # Filter training pool (train_sample_n_10000 == 1)
  training_pool_10k <- df_with_10k_sample_column[train_sample_n_10000 == 1, .(RINPERSOON)]
  
  # Number of available training rows
  n_total <- 10000
  
  # Loop over sizes and create binary columns
  set.seed(seed)
  for (n in train_sample_sizes_under_10k) {
    colname <- paste0("train_sample_n_", format(n, scientific = FALSE))
    if (n > n_total) {
      stop("Sample size exceeds available training rows")
    } else {
      # Randomly select n row indices from the training pool
      selected_rows <- sample.int(n_total, n)
      # Create an integer vector of 0s (length = total number of training rows)
      indicator <- integer(n_total)
      # Set the positions of the sampled rows to 1
      indicator[selected_rows] <- 1
      # Add a new column to the data.table with 1s for selected rows and 0s elsewhere
      training_pool_10k[, (colname) := indicator]
    }
  }
  
  # Merge the new samples with the 10k sample and eval splits data
  df_with_10k_sample_column <- merge(
    df_with_10k_sample_column,
    training_pool_10k,
    by = "RINPERSOON",
    all.x = TRUE,
    sort = FALSE
  )
  
  # Fill NA train_sample_n_* columns with 0 
  # Note: NA values were introduced when merging the training pool with df_with_10k_sample_column, which has more rows
  df_with_10k_sample_column[, (grep("^train_sample_n_", names(df_with_10k_sample_column), value = TRUE)) := 
               lapply(.SD, function(x) fifelse(is.na(x), 0L, x)),
               .SDcols = patterns("^train_sample_n_")]
  
  # Save the file
  seed_from_main_sampling_file <- as.integer(sub(".*_seed_(\\d+).*", "\\1", main_sampling_file_name))
  file_name <- sprintf(
    "dms_samples_seed_%d_subsamples_%d.csv",
    seed_from_main_sampling_file,
    seed
  )
  fwrite(df_with_10k_sample_column, file = file_name)
  
  # Record the file name in order to read it back in later
  vector_of_subsampling_file_names <<- c(vector_of_subsampling_file_names, file_name)
  
  gc()
}

# Generate function to read in one of the main sampling files and create subsamples
read_main_file_and_generate_subsamples <- function(index_for_vector_of_main_sampling_file_names) { 
  # Read in one of the main sampling files
  main_sampling_file_name <- vector_of_main_sampling_file_names[index_for_vector_of_main_sampling_file_names]
  main_sampling_file <- fread(main_sampling_file_name, colClasses = c(RINPERSOON = "character"))

  # Generate the subsamples & save the files
  for(seed_i in 1:number_of_seeds_for_subsamples) { 
    generate_subsamples(seed_i, main_sampling_file, main_sampling_file_name)
    }
  }

# Run the function to read in the main sampling files and generate subsamples
number_of_seeds_for_subsamples <- 5
vector_of_subsampling_file_names <- c() # Empty vector to store file names
# Note: it's correct that the line below says number_of_seeds_for_main_samples, 
# as this corresponds to the number of main sampling files. 
# The value number_of_seeds_for_subsamples that I just created above is used internally in the function.
for (file_i in 1:number_of_seeds_for_main_samples) {
  read_main_file_and_generate_subsamples(file_i)
}

#### QUALITY ASSURANCE CHECKS FOR INDIVIDUAL FILES ####

# Create function with checks that apply to both the main sampling file and subsample files
run_tests_for_sampling_file <- function(sampling_file_name, type_of_sampling_file = c("main sample", "subsample")) { 
  # Ensure a valid argument is used
  type_of_sampling_file <- match.arg(type_of_sampling_file)
  
  # Read in a sampling file 
  df_to_test <- fread(sampling_file_name, colClasses = c(RINPERSOON = "character"))
  
  # The rowwise sum of the training_set, evaluation_set, and official_holdout_set columns should equal 1 for each row
  if(type_of_sampling_file == "main sample") { 
    rowwise_sum_check <- rowSums(df_to_test[, .(training_set, evaluation_set, official_holdout_set)]) 
    if(!all(rowwise_sum_check == 1)) { 
      stop("Some rows are in more than one set (training, evaluation, holdout) or not in a set.")
    } else { 
      print("Check passed: training, evaluation, and holdout sets sum to 1 in each row.")
    }
  }

  # No cells should be NA
  if(!all(!is.na(df_to_test))) { 
    stop("There are NA values in the sampling file.")
  } else { 
    print("Check passed: No NA values in the sampling file.")
  }
  
  # RINPERSOON should be unique
  all_RINPERSOON_values_are_unique <- length(unique(df_to_test$RINPERSOON)) == nrow(df_to_test)
  if(!all_RINPERSOON_values_are_unique) { 
    stop("RINPERSOON column contains duplicate values.")
  } else { 
    print("Check passed: all RINPERSOON values are unique.")
  }
  
  # All cases in train_sample_n_* should have evaluation_set == 0 and official_holdout_set == 0
  train_sample_columns <- df_to_test %>%
    select(matches("^train"))
  eval_rows <- df_to_test$evaluation_set == 1
  train_samples_mistakenly_came_from_evaluation_rows <- any(train_sample_columns[eval_rows] == 1)
  holdout_rows <- df_to_test$official_holdout_set == 1
  train_samples_mistakenly_came_from_holdout_rows <- any(train_sample_columns[holdout_rows] == 1)
  if(train_samples_mistakenly_came_from_evaluation_rows | train_samples_mistakenly_came_from_holdout_rows) { 
    stop("Some train samples came from the evaluation set or holdout set.")
  } else { 
    print("Check passed: All train samples come from the training set.")
  }
  
  # For rows in the evaluation set, the 50% eval splits should sum to 1
  eval_row_sums_df <- df_to_test %>% 
    filter(evaluation_set == 1) %>%
    mutate(rowwise_sum_eval_splits = 
             rowSums(select(., evaluation_selection_50_percent_split, evaluation_test_50_percent_split)))
  if(!all(eval_row_sums_df$rowwise_sum_eval_splits == 1)) { 
    stop("Some evaluation rows are in both 50% splits, or not in a 50% split.")
  } else { 
    print("Check passed: 50% splits of evaluation set are correct.")
  }
  rm(eval_row_sums_df)
  
  # If a row is in one of the 50% evaluation splits, it should be from the evaluation set.
  fifty_percent_eval_splits_df <- df_to_test %>%
    filter(evaluation_selection_50_percent_split == 1 | 
             evaluation_test_50_percent_split == 1)
  if(!all(fifty_percent_eval_splits_df$evaluation_set == 1)) { 
    stop("Some rows in the 50% evaluation splits are not from the evaluation set.")
  } else { 
    print("Check passed: All rows in the 50% evaluation splits are from the evaluation set.")
  }
  
  # The sample size of evaluation_selection_50_percent_split and evaluation_test_50_percent_split should differ by no more than 1
  selection_set_size <- sum(df_to_test$evaluation_selection_50_percent_split)
  test_set_size <- sum(df_to_test$evaluation_test_50_percent_split)
  if(abs(selection_set_size - test_set_size) > 1) {
    stop("The sizes of selection set and test set differ by more than 1 row.")
  } else {
    print("Check passed: The sizes of selection set and test set differ by 1 row or less.")
  }
  
  # Samples should not be nested (except that in the subsamples files, all samples are within the 10k sample). 
  # I will test that not all rows in the 1k sample are in the 2k sample. 
  # This is an arbitrary choice of columns to test; 
  # I don't want to test every pair of columns due to computing limitations.
  ids_in_1k_sample <- df_to_test %>% 
    filter(train_sample_n_1000 == 1) %>%
    pull(RINPERSOON)
  ids_in_2k_sample <- df_to_test %>%
    filter(train_sample_n_2000 == 1) %>%
    pull(RINPERSOON)
  if(all(ids_in_1k_sample %in% ids_in_2k_sample)) {
    stop("All rows in the 1k sample are also in the 2k sample, suggesting the samples are nested (though this could occur by chance).")
  } else {
    print("Check passed: The tested pair of samples is not nested.")
  }
  
  # Within the subsampling files, all samples should be nested within the 10k sample.
  if(type_of_sampling_file == "subsample") { 
    ids_for_10k_sample <- df_to_test %>%
      filter(train_sample_n_10000 == 1) %>%
      pull(RINPERSOON)
    training_set_columns <- df_to_test %>%
      select(matches("^train_sample_n_"))
    for (col in names(training_set_columns)) {
      ids_for_sample <- df_to_test %>%
        filter(get(col) == 1) %>%
        pull(RINPERSOON)
      if(!all(ids_for_sample %in% ids_for_10k_sample)) {
        stop(sprintf("Some rows in the %s sample are not nested in the 10k sample.", col))
      }
    }
    print("Check passed: All rows in samples <10k are in the 10k sample.")
  }
  
  rm(df_to_test)
  print(sprintf("ALL CHECKS PASSED FOR SAMPLING FILE: %s", sampling_file_name))
  }

# Run the tests
for(sampling_file_name in vector_of_main_sampling_file_names) {
  run_tests_for_sampling_file(sampling_file_name, type_of_sampling_file = "main sample")
}

for(sampling_file_name in vector_of_subsampling_file_names) {
  run_tests_for_sampling_file(sampling_file_name, type_of_sampling_file = "subsample")
}

#### QUALITY ASSURANCE CHECKS ACROSS FILES ####

# Compare main sampling files with different seeds to ensure they contain different samples
main_file_1 <- fread("dms_samples_seed_1.csv", colClasses = c(RINPERSOON = "character"))
main_file_2 <- fread("dms_samples_seed_2.csv", colClasses = c(RINPERSOON = "character"))
# I will arbitrarily test the column for n = 100k (but could apply the same test to any column)
all_rows_match <- identical(
  main_file_1$train_sample_n_100000,
  main_file_2$train_sample_n_100000
)
if (all_rows_match) { 
  stop("Files that are supposed to differ have identical samples.")
  } else { 
  print("Check passed: the two samples differ.")
  }
rm(main_file_1, main_file_2)

# Compare subsampling files with the same main seed and different subsample seeds
subsampling_file_1 <- fread("dms_samples_seed_1_subsamples_1.csv", colClasses = c(RINPERSOON = "character"))
subsampling_file_2 <- fread("dms_samples_seed_1_subsamples_2.csv", colClasses = c(RINPERSOON = "character"))
# I will arbitrarily test the column for n = 5k (but could apply the same test to any column)
all_rows_match <- identical(
  subsampling_file_1$train_sample_n_5000,
  subsampling_file_2$train_sample_n_5000
)
if (all_rows_match) { 
  stop("Files that are supposed to differ have identical samples.")
} else { 
  print("Check passed: the two samples differ.")
}
rm(subsampling_file_2)

# Compare subsampling files with different main seed and same subsample seeds
subsampling_file_3 <- fread("dms_samples_seed_2_subsamples_1.csv", colClasses = c(RINPERSOON = "character"))
# I will arbitrarily test the column for n = 5k (but could apply the same test to any column)
all_rows_match <- identical(
  subsampling_file_1$train_sample_n_5000,
  subsampling_file_3$train_sample_n_5000
)
if (all_rows_match) { 
  stop("Files that are supposed to differ have identical samples.")
} else { 
  print("Check passed: the two samples differ.")
}
