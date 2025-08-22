# This file is for quick test runs. It uses a data file that is already preprocessed, 
# to save time on the feature engineering and preprocessing steps.

setwd("H:/pmt/stork_oracle/stork_oracle_december_2024")

print("Started running the full code:")
print(Sys.time())
start <- Sys.time()

library(tidyverse)
library(data.table)
library(furrr)

args <- commandArgs(trailingOnly = TRUE)

# data_files <- list(
#  prefer_official_train =  "H:/DATASETS/train.csv",
#  gbapersoontab =  "G:/Bevolking/GBAPERSOONTAB/2020/geconverteerde data/GBAPERSOON2020TABV3.csv",
#  gbahuishoudensbus =  "G:/Bevolking/GBAHUISHOUDENSBUS/geconverteerde data/GBAHUISHOUDENS2020BUSV1.csv",
#  familienetwerktab = "G:/Bevolking/FAMILIENETWERKTAB/FAMILIENETWERK2020TABV1.csv"
# )

prefer_official_train_codebook_path <-  "H:/DATASETS/documentation/Codebook UPD.xlsx"

sampling_files_path <-  "H:/pmt/eval/train_and_eval_samples/"

source("jobfile_AS_FS_IN_ED_IM_HO_plus_topics.R")

#source("filter_to_prefer_train_and_eval_set.R")

#source("feature_engineering.R")

#source("create_metadata.R")

#source("preprocessing.R")

#### SPECIAL SECTION FOR QUICK TEST RUNS ####

data <- fread("H:/pmt/stork_oracle/stork_oracle_december_2024/preprocessed_files_for_quick_test_runs/data.csv", 
              colClasses = c(RINPERSOON = "character"))
outcome_data <- fread("H:/pmt/stork_oracle/stork_oracle_december_2024/preprocessed_files_for_quick_test_runs/outcome_data.csv",
                      colClasses = c(RINPERSOON = "character"))
metadata <- fread("H:/pmt/stork_oracle/stork_oracle_december_2024/preprocessed_files_for_quick_test_runs/metadata.csv")
binary_one_hot_variables <- c()

categorical_variables <- metadata %>%
  filter(variable_type == "categorical_or_binary_original_variable") %>%
  pull(variable_name)
continuous_variables <- metadata %>%
  filter(variable_type == "continuous") %>%
  pull(variable_name)
data <- mutate(data,
               across(any_of(continuous_variables), as.numeric),
               across(any_of(categorical_variables), as.factor))

print("Finished reading data, starting chunking:")
print(Sys.time())

############################################

source("chunking.R")

print("Finished chunking, starting running grid rows:")
print(Sys.time())

source("run_grid_row.R")

print("Finished running grid rows, starting running metrics for selecting pipelines:")
print(Sys.time())

source("run_metric_for_selecting_pipelines.R")

Sys.time() - start
print(Sys.time())
print("Finished running the full code")