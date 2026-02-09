# This file produces preprocessed datasets
# Please set the working directory to the main directory, the one containing 
# folders `data`, `results`, and others.
# Set up
setwd("") 
print(Sys.time())
start <- Sys.time()
library(tidyverse)
library(data.table)
library(furrr)

# Define file paths
args <- commandArgs(trailingOnly = TRUE)
data_files <- list(
  prefer_official_train = "data/train.csv",
  gbapersoontab = "data/GBAPERSOON2020TABV3.csv",
  gbahuishoudensbus = "data/GBAHUISHOUDENS2020BUSV1.csv",
  familienetwerktab = "data/FAMILIENETWERK2020TABV1.csv",
  prefer_official_holdout_features = "data/holdout_final_leaderboard.csv",
  prefer_official_holdout_labels = "data/final_leaderboard_outcome.csv"
)
prefer_official_train_codebook_path <- "data/Codebook UPD.xlsx"
sampling_files_path <-  "data/"

# Run the code up to preprocessing.R
source("jobfiles/jobfile_learning_curves_seed_1_2025-08.R") # Any jobfile would work
source("model/filter_to_prefer_train_and_eval_set.R")
source("model/feature_engineering.R")
source("model/create_metadata.R")
source("model/preprocessing.R")

# Save preprocessed data files
fwrite(data, "data/data.csv")
fwrite(metadata, "data/metadata.csv")
fwrite(outcome_data, "data/outcome_data.csv")
Sys.time() - start
print(Sys.time())