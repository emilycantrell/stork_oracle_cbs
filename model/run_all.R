# This file calls all other files in this folder
# In the terminal, set current directory to working directory to the main 
# directory, the one containing folders `data`, `results`, and others
# Then run the following line
# RScript model/run_all.R "data/GBAPERSOON2020TABV3.csv" "data/GBAHUISHOUDENS2020BUSV1.csv" "data/FAMILIENETWERK2020TABV1.csv" "data/holdout_final_leaderboard.csv" "data/final_leaderboard_outcome.csv" "data/Codebook UPD.xlsx" "data/"
print("Started running the full code:")
print(Sys.time())
start <- Sys.time()
library(tidyverse)
library(data.table)
library(furrr)

# Define file paths
args <- commandArgs(trailingOnly = TRUE)
data_files <- list(
  prefer_official_train = args[1],
  gbapersoontab = args[2],
  gbahuishoudensbus = args[3],
  familienetwerktab = args[4],
  prefer_official_holdout_features = args[5],
  prefer_official_holdout_labels = args[6]
)
prefer_official_train_codebook_path <- args[length(args) - 1]
sampling_files_path <- args[length(args)]

# Load jobfile
source("jobfiles/jobfile_learning_curves_seed_1_2025-08.R")

# Prepare the data
source("model/filter_to_prefer_train_and_eval_set.R")
source("model/feature_engineering.R")
source("model/create_metadata.R")
source("model/preprocessing.R")
print("Finished preparing data, starting chunking:")
print(Sys.time())

# Fit and evaluate the models
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