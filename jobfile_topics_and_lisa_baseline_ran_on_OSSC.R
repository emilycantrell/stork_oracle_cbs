# This is a configuration file specifying settings for parallelizaton, feature
# choices, train-test splits, modeling options, and results output

# Set up
library(tidyverse)

# Parallelization settings
seed_job <- 0
seed_worker <- 0
workers_grid_row <- 3
workers_metric_for_selecting_pipelines <- 3
#n_thread_within_worker <- -1

# Feature choices
# NB: make sure to use equal signs here, not arrows
feature_set_settings <- list(
  AS = c("family_age_and_sex_from_prefer_submission"),
  #FS = c("family_structure"), 
  #IN = c("income_assets_benefits"), 
  #ED = c("education"), 
  #IM = c("immigration_ethnicity"), 
  #HO = c("housing"), 
  #EM = c("employment"), 
  #CH = c("childcare_proximity"), 
  AS_FS = c("family_age_and_sex_from_prefer_submission",
                              "family_structure"),
  AS_FS_IN = c("family_age_and_sex_from_prefer_submission",
                              "family_structure",
                              "income_assets_benefits"),
  AS_FS_IN_ED = c("family_age_and_sex_from_prefer_submission",
                              "family_structure",
                              "income_assets_benefits",
                              "education"),
  AS_FS_IN_ED_IM = c("family_age_and_sex_from_prefer_submission",
                              "family_structure",
                              "income_assets_benefits",
                              "education",
                              "immigration_ethnicity"),
  AS_FS_IN_ED_IM_HO = c("family_age_and_sex_from_prefer_submission",
                              "family_structure",
                              "income_assets_benefits",
                              "education",
                              "immigration_ethnicity",
                              "housing"),
  AS_FS_IN_ED_IM_HO_EM = c("family_age_and_sex_from_prefer_submission",
                              "family_structure",
                              "income_assets_benefits",
                              "education",
                              "immigration_ethnicity",
                              "housing",
                              "employment"),
  AS_FS_IN_ED_IM_HO_EM_CH = c("family_age_and_sex_from_prefer_submission",
                   "family_structure",
                   "income_assets_benefits",
                   "education",
                   "immigration_ethnicity",
                   "housing",
                   "employment",
                   "childcare_proximity")
  #family_structure = c("family_structure")
  #all_topics = c("family_structure", "family_age_and_sex_from_prefer_submission", "immigration_ethnicity", 
  # "income_assets_benefits", "education","employment", "housing", "childcare_proximity"),
  #family_structure = c("family_structure"),
  #FS_family_AS = c("family_structure", "family_age_and_sex_from_prefer_submission", "family_age_and_sex_not_used_in_prefer_submission"),
  #FS_family_AS_prefer = c("family_structure", "family_age_and_sex_from_prefer_submission"), 
  #FS_family_AS_not_prefer = c("family_structure", "family_age_and_sex_not_used_in_prefer_submission"),
  #FS_immigration = c("family_structure", "immigration_ethnicity"), 
  #FS_income = c("family_structure", "income_assets_benefits"), 
  #FS_education = c("family_structure", "education"), 
  #FS_employment = c("family_structure", "employment"),
  #FS_housing = c("family_structure", "housing"), 
  #FS_childcare = c("family_structure", "childcare_proximity")
  #family_AS = c("family_age_and_sex_from_prefer_submission", "family_age_and_sex_not_used_in_prefer_submission"),
  #family_AS_prefer = c("family_age_and_sex_from_prefer_submission"), 
  #family_AS_not_prefer = c("family_age_and_sex_not_used_in_prefer_submission"),
  #immigration = c("immigration_ethnicity"), 
  #income = c("income_assets_benefits"), 
  #education = c("education"), 
  #employment = c("employment"),
  #housing = c("housing"), 
  #childcare = c("childcare_proximity")
  #ego_AS = c("ego_AS"), 
  #ego_partner_AS = c("ego_AS", "partner_AS"), 
  #ego_partner_hhchildren_AS = c("ego_AS", "partner_AS", "child_age_and_sex_by_household")
  #all_plus_LiveInPartner = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS", "prefer_official_train", "FAMILIENETWERKTAB", "live_in_partner")
  #all_plus_hhchildAS = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS", "prefer_official_train", "FAMILIENETWERKTAB", "child_age_and_sex_by_household")
  #all_records = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS", "prefer_official_train", "FAMILIENETWERKTAB")
  #augmented_records = c("GBAPERSOONTAB", "GBAHUISHOUDENSBUS", "prefer_official_train")
  #householdbus = c("GBAHUISHOUDENSBUS")
  #number_of_children = c("number_of_children")
  #household_birthdays = c("household_birthdays")
)

# Train-test splits 
sampling_files <- c("pmt_train_and_evaluation_samples_seed_1_241016_with_holdout.csv")
data_splits <- bind_rows(
  expand_grid(
    training_sets = c("train_sample_n_1000",
                      "train_sample_n_10000",
                      "train_sample_n_100000",
                      "train_sample_n_1000000",
                      "training_set"
                      ),
    selection_sets = c("evaluation_selection_50_percent_split"), # Evaluation sets we use to select the best pipelines
    test_sets = c("evaluation_test_50_percent_split",
                  "official_holdout_set") # Evaluation sets we use for holdout evaluations.
  )
)

# Modeling options
model_settings <- list(
  # These are default catboost settings, and these are the only model-
  # hyperparameter combination we have time to explore, but our code supports
  # tuning other models and hyperparameter options, as commented out below
  catboost = list(tibble(grid_row = pmap(expand_grid(
    learning_rate = c(NA),
    subsample = c(.8),
    depth = c(6)
  ),
  list)),
  steps = c(1000)) # how many trees in catboost?
  # catboost = list(tibble(grid_row = pmap(expand_grid(
  #   learning_rate = c(.009, .03, .09, .3, .9, NA),
  #   subsample = c(.2, .5, .8, 1),
  #   depth = c(1, 2, 4, 6, 8, 10)
  # ),
  # list)),
  # steps = c(200, 400, 600, 800, 1000)),
  # xgboost = list(tibble(grid_row = pmap(expand_grid(
  #   eta = c(.009, .03, .09, .3, .9),
  #   subsample = c(.2, .5, .8, 1),
  #   max_depth = c(1, 2, 4, 6, 8, 10)
  # ),
  # list)),
  # steps = c(200, 400, 600, 800, 1000)),
  # elastic_net = list(tibble(grid_row = pmap(bind_rows(expand_grid(
  #   alpha = c(0, .15, .3, .5, .7, .85, 1),
  #   lambda = c(NA)
  # ),
  # tibble(alpha = 1, lambda = 0)),
  # list)),
  # steps = c(1, 2, 3, 4, 5))
  )
n_grid_row <- 1 # how many hyperparameter combinations to sample from expanded
# grid?

# Performance metrics
metrics_for_all_pipelines <- c("LogLoss", "MSE", "In_Sample_R2", "R2_Holdout", "AUC") # Deciles_for_Calibration
metrics_for_selecting_pipelines <- c("LogLoss")
metrics_for_winning_pipelines <- c("F1_Score", "Accuracy") # F1_Score
threshold_increment <- .01
n_bootstrap <- 2000

save_only_winning_hyperparameter_draw_results <- FALSE
results_path <- "results_cumulative_topics_2025-04-14.csv"