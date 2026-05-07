# This file counts the number of missing data cells for both the outcome an the
# features
# Please make sure you read README.md if you encounter problems with fread()

# Set up
library(data.table)
library(tidyverse)

# Count missing outcomes in training and test data
prefer_train_and_eval <- fread("data/train.csv", colClasses = c(RINPERSOON = "character"))
table1 <- table(prefer_train_and_eval$children_post2021, prefer_train_and_eval$evaluation_set, useNA = "always")
table1
prefer_official_holdout_labels <- fread("data/final_leaderboard_outcome.csv", colClasses = c(RINPERSOON = "character"))
table2 <- table(prefer_official_holdout_labels$children_post2021, useNA = "always")
prefer_official_holdout_features <- fread("data/holdout_final_leaderboard.csv", colClasses = c(RINPERSOON = "character"))
which(prefer_official_holdout_features$RINPERSOON != prefer_official_holdout_labels$RINPERSOON)
if (table2[3] == 0 & length(which(prefer_official_holdout_features$RINPERSOON != prefer_official_holdout_labels$RINPERSOON)) == 0) {
  holdout_outcome_missingness <- 0
}

# Count missing features in training and test data in various subsets of the
# data depending on random data splits and outcome class
sampling_file <- "data/pmt_train_and_evaluation_samples_seed_1_241016_with_holdout.csv" %>%
  fread(colClasses = c(RINPERSOON = "character")) %>%
  mutate(sample = case_when(training_set == 1 ~ "training_set",
                            evaluation_test_50_percent_split == 1 ~ "evaluation_test_50_percent_split",
                            evaluation_selection_50_percent_split == 1 ~ "evaluation_selection_50_percent_split",
                            official_holdout_set == 1 ~ "official_holdout_set"
                            )) %>%
  select(RINPERSOON, sample)
outcome_data <- "data/outcome_data.csv" %>%
  fread(colClasses = c(RINPERSOON = "character"))
data <- "data/data.csv" %>%
  fread(colClasses = c(RINPERSOON = "character")) %>%
  # While included here as part of the code ran within the CBS secure 
  # environment, INHUAFL and variables containing MOEDER and VADER were later
  # manually excluded from Appendix Table A1 as they have structural
  # missingness. INHUAFL is structurally missing for people who recently
  # entered the target population and for institutional household members.
  # MOEDER and VADER variables are structurally missing for people without two
  # officially recognized parents.
  # The variable has_partner is accidentally excluded here even though it does
  # not have structural missingness by construction (one either lives or does
  # not live with a partner recorded in huishoudensbus). Fortunately, the
  # variable also has no missingness, as everyone in PreFer has an entry in
  # huishoudensbus.
  select(-GBAIMPUTATIECODE, -DATUMEINDEHH, -INHPOPIIV, -INHUAF, INHUAFL, -starts_with(c("GEB", "n_", "INPEM", "INPP100P", "INHARMEUR", "VEHP100")), -contains(c("household_child_", "_partner", "children_of_sex_")), -ends_with(c("_youngest", "_main", "PARTNER"))) %>%
  mutate(across(starts_with("GBAGEBOORTELAND"), ~if_else(.x == "_0", NA, .x)),
         across(all_of(c("INPPINK", "INPPOSHHK", "INHEHALGR", "INHUAFTYP")), ~if_else(.x == "_9", NA, .x)),
         across(starts_with("INHSAM"), ~if_else(.x == "_88", NA, .x)),
         across(all_of(c("INHBBIHJ", "VBOWoningtype")), ~ if_else(.x == "_99", NA, .x)),
         across(-RINPERSOON, ~if_else(.x %in% c("_-", "_NA", NA), 1, 0))) %>%
  full_join(sampling_file) %>%
  full_join(outcome_data) %>%
  select(-RINPERSOON) %>%
  group_by(sample, outcome) %>%
  summarize(n = n(), across(everything(), sum)) %>%
  ungroup() %>%
  mutate(
    prefer_set = c("evaluation_set", "evaluation_set", "evaluation_set", "evaluation_set", "official_holdout_set", "official_holdout_set", "training_set",  "training_set"),
    outcome_missing_from_prefer_set = c(table1[3,2], table1[3,2], table1[3,2], table1[3,2], holdout_outcome_missingness, holdout_outcome_missingness, table1[3,1], table1[3,1])) %>%
  select(sample, outcome, n, prefer_set, outcome_missing_from_prefer_set, everything())
fwrite(data, "results/missingness.csv")
