library(data.table) # 1.15.4 on CBS server
library(tidyverse) # 2.0.0 on CBS server

set.seed(seed_job)

# This file filters CBS datasets to contain only the official PreFer training and evaluation set, 
# so that the necessary data can be read into other code files more quickly. 

# Each section of the code is written so that it can be run without running the sections above.

# Save RINPERSOON and non-missing outcomes for official PreFer train set
prefer_official_train <- fread(data_files$prefer_official_train, colClasses = c(RINPERSOON = "character")) %>%
  filter(!is.na(children_post2021))
outcome_data <- prefer_official_train %>%
  select(RINPERSOON, children_post2021) %>%
  rename(outcome = children_post2021)

# Save the rest of the official PreFer train set, filtered to just the PreFer official train set with non-missing outcomes
prefer_official_train <- prefer_official_train %>%
  select(-RINPERSOONS, -children_post2021, -RINPERSOONSVERBINTENISP, -RINPERSOONVERBINTENISP, -RINPERSOONSHKW, -RINPERSOONHKW, -SOORTOBJECTNUMMER, -RINOBJECTNUMMER, -HUISHOUDNR, -evaluation_set)
print("filter_to_prefer_train_and_eval_set: steps for prefer_official_train and outcome dataframe are complete.")

# Append the PreFer official holdout rows onto the training set (for the feature df)
prefer_official_holdout_features <- fread(data_files$prefer_official_holdout_features, colClasses = c(RINPERSOON = "character"))
prefer_official_holdout_features <- prefer_official_holdout_features %>%
  select(-RINPERSOONS, -RINPERSOONSVERBINTENISP, -RINPERSOONVERBINTENISP, -RINPERSOONSHKW, -RINPERSOONHKW, -SOORTOBJECTNUMMER, -RINOBJECTNUMMER, -HUISHOUDNR)
# Note: the sampling file will tell the code which RINPERSOON belong to train vs. eval vs. holdout
# TODO: Improve naming convention throughout code, to indicate that this includes holdout data now
prefer_official_train <- rbind.data.frame(prefer_official_train, prefer_official_holdout_features)
# Overwrite official train rinpersoon to be train + holdout RINPERSOON
official_train_rinpersoon <- prefer_official_train$RINPERSOON

# Append the PreFer official holdout rows onto the training set (for the outcome df)
prefer_official_holdout_labels <- fread(data_files$prefer_official_holdout_labels, 
                                        colClasses = c(RINPERSOON = "character"))
prefer_official_holdout_labels <- prefer_official_holdout_labels %>%
  rename(outcome = children_post2021)
outcome_data <- rbind.data.frame(outcome_data, prefer_official_holdout_labels)

# Save PERSOONTAB, filtered to just the PreFer official train set with non-missing outcomes
gbapersoontab_full <- fread(data_files$gbapersoontab, colClasses = c(RINPERSOON = "character")) %>%
  select(-RINPERSOONS, -GBAGEBOORTEDAG, -GBAGEBOORTEDAGMOEDER, -GBAGEBOORTEDAGVADER)
gbapersoontab_train <- gbapersoontab_full %>%
  filter(RINPERSOON %in% official_train_rinpersoon)
print("filter_to_prefer_train_and_eval_set: steps for PERSOONTAB are complete.")

# Save householdbus (HUISHOUDENSBUS), filtered to just the PreFer official train set with non-missing outcomes
gbahuishoudensbus_full <- fread(data_files$gbahuishoudensbus, colClasses = c(RINPERSOON = "character", HUISHOUDNR = "character")) %>%
  # Additional step: make unique household ID (must combine household ID with household start date to identify a unique household)
  mutate(household_id = paste0(HUISHOUDNR, "_", DATUMAANVANGHH)) %>%
  select(-HUISHOUDNR, -RINPERSOONS, -REFPERSOONHH) # remove the raw ID so we can never use it accidentally
# The following file only has rows for focal people. 
# Later, we utilize a version of huishodensbus with rows for all people in the train households, hence the "focal_people_only" name.
gbahuishoudensbus_train_focal_people_only <- gbahuishoudensbus_full %>%
  filter(RINPERSOON %in% official_train_rinpersoon) 
print("filter_to_prefer_train_and_eval_set: steps for HUISHOUDENSBUS are complete.")

# Read network data & filter to train set
familienetwerktab_train <- fread(data_files$familienetwerktab, colClasses = c(RINPERSOON = "character", RINPERSOONRELATIE = "character")) %>%
  filter(RINPERSOON %in% official_train_rinpersoon) %>%
  select(-RINPERSOONS, -RINPERSOONSRELATIE)
print("filter_to_prefer_train_and_eval_set: steps for FAMILIENETWERKTAB are complete.")



###### CODE FOR INTERACTIVE USE ######
# This code saves files for use during code development. 
# This code should only be used interactively (i.e., NOT as part of the OSSC workflow), 
# and should always be commented out when not in use.
#train_path <- "H:/DATASETS/train.csv"
#prefer_official_train <- fread(train_path, colClasses = c(RINPERSOON = "character")) %>%
#  filter(!is.na(children_post2021))
#official_train_rinpersoon <- prefer_official_train$RINPERSOON
#fwrite(prefer_official_train, "H:/pmt/stork_oracle/october_2024_updates_to_in_development_code/data_filtered_to_train_and_eval_set_for_exploratory_work/prefer_official_train_filtered.csv")
#fwrite(gbapersoontab, "H:/pmt/stork_oracle/october_2024_updates_to_in_development_code/data_filtered_to_train_and_eval_set_for_exploratory_work/gbapersoontab_filtered.csv")
#fwrite(gbahuishoudensbus, "H:/pmt/stork_oracle/october_2024_updates_to_in_development_code/data_filtered_to_train_and_eval_set_for_exploratory_work/gbahuishoudensbus_filtered.csv")
