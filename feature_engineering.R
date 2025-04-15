library(data.table) # 1.15.4 on CBS server
library(tidyverse) # 2.0.0 on CBS server

# Create meaningfully new features at the individual level

# Note: I'm using data.table syntax to wrangle the data because it is MUCH faster than dplyr.

#### CURRENT HOUSEHOLD FROM GBAHUISHOUDENSBUS ###################################

# Note: 
# For people aged approx. 18 - 25 in 2020, we have info on their birth household.
# For people aged approx. 25 - 42, we have info on household(s) from some year(s) under age 18.
# For people aged approx. 43 - 45, we don't have info on their household until adulthood.

# Current household
gbahuishoudensbus_train_focal_people_only_current_household <- gbahuishoudensbus_train_focal_people_only[order(-DATUMAANVANGHH), # put household start date in descending order (most recent first)
                            .SD[1], # choose the top row per group (i.e., per person)
                            by = RINPERSOON] # the grouping variable is RINPERSOON
print("feature_engineering: selection of current household from gbahuishoudensbus is complete.")


#### LIVE-IN PARTNER DATA ######################################################
# For egos who live with a married, registered, or unregistered partner, this section 
# generates features about that partner.
# The key difference between this and other partner data is that here, we include unregistered partners.

#### Step 1: Link ego RINPERSOON with partner RINPERSOON ####

# Filter to egos who have a "partner" place in household (PLHH) (married, registered, or unregistered) and select just the id columns
rinpersoon_and_current_household_id_for_train_egos_with_partner_place <- gbahuishoudensbus_train_focal_people_only_current_household %>%
  filter(PLHH %in% c(3, 4, 5, 6)) %>%
  select(RINPERSOON, household_id) %>%
  rename(RINPERSOON_ego = RINPERSOON)

# Identify anyone who is a married or unmarried partner in any train-set ego's current household 
# (for egos who have "partner" place in household)
# Note: this includes the egos themselves
current_household_ids_for_egos_with_partners <- rinpersoon_and_current_household_id_for_train_egos_with_partner_place$household_id
people_with_partner_place_in_current_train_households <- gbahuishoudensbus_full %>% 
  filter(household_id %in% current_household_ids_for_egos_with_partners) %>%
  filter(PLHH %in% c(3, 4, 5, 6)) %>%
  select(RINPERSOON, household_id) %>%
  rename(RINPERSOON_partner = RINPERSOON)

# Merge together the ego data and partner data by household; this means egos will also get merged with themselves
people_with_partner_place_joined_by_household <- inner_join(rinpersoon_and_current_household_id_for_train_egos_with_partner_place, 
                                                            people_with_partner_place_in_current_train_households, 
                                                            by = "household_id", 
                                                            relationship = "many-to-many")

# Filter to rows where the ego is NOT merged with the self
ego_and_partner_id_linkage <- people_with_partner_place_joined_by_household %>%
  filter(RINPERSOON_ego != RINPERSOON_partner) %>%
  select(RINPERSOON_ego, RINPERSOON_partner)

#### Step 2: Create an indicator of whether ego has a live-in partner ####
# This is "1" for everyone in the ego_and_partner_id_linkage dataframe. 
# Later, in preprocessing.R, we will merge this dataframe with data that includes 
# people who do not have partners, and then we will set has_partner = 0 for those people.

# QUESTION FOR MARK: As stated above, I think this should be 1 for everyone in the dataframe because
# the dataframe is just people with partners. Could you please confirm whether that's true?
ego_and_partner_id_linkage <- ego_and_partner_id_linkage %>%
  mutate(has_partner = ifelse(!is.na(RINPERSOON_partner), 1, 0))

#### Step 3: When did ego and spouse/registered/unregistered partner start living together? ####

partner_train_rinpersoon <- ego_and_partner_id_linkage$RINPERSOON_partner

# Prepare versions of householdbus with just the egos and just the partners 
huishoudensbus_train_ego_with_subset_of_columns <- gbahuishoudensbus_full %>%
  filter(RINPERSOON %in% official_train_rinpersoon) %>%
  rename(RINPERSOON_ego = RINPERSOON) %>%
  select(RINPERSOON_ego, household_id, DATUMAANVANGHH)
huishoudensbus_train_partner_with_subset_of_columns <- gbahuishoudensbus_full %>% 
  filter(RINPERSOON %in% partner_train_rinpersoon) %>%
  rename(RINPERSOON_partner = RINPERSOON) %>%
  select(RINPERSOON_partner, household_id)

# Merge by household ID to find households where ego and partner may lived together.
# Often, someone who is not the current partner used to be in same household as ego, 
# so the rows in the merged df include, but are not limited to, the current partner. 
households_where_ego_and_someone_from_partner_list_lived_together <- inner_join(huishoudensbus_train_ego_with_subset_of_columns, 
                                                                                huishoudensbus_train_partner_with_subset_of_columns, 
                                                                                by = "household_id", 
                                                                                relationship = "many-to-many")
# Identify households where ego lived with the current partner
households_where_ego_and_current_partner_lived_together <- left_join(ego_and_partner_id_linkage, 
                                                                     households_where_ego_and_someone_from_partner_list_lived_together, 
                                                                     by = c("RINPERSOON_ego", "RINPERSOON_partner"))

# Identify first household where ego and partner lived together
first_household_where_ego_and_partner_lived_together <- households_where_ego_and_current_partner_lived_together %>%
  group_by(RINPERSOON_ego, RINPERSOON_partner) %>%
  summarise(start_date_of_first_household_with_partner = min(DATUMAANVANGHH), .groups = "drop") %>%
  select(RINPERSOON_ego, start_date_of_first_household_with_partner) %>%
  rename(RINPERSOON = RINPERSOON_ego)

#### Step 4: Get the live-in partner's age and sex ####

# Make dataframe with ID and sex
# Calculate age at the start of 2021 as if people were born the first day of the month
age_and_sex <- gbapersoontab_full %>%
  select(RINPERSOON, GBAGEBOORTEJAAR, GBAGEBOORTEMAAND, GBAGESLACHT) %>%
  mutate(birthyear_and_month = GBAGEBOORTEJAAR + (GBAGEBOORTEMAAND-1)/12) %>%
  mutate(age = 2021 - birthyear_and_month) %>%
  select(RINPERSOON, GBAGESLACHT, age)

live_in_partner_data <- left_join(ego_and_partner_id_linkage, 
                                  age_and_sex, 
                                  by = c("RINPERSOON_partner" = "RINPERSOON")) %>%
  rename(live_in_partner_GBAGESLACHT = GBAGESLACHT, 
         live_in_partner_age = age, 
         RINPERSOON = RINPERSOON_ego) %>%
  select(RINPERSOON, has_partner, live_in_partner_GBAGESLACHT, live_in_partner_age)

#### Step 5: Put age, sex, and date of moving in together in one data frame #### 
live_in_partner_data <- left_join(live_in_partner_data, first_household_where_ego_and_partner_lived_together)

print("feature_engineering: creation of data about live-in-partners is complete.")


#### NETWORK DATA: FAMILIENETWERKTAB ###########################################

# Count number of family members by type, and in total 
family_member_count_by_type <- familienetwerktab_train %>%
  group_by(RINPERSOON, RELATIE) %>%
  summarise(count = n()) %>% 
  pivot_wider(names_from = RELATIE,
              names_glue = "family_member_count_type_{RELATIE}",
              values_from = count, 
              values_fill = 0) %>%
  ungroup()
print("feature_engineering: family member counts by type are complete.")

total_family_member_count <- familienetwerktab_train %>%
  group_by(RINPERSOON) %>%
  summarise(family_member_count_total = n()) %>%
  ungroup()
print("feature_engineering: total count of family members is complete.")

# Count number of (step)daughters and number of (step)sons (i.e., differentiate the child & stepchild counts by sex)

# Make dataframe with ID and sex
sex_df <- gbapersoontab_full %>%
  select(RINPERSOON, GBAGESLACHT)

# Identify sex of children 
# Note: this also makes a feature counting children for whom sex is NA (they are not in data; this is 0.5% of children) 
child_df <- familienetwerktab_train %>%
  filter(RELATIE == 304) # 304 is relative type "child"
child_df <- left_join(child_df, sex_df, by = c("RINPERSOONRELATIE" = "RINPERSOON")) %>%
  rename(sex_of_child = GBAGESLACHT)
daughter_and_son_counts <- child_df %>%
  group_by(RINPERSOON, sex_of_child) %>%
  summarise(count = n()) %>% 
  pivot_wider(names_from = sex_of_child,
              names_glue = "children_of_sex_{sex_of_child}",
              values_from = count, 
              values_fill = 0) %>%
  ungroup()
print("feature_engineering: daughter and son counts are complete.")

# Identify sex of stepchildren
# Note: this also makes a feature counting stepchildren for whom sex is NA (they are not in data; this is 10% of stepchildren)
# I believe the reason we have more missingness for stepchildren is that they can be in the holdout sets, whereas children are always kept in the same set as parents
stepchild_df <- familienetwerktab_train %>%
  filter(RELATIE == 318) # 318 is relative type "stepchild"
stepchild_df <- left_join(stepchild_df, sex_df, by = c("RINPERSOONRELATIE" = "RINPERSOON")) %>%
  rename(sex_of_stepchild = GBAGESLACHT)
stepdaughter_and_stepson_counts <- stepchild_df %>%
  group_by(RINPERSOON, sex_of_stepchild) %>%
  summarise(count = n()) %>% 
  pivot_wider(names_from = sex_of_stepchild,
              names_glue = "stepchildren_of_sex_{sex_of_stepchild}",
              values_from = count, 
              values_fill = 0) %>%
  ungroup()
print("feature_engineering: stepdaughter and Stepson counts are complete.")

# Join together all the features from familienetwerktab
features_from_familienetwerktab <- full_join(family_member_count_by_type, total_family_member_count, by = "RINPERSOON")
features_from_familienetwerktab <- full_join(features_from_familienetwerktab, daughter_and_son_counts, by = "RINPERSOON")
features_from_familienetwerktab <- full_join(features_from_familienetwerktab, stepdaughter_and_stepson_counts, by = "RINPERSOON")
print("feature_engineering: merging of features from FAMILIENETWERKTAB into one data frame is complete.")

# Create columns for household_child_1_sex, household_child_2_sex, etc., 
# and household_child_1_sex, household_child_2_sex, etc., where
# child_1 is the oldest person in the household of type "child living at home" (which can be an adult)
train_household_ids <- unique(gbahuishoudensbus_train_focal_people_only_current_household$household_id)
gbahuishoudensbus_everyone_in_train_households <- gbahuishoudensbus_full %>%
  filter(household_id %in% train_household_ids)
# Filter to people who are "child living at home", and select RINPERSSON and HHID
children_living_at_home <- gbahuishoudensbus_everyone_in_train_households %>%
  filter(PLHH == 1) %>%
  select(household_id, RINPERSOON)
# For children living at home, merge in their age and sex from persoontab
# The "age and sex" dataframe was created above, in the live-in partner section
children_living_at_home <- left_join(children_living_at_home, 
                                     age_and_sex, 
                                     by = "RINPERSOON")
# Group by household and make columns for oldest child, second-oldest child, etc. 
# Note: if you make a histogram of ages of children living at home, it has a weird shape
# where it falls from ages 10 to 18, then spikes. This is likely because people age 18 to 
# early 20s are eligible to be younger members of the train set, while at the same time, 
# people from this age group can also be children of older members of the train set.
child_age_and_sex_by_household <- children_living_at_home %>%
  group_by(household_id) %>% 
  arrange(desc(age)) %>%
  mutate(child_rank_in_birth_order = row_number()) %>%
  ungroup() %>%
  select(household_id, child_rank_in_birth_order, age, GBAGESLACHT) %>%
  pivot_wider(
    names_from = child_rank_in_birth_order, 
    values_from = c(age, GBAGESLACHT),
    names_prefix = "household_child_"
  )
print("feature_engineering: creation of child_age_and_sex_by_household is complete.")

#### EGO'S AGE #################################################################
# This feature is created for the feature set analysis where we include ages of ego,
# partner, and children. I don't expect results to differ much from using ego's 
# birthyear, but since I wanted exact age in months for the children, I'm doing it the
# same way for adults too. 

ego_age <- age_and_sex %>% 
  filter(RINPERSOON %in% official_train_rinpersoon) %>%
  select(RINPERSOON, age) %>%
  rename(ego_age = age)
print("feature_engineering: creation of feature about ego's age is complete.")


#### REMOVE FILES TO CLEAR UP MEMORY ###########################################
rm(gbahuishoudensbus_full)
rm(gbahuishoudensbus_everyone_in_train_households)
rm(gbahuishoudensbus_train_focal_people_only)
rm(age_and_sex)
rm(child_df)
rm(children_living_at_home)
rm(daughter_and_son_counts)
rm(ego_and_partner_id_linkage)
rm(familienetwerktab_train)
rm(gbapersoontab_full)
rm(households_where_ego_and_current_partner_lived_together)
rm(households_where_ego_and_someone_from_partner_list_lived_together)
rm(family_member_count_by_type)
rm(first_household_where_ego_and_partner_lived_together)
rm(stepdaughter_and_stepson_counts)
rm(total_family_member_count)
rm(sex_df)
rm(stepchild_df)
rm(huishoudensbus_train_ego_with_subset_of_columns)
rm(huishoudensbus_train_partner_with_subset_of_columns)
rm(people_with_partner_place_in_current_train_households)
rm(people_with_partner_place_joined_by_household)
gc()
   




#### OLD CODE NOt CURRENTLY IN USE ##################################################

# Previous household (the household immediately before the current household)
#previous_household <- gbahuishoudensbus[order(-DATUMAANVANGHH), # put household start date in descending order (most recent first)  
#                            .SD[2], # choose the second-from-the-top row per group (i.e., per person)
#                            by = RINPERSOON] # the grouping variable is RINPERSOON 
#setnames(previous_household, 
#         old = setdiff(names(previous_household), "RINPERSOON"), 
#         new = paste0("previous_hh_", setdiff(names(previous_household), "RINPERSOON")))

# Merge the wrangled features
#features_from_huishoudensbus <- merge(current_household, previous_household, by = "RINPERSOON", all = TRUE) # "all = TRUE" does a full outer join




#### CODE FOR DEVELOPMENT PROCESS (NOT USED DURING ACTUAL RUNS) ##################
# Keep this code commented out whenever it is not actively in use!
 
# Smaller dataframe with just a few miscellaneous people, for faster run times while developing the main code 
#toy <- gbahuishoudensbus %>%
 # filter(RINPERSOON %in% c("XX", "XX", "XX", "XX", "XX"))
 
# Code to filter to just one person, to manually examine what their household pattern looks like
#person_to_check <- "XX"
#toy %>%
 # filter(RINPERSOON == person_to_check) %>%
  #arrange((DATUMAANVANGHH)) %>%
  #View()
 
# Quality checks (run these before merging & deleting objects; manually compare to the df that is filtered to just one person)
# current_household %>% filter(RINPERSOON == person_to_check)
# previous_household %>% filter(RINPERSOON == person_to_check)
# last_household_as_child %>% filter(RINPERSOON == person_to_check)
# number_of_siblings %>% filter(RINPERSOON == person_to_check)

#gbahuishoudensbus <- fread("H:/pmt/stork_oracle/october_2024_updates_to_in_development_code/data_filtered_to_train_and_eval_set_for_exploratory_work/gbahuishoudensbus_filtered.csv", 
 #                          colClasses = c(RINPERSOON = "character"))
