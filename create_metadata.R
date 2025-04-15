library(tidyverse) # 2.0.0 on CBS server
library(readxl) # 1.4.3 on CBS server
library(fastDummies) # 1.7.3 on CBS server

# This file creates a file that assigns data type to columns

record_variable_types_in_df <- function(data_file) { 
  # Set up a data frame which will be filled in with data type in a subsequent step
  data_file <- select(data_file, -any_of(c("RINPERSOON", "household_id")))
  variable_name <- colnames(data_file)
  variable_type <- rep(NA, length(variable_name))
  variable_type_metadata <- cbind.data.frame(variable_name, variable_type)
  # Test that every column's data type was identified exactly once
  if (ncol(data_file) != length(continuous_features) + 
      length(categorical_features)) {
        stop("Each column should have its data type identified exactly once.")
      }
  
  # Add the data types to the data frame
  variable_type_metadata <- variable_type_metadata %>%
    mutate(variable_type = case_when(
      variable_name %in% continuous_features ~ "continuous", 
      variable_name %in% categorical_features ~ "categorical_or_binary_original_variable",
      TRUE ~ NA
    ))
  return(variable_type_metadata)
}

#### prefer_official_train ####
# Identify variables by shared strings in the name
continuous_features <- prefer_official_train %>%
  select(contains(c("JAAR", "MAAND", "DAG", "AANTAL", "pre2021", "_youngest", "birthday", "AANV", "n_", "average_", "s_total", "INPBELI", "P100", "INPPERSPRIM", "INHA", "BRUT", "INHUAFL", "VEH", "VZA"))) %>% # Note: month could reasonably be marked as categorical instead
  names() %>%
  c("INHUAF")

categorical_features <- prefer_official_train %>%
  select(contains(c("GEBOORTELAND", "GESLACHT", "HERKOM", "GENERATIE", "IMPUTATIECODE", "OPLNIVSOI2021AGG4H","SCONTRACTSOORT_main", "SPOLISDIENSTVERBAND_main", "GBABURGERLIJKESTAATNW", "INPEM", "INPPINK", "INPPOSHHK", "INHBBIHJ", "INHEHALGR", "INHPOPIIV", "INHSAMAOW", "INHSAMHH", "TYP"))) %>%
  names() %>%
  c("SECM", "SECM_partner")

# Record the variable names in a dataframe
variable_type_metadata_prefer_official_train <- record_variable_types_in_df(data_file = prefer_official_train)

# Add more information about feature sets
prefer_official_train_codebook_path <- prefer_official_train_codebook_path
prefer_official_train_codebook <- read_xlsx(prefer_official_train_codebook_path) %>%
  mutate(Source = sub("calculated based on ", "", Source),
         Source = sub("created based on ", "", Source)) %>%
  rename(variable_name = Var_name, feature_set = Source) %>%
  select(variable_name, feature_set)
metadata_prefer_official_train <- left_join(variable_type_metadata_prefer_official_train, prefer_official_train_codebook) %>%
  dummy_cols(select_columns = "feature_set", remove_selected_columns = TRUE) %>%
  # rename(feature_set_KINDOUDERTAB = "feature_set_KINDOUDERTAB and GBAPERSOONTAB") %>%
  mutate(# feature_set_GBAPERSOONTAB = ifelse(feature_set_KINDOUDERTAB == 1, 1, feature_set_GBAPERSOONTAB),
         feature_set_prefer_official_train = 1,
         feature_set_sex_and_birthyear = ifelse(variable_name %in% c("GBAGESLACHT", "GBAGEBOORTEJAAR"), 1, 0))

# Notes: 
# GBAGEBOORTEJAAR has a very small number of values that are not plausible 

#### PERSOONTAB ####
# Identify variables by shared strings in the name
continuous_features <- gbapersoontab_train %>%
  select(contains(c("JAAR", "MAAND", "DAG", "AANTAL"))) %>% # Note: month could reasonably be marked as categorical instead
  names()
categorical_features <- gbapersoontab_train %>%
  select(contains(c("GEBOORTELAND", "GESLACHT", "HERKOMSTGROEPERING", "GENERATIE", "IMPUTATIECODE"))) %>%
  names()

# Record the variable names in a dataframe
variable_type_metadata_gbapersoontab <- record_variable_types_in_df(data_file = gbapersoontab_train)
metadata_gbapersoontab <- variable_type_metadata_gbapersoontab %>%
  filter(!variable_name %in% variable_type_metadata_prefer_official_train$variable_name) %>%
  mutate(feature_set_GBAPERSOONTAB = 1)


#### HOUSEHOLDBUS ####
# Identify variables by shared strings in the name
continuous_features <- gbahuishoudensbus_train_focal_people_only_current_household %>%
  select(contains(c("DATUM", "JAAR", "MAAND", "AANTAL", "number"))) %>% # Note: month could reasonably be marked as categorical instead
  names()
categorical_features <- gbahuishoudensbus_train_focal_people_only_current_household %>%
  select(contains(c("TYPHH", "PLHH", "IMPUTATIECODEHH"))) %>%
  names()

# Record the variable names in a dataframe
variable_type_metadata_gbahuishoudensbus <- record_variable_types_in_df(data_file = gbahuishoudensbus_train_focal_people_only_current_household)
metadata_gbahuishoudensbus <- variable_type_metadata_gbahuishoudensbus %>%
  filter(!variable_name %in% variable_type_metadata_prefer_official_train$variable_name) %>%
  mutate(feature_set_GBAHUISHOUDENSBUS = 1)

#### live_in_partner_data ####
# This is data about married, registered, or unregistered partners who live in ego's most recent household (end of 2020)

continuous_features <- c("live_in_partner_age", "start_date_of_first_household_with_partner")
categorical_features <- c("has_partner", "live_in_partner_GBAGESLACHT")
variable_type_metadata_live_in_partner_data <- record_variable_types_in_df(data_file = live_in_partner_data)
metadata_live_in_partner_data <- variable_type_metadata_live_in_partner_data %>%
  mutate(feature_set_live_in_partner = 1)

#### FAMILIENETWERKTAB ####
# Note: this is named "features_from_familienetwerktab" rather than "familienetwerktab" because it has undergone significant feature engineering and does not resemble the raw file
continuous_features <- features_from_familienetwerktab %>%
  select(-RINPERSOON) %>% # all features are continuous counts of number of family members of various types
  names()
categorical_features <- c()
variable_type_metadata_features_from_familienetwerktab <- record_variable_types_in_df(data_file = features_from_familienetwerktab)
metadata_features_from_familienetwerktab <- variable_type_metadata_features_from_familienetwerktab %>%
  mutate(feature_set_FAMILIENETWERKTAB = 1)


#### child_age_and_sex_by_household #### 
continuous_features <- child_age_and_sex_by_household %>% 
  select(contains("age_household")) %>% # Note: age is a substring of GBAGESLACHT, hence the need for a longer string
  names()
categorical_features <- child_age_and_sex_by_household %>%
  select(contains("GBAGESLACHT_household")) %>% 
  names()
variable_type_metadata_child_age_and_sex_by_household <- record_variable_types_in_df(data_file = child_age_and_sex_by_household)
metadata_features_from_child_age_and_sex_by_household <- variable_type_metadata_child_age_and_sex_by_household %>%
  filter(!variable_name %in% variable_type_metadata_prefer_official_train$variable_name) %>%
  mutate(feature_set_child_age_and_sex_by_household = 1)

#### ego_age ####
continuous_features <- c("ego_age")
categorical_features <- c()
variable_type_metadata_ego_age <- record_variable_types_in_df(data_file = ego_age)
metadata_ego_age <- variable_type_metadata_ego_age %>% 
  filter(!variable_name %in% variable_type_metadata_prefer_official_train$variable_name) %>%
  mutate(feature_set_ego_age = 1)

#### MERGE RESULTS TOGETHER ####
metadata <- list_rbind(list(metadata_gbapersoontab, 
                            metadata_gbahuishoudensbus, 
                            metadata_prefer_official_train,
                            metadata_live_in_partner_data, 
                            metadata_features_from_familienetwerktab, 
                            metadata_features_from_child_age_and_sex_by_household, 
                            metadata_ego_age))
metadata[is.na(metadata)] <- 0

#### CREATE ADDITIONAL FEATURE SETS ####

#### Number of children ####
# This feature set was created to test a simple baseline model
features_about_number_of_children <- c("children_pre2021", "AANTALKINDHH")
metadata <- metadata %>% 
  mutate(feature_set_number_of_children = ifelse(variable_name %in% features_about_number_of_children, 1, 0))

#### Huishoudensbus without leakage ####
metadata <- metadata %>% 
  mutate(feature_set_GBAHUISHOUDENSBUS_without_leakage = case_when(
    feature_set_GBAHUISHOUDENSBUS == 1 & variable_name != "DATUMEINDEHH" ~ 1, 
    TRUE ~ 0))

#### Expanding circles: age & sex ####
# "Live-in" partner means cohabiting partner, whether married, registered, or unregistered
ego_age_and_sex <- c("ego_age", "GBAGESLACHT")
live_in_partner_age_and_sex <- c("live_in_partner_age", "live_in_partner_GBAGESLACHT")
# Note: age and sex for household children are in "feature_set_child_age_and_sex_by_household"
metadata <- metadata %>%
  mutate(feature_set_ego_AS = ifelse(variable_name %in% ego_age_and_sex, 1, 0), 
         feature_set_partner_AS = ifelse(variable_name %in% c(live_in_partner_age_and_sex), 1, 0))

#### Topic areas ####
family_structure <- c("PLHH", "AANTALKINDHH", "children_pre2021", 
                      "marriages_total", "partnerships_total", 
                      "GBABURGERLIJKESTAATNW", "AANVANGVERBINTENIS", 
                      "DATUMAANVANGHH", "TYPHH", "AANTALPERSHH", "AANTALOVHH", 
                      "start_date_of_first_household_with_partner", 
                      "family_member_count_type_301",
                      "family_member_count_type_303",
                      "family_member_count_type_309",
                      "family_member_count_type_311",
                      "family_member_count_type_320",
                      "family_member_count_type_322",
                      "family_member_count_type_306",
                      "family_member_count_type_302",
                      "family_member_count_type_304",
                      "family_member_count_type_310",
                      "family_member_count_type_313",
                      "family_member_count_type_314",
                      "family_member_count_type_316",
                      "family_member_count_type_308",
                      "family_member_count_type_317",
                      "family_member_count_type_319",
                      "family_member_count_type_312",
                      "family_member_count_type_307",
                      "family_member_count_type_318",
                      "family_member_count_type_321",
                      "family_member_count_type_305",
                      "family_member_count_type_315",
                      "family_member_count_total")
# Note: I'm not sure if we will want to use the family age and sex variables that were
# not used in our PreFer submission, so I am creating two feature sets about age and sex
# based on what was included in our official submission.
family_age_and_sex_from_prefer_submission <- c("GBAGEBOORTEMAAND", "GBAGEBOORTEJAAR",
                        "GBAGEBOORTEMAANDMOEDER", "GBAGEBOORTEMAANDVADER",
                        "GEBMAANDJONGSTEKINDHH", "GEBJAAROUDSTEKINDHH", "GEBMAANDOUDSTEKINDHH", 
                        "age_youngest", "birthday_youngest", "birthday",  
                        "GBAGESLACHT", "GBAGESLACHTMOEDER", "GBAGESLACHTVADER", 
                        "GBAGEBOORTEJAARMOEDER", "GBAGEBOORTEJAARVADER", 
                        "GBAGEBOORTEJAARPARTNER", "GBAGEBOORTEMAANDPARTNER", "GBAGESLACHTPARTNER", 
                        "GEBJAARJONGSTEKINDHH", 
                        "live_in_partner_GBAGESLACHT", "live_in_partner_age", 
                        "children_of_sex_1", "children_of_sex_2",
                        "stepchildren_of_sex_1", "stepchildren_of_sex_2")
family_age_and_sex_not_used_in_prefer_submission <- c("age_household_child_1",
                                                      "age_household_child_2",
                                                      "age_household_child_3",
                                                      "age_household_child_4",
                                                      "age_household_child_5",
                                                      "age_household_child_6",
                                                      "age_household_child_7",
                                                      "age_household_child_8",
                                                      "age_household_child_9",
                                                      "age_household_child_10",
                                                      "age_household_child_11",
                                                      "age_household_child_12",
                                                      "age_household_child_13",
                                                      "age_household_child_14",
                                                      "age_household_child_15",
                                                      "age_household_child_16",
                                                      "GBAGESLACHT_household_child_1",
                                                      "GBAGESLACHT_household_child_2",
                                                      "GBAGESLACHT_household_child_3",
                                                      "GBAGESLACHT_household_child_4",
                                                      "GBAGESLACHT_household_child_5",
                                                      "GBAGESLACHT_household_child_6",
                                                      "GBAGESLACHT_household_child_7",
                                                      "GBAGESLACHT_household_child_8",
                                                      "GBAGESLACHT_household_child_9",
                                                      "GBAGESLACHT_household_child_10",
                                                      "GBAGESLACHT_household_child_11",
                                                      "GBAGESLACHT_household_child_12",
                                                      "GBAGESLACHT_household_child_13",
                                                      "GBAGESLACHT_household_child_14",
                                                      "GBAGESLACHT_household_child_15",
                                                      "GBAGESLACHT_household_child_16")
immigration_ethnicity <- c("GBAGEBOORTELAND", 
                           "GBAGEBOORTELANDMOEDER", "GBAGEBOORTELANDVADER", 
                           "GBAAANTALOUDERSBUITENLAND", "GBAHERKOMSTGROEPERING", 
                           "GBAGENERATIE", "GBAHERKOMSTLAND", "GBAGEBOORTELANDNL", 
                           "GBAGEBOORTELANDPARTNER")
income_assets_benefits <- c("INPBELI", "INPEMEZ", "INPEMFO", "INPP100PBRUT", "INPP100PPERS", 
                            "INPPERSPRIM", "INPPINK", "INHARMEUR", "INHARMEURL", "INHBBIHJ", 
                            "INHBRUTINKH", "INHUAF", "INHUAFL", "INHUAFTYP", 
                            "VEHP100WELVAART", "VEHP100HVERM", "VEHW1000VERH", "VEHW1100BEZH", 
                            "VEHW1110FINH", "VEHW1120ONRH", "VEHW1130ONDH", "VEHW1140ABEH", 
                            "VEHW1150OVEH", "VEHW1200STOH", "VEHW1210SHYH", "VEHW1220SSTH", 
                            "VEHW1230SOVH", "VEHWVEREXEWH")
eduction <- c("OPLNIVSOI2021AGG4HBmetNIRWO", "OPLNIVSOI2021AGG4HGmetNIRWO", 
              "OPLNIVSOI2021AGG4HBmetNIRWO_isced", "OPLNIVSOI2021AGG4HGmetNIRWO_isced", 
              "OPLNIVSOI2021AGG4HBmetNIRWO_partner", "OPLNIVSOI2021AGG4HGmetNIRWO_partner", 
              "OPLNIVSOI2021AGG4HBmetNIRWO_partner_isced", "OPLNIVSOI2021AGG4HGmetNIRWO_partner_isced")
employment <- c("n_jobs", "n_full", "n_part", "n_permanent", "n_temporary", "n_months_main", 
                "average_hours_main", "SCONTRACTSOORT_main", "SPOLISDIENSTVERBAND_main")
housing <- c("GBADATUMAANVANGADRESHOUDING", "VBOWoningtype")
childcare_proximity <- c("VZAFSTANDKDV", "VZAANTKDV01KM", "VZAANTKDV03KM", "VZAANTKDV05KM", 
                         "VZAFSTANDBSO", "VZAANTBSO01KM", "VZAANTBSO03KM", "VZAANTBSO05KM")

metadata <- metadata %>%
  mutate(feature_set_family_structure = ifelse(variable_name %in% family_structure, 1, 0),
         feature_set_family_age_and_sex_from_prefer_submission = ifelse(variable_name %in% family_age_and_sex_from_prefer_submission, 1, 0),
         feature_set_family_age_and_sex_not_used_in_prefer_submission = ifelse(variable_name %in% family_age_and_sex_not_used_in_prefer_submission, 1, 0),
         feature_set_immigration_ethnicity = ifelse(variable_name %in% immigration_ethnicity, 1, 0),
         feature_set_income_assets_benefits = ifelse(variable_name %in% income_assets_benefits, 1, 0),
         feature_set_education = ifelse(variable_name %in% eduction, 1, 0),
         feature_set_employment = ifelse(variable_name %in% employment, 1, 0),
         feature_set_housing = ifelse(variable_name %in% housing, 1, 0),
         feature_set_childcare_proximity = ifelse(variable_name %in% childcare_proximity, 1, 0)
         )

# Note: I did not include the following variables in the topic areas: 
# Variable that was not included in our submission: ego_age (I created this later, and I think it is a duplicate of birthday anyway)
# Imputation codes: GBAIMPUTATIECODE, IMPUTATIECODEHH
# Variable that introduces leakage and shouldn't help anyway: DATUMEINDEHH
# Variables that can't be cleanly assigned to just one topic area: 
  # AANVSECM, SECM, AANVSECM_partner, SECM_partner (relates to employment & benefits)
  # INPPOSHHK, INHAHL, INHAHLMI (relates to income, employment, and family structure)
  # INHEHALGR (relates to both housing & benefits)
  # INHPOPIIV, INHSAMAOW, INHSAMHH (relates to household structure and age)
  
# Notes about duplicate variables: 
# Some of the variables included may be extremely similar to each other, to the point of 
# being nearly identical. However, rather than sifting through them all and choosing 
# which ones to use, I simply included everything we used in the PreFer submission
# (except those variables that don't fall cleanly into a single topic area).

# Note: according to Lisa's codebook, age_youngest differs from GEBJAARJONGSTEKINDHH because
# age_youngest is about the ego's bio/adopted children, while GEBJAARJONGSTEKINDHH is about children in household

# Note: some of the "partner" age/sex variables which appear to be duplicates of each other are not  
# duplicates, as some refer only to registered partners, while other refer to all cohabiting partners. 

#### "Demography 101" feature set and associated feature sets ####
demography_101 <- c("ego_age", "GBAGESLACHT", 
                    "has_partner", "AANTALKINDHH", 
                    "live_in_partner_age", "live_in_partner_GBAGESLACHT",
                    # Note to Mark: you can copy-paste the feature names below from the section on line 208
                    "age_household_child_1",
                    "age_household_child_2",
                    "age_household_child_3",
                    "age_household_child_4",
                    "age_household_child_5",
                    "age_household_child_6",
                    "age_household_child_7",
                    "age_household_child_8",
                    "age_household_child_9",
                    "age_household_child_10",
                    "age_household_child_11",
                    "age_household_child_12",
                    "age_household_child_13",
                    "age_household_child_14",
                    "age_household_child_15",
                    "age_household_child_16",
                    "GBAGESLACHT_household_child_1",
                    "GBAGESLACHT_household_child_2",
                    "GBAGESLACHT_household_child_3",
                    "GBAGESLACHT_household_child_4",
                    "GBAGESLACHT_household_child_5",
                    "GBAGESLACHT_household_child_6",
                    "GBAGESLACHT_household_child_7",
                    "GBAGESLACHT_household_child_8",
                    "GBAGESLACHT_household_child_9",
                    "GBAGESLACHT_household_child_10",
                    "GBAGESLACHT_household_child_11",
                    "GBAGESLACHT_household_child_12",
                    "GBAGESLACHT_household_child_13",
                    "GBAGESLACHT_household_child_14",
                    "GBAGESLACHT_household_child_15",
                    "GBAGESLACHT_household_child_16")

hh_child_ages <- c("age_household_child_1",
                   "age_household_child_2",
                   "age_household_child_3",
                   "age_household_child_4",
                   "age_household_child_5",
                   "age_household_child_6",
                   "age_household_child_7",
                   "age_household_child_8",
                   "age_household_child_9",
                   "age_household_child_10",
                   "age_household_child_11",
                   "age_household_child_12",
                   "age_household_child_13",
                   "age_household_child_14",
                   "age_household_child_15",
                   "age_household_child_16")

hh_child_sexes <- c("GBAGESLACHT_household_child_1",
                    "GBAGESLACHT_household_child_2",
                    "GBAGESLACHT_household_child_3",
                    "GBAGESLACHT_household_child_4",
                    "GBAGESLACHT_household_child_5",
                    "GBAGESLACHT_household_child_6",
                    "GBAGESLACHT_household_child_7",
                    "GBAGESLACHT_household_child_8",
                    "GBAGESLACHT_household_child_9",
                    "GBAGESLACHT_household_child_10",
                    "GBAGESLACHT_household_child_11",
                    "GBAGESLACHT_household_child_12",
                    "GBAGESLACHT_household_child_13",
                    "GBAGESLACHT_household_child_14",
                    "GBAGESLACHT_household_child_15",
                    "GBAGESLACHT_household_child_16")

metadata <- metadata %>%
  mutate(feature_set_demography_101 = ifelse(variable_name %in% demography_101, 1, 0), 
         feature_set_hh_child_ages = ifelse(variable_name %in% hh_child_ages, 1, 0),
         feature_set_hh_child_sexes = ifelse(variable_name %in% hh_child_sexes, 1, 0))