library(tidyverse) # 2.0.0 on CBS server
library(fastDummies) # 1.7.3 on CBS server

#### READ IN DATA ####
household_data <- left_join(gbahuishoudensbus_train_focal_people_only_current_household, 
                            child_age_and_sex_by_household, 
                            by = join_by(household_id)) %>%
  select(-household_id)
data <- list(gbapersoontab_train, household_data, prefer_official_train, live_in_partner_data, features_from_familienetwerktab, ego_age) %>% 
  reduce(full_join, by = "RINPERSOON") %>%
  select(-ends_with(".y")) %>%
  rename(GBAGEBOORTELAND = GBAGEBOORTELAND.x,
         GBAGESLACHT = GBAGESLACHT.x,
         GBAGEBOORTELANDMOEDER = GBAGEBOORTELANDMOEDER.x,
         GBAGEBOORTELANDVADER = GBAGEBOORTELANDVADER.x,
         GBAAANTALOUDERSBUITENLAND = GBAAANTALOUDERSBUITENLAND.x,
         GBAHERKOMSTGROEPERING = GBAHERKOMSTGROEPERING.x,
         GBAGENERATIE = GBAGENERATIE.x,
         GBAGEBOORTEJAAR = GBAGEBOORTEJAAR.x,
         GBAGESLACHTMOEDER = GBAGESLACHTMOEDER.x,
         GBAGESLACHTVADER = GBAGESLACHTVADER.x,
         GBAGEBOORTEJAARMOEDER = GBAGEBOORTEJAARMOEDER.x,
         GBAGEBOORTEJAARVADER = GBAGEBOORTEJAARVADER.x,
         DATUMAANVANGHH = DATUMAANVANGHH.x,
         TYPHH = TYPHH.x,
         AANTALPERSHH = AANTALPERSHH.x,
         AANTALOVHH = AANTALOVHH.x,
         GEBJAARJONGSTEKINDHH = GEBJAARJONGSTEKINDHH.x)

# Remove files to clear up memory 
rm(gbahuishoudensbus_train_focal_people_only_current_household)
rm(child_age_and_sex_by_household)
rm(gbapersoontab_train)
rm(prefer_official_train)
rm(live_in_partner_data)
rm(features_from_familienetwerktab)
rm(ego_age)
gc()


# Set "has_partner" to 0 for anyone where has_partner is NA (the current values are only 1 and NA)
# For catboost, we could have simply left these as NA, but we want the ability to use other model types too, so we're applying this extra step.
data <- data %>%
  mutate(has_partner = ifelse(is.na(has_partner), 0, has_partner))

# Use metadata to identify variables types 
continuous_variables <- metadata %>%
  filter(variable_type == "continuous") %>%
  pull(variable_name)
categorical_variables <- metadata %>%
  filter(variable_type == "categorical_or_binary_original_variable") %>%
  pull(variable_name)

#### PREPROCESS DATA ####

# Columns of type "integer64" need to be converted to numeric
classes <- map_chr(data, class)
integer64 <- names(classes)[which(classes == "integer64")]
data <- mutate(data,
  across(all_of(integer64), as.numeric),
  
  # Address missingness
  INPBELI = ifelse(INPBELI == 9999999999, NA, INPBELI),
  INPP100PBRUT = ifelse(INPP100PBRUT < 0, NA, INPP100PBRUT),
  INPP100PPERS = ifelse(INPP100PPERS < 0, NA, INPP100PPERS),
  INPPERSPRIM = ifelse(INPPERSPRIM == 9999999999, NA, INPPERSPRIM),
  INHAHL = ifelse(INHAHL == 99, NA, INHAHL),
  INHAHLMI = ifelse(INHAHLMI == 99, NA, INHAHLMI),
  INHARMEUR = ifelse(INHARMEUR < 0, NA, INHARMEUR),
  INHARMEURL = ifelse(INHARMEURL < 0, NA, INHARMEURL),
  INHBRUTINKH = ifelse(INHBRUTINKH == 9999999999, NA, INHBRUTINKH),
  INHUAF = ifelse(INHUAF < 0, NA, INHUAF),
  INHUAFL = ifelse(INHUAFL < 0, NA, INHUAFL),
  VEHP100WELVAART = ifelse(VEHP100WELVAART < 0, NA, VEHP100WELVAART),
  VEHP100HVERM = ifelse(VEHP100HVERM < 0, NA, VEHP100HVERM),
  VEHW1000VERH = ifelse(VEHW1000VERH == 99999999999, NA, VEHW1000VERH),
  VEHW1100BEZH = ifelse(VEHW1100BEZH == 99999999999, NA, VEHW1100BEZH),
  VEHW1110FINH = ifelse(VEHW1110FINH == 99999999999, NA, VEHW1110FINH),
  VEHW1120ONRH = ifelse(VEHW1120ONRH == 99999999999, NA, VEHW1120ONRH),
  VEHW1130ONDH = ifelse(VEHW1130ONDH == 99999999999, NA, VEHW1130ONDH),
  VEHW1140ABEH = ifelse(VEHW1140ABEH == 99999999999, NA, VEHW1140ABEH),
  VEHW1150OVEH = ifelse(VEHW1150OVEH == 99999999999, NA, VEHW1150OVEH),
  VEHW1200STOH = ifelse(VEHW1200STOH == 99999999999, NA, VEHW1200STOH),
  VEHW1210SHYH = ifelse(VEHW1210SHYH == 99999999999, NA, VEHW1210SHYH),
  VEHW1220SSTH = ifelse(VEHW1220SSTH == 99999999999, NA, VEHW1220SSTH),
  VEHW1230SOVH = ifelse(VEHW1230SOVH == 99999999999, NA, VEHW1230SOVH),
  VEHWVEREXEWH = ifelse(VEHWVEREXEWH == 99999999999, NA, VEHWVEREXEWH),
  across(any_of(continuous_variables), ~as.numeric(ifelse(.x %in% c("-", "--", "---", "----", "0000"), NA, .x))),
  
  # One-hot encode categorical variables
  across(any_of(categorical_variables), ~as.factor(paste0("_", .x)))) # %>%
  # dummy_cols(select_columns = categorical_variables)

# #### UPDATE METADATA ####
# dummies <- colnames(data)[grepl("__", colnames(data))]
# get_metadata <- function(dummy) {
#   filter(metadata, variable_name == sub("__.*", "", dummy)) %>%
#     mutate(variable_name = dummy,
#            variable_type = "binary_one_hot")
# }
# metadata <- map_dfr(dummies, get_metadata) %>%
#   bind_rows(metadata)
binary_one_hot_variables <- c()
# binary_one_hot_variables <- metadata %>%
#   filter(variable_type == "binary_one_hot") %>%
#   pull(variable_name)