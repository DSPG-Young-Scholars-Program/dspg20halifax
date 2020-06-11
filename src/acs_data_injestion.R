##################################################################
# Desc: Load in useful data from the ACS for the
#   Incarceration/Recidivism in Halifax County Project
#
# Author: Ellen Graham
# ################################################################

# Load in Libraries

library(tidycensus)
library(dplyr)
library(tidyr)
library(sf)
library(tigris)
library(stringr)

# set my census api key
census_api_key(my_key)

# get tables we need

v2018profile <- load_variables(2018, dataset = "acs5/profile")

acs_age_sex_race_estimates <- get_acs(geography = "county",
                          year = 2018,
                          table = "DP05",
                          state = "VA")


acs_age_sex_race_estimates %>%
  left_join(v2018profile, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(label, )))

