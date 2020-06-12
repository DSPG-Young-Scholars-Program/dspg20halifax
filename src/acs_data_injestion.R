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
library(glue)

# set my census api key
# census_api_key(my_key)

# make name tables



# get tables we need

v2018profile <- load_variables(2018, dataset = "acs5/profile")

# load county wide age sex race variables
acs_age_sex_race_estimates <- get_acs(geography = "county",
                                      year = 2018,
                                      table = "DP05",
                                      state = "VA") %>%
  left_join(v2018profile, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "-", gsub("!!", "_", label)))))

v2018subject <- load_variables(2018, dataset = "acs5/subject")



# load county wide income variables
acs_median_income_estimates <- get_acs(geography = "county",
                                       year = 2018,
                                       table = "S1903",
                                       state = "VA") %>%
  left_join(v2018subject, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "-", gsub("!!", "_", label)))))

# load county wide unemployment variables
acs_unemployment_estimates <- get_acs(geography = "county",
                                      year = 2018,
                                      table = "S2301",
                                      state = "VA") %>%
  left_join(v2018subject, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "-", gsub("!!", "_", label)))))


# can get this data by age/sex/race, but table measurements are odd and would take work to fix.
acs_transportation_estimates <- get_acs(geography = "county",
                                        year = 2018,
                                        variables = c(workers_total = "S0802_C01_001",
                                                      workers_drove_alone = "S0802_C02_001",
                                                      workers_carpooled = "S0802_C03_001",
                                                      workers_transit = "S0802_C04_001"),
                                        state = "VA")

dec_census_group_pops <- get_decennial(geography = "county",
                                       year = 2010,
                                       sumfile = "sf1",
                                       variables = c(paste0(rep("PCT02000"), 1:9),
                                                     paste0(rep("PCT0200"), 10:13)), # only PCT020001-PCT020013 deal with incarceration
                                       state = "VA",
                                       summary_var = "P001001")

dec_census_group_pops %>%
  pivot_wider(names_from = c(variable),
              values_from = c(value))


