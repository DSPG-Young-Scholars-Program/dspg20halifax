
library(here)
library(tidycensus)
library(dplyr)
library(tidyr)
library(sf)
library(tigris)
library(leaflet)
library(geojsonio)
library(ggplot2)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

source(here("src", "Data_Ingestion", "acs_multi_year_import.R"))

## Variable names for ACS
vars_2018 <- load_variables(2018, dataset = "acs5", cache = TRUE)
profile_vars_2018 <- load_variables(2018, dataset = "acs5/profile", cache = TRUE)
subject_vars_2018 <- load_variables(2018, dataset = "acs5/subject", cache = TRUE)

## ACS table IDs
tables <- c("B07013", ## Geographic mobility
            "S2507", ## Financial characteristics - no mortgage
            "S2504", ## Physical characteristics
            "S2502", ## Demographic characteristics
            "DP04") ## General housing statistics

## Spatial data for VA counties
va_counties <- counties(state = "VA",
                        class = "sf",
                        cb = TRUE,
                        resolution = "20m") %>% 
  st_transform(crs = 4326)

#
#
# Geographic Mobility --------------------------------------------------------------------------------
#
#

geog_mobility <- get_acs_multi_years(table = "S0701", var_names = subject_vars_2018, survey = "acs1")

# geojson_write(geog_mobility, geometry = "polygon", file = here("data","original", "Housing", "acs_geog_mobility.geojson"))

#
#
# Financial Characteristics --------------------------------------------------------------------------------
#
#

financial_chars <- get_acs(geography = "county", year = 2018, table = tables[2], state = "VA") %>%
  left_join(subject_vars_2018, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "-", gsub("!!", "_", label))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

financial_chars <- left_join(va_counties, financial_chars, by = c("GEOID")) ## All owner-occupied....?

# ----- Rent percentage of income ---- #

rent_data <- get_acs_multi_years(table = "B25070", var_names = vars_2018, years = seq(2017, 2018))

# geojson_write(rent_data, geometry = "polygon", file = here("data","original","Housing", "acs_rent_data.geojson"))

