
library(here)
library(tidycensus)
library(dplyr)
library(tidyr)
library(sf)
library(tigris)
library(leaflet)
library(geojsonio)

census_api_key(Sys.getenv("CENSUS_API_KEY"))

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

## Geographic mobility variables
geog_mobility <- get_acs(geography = "county", year = 2018, table = tables[1], state = "VA") %>%
  left_join(vars_2018, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "-", gsub("!!", "_", label))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

# bind to spatial data
geog_mobility <- left_join(va_counties, geog_mobility, by = c("GEOID"))

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

# ----- Real Estate Subset ---- # - probably not useful

## Extract real estate data and clean up formatting
real_estate <- financial_chars %>% 
  select("GEOID", contains("real-estate") & contains("perc") & !contains("median"))

colnames(real_estate) <- c("GEOID",
                           "pct_owner_taxes_less_800", 
                           "pct_owner_taxes_800_1499", 
                           "pct_owner_taxes_more_1500", 
                           "pct_owner_no_taxes",
                           "pct_owner_taxes_less_800_moe", 
                           "pct_owner_taxes_800_1499_moe", 
                           "pct_owner_taxes_more_1500_moe", 
                           "pct_owner_no_taxes_moe")

real_estate <- left_join(va_counties, real_estate, by = c("GEOID"))

# ----- Rent percentage of income ---- #

rent_data <- get_acs(geography = "county", year = 2018, table = "B25070", state = "VA") %>%
  left_join(vars_2018, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "-", gsub("!!", "_", label))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

rent_data <- left_join(va_counties, rent_data, by = c("GEOID"))

#
#
# Write Files --------------------------------------------------------------------------------
#
#

## Geographic mobility
# geojson_write(geog_mobility, geometry = "polygon", file = here("data","original","acs_geog_mobility.geojson"))


