
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
vars_2018 <- load_variables(2018, dataset = "acs1", cache = TRUE)
profile_vars_2018 <- load_variables(2018, dataset = "acs1/profile", cache = TRUE)
subject_vars_2018 <- load_variables(2018, dataset = "acs1/subject", cache = TRUE)

## ACS table IDs
tables <- c("B07013", ## Geographic mobility
            "S2507", ## Financial characteristics - no mortgage
            "S2504", ## Physical characteristics
            "S2502", ## Demographic characteristics
            "DP04") ## General housing statistics

# ----- Geographic mobility ---- #

#geog_mobility <- get_acs_multi_years(table = "S0701", var_names = subject_vars_2018, survey = "acs1", years = 2010)
geog_mobility <- get_acs_multi_years(table = "S0701", var_names = subject_vars_2018, survey = "acs5", years = 2018)

#geojson_write(geog_mobility, geometry = "polygon", file = here("data", "original", "Housing", "acs5_2018_geog_mobility.geojson"))

# ----- Rent percentage of income ---- #

rent_data <- get_acs_multi_years(table = "B25070", var_names = vars_2018, years = seq(2010, 2018))

#geojson_write(rent_data, geometry = "polygon", file = here("data", "original", "Housing", "acs_rent_data.geojson"))

# ----- Demographics by housing status ---- #

## Was having trouble with data from 2010-2012
housing_demo_data <- get_acs_multi_years(table = "S2502", var_names = subject_vars_2018, years = seq(2013, 2018))

#geojson_write(housing_demo_data, geometry = "polygon", file = here("data", "original", "Housing", "housing_demo_data.geojson"))

