
library(here)
library(tidycensus)
library(dplyr)
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

## Calculate rates for variables of interest
geog_mobility <- geog_mobility %>% 
  mutate(pct_stay = geog_mobility$`estimate_total_same-house-1-year-ago_householder-lived-in-renter-occupied-housing-units_estimate` / geog_mobility$`estimate_total_householder-lived-in-renter-occupied-housing-units_estimate`)

## Palette
fill_pal <- colorBin("BuPu", range(geog_mobility$pct_stay), bins = 9)

## Map
leaflet(geog_mobility) %>%
  addTiles() %>%
  addPolygons(
    weight = 1,
    fillColor = ~fill_pal(geog_mobility$pct_stay),
    fillOpacity = 1
  ) %>%
  addLegend(
    "bottomright", 
    pal = fill_pal, 
    values = ~pct_stay,
    title = "Pct of renters in <br>same household from 1 year ago",
    opacity = .8
  )

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

financial_chars <- left_join(va_counties, financial_chars, by = c("GEOID"))


#
#
# Write Files --------------------------------------------------------------------------------
#
#

## Geographic mobility
geojson_write(geog_mobility, geometry = "polygon", file = here("data","working","geog_mobility.geojson"))

## When reading in geo-json, read in as sp, then convert to sf
# test <- st_as_sf(geojson_read(here("data","working","sp_test.geojson"), what = "sp"))

