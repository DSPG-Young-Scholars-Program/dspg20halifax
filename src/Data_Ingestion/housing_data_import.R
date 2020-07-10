
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

va_counties <- counties(state = "VA", class = "sf", cb = TRUE, resolution = "20m") %>% 
  st_transform(crs = 4326)

halifax_tracts <- tracts(state = "VA", county = "Halifax", class = "sf", cb = TRUE) %>%
  st_transform(crs = 4326)

## ACS table IDs
tables <- c("B07013", ## Geographic mobility
            "S2507", ## Financial characteristics - no mortgage
            "S2504", ## Physical characteristics
            "S2502", ## Demographic characteristics
            "DP04") ## General housing statistics

# ----- Geographic mobility ---- #

#geog_mobility <- get_acs_multi_years(table = "S0701", var_names = subject_vars_2018, survey = "acs1", years = 2010)
#geog_mobility <- get_acs_multi_years(table = "S0701", var_names = subject_vars_2018, survey = "acs5", years = 2018)

#geojson_write(geog_mobility, geometry = "polygon", file = here("data", "original", "Housing", "acs5_2018_geog_mobility.geojson"))

geog_mobility <- get_acs(geography = "county",
                         year = 2018,
                         table = "S0701",
                         state = "VA") %>%
  left_join(subject_vars_2018, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(";", "", gsub(",", "", gsub(" ", "_", gsub("!!", "_", label)))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

geog_mobility_tracts <- get_acs(geography = "tract",
                                year = 2018,
                                table = "S0701",
                                state = "VA",
                                county = "Halifax") %>%
  left_join(subject_vars_2018, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(";", "", gsub(",", "", gsub(" ", "_", gsub("!!", "_", label))))))%>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

geog_mobility_counties <- left_join(va_counties, geog_mobility, by = c("GEOID"))
geog_mobility_tracts <- left_join(halifax_tracts, geog_mobility_tracts, by = c("GEOID"))


source(here("src", "Mapping", "acs_se_viz_experiment.R"))

geog_mobility_counties %>% 
  create_map(variable = "estimate_moved_within_same_county_population_1_year_and_over", 
             group_names = "Intra-County Mobility",
             legend_name = "Intra-County Mobility",
             label_name = "Intra_County Mobility",
             scale_domain = range(geog_mobility_counties$`estimate_moved;_within_same_county_population_1_year_and_over_estimate`),

             
             unstable_threshold = 2)

geog_mobility_tracts %>% 
  create_map(variable = "estimate_moved_within_same_county_population_1_year_and_over", 
             group_names = "Intra-County Mobility",
             legend_name = "Intra-County Mobility",
             label_name = "Intra_County Mobility",
             scale_domain = range(geog_mobility_counties$`estimate_moved;_within_same_county_population_1_year_and_over_estimate`),
             bins = 5,
             unstable_threshold = 0)

pal <- colorBin("BuPu", c(0, max(geog_mobility_tracts$`estimate_moved;_within_same_county_population_1_year_and_over_estimate`) + max(geog_mobility_tracts$`estimate_moved;_within_same_county_population_1_year_and_over_moe`)), bins = 5, na.color = "#fafafa")

map_samples(data = geog_mobility_tracts, 
            var = "estimate_moved_within_same_county_population_1_year_and_over_estimate", 
            se_var = "estimate_moved_within_same_county_population_1_year_and_over_moe", 
            x = 5, 
            palette = pal)


# ----- Rent percentage of income ---- #

rent_data <- get_acs_multi_years(table = "B25070", var_names = vars_2018, years = seq(2010, 2018))

#geojson_write(rent_data, geometry = "polygon", file = here("data", "original", "Housing", "acs_rent_data.geojson"))

# ----- Demographics by housing status ---- #

## Was having trouble with data from 2010-2012
housing_demo_data <- get_acs_multi_years(table = "S2502", var_names = subject_vars_2018, years = seq(2013, 2018))

#geojson_write(housing_demo_data, geometry = "polygon", file = here("data", "original", "Housing", "housing_demo_data.geojson"))

