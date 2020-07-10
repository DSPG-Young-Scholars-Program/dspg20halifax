
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

source(here("src", "Mapping", "map_template.R"))

## Variable names for ACS
vars_2018 <- load_variables(2018, dataset = "acs5", cache = TRUE)
profile_vars_2018 <- load_variables(2018, dataset = "acs5/profile", cache = TRUE)
subject_vars_2018 <- load_variables(2018, dataset = "acs5/subject", cache = TRUE)

## Polygons for counties and tracts
va_counties <- counties(state = "VA", class = "sf", cb = TRUE, resolution = "20m") %>% 
  st_transform(crs = 4326)

halifax_tracts <- tracts(state = "VA", county = "Halifax", class = "sf", cb = TRUE) %>%
  st_transform(crs = 4326)

## ACS table IDs
# tables <- c("B07013", ## Geographic mobility
#             "S2507", ## Financial characteristics - no mortgage
#             "S2504", ## Physical characteristics
#             "S2502", ## Demographic characteristics
#             "DP04") ## General housing statistics

# ----- Geographic mobility ---- #

## County level geographic mobility variables
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

## Tract level geographic mobility variables
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

## Merge ACS on spatial data
geog_mobility_counties <- left_join(va_counties, geog_mobility, by = c("GEOID"))
geog_mobility_tracts <- left_join(halifax_tracts, geog_mobility_tracts, by = c("GEOID"))

## Plot county map
geog_mobility_counties %>% 
  create_map(variable = "estimate_moved_within_same_county_population_1_year_and_over", 
             group_names = "Intra-County Mobility",
             legend_name = "Moved within County",
             label_name = "Moved within County",
             scale_domain = range(geog_mobility_counties$`estimate_moved_within_same_county_population_1_year_and_over_estimate`),
             scale_breaks = c(0, 2, 4, 6, 8, 10, 12),
             unstable_threshold = 0)

## Plot tract map
geog_mobility_tracts %>% 
  create_map(variable = "estimate_moved_within_same_county_population_1_year_and_over", 
             group_names = "Intra-County Mobility",
             legend_name = "Moved within County",
             label_name = "Moved within County",
             scale_domain = range(geog_mobility_tracts$`estimate_moved_within_same_county_population_1_year_and_over_estimate`),
             scale_breaks = c(0, 2, 4, 6, 8, 10, 12),
             unstable_threshold = 0)

## Sample from independent normals at tract level and plot across layers to show uncertainty in estimates
pal <- colorBin("BuPu", c(geog_mobility_tracts$estimate_moved_within_same_county_population_1_year_and_over_estimate - geog_mobility_tracts$estimate_moved_within_same_county_population_1_year_and_over_moe, geog_mobility_tracts$estimate_moved_within_same_county_population_1_year_and_over_estimate + geog_mobility_tracts$estimate_moved_within_same_county_population_1_year_and_over_moe), bins = 10, na.color = "gray")

map_samples(data = geog_mobility_tracts, 
            var = "estimate_moved_within_same_county_population_1_year_and_over_estimate", 
            se_var = "estimate_moved_within_same_county_population_1_year_and_over_moe", 
            x = 10, 
            palette = pal)

# ----- Rent percentage of income ---- #


#geojson_write(rent_data, geometry = "polygon", file = here("data", "original", "Housing", "acs_rent_data.geojson"))

## Rent as a pct of income at county level
rent_pct_income <- get_acs(geography = "county",
                           year = 2018,
                           table = "B25070",
                           state = "VA") %>%
  left_join(vars_2018, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(";", "", gsub(",", "", gsub(" ", "_", gsub("!!", "_", label)))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

## Rent as a pct of income at tract level
rent_pct_income_tracts <- get_acs(geography = "tract",
                                  year = 2018,
                                  table = "B25070",
                                  state = "VA",
                                  county = "Halifax") %>%
  left_join(vars_2018, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(";", "", gsub(",", "", gsub(" ", "_", gsub("!!", "_", label)))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

## Merge ACS on spatial data
rent_pct_income_counties <- left_join(va_counties, rent_pct_income, by = c("GEOID"))
rent_pct_income_tracts <- left_join(halifax_tracts, rent_pct_income_tracts, by = c("GEOID"))
# 
# rent_pct_income_counties <- rent_pct_income_counties %>% 
#   group_by(GEOID) %>%
#   mutate(pct_pop_greater_than_30_pct_rent_estimate = sum(estimate_total_30.0_to_34.9_percent_estimate, estimate_total_35.0_to_39.9_percent_estimate, estimate_total_40.0_to_49.9_percent_estimate, estimate_total_50.0_percent_or_more_estimate) / estimate_total_estimate)
# 
# rent_pct_income_tracts <- rent_pct_income_tracts %>% 
#   group_by(GEOID) %>%
#   mutate(pct_pop_greater_than_30_pct_rent_estimate = sum(estimate_total_30.0_to_34.9_percent_estimate, estimate_total_35.0_to_39.9_percent_estimate, estimate_total_40.0_to_49.9_percent_estimate, estimate_total_50.0_percent_or_more_estimate) / estimate_total_estimate)

## Plot county map
rent_pct_income_counties %>% 
  create_map(variable = "pct_pop_greater_than_30_pct_rent_estimate", 
             group_names = "Rent Percentage",
             legend_name = "Total < 10 pct",
             label_name = "Total",
             scale_domain = range(rent_pct_income_counties$pct_pop_greater_than_30_pct_rent_estimate),
             scale_breaks = c(0, 200, 400, 600, 800),
             unstable_threshold = 0)

## Plot tract map
rent_pct_income_tracts %>% 
  create_map(variable = "pct_pop_greater_than_30_pct_rent", 
             group_names = "Rent Percentage",
             legend_name = "Total < 10 pct",
             label_name = "Total",
             scale_domain = range(rent_pct_income_tracts$pct_pop_greater_than_30_pct_rent),
             scale_breaks = c(0, 20, 40, 60, 80, 100),
             unstable_threshold = 0)

## Sampling Viz
# pal <- colorBin("BuPu", c(0, 20, 40, 60, 80, 100), bins = 5, na.color = "gray")
# 
# map_samples(data = rent_pct_income_tracts, 
#             var = "estimate_total_less_than_10.0_percent_estimate", 
#             se_var = "estimate_total_less_than_10.0_percent_moe", 
#             moe = TRUE,
#             x = 5, 
#             palette = pal)

# ----- Demographics by housing status ---- #
housing_demo_counties <- get_acs(geography = "county",
                                 year = 2018,
                                 table = "S2502",
                                 state = "VA") %>%
  left_join(subject_vars_2018, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(";", "", gsub(",", "", gsub(" ", "_", gsub("!!", "_", label)))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}") 

housing_demo_tracts <- get_acs(geography = "tract",
                               year = 2018,
                               table = "S2502",
                               state = "VA",
                               county = "Halifax") %>%
  left_join(subject_vars_2018, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(";", "", gsub(",", "", gsub(" ", "_", gsub("!!", "_", label)))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

## Merge ACS on spatial data
housing_demo_counties <- left_join(va_counties, housing_demo_counties, by = c("GEOID")) %>% 
  mutate(rel_pct_renters_white_estimate = `estimate_percent_renter-occupied_housing_units_occupied_housing_units_race_and_hispanic_or_latino_origin_of_householder_one_race_--_white_estimate` / `estimate_percent_occupied_housing_units_occupied_housing_units_race_and_hispanic_or_latino_origin_of_householder_one_race_--_white_estimate`,
         rel_pct_renters_white_moe = 0)

housing_demo_tracts <- left_join(halifax_tracts, housing_demo_tracts, by = c("GEOID")) %>%
  mutate(rel_pct_renters_white_estimate = `estimate_percent_renter-occupied_housing_units_occupied_housing_units_race_and_hispanic_or_latino_origin_of_householder_one_race_--_white_estimate` - `estimate_percent_occupied_housing_units_occupied_housing_units_race_and_hispanic_or_latino_origin_of_householder_one_race_--_white_estimate`,
         rel_pct_renters_white_moe = 0)
  
housing_demo_counties %>% 
  create_map(variable = "rel_pct_renters_white",
             group_names = "Percent Renters White",
             legend_name = "Pct Renters White Relative to base pct",
             label_name = "Pct Renters White",
             scale_domain = range(housing_demo_counties$rel_pct_renters_white_estimate),
             scale_breaks = c(0.5, 0.75, 1, 1.25, 1.5),
             unstable_threshold = 0)

housing_demo_tracts %>% 
  create_map(variable = "rel_pct_renters_white",
             group_names = "Percent Renters White",
             legend_name = "Pct Renters White Relative to base pct",
             label_name = "Pct Renters White",
             scale_domain = range(housing_demo_counties$rel_pct_renters_white_estimate),
             scale_breaks = c(-25, -15, -5, 5, 10, 15, 25),
             unstable_threshold = 0)


# ----- Housing Stock ---- #

housing_stock <- get_acs(geography = "tract",
                         year = 2018,
                         table = "B25001",
                         state = "VA",
                         county = "Halifax") %>%
  left_join(vars_2018, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(";", "", gsub(",", "", gsub(" ", "_", gsub("!!", "_", label)))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

housing_stock <- left_join(halifax_tracts, housing_stock, by = c("GEOID"))

housing_stock %>% 
  create_map(variable = "estimate_total", 
             group_names = "Housing Units",
             legend_name = "Total Housing Units",
             label_name = "Total Units",
             scale_domain = range(housing_stock$estimate_total_estimate),
             scale_breaks = c(1000, 1500, 2000, 2500, 3000),
             unstable_threshold = 0)

pal <- colorBin("BuPu", range(housing_stock$estimate_total_estimate - housing_stock$estimate_total_moe, housing_stock$estimate_total_estimate + housing_stock$estimate_total_moe), bins = 5, na.color = "gray")

map_samples(data = housing_stock, 
            var = "estimate_total_estimate", 
            se_var = "estimate_total_moe",
            moe = TRUE,
            x = 10, 
            palette = pal)


