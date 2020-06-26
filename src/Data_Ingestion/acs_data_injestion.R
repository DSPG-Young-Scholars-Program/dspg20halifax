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

v2018profile_unique <- v2018profile %>% # this works for this table, not sure if works for all tables
  group_by(label) %>%
  top_n(1, wt = rev(name)) %>%
  ungroup()

# load county wide age sex race variables
acs_age_sex_race_county <- get_acs(geography = "county",
                                      year = 2018,
                                      table = "DP05",
                                      state = "VA") %>%
  inner_join(v2018profile_unique, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "_", gsub("!!", "_", label))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

table(acs_age_sex_race_county$label)

v2018subject <- load_variables(2018, dataset = "acs5/subject", cache = TRUE)



# load county wide income variables
acs_median_income_county <- get_acs(geography = "county",
                                       year = 2018,
                                       table = "S1903",
                                       state = "VA") %>%
  left_join(v2018subject, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "_", gsub("!!", "_", label))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")


get_acs(geography = "county",
        year = 2018,
        table = "DP05",
        state = "VA") %>%
  left_join(v2018subject, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "_", gsub("!!", "_", label))))) %>%
  select(-variable)# %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

# can get this data by age/sex/race, but table measurements are odd and would take work to fix.
acs_transportation_county <- get_acs(geography = "county",
                                     year = 2018,
                                     variables = c(workers_total = "S0802_C01_001",
                                                   workers_drove_alone = "S0802_C02_001",
                                                   workers_carpooled = "S0802_C03_001",
                                                   workers_transit = "S0802_C04_001"),
                                     state = "VA") %>%
  pivot_wider(names_from = variable,
              values_from = c(estimate, moe),
              names_glue = "{variable}_{.value}")

dec_group_pops_county <- get_decennial(geography = "county",
                                       year = 2010,
                                       sumfile = "sf1",
                                       variables = c(paste0(rep("PCT02000"), 1:9),
                                                     paste0(rep("PCT0200"), 10:13)), # only PCT020001-PCT020013 deal with incarceration
                                       state = "VA",
                                       summary_var = "P001001") %>%
  mutate(per_1000 = 1000 * value / summary_value) %>%
  pivot_wider(names_from = variable,
              values_from = c(value, per_1000),
              names_glue = "{variable}_{.value}") %>%
  mutate(fed_state_rate = PCT020004_per_1000 + PCT020005_per_1000 + PCT020005_per_1000) %>%
  mutate(local_rate = PCT020007_per_1000 + PCT020008_per_1000) %>%
  mutate(foster_care_rate = PCT020011_per_1000 + PCT020012_per_1000) %>%
  select(NAME, fed_state_rate, local_rate, foster_care_rate, everything())


virginia_counties <- counties(state = "VA",
                              class = "sf",
                              cb = TRUE,
                              resolution = "20m") %>%
  st_transform(crs = 4326)

virginia_counties_2010 <- counties(state = "VA",
                                   class = "sf",
                                   cb = TRUE,
                                   resolution = "20m",
                                   year = 2010) %>%
  st_transform(crs = 4326) %>%
  mutate(GEOID_short = str_sub(GEO_ID, 10))

# bind to spatial data
acs_age_sex_race_county_sp <- left_join(virginia_counties, acs_age_sex_race_county, by = c("GEOID"))
acs_median_income_county_sp <- left_join(virginia_counties, acs_median_income_county, by = c("GEOID"))
acs_transportation_county_sp <- left_join(virginia_counties, acs_transportation_county, by = c("GEOID"))
dec_group_pops_county_sp <- left_join(virginia_counties_2010, dec_group_pops_county, by = c("GEOID_short" = "GEOID"))


# st_write(acs_age_sex_race_county_sp,
#          here::here("data", "original", "acs_age_sex_race_county.geojson"),
#          driver = "GeoJSON")
# st_write(acs_median_income_county_sp,
#          here::here("data", "original", "acs_median_income_county.geojson"),
#          driver = "GeoJSON")
# st_write(acs_transportation_county_sp,
#          here::here("data", "original", "acs_transportation_county.geojson"),
#          driver = "GeoJSON")
# st_write(dec_group_pops_county_sp,
#          here::here("data", "original", "dec_group_pops_county.geojson"),
#          driver = "GeoJSON")
