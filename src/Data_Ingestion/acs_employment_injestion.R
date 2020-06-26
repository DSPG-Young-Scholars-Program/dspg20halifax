
# Load in Libraries

library(tidycensus)
library(dplyr)
library(tidyr)
library(tigris)
library(glue)
library(sf)


v2018subject <- load_variables(2018, dataset = "acs5/subject", cache = TRUE)

virginia_counties <- counties(state = "VA",
                              class = "sf",
                              cb = TRUE,
                              resolution = "20m") %>%
  st_transform(crs = 4326)

# Unemployment
# -------------------------------------------------------------------

# load county level unemployment data
unemployment_county <- get_acs(geography = "county",
                              year = 2018,
                              table = "S2301",
                              state = "VA") %>%
  left_join(v2018subject, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "_", gsub("!!", "_", label))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")



# bind to spatial data
unemployment_county_sp <- left_join(virginia_counties, unemployment_county, by = c("GEOID"))

# st_write(unemployment_county_sp,
#          here::here("data", "original", "acs_unemployment_county.geojson"),
#          driver = "GeoJSON")


# Poverty
# -------------------------------------------------------------------

poverty_county <- get_acs(geography = "county",
                               year = 2018,
                               table = "S1701",
                               state = "VA") %>%
  left_join(v2018subject, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "_", gsub("!!", "_", label))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")


# bind to spatial data
poverty_county_sp <- left_join(virginia_counties, poverty_county, by = c("GEOID"))



# st_write(poverty_county_sp,
#          here::here("data", "original", "acs_poverty_county.geojson"),
#          driver = "GeoJSON")


