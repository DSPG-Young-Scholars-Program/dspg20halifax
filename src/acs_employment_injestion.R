
# Load in Libraries

library(tidycensus)
library(dplyr)
library(tidyr)
library(sf)
library(tigris)
library(stringr)
library(glue)
library(ggplot2)
library(leaflet)
library(purrr)


v2018subject <- load_variables(2018, dataset = "acs5/subject", cache = TRUE)


# load tract level unemployment data
unemployment_tract <- get_acs(geography = "tract",
                              county = "Halifax",
                              year = 2018,
                              table = "S2301",
                              state = "VA") %>%
  left_join(v2018subject, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(",", "", gsub(" ", "_", gsub("!!", "_", label))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

halifax_tracts <- tracts(state = "VA",
                         county = "Halifax",
                         class = "sf") %>%
  st_transform(crs = 4326)

# bind to spatial data
unemployment_tract_sp <- left_join(halifax_tracts, unemployment_tract, by = c("GEOID"))

# -------------------------------------------------------------------------------------------------

# quick map of unemployment by white and black. Notice the huge margins of error!!
unemployment_scale <- colorBin("BuPu", c(0,20), c(0, 2.5, 5, 7.5, 10, 20))
m <- leaflet(unemployment_tract_sp) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = ~unemployment_scale(estimate_unemployment_rate_population_16_years_and_over_estimate),
              group = "Overall Unemployment",
              label = ~map(glue("Tract {NAME.x}<br/>
                                Unemployment Rate: {estimate_unemployment_rate_population_16_years_and_over_estimate}%<br/>
                                MOE: {estimate_unemployment_rate_population_16_years_and_over_moe}%"), htmltools::HTML)
  ) %>%
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = ~unemployment_scale(estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_white_alone_estimate),
              group = "White Alone Unemployment",
              label = ~map(glue("Tract {NAME.x}<br/>
                                Unemployment Rate: {estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_white_alone_estimate}%<br/>
                                MOE: {estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_white_alone_moe}%"), htmltools::HTML)
  ) %>%
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = , fillOpacity = 0.8,
              fillColor = ~unemployment_scale(estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_black_or_african_american_alone_estimate),
              group = "Black Alone Unemployment",
              label = ~map(glue("Tract {NAME.x}<br/>
                                Unemployment Rate: {estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_black_or_african_american_alone_estimate}%<br/>
                                MOE: {estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_black_or_african_american_alone_moe}%"), htmltools::HTML)
  ) %>%
  addLegend("bottomright", pal = unemployment_scale, values = ~estimate_unemployment_rate_population_16_years_and_over_estimate,
            title = "Unemployment Rate",
            opacity = .8,
            labFormat = labelFormat(suffix = "%")
  ) %>%
  addLayersControl(
    baseGroups = c("Overall Unemployment", "White Alone Unemployment", "Black Alone Unemployment"),
    options = layersControlOptions(collapsed = FALSE)
  )
m
