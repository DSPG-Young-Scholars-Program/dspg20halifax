library(leaflet)
library(purrr)
library(dplyr)
library(glue)
library(sf)

acs_poverty_county_sp <- st_read(here::here("data", "original", "acs_poverty_county.geojson"))

# -------------------------------------------------------------------------------------------------

# check ranges for three variables of interest
# range(acs_poverty_county_sp$estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_estimate)
# 2.7 35.9
# range(acs_poverty_county_sp$estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_white_alone_estimate)
# 2.2 35.2
# range(range(acs_poverty_county_sp$estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_black_or_african_american_alone_estimate, na.rm = TRUE))
# 0.0, 88.1

# check distributions
hist(acs_poverty_county_sp$estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_estimate)
hist(acs_poverty_county_sp$estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_white_alone_estimate)
hist(acs_poverty_county_sp$estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_black_or_african_american_alone_estimate)
# I bet anything over ~40 is due to absurd margins of error

# function to check if moe very high, which I'm calling > 50% of value. Returns NA for elements that are

check_unstable <- function(variable_name) {
  ifelse((acs_poverty_county_sp[[glue("{variable_name}_estimate")]]) < 2 * acs_poverty_county_sp[[glue("{variable_name}_moe")]],
         NA,
         acs_poverty_county_sp[[glue("{variable_name}_estimate")]])
}

# range(check_unstable("estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined"), na.rm = TRUE)
# range(check_unstable("estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_white_alone"), na.rm = TRUE)
# range(check_unstable("estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_black_or_african_american_alone"), na.rm = TRUE)



# quick map of unemployment by white and black. Note that unemployments above 30% are assumed unstable
poverty_scale <- colorBin("BuPu", c(0,100), c(0, 5, 10, 20, 40, 100))
m <- leaflet(acs_poverty_county_sp) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = poverty_scale(check_unstable("estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined")),
              group = "Overall Poverty",
              label = ~map(glue("{NAME.x} County<br/>
                                Poverty Rate: {estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_estimate}%<br/>
                                MOE: {estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_moe}%"), htmltools::HTML)
              ) %>%
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = poverty_scale(check_unstable("estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_white_alone")),
              group = "White Alone Poverty",
              label = ~map(glue("{NAME.x} County<br/>
                                Poverty Rate: {estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_white_alone_estimate}%<br/>
                                MOE: {estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_white_alone_moe}%"), htmltools::HTML)
  ) %>%
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = , fillOpacity = 0.8,
              fillColor = poverty_scale(check_unstable("estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_black_or_african_american_alone")),
              group = "Black Alone Poverty",
              label = ~map(glue("{NAME.x} County<br/>
                                Poverty Rate: {estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_black_or_african_american_alone_estimate}%<br/>
                                MOE: {estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_black_or_african_american_alone_moe}%"), htmltools::HTML)
  ) %>%
  addLegend("bottomright", pal = poverty_scale, values = ~estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_estimate,
            title = "Poverty Rate",
            opacity = .8,
            labFormat = labelFormat(suffix = "%")
  ) %>%
  addLayersControl(
    baseGroups = c("Overall Poverty", "White Alone Poverty", "Black Alone Poverty"),
    options = layersControlOptions(collapsed = FALSE)
  )

m
