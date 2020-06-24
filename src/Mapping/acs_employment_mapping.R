library(leaflet)
library(purrr)
library(dplyr)
library(glue)

source(here::here("src", "Data_Ingestion", "acs_employment_injestion.R"))

# -------------------------------------------------------------------------------------------------

# check ranges for three variables of interest
# range(unemployment_county$estimate_unemployment_rate_population_16_years_and_over_estimate)
# 0.6 23.3
# range(unemployment_county$estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_white_alone_estimate)
# 0.6 14.1
# range(unemployment_county$estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_black_or_african_american_alone_estimate, na.rm = TRUE)
# 0.0, 31.7

# check distributions
# hist(unemployment_county$estimate_unemployment_rate_population_16_years_and_over_estimate)
# hist(unemployment_county$estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_white_alone_estimate)
# hist(unemployment_county$estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_black_or_african_american_alone_estimate)
# breaks seem reasonable

# function to check if moe very high, which I'm calling > 50% of value. Returns NA for elements that are

check_unstable <- function(variable_name) {
  ifelse((unemployment_county_sp[[glue("{variable_name}_estimate")]]) < 2 * unemployment_county_sp[[glue("{variable_name}_moe")]],
         NA,
         unemployment_county_sp[[glue("{variable_name}_estimate")]])

}

# quick map of unemployment by white and black. Note that unemployments above 20% are assumed unstable
unemployment_scale <- colorBin("BuPu", c(0,100), c(0, 2.5, 5, 7.5, 10, 100))
m <- leaflet(unemployment_county_sp) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = unemployment_scale(check_unstable("estimate_unemployment_rate_population_16_years_and_over")),
              group = "Overall Unemployment",
              label = ~map(glue("Tract {NAME.x}<br/>
                                Unemployment Rate: {estimate_unemployment_rate_population_16_years_and_over_estimate}%<br/>
                                MOE: {estimate_unemployment_rate_population_16_years_and_over_moe}%"), htmltools::HTML)
              ) %>%
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = unemployment_scale(check_unstable("estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_white_alone")),
              group = "White Alone Unemployment",
              label = ~map(glue("Tract {NAME.x}<br/>
                                Unemployment Rate: {estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_white_alone_estimate}%<br/>
                                MOE: {estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_white_alone_moe}%"), htmltools::HTML)
  ) %>%
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = , fillOpacity = 0.8,
              fillColor = unemployment_scale(check_unstable("estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_black_or_african_american_alone")),
              group = "Black Alone Unemployment",
              label = ~map(glue("{NAME.x} County<br/>
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
