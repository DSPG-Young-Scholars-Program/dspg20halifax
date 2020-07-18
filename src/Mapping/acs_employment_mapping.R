library(leaflet)
library(purrr)
library(dplyr)
library(glue)
library(sf)
library(ggplot2)
library(gghighlight)

acs_unemployment_county_sp <- st_read(here::here("data", "original", "ACS", "acs_unemployment_county.geojson"))

# -------------------------------------------------------------------------------------------------

# check ranges for three variables of interest
# range(acs_unemployment_county_sp$estimate_unemployment_rate_population_16_years_and_over_estimate)
# 0.6 23.3
# range(acs_unemployment_county_sp$estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_white_alone_estimate)
# 0.6 14.1
# range(acs_unemployment_county_sp$estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_black_or_african_american_alone_estimate, na.rm = TRUE)
# 0.0, 31.7

# check distributions
# hist(acs_unemployment_county_sp$estimate_unemployment_rate_population_16_years_and_over_estimate)
# hist(acs_unemployment_county_sp$estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_white_alone_estimate)
# hist(acs_unemployment_county_sp$estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_black_or_african_american_alone_estimate)
# breaks seem reasonable

# function to check if moe very high, which I'm calling > 50% of value. Returns NA for elements that are

check_unstable <- function(variable_name) {
  ifelse((acs_unemployment_county_sp[[glue("{variable_name}_estimate")]]) < 2 * acs_unemployment_county_sp[[glue("{variable_name}_moe")]],
         NA,
         acs_unemployment_county_sp[[glue("{variable_name}_estimate")]])

}

# quick map of unemployment by white and black. Note that unemployments above 30% are assumed unstable
unemployment_scale <- colorBin("BuPu", c(0,100), c(0, 3, 5, 7.5, 10, 30))
m <- leaflet(acs_unemployment_county_sp) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = unemployment_scale(check_unstable("estimate_unemployment_rate_population_16_years_and_over")),
              group = "Overall Unemployment",
              label = ~map(glue("{NAME.x} County<br/>
                                Unemployment Rate: {estimate_unemployment_rate_population_16_years_and_over_estimate}%<br/>
                                MOE: {estimate_unemployment_rate_population_16_years_and_over_moe}%"), htmltools::HTML)
              ) %>%
  addPolygons(color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = unemployment_scale(check_unstable("estimate_unemployment_rate_population_16_years_and_over_race_and_hispanic_or_latino_origin_white_alone")),
              group = "White Alone Unemployment",
              label = ~map(glue("{NAME.x} County<br/>
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


###########################################################################################
# Static Plot Using LAUS to Display Change Over Time
###########################################################################################

county_info <- tigris::counties("VA", cb = TRUE, resolution = "20m", year = 2018, class = "sf") %>%
  st_drop_geometry()

laus_unemployment_county <- read.csv(here::here("data", "original","Unemployment", "laus_unemployment_county.csv")) %>%
  mutate(GEOID = substr(Series.ID, 6, 10)) %>%
  left_join(county_info, by = "GEOID") %>%
  mutate(year_month_frac = Year + 1/12 * (as.numeric(substr(Period, 2, 3))-1))


readr::write_csv(laus_unemployment_county, here::here("data", "original","Unemployment", "laus_unemployment_county.csv"))

laus_unemployment_county %>%
  filter(NAME != "Halifax") %>%
  ggplot() +
  geom_line(aes(x = year_month_frac, y = Value, group = NAME), color = "#AAAAAA", alpha = .3) +
  geom_line(data = filter(laus_unemployment_county, NAME == "Halifax"),
            aes(x = year_month_frac, y = Value),
            color = "#d400d0") +
  theme_minimal() +
  labs(x = "Year",
       y = "Unemployment Rate",
       title = "Unemployment Rate of Halifax County",
       subtitle = "Against all other counties in Virginia",
       caption = "Halifax county rate shown in purple. All other counties shown in grey\nData from the Local Area Unemployment Survey") +
  coord_cartesian(ylim = c(0, 20), clip = "off") +
  scale_x_continuous(breaks = seq(2010, 2020, by = 2),
                     labels = as.character(seq(2010, 2020, by = 2))) +
  theme(plot.caption = element_text(color = "#888888"),
        axis.title = element_text(color = "#555555"),
        axis.text = element_text(color = "#555555"),
        plot.subtitle = element_text(color = "#555555"))

# ggsave(here::here("src", "unemployment_plot.png"),
#        width = 10,
#        height = 6)
