#######################################################
# Module containing code to create Unemployment Pane
#######################################################

laus_unemployment_county <- readr::read_csv(here::here("data", "original", "Unemployment", "laus_unemployment_county.csv"))
acs_unemployment_county_sp <- st_read(here::here("data", "original", "ACS", "acs_unemployment_county.geojson"))
check_unstable <- function(variable_name) {
  ifelse((acs_unemployment_county_sp[[glue("{variable_name}_estimate")]]) < 2 * acs_unemployment_county_sp[[glue("{variable_name}_moe")]],
         NA,
         acs_unemployment_county_sp[[glue("{variable_name}_estimate")]])

}
laus_unemployment_county_not_halifax <- laus_unemployment_county %>%
  filter(NAME != "Halifax")

laus_unemployment_county_halifax <- laus_unemployment_county %>%
  filter(NAME == "Halifax")

# quick map of unemployment by white and black. Note that unemployments above 30% are assumed unstable
unemployment_scale <- colorBin("BuPu", c(0,100), c(0, 3, 5, 7.5, 10, 30))


unemploymentUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(width = 12,
      fluidRow(
        box(plotOutput(ns("longitudinalPlot")),
            width = 12)
      ),
      fluidRow(
        box(leafletOutput(ns("leafletPlot")),
            width = 12)
      )
    )
  )
}

unemploymentServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$longitudinalPlot <- renderPlot({
          ggplot(laus_unemployment_county_not_halifax) +
          geom_line(aes(x = year_month_frac, y = Value, group = NAME), color = "#AAAAAA", alpha = .3) +
          geom_line(data = laus_unemployment_county_halifax,
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
      })

      output$leafletPlot <- renderLeaflet({
        leaflet(acs_unemployment_county_sp) %>%
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
      })
    }
  )
}
