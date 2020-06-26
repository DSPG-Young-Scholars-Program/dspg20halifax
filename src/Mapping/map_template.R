library(leaflet)
library(purrr)
library(dplyr)
library(glue)
library(sf)





create_map <- function(.data, # spatial dataset to use
                       variables, # character vector of names to be used, does not include _estimate or _moe at end. Example: estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined
                       group_names, # The names to appear on the radio button widget, determing which variable to display
                       legend_name, # name of the legend
                       label_name, # What precedes the variable value on the hoverover label
                       scale_domain, # over what values is the scale defined? Values outside will be color NA
                       scale_breaks, # what divides the colors of the scale? For a 4 color scale, and example would be c(0, 25, 50, 75, 100) Note endpoints are included
                       unstable_threshold # How many times larger does the estimate have to be than the margin of error to be considered non-null?
                       ) {
  color_scale <- colorBin("BuPu", scale_domain, scale_breaks)

  check_unstable <- function(variable) {
    ifelse((.data[[glue("{variable}_estimate")]]) < unstable_threshold * .data[[glue("{variable}_moe")]],
           NA,
           .data[[glue("{variable}_estimate")]])
  }



  add_poly_layer <- function(map, variable, group_name, color_scale) {
    addPolygons(map, color = "#444444", weight = 0.5, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.8,
                fillColor = color_scale(check_unstable(variable)),
                group = group_name,
                label = ~purrr::map(glue("{NAME.x} County<br/>
                                  {label_name}: {.data[[paste0(variable, \"_estimate\")]]}<br/>
                                  MOE: {.data[[paste0(variable, \"_moe\")]]}"), htmltools::HTML))
  }

  map <- leaflet(.data) %>%
    addTiles()

  for(i in 1:length(variables)) {
    map <- map %>%
      add_poly_layer(variable = variables[i], group_name = group_names[i], color_scale = color_scale)
  }

  map %>%
    addLegend("bottomright", pal = color_scale, values = .data[[glue("{variables[1]}_estimate")]],
              title = "Poverty Rate",
              opacity = .8
    ) %>%
    addLayersControl(
      baseGroups = c("Overall Poverty", "White Alone Poverty", "Black Alone Poverty"),
      options = layersControlOptions(collapsed = FALSE)
    )
}

create_map(acs_poverty_county_sp,
           variables= c("estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined",
                        "estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_white_alone",
                        "estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_black_or_african_american_alone"),
           group_names = c("Overall Poverty",
                           "White Alone Poverty",
                           "Black Alone Poverty"),
           legend_name = "Poverty Rate",
           label_name = "Poverty Rate",
           scale_domain = c(0,100),
           scale_breaks = c(0, 5, 10, 20, 40, 100),
           unstable_threshold = 2)

