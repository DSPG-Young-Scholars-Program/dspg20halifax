
library(leaflet)
library(purrr)
library(dplyr)
library(glue)
library(sf)
library(leaflet.mapboxgl)

token <- Sys.getenv("MAPBOX_TOKEN")
options(mapbox.accessToken = token)

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
  
  for (i in 1:length(variables)) {
    map <- map %>%
      add_poly_layer(variable = variables[i], group_name = group_names[i], color_scale = color_scale)
  }
  
  map <- map %>%
    addLegend("bottomright", pal = color_scale, values = .data[[glue("{variables[1]}_estimate")]],
              title = legend_name,
              opacity = .9
    )
  
  if(!is.null(group_names)) {
    map <- map %>%
      addLayersControl(
        baseGroups = group_names,
        options = layersControlOptions(collapsed = FALSE)
      )
  }
  
  map
}


# create_map(acs_poverty_county_sp,
#            variables= c("estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined",
#                         "estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_white_alone",
#                         "estimate_percent_below_poverty_level_population_for_whom_poverty_status_is_determined_race_and_hispanic_or_latino_origin_black_or_african_american_alone"),
#            group_names = c("Overall Poverty",
#                            "White Alone Poverty",
#                            "Black Alone Poverty"),
#            legend_name = "Poverty Rate",
#            label_name = "Poverty Rate",
#            scale_domain = c(0,100),
#            scale_breaks = c(0, 5, 10, 20, 40, 100),
#            unstable_threshold = 0)

#
#
# Generating samples based on estimates and SEs recorded ------------------------------------------------------------------------------------
#
#

## This function takes samples of the variable given and plots them on a single map so the user can toggle between them
## An attempt to visualize uncertainty in small-sample ACS estimates.
## For sampling it assumes that each value is independent of other values (obviously a simplification...)

map_samples <- function(data, ## sf object containing the variable and se_variable
                        var, ## string name of variable of interest
                        se_var, ## string name of variable storing standard error values
                        moe = FALSE, ## Switch to true if the se_var is actually a moe
                        x, ## Number of samples for each region
                        palette,
                        legend_pos = "bottomright",
                        legend_title) {
  
  ## Helper function used in map_samples to add sample polygons 
  addAdjPolygons <- function(map, variable, group_name, palette) {
    addPolygons(map, color = "#444444", weight = 0.5, smoothFactor = 0.5,
                opacity = 0.4, fillOpacity = 0.5,
                fillColor = ~pal(variable),
                group = group_name,
                label = ~round(variable, 2))
  }
  
  ## Create matrix of sample values
  samps <- matrix(nrow = nrow(data), ncol = x)
  colnames(samps) <- paste("Sample", seq(1:x), sep = "_")
  
  ## Sample based on estimates and SEs in the data
  ## Assumes there is a column in the data labeled with the same name as the estimates plus an additional _se
  for (i in seq(1, nrow(data))) {
    
    if (moe == TRUE) {
      samp <- rnorm(x, mean = data[[i,var]], sd = data[[i,se_var]] / 1.64)
      samps[i,] <- samp
    } else {
      samp <- rnorm(x, mean = data[[i,var]], sd = data[[i,se_var]])
      samps[i,] <- samp
    }

  }
  
  ## Add sampled data to sf object
  samp_data <- samps %>% 
    cbind(data) %>% 
    st_as_sf()
  
  ## Generate base map based on actual estimates
  base_map <- leaflet(samp_data, options = leafletOptions(minZoom = 9, maxZoom = 9), width = "100%") %>%
    addMapboxGL(style = "mapbox://styles/mapbox/light-v9") %>%
    addPolygons(fillColor = ~pal(samp_data[[var]]),
                fillOpacity = 0.7,
                opacity = 0.4,
                smoothFactor = 0.5,
                weight = 0.5,
                color = "#444444",
                label = ~round(samp_data[[var]], 2),
                group = "Estimates") %>%
    addLegend(legend_pos,
              title = legend_title,
              pal = pal,
              values = samp_data[[var]])
  
  ## Add all sampled values as separate layers
  for (i in seq(1, x)) {
    variable <- paste("Sample", i, sep = "_")
    base_map <- base_map %>% addAdjPolygons(var = samps[,variable], group_name = gsub("_", " ", variable), palette = pal)
  }
  
  ## Add layers control for toggling
  group_names <- gsub("_", " ", colnames(samps))
  final_map <- base_map %>% addLayersControl(baseGroups = c("Estimates", group_names), options = layersControlOptions(collapsed = FALSE))
  
  return(final_map)
  
}