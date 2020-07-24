#######################################################
# Module containing code to create Housing Pane
#######################################################

# source(here::here("src", "Mapping", "mapbox_isochrone_functions.R"))
# source(here::here("src", "Mapping", "isochrone_map.R"))
# 
# ## Get iso polygons based on these points
# housing_iso_polys_drive <- get_multi_isochrones(profiles = "driving", coords = assisted_housing_points, coord_id_col = "project", minutes = c(30, 20, 10), token = token)
# housing_iso_polys_walk <- get_multi_isochrones(profiles = "walking", coords = assisted_housing_points, coord_id_col = "project", minutes = c(30, 20, 10), token = token)
# housing_iso_polys_bike <- get_multi_isochrones(profiles = "cycling", coords = assisted_housing_points, coord_id_col = "project", minutes = c(30, 20, 10), token = token)
# 
# # ## Arrange to make sure smaller contours plotted on top of larger ones
# housing_iso_polys_drive <- housing_iso_polys_drive %>% arrange(desc(contour))
# housing_iso_polys_walk <- housing_iso_polys_walk %>% arrange(desc(contour))
# housing_iso_polys_bike <- housing_iso_polys_bike %>% arrange(desc(contour))
# 
# ## Color palette for contour fill
# pal <- colorFactor("OrRd", domain = as.factor(housing_iso_polys_drive$contour), reverse = TRUE)

isochroneUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      h2("Resource Accessibility"),
      fluidRow(
        column(12, leafletOutput(ns("map"), width = "100%", height = 1200))
      )
      
      
      # box(
      #   title = "Controls",
      #   sliderInput(ns("slider"), "Number of observations:", 1, 100, 50)
      # )
    )
  )
}

isochroneServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$map <- renderLeaflet({
        
        ## Assemble map of point locations for schools, LIHTC projects, and isochrone polygons
        leaflet() %>%
          addMapboxGL(style = "mapbox://styles/mapbox/light-v9") %>%
          addMapPane("Schools", zIndex = 410) %>%
          addMapPane("LIHTC", zIndex = 410) %>%
          addMapPane("Treatment Centers", zIndex = 410) %>%
          addMapPane("Employers", zIndex = 410) %>%
          # addCircleMarkers(data = school_points,
          #                  color = "#7570B3",
          #                  radius = 4,
          #                  label = ~purrr::map(glue("<strong>{str_to_title(school_name)}</strong>", "{street_mailing}, {city_mailing}, {state_mailing}", .sep = "<br>"), htmltools::HTML),
          #                  weight = 1,
          #                  fillOpacity = 0.6,
          #                  group = "Schools",
          #                  options = pathOptions(pane = "Schools")) %>%
          # addCircleMarkers(data = assisted_housing_points,
          #                  color = "#1B9E77",
          #                  radius = 4,
        #                  label = ~purrr::map(glue("<strong>{project}</strong>", "{address}, {city}, {state}<br>", "{cat_print}", .sep = "<br>"), htmltools::HTML),
        #                  weight = 1,
        #                  fillOpacity = 0.6,
        #                  group = "LIHTC",
        #                  options = pathOptions(pane = "LIHTC")) %>%
        # addCircleMarkers(data = treatment_centers,
        #                  color = "#D95F02",
        #                  radius = 4,
        #                  label = ~purrr::map(glue("<strong>{Name}</strong>", "{Street}, {City}, {State}", .sep = "<br>"), htmltools::HTML),
        #                  weight = 1,
        #                  fillOpacity = 0.6,
        #                  group = "Treatment Centers",
        #                  options = pathOptions(pane = "Treatment Centers")) %>%
        # addCircleMarkers(data = employers[which(employers$empname != "Sentara Healthcare"),],
        #                  color = "pink",
        #                  radius = 4,
        #                  label = ~purrr::map(glue("<strong>{empname}</strong>", "{codetitle}", .sep = "<br>"), htmltools::HTML),
        #                  weight = 1,
        #                  fillOpacity = 0.6,
        #                  group = "Employers",
        #                  options = pathOptions(pane = "Employers")) %>%
        plot_multi_isochrones(data = housing_iso_polys_drive,
                              color_var = "contour",
                              opacity = 0.5,
                              weight = 1,
                              fillOpacity = 0.3,
                              palette = pal,
                              group_var = "label") %>%
          # plot_multi_isochrones(data = housing_iso_polys_walk,
          #                       color_var = "contour",
          #                       opacity = 0.5,
          #                       weight = 1,
          #                       fillOpacity = 0.3,
          #                       palette = pal,
          #                       group_var = "label") %>%
          # plot_multi_isochrones(data = housing_iso_polys_bike,
          #                       color_var = "contour",
          #                       opacity = 0.5,
          #                       weight = 1,
        #                       fillOpacity = 0.3,
        #                       palette = pal,
        #                       group_var = "label") %>%
        addPolygons(data = halifax_border,
                    fillOpacity = 0,
                    color = "gray",
                    weight = 2) %>%
          addAwesomeMarkers(data = employers,
                            icon = ~icons["employers"],
                            label = ~purrr::map(glue("<strong>{empname}</strong>", "{codetitle}", .sep = "<br>"), htmltools::HTML),
                            group = "Employers") %>%
          addAwesomeMarkers(data = school_points,
                            icon = ~icons["schools"],
                            label = ~purrr::map(glue("<strong>{str_to_title(school_name)}</strong>", "{street_mailing}, {city_mailing}, {state_mailing}", .sep = "<br>"), htmltools::HTML),
                            group = "Schools") %>%
          addAwesomeMarkers(data = assisted_housing_points,
                            icon = ~icons["housing"],
                            label = ~purrr::map(glue("<strong>{project}</strong>", "{address}, {city}, {state}<br>", "{cat_print}", .sep = "<br>"), htmltools::HTML),
                            group = "Housing") %>%
          addAwesomeMarkers(data = treatment_centers,
                            icon = ~icons["treatment"],
                            label = ~purrr::map(glue("<strong>{Name}</strong>", "{Street}, {City}, {State}", .sep = "<br>"), htmltools::HTML),
                            group = "Wellness") %>%
          addLayersControl(baseGroups = housing_iso_polys_drive$label,
                           overlayGroups = c("Employers", "Schools", "Housing", "Wellness")) %>%
          addLegend(position = "bottomright",
                    pal = pal,
                    values = housing_iso_polys_drive$contour,
                    title = "Driving Time (minutes)")
        
      })
      
    }
  )
}
