
library(here)
library(leaflet)
library(stringr)
library(dplyr)
library(tidyr)
library(tigris)
library(sf)
library(tidycensus)
library(leaflet.mapboxgl)
library(ggmap)
library(glue)

# google_key <- Sys.getenv("GOOGLE_API_KEY")
# register_google(google_key)

## Isochrone retrieval functions
source(here("src", "Mapping", "mapbox_isochrone_functions.R"))

## Mapbox API token
token <- Sys.getenv("MAPBOX_TOKEN")
options(mapbox.accessToken = token)

#
#
# Quicker Maps (RDS files used for website, might as well use them here if making updates) ---------------------------
#
#

employers_points <- readRDS(here::here("data", "working", "isochrones", "employers_points.RDS"))
treatment_points <- readRDS(here::here("data", "working", "isochrones", "treatment_points.RDS"))
school_points <- readRDS(here::here("data", "working", "isochrones", "school_points.RDS"))
housing_points <- readRDS(here::here("data", "working", "isochrones", "housing_points.RDS"))

va_borders <- readRDS(here::here("data", "working", "isochrones", "va_borders.RDS"))
halifax_border <- readRDS(here::here("data", "working", "isochrones", "halifax_border.RDS"))

bike_time_polys <- readRDS(here::here("data", "working", "isochrones", "bike_time_polygons.RDS"))
walk_time_polys <- readRDS(here::here("data", "working", "isochrones", "walk_time_polygons.RDS"))
drive_time_polys <- readRDS(here::here("data", "working", "isochrones", "drive_time_polygons.RDS"))

icons <- awesomeIconList(
  "employers" = makeAwesomeIcon(icon= "briefcase", library = "glyphicon", markerColor = "gray", iconColor = "white", squareMarker = FALSE),
  "schools" = makeAwesomeIcon(icon= "education", library = "glyphicon", markerColor = "darkred", iconColor = "white", squareMarker = FALSE),
  "housing" = makeAwesomeIcon(icon= "home", library = "glyphicon", markerColor = "cadetblue", iconColor = "white", squareMarker = FALSE),
  "treatment" = makeAwesomeIcon(icon= "plus", library = "glyphicon", markerColor = "darkgreen", iconColor = "white", squareMarker = FALSE)
)

## Color palette for contour fill
pal <- colorFactor("OrRd", domain = as.factor(drive_time_polys$contour), reverse = TRUE)

create_map <- function(data) {
  
  ## Assemble map of point locations for schools, LIHTC projects, and isochrone polygons
  map <- leaflet(width = "100%") %>%
    addMapboxGL(style = "mapbox://styles/mapbox/light-v9") %>%
    addMapPane("Schools", zIndex = 410) %>%
    addMapPane("LIHTC", zIndex = 410) %>%
    addMapPane("Treatment Centers", zIndex = 410) %>%
    addMapPane("Employers", zIndex = 410) %>%
    plot_multi_isochrones(data = data,
                          color_var = "contour",
                          opacity = 0.5,
                          weight = 1,
                          fillOpacity = 0.3,
                          palette = pal,
                          group_var = "label") %>%
    addPolygons(data = halifax_border,
                fillOpacity = 0,
                color = "gray",
                weight = 2) %>%
    addAwesomeMarkers(data = employers_points,
                      icon = ~icons["employers"],
                      label = ~purrr::map(glue("<strong>{empname}</strong>", "{codetitle}", .sep = "<br>"), htmltools::HTML),
                      group = "Employers") %>%
    addAwesomeMarkers(data = school_points,
                      icon = ~icons["schools"],
                      label = ~purrr::map(glue("<strong>{str_to_title(school_name)}</strong>", "{street_mailing}, {city_mailing}, {state_mailing}", .sep = "<br>"), htmltools::HTML),
                      group = "Schools") %>%
    addAwesomeMarkers(data = housing_points,
                      icon = ~icons["housing"],
                      label = ~purrr::map(glue("<strong>{project}</strong>", "{address}, {city}, {state}<br>", "{cat_print}", .sep = "<br>"), htmltools::HTML),
                      group = "Housing") %>%
    addAwesomeMarkers(data = treatment_points,
                      icon = ~icons["treatment"],
                      label = ~purrr::map(glue("<strong>{Name}</strong>", "{Street}, {City}, {State}", .sep = "<br>"), htmltools::HTML),
                      group = "Wellness") %>%
    addLayersControl(baseGroups = data$label,
                     overlayGroups = c("Employers", "Schools", "Housing", "Wellness"),
                     options = list(collapsed = FALSE)) %>%
    addLegend(position = "bottomright",
              pal = pal,
              values = data$contour,
              title = glue(str_to_title(data$profile[1]), " Time (minutes)"))
  
  return(map)
  
}

# drive_map <- create_map(drive_time_polys) %>% setView(lat = 36.6987, lng = -78.9014, zoom = 10)
# bike_map <- create_map(bike_time_polys) %>% setView(lat = 36.6987, lng = -78.9014, zoom = 11)
# walk_map <- create_map(walk_time_polys) %>% setView(lat = 36.6987, lng = -78.9014, zoom = 12)
# 
# saveRDS(drive_map, here::here("data", "working", "isochrones", "drive_map.RDS"))
# saveRDS(bike_map, here::here("data", "working", "isochrones", "bike_map.RDS"))
# saveRDS(walk_map, here::here("data", "working", "isochrones", "walk_map.RDS"))


#
#
# Original code to read in these various data sources, if needed ------------------------------------------------
#
#


# ## VA and Halifax Polygons
# va_borders <- counties("VA", year = 2018) %>% st_as_sf() %>% st_transform(crs = 4326)
# halifax_border <- va_borders %>% filter(GEOID == "51083")
# 
# #saveRDS(halifax_border, here::here("data", "working", "isochrones", "halifax_border.RDS"))
# 
# #
# #
# # School Point Data ----------------------------------------------------------------------------------------------------------------
# #
# #
# 
# ## Read in school data and turn lat/long into point data
# school_data <- data.table::fread(here("data", "original", "Schools", "halifax_school_data.csv"), colClasses = c(rep("character", 68))) %>% 
#   as.data.frame() %>%
#   mutate(across(c("latitude", "longitude", "enrollment"), as.numeric))
# 
# ## Filter to get unique school points rather than pivoting to wide format
# school_points <- readr::read_csv(here("data", "original", "Schools", "halifax_school_point_data.csv"))
# 
# school_points_sf <- school_points %>%
#   st_as_sf(coords = c("longitude", "latitude"))
# 
# #
# #
# # LIHTC Point Data ------------------------------------------------------------------------------------------------------------------------
# #
# #
# 
# ## Read in LIHTC Projects
# lihtc <- data.table::fread(here("data", "original", "Housing", "LIHTC_data", "lihtc_data_clean")) %>% as.data.frame()
# 
# ## Subset to areas needed
# lihtc_halifax <- lihtc %>% filter(GEOID == "51083", hud_id != "VAA19970035") ## This appears to be a duplicate entry resulting from the same project having multiple owners
# 
# ## Additional point locations manually recorded from HUD
# assisted_housing_points <- readr::read_csv(here("data", "original", "Housing", "assisted_housing_point_data.csv")) %>%
#   mutate(cat_print = str_replace_all(category, ";", "<br>"))
# 
# #
# #
# # Treatment Centers Point Data ------------------------------------------------------------------------------------------------------------
# #
# #
# 
# treatment_centers <- readr::read_csv(here("data", "original", "Substance_Abuse", "Drug Treatment Centers Near Halifax - Lat Long.csv")) %>% 
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
# 
# #
# #
# # Employment locations ------------------------------------------------------------------------------------------------------------------------
# #
# #
# 
# employers <- readr::read_csv(here("data", "original", "Unemployment", "halifax_largest_employers.csv"))
# 
# #
# #
# # Create Isochrone Map ------------------------------------------------------------------------------------------------------------------------
# #
# #
# 
# icons <- awesomeIconList(
#   "employers" = makeAwesomeIcon(icon= "briefcase", library = "glyphicon", markerColor = "gray", iconColor = "white", squareMarker = FALSE),
#   "schools" = makeAwesomeIcon(icon= "education", library = "glyphicon", markerColor = "darkred", iconColor = "white", squareMarker = FALSE),
#   "housing" = makeAwesomeIcon(icon= "home", library = "glyphicon", markerColor = "cadetblue", iconColor = "white", squareMarker = FALSE),
#   "treatment" = makeAwesomeIcon(icon= "plus", library = "glyphicon", markerColor = "darkgreen", iconColor = "white", squareMarker = FALSE)
# )
# 
# ## Get iso polygons based on these points
# housing_iso_polys_drive <- get_multi_isochrones(profiles = "driving", coords = assisted_housing_points, coord_id_col = "project", minutes = c(30, 20, 10), token = token)
# housing_iso_polys_walk <- get_multi_isochrones(profiles = "walking", coords = assisted_housing_points, coord_id_col = "project", minutes = c(30, 20, 10), token = token)
# housing_iso_polys_bike <- get_multi_isochrones(profiles = "cycling", coords = assisted_housing_points, coord_id_col = "project", minutes = c(30, 20, 10), token = token)
# 
# ## Arrange to make sure smaller contours plotted on top of larger ones
# housing_iso_polys_drive <- housing_iso_polys_drive %>% arrange(desc(contour))
# housing_iso_polys_walk <- housing_iso_polys_walk %>% arrange(desc(contour))
# housing_iso_polys_bike <- housing_iso_polys_bike %>% arrange(desc(contour))
# 
# # saveRDS(housing_iso_polys_walk, file = here::here("data", "working" ,"isochrones", "walk_time_polygons.RDS")) 
# 
# ## Color palette for contour fill
# pal <- colorFactor("OrRd", domain = as.factor(housing_iso_polys_drive$contour), reverse = TRUE)
# 
# ## Assemble map of point locations for schools, LIHTC projects, and isochrone polygons
# leaflet() %>%
#   addMapboxGL(style = "mapbox://styles/mapbox/light-v9") %>%
#   addMapPane("Schools", zIndex = 410) %>%
#   addMapPane("LIHTC", zIndex = 410) %>%
#   addMapPane("Treatment Centers", zIndex = 410) %>%
#   addMapPane("Employers", zIndex = 410) %>%
#   # addCircleMarkers(data = school_points,
#   #                  color = "#7570B3",
#   #                  radius = 4,
#   #                  label = ~purrr::map(glue("<strong>{str_to_title(school_name)}</strong>", "{street_mailing}, {city_mailing}, {state_mailing}", .sep = "<br>"), htmltools::HTML),
#   #                  weight = 1,
#   #                  fillOpacity = 0.6,
#   #                  group = "Schools",
#   #                  options = pathOptions(pane = "Schools")) %>%
#   # addCircleMarkers(data = assisted_housing_points,
#   #                  color = "#1B9E77",
#   #                  radius = 4,
#   #                  label = ~purrr::map(glue("<strong>{project}</strong>", "{address}, {city}, {state}<br>", "{cat_print}", .sep = "<br>"), htmltools::HTML),
#   #                  weight = 1,
#   #                  fillOpacity = 0.6,
#   #                  group = "LIHTC",
#   #                  options = pathOptions(pane = "LIHTC")) %>%
#   # addCircleMarkers(data = treatment_centers,
#   #                  color = "#D95F02",
#   #                  radius = 4,
#   #                  label = ~purrr::map(glue("<strong>{Name}</strong>", "{Street}, {City}, {State}", .sep = "<br>"), htmltools::HTML),
#   #                  weight = 1,
#   #                  fillOpacity = 0.6,
#   #                  group = "Treatment Centers",
#   #                  options = pathOptions(pane = "Treatment Centers")) %>%
#   # addCircleMarkers(data = employers[which(employers$empname != "Sentara Healthcare"),],
#   #                  color = "pink",
#   #                  radius = 4,
#   #                  label = ~purrr::map(glue("<strong>{empname}</strong>", "{codetitle}", .sep = "<br>"), htmltools::HTML),
#   #                  weight = 1,
#   #                  fillOpacity = 0.6,
#   #                  group = "Employers",
#   #                  options = pathOptions(pane = "Employers")) %>%
#   plot_multi_isochrones(data = housing_iso_polys_drive,
#                         color_var = "contour",
#                         opacity = 0.5,
#                         weight = 1,
#                         fillOpacity = 0.3,
#                         palette = pal,
#                         group_var = "label") %>%
#   # plot_multi_isochrones(data = housing_iso_polys_walk,
#   #                       color_var = "contour",
#   #                       opacity = 0.5,
#   #                       weight = 1,
#   #                       fillOpacity = 0.3,
#   #                       palette = pal,
#   #                       group_var = "label") %>%
#   # plot_multi_isochrones(data = housing_iso_polys_bike,
#   #                       color_var = "contour",
#   #                       opacity = 0.5,
#   #                       weight = 1,
#   #                       fillOpacity = 0.3,
#   #                       palette = pal,
#   #                       group_var = "label") %>%
#   addPolygons(data = halifax_border,
#               fillOpacity = 0,
#               color = "gray",
#               weight = 2) %>%
#   addAwesomeMarkers(data = employers,
#                     icon = ~icons["employers"],
#                     label = ~purrr::map(glue("<strong>{empname}</strong>", "{codetitle}", .sep = "<br>"), htmltools::HTML),
#                     group = "Employers") %>%
#   addAwesomeMarkers(data = school_points,
#                     icon = ~icons["schools"],
#                     label = ~purrr::map(glue("<strong>{str_to_title(school_name)}</strong>", "{street_mailing}, {city_mailing}, {state_mailing}", .sep = "<br>"), htmltools::HTML),
#                     group = "Schools") %>%
#   addAwesomeMarkers(data = assisted_housing_points,
#                     icon = ~icons["housing"],
#                     label = ~purrr::map(glue("<strong>{project}</strong>", "{address}, {city}, {state}<br>", "{cat_print}", .sep = "<br>"), htmltools::HTML),
#                     group = "Housing") %>%
#   addAwesomeMarkers(data = treatment_centers,
#                     icon = ~icons["treatment"],
#                     label = ~purrr::map(glue("<strong>{Name}</strong>", "{Street}, {City}, {State}", .sep = "<br>"), htmltools::HTML),
#                     group = "Wellness") %>%
#   addLayersControl(baseGroups = housing_iso_polys_drive$label,
#                    overlayGroups = c("Employers", "Schools", "Housing", "Wellness")) %>%
#   addLegend(position = "bottomright",
#             pal = pal,
#             values = housing_iso_polys_drive$contour,
#             title = "Driving Time (minutes)")




