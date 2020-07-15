
library(here)
library(leaflet)
library(stringr)
library(dplyr)
library(tidyr)
library(tigris)
library(sf)
library(tidycensus)
library(leaflet.mapboxgl)

## Isochrone retrieval functions
source(here("src", "Mapping", "mapbox_isochrone_functions.R"))

## Mapbox API token
token <- Sys.getenv("MAPBOX_TOKEN")
options(mapbox.accessToken = token)

## VA and Halifax Polygons
va_borders <- counties("VA", year = 2018) %>% st_as_sf() %>% st_transform(crs = 4326)
halifax_border <- va_borders %>% filter(GEOID == "51083")

#
#
# School Point Data ----------------------------------------------------------------------------------------------------------------
#
#

## Read in school data and turn lat/long into point data
school_data <- data.table::fread(here("data", "original", "Schools", "halifax_school_data.csv"), colClasses = c(rep("character", 68))) %>% 
  as.data.frame() %>%
  mutate(across(c("latitude", "longitude", "enrollment"), as.numeric))

## Filter to get unique school points rather than pivoting to wide format
school_points <- school_data %>% filter(year == 2015) %>%
  select(ncessch, school_name, enrollment, latitude, longitude) %>%
  distinct(ncessch, school_name, enrollment, latitude, longitude) 

school_points_sf <- school_points %>%
  st_as_sf(coords = c("longitude", "latitude"))

#
#
# LIHTC Point Data ------------------------------------------------------------------------------------------------------------------------
#
#

## Read in LIHTC Projects
lihtc <- data.table::fread(here("data", "original", "Housing", "LIHTC_data", "lihtc_data_clean")) %>% as.data.frame()

## Subset to areas needed
lihtc_halifax <- lihtc %>% filter(GEOID == "51083", 
                                  hud_id != "VAA19970035") ## This appears to be a duplicate entry resulting from the same project having multiple owners

#
#
# Treatment Centers Point Data ------------------------------------------------------------------------------------------------------------
#
#

treatment_centers <- readr::read_csv(here("data", "original", "Substance_Abuse", "Drug Treatment Centers Near Halifax - Lat Long.csv")) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

#
#
# Employment locations ------------------------------------------------------------------------------------------------------------------------
#
#





#
#
# Create Isochrone Map ------------------------------------------------------------------------------------------------------------------------
#
#

## Point coordinates for isochrone centers
lihtc_coords <- lihtc_halifax[, c("project", "latitude", "longitude")]
#school_coords <- school_points[, c("school_name", "latitude", "longitude")]

## Get iso polygons based on these points
lihtc_iso_polys <- get_multi_isochrones(profiles = "cycling", coords = lihtc_coords, minutes = c(30, 20, 10), token = token)
# lihtc_iso_polys_walk <- get_multi_isochrones(profiles = "walking", coords = lihtc_coords, minutes = c(30, 20, 10), token = token)
# lihtc_iso_polys_drive <- get_multi_isochrones(profiles = "driving", coords = lihtc_coords, minutes = c(30, 20, 10), token = token)

#school_iso_polys <- get_multi_isochrones(profiles = "driving", coords = school_coords, minutes = c(15, 10, 5), token = token)

## Arrange to make sure smaller contours plotted on top of larger ones
lihtc_iso_polys <- lihtc_iso_polys %>% arrange(desc(contour))
#school_iso_polys <- school_iso_polys %>% arrange(desc(contour))

## Color palette for contour fill
pal <- colorFactor("OrRd", domain = as.factor(lihtc_iso_polys$contour), reverse = TRUE)

## Assemble map of point locations for schools, LIHTC projects, and isochrone polygons
leaflet() %>%
  #addProviderTiles("CartoDB.Positron") %>%
  addMapboxGL(style = "mapbox://styles/mapbox/light-v9") %>%
  addMapPane("Schools", zIndex = 410) %>%
  addMapPane("LIHTC", zIndex = 410) %>%
  addMapPane("Treatment Centers", zIndex = 410) %>%
  addCircleMarkers(data = school_points_sf,
                   color = "#7570B3",
                   radius = 4,
                   label = ~school_name,
                   weight = 1,
                   fillOpacity = 0.6,
                   group = "Schools",
                   options = pathOptions(pane = "Schools")) %>%
  addCircleMarkers(data = lihtc_halifax,
                   lng = ~longitude,
                   lat = ~latitude,
                   color = "#1B9E77",
                   radius = 4,
                   label = ~project,
                   weight = 1,
                   fillOpacity = 0.6,
                   group = "LIHTC",
                   options = pathOptions(pane = "LIHTC")) %>% 
  addCircleMarkers(data = treatment_centers,
                   color = "#D95F02",
                   radius = 4,
                   label = ~Name,
                   weight = 1,
                   fillOpacity = 0.6,
                   group = "Treatment Centers",
                   options = pathOptions(pane = "Treatment Centers")) %>%
  plot_multi_isochrones(data = lihtc_iso_polys,
                        color_var = "contour",
                        label_var = "contour",
                        opacity = 0,
                        fillOpacity = 0.3,
                        palette = pal,
                        group_var = "label") %>%
  addPolygons(data = halifax_border,
              fillOpacity = 0,
              color = "gray",
              weight = 2) %>%
  # plot_multi_isochrones(data = lihtc_iso_polys_walk,
  #                       color_var = "contour",
  #                       label_var = "contour",
  #                       opacity = 0,
  #                       fillOpacity = 0.1,
  #                       palette = pal) %>%
  addLayersControl(baseGroups = lihtc_iso_polys$label) %>%
  addLegend(position = "bottomright",
            pal = pal,
            values = test$contour,
            title = "Biking Time (minutes)")
