
library(here)
library(leaflet)
library(stringr)
library(dplyr)
library(tidyr)
library(tigris)
library(sf)
library(tidycensus)

## Isochrone retrieval functions
source(here("src", "Mapping", "mapbox_isochrone_functions.R"))

## Mapbox API token
token <- Sys.getenv("MAPBOX_TOKEN")

## VA and Halifax Polygons
va_borders <- counties("VA", resolution = "20m", year = 2015) %>% st_as_sf() %>% st_transform(crs = 4326)
halifax_border <- va_borders %>% filter()

#
#
# School Point Data ----------------------------------------------------------------------------------------------------------------
#
#

## Read in school data and turn lat/long into point data
school_data <- data.table::fread(here("data", "original", "Schools", "halifax_school_data.csv"), colClasses = c(rep("character", 68))) %>% 
  as.data.frame() %>%
  mutate(across(c("latitude", "longitude"), as.numeric))

## Filter to get unique school points rather than pivoting to wide format
school_points <- school_data %>% filter(year == 2015) %>%
  select(ncessch, school_name, latitude, longitude) %>%
  distinct(ncessch, school_name, latitude, longitude) 

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
# Create Isochrone Map ------------------------------------------------------------------------------------------------------------------------
#
#

## Point coordinates for isochrone centers
lihtc_coords <- lihtc_halifax[, c("project", "latitude", "longitude")]
#school_coords <- school_points[, c("school_name", "latitude", "longitude")]

## Get iso polygons based on these points
lihtc_iso_polys <- get_multi_isochrones(profiles = "driving", coords = lihtc_coords, minutes = c(15, 10, 5), token = token)
#school_iso_polys <- get_multi_isochrones(profiles = "driving", coords = school_coords, minutes = c(15, 10, 5), token = token)

## Arrange to make sure smaller contours plotted on top of larger ones
lihtc_iso_polys <- lihtc_iso_polys %>% arrange(desc(contour))
#school_iso_polys <- school_iso_polys %>% arrange(desc(contour))

## Color palette for contour fill
pal <- colorFactor("YlOrRd", domain = as.factor(lihtc_iso_polys$contour), reverse = TRUE)

## Assemble map of point locations for schools, LIHTC projects, and isochrone polygons
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMapPane("Schools", zIndex = 410) %>%
  addMapPane("LIHTC", zIndex = 410) %>%
  addCircleMarkers(data = school_points_sf,
                   color = "blue",
                   radius = 4,
                   label = ~school_name,
                   weight = 1,
                   fillOpacity = 0.6,
                   group = "Schools",
                   options = pathOptions(pane = "Schools")
  ) %>%
  addCircleMarkers(data = lihtc_halifax,
                   lng = ~longitude,
                   lat = ~latitude,
                   color = "red",
                   radius = 4,
                   label = ~project,
                   weight = 1,
                   fillOpacity = 0.6,
                   group = "LIHTC",
                   options = pathOptions(pane = "LIHTC")
  ) %>% 
  plot_multi_isochrones(data = lihtc_iso_polys,
                        color_var = "contour",
                        label_var = "label",
                        opacity = 0,
                        fillOpacity = 0.1,
                        palette = pal) %>%
  addLayersControl(baseGroups = lihtc_iso_polys$label)
