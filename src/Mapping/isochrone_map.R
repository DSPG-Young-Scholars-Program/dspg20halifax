
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

# google_key <- Sys.getenv("GOOGLE_API_KEY")
# register_google(google_key)

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
lihtc_halifax <- lihtc %>% filter(GEOID == "51083", hud_id != "VAA19970035") ## This appears to be a duplicate entry resulting from the same project having multiple owners

## Additional point locations manually recorded from HUD
assisted_housing_points <- readr::read_csv(here("data", "original", "Housing", "assisted_housing_point_data.csv"))

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

employers <- readr::read_csv(here("data", "original", "Unemployment", "halifax_largest_employers.csv"))

#
#
# Create Isochrone Map ------------------------------------------------------------------------------------------------------------------------
#
#

## Get iso polygons based on these points
housing_iso_polys <- get_multi_isochrones(profiles = "walking", coords = assisted_housing_points, coord_id_col = "project", minutes = c(30, 20, 10), token = token)

## Arrange to make sure smaller contours plotted on top of larger ones
housing_iso_polys <- housing_iso_polys %>% arrange(desc(contour))

## Color palette for contour fill
pal <- colorFactor("OrRd", domain = as.factor(housing_iso_polys$contour), reverse = TRUE)

## Assemble map of point locations for schools, LIHTC projects, and isochrone polygons
leaflet() %>%
  addMapboxGL(style = "mapbox://styles/mapbox/light-v9") %>%
  addMapPane("Schools", zIndex = 410) %>%
  addMapPane("LIHTC", zIndex = 410) %>%
  addMapPane("Treatment Centers", zIndex = 410) %>%
  addMapPane("Employers", zIndex = 410) %>%
  addCircleMarkers(data = school_points_sf,
                   color = "#7570B3",
                   radius = 4,
                   label = ~str_to_title(school_name),
                   weight = 1,
                   fillOpacity = 0.6,
                   group = "Schools",
                   options = pathOptions(pane = "Schools")) %>%
  addCircleMarkers(data = assisted_housing_points,
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
  addCircleMarkers(data = employers[which(employers$empname != "Sentara Healthcare"),],
                   color = "pink",
                   radius = 4,
                   label = ~empname,
                   weight = 1,
                   fillOpacity = 0.6,
                   group = "Employers",
                   options = pathOptions(pane = "Employers")) %>%
  plot_multi_isochrones(data = housing_iso_polys,
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
  addLayersControl(baseGroups = housing_iso_polys$label) %>%
  addLegend(position = "bottomright",
            pal = pal,
            values = housing_iso_polys$contour,
            title = "Driving Time (minutes)")
  
  
