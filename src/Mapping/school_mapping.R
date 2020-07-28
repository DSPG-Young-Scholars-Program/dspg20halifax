install.packages('leaflet.mapboxgl')
install.packages('ggmap')

devtools::install_github("rstudio/leaflet.mapboxgl")

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

#get census key
census_api_key("1288a5a1e23422dbd03d06071f74b4cd50af12be", install = TRUE)
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

#read data source and VA geographic data
datasource <- readr::read_csv(here::here("git", "TestDSPG", "Halifaxx", "data",
                                         "original", "Schools",
                                         "halifax_school_point_data.csv"))

pops <- get_acs(table = "B01003", geography = "county", year = 2018, state = "VA",
                      survey = "acs5", geometry = TRUE, cache_table = TRUE) %>% st_transform(crs = 4326)

#create label for each school
label <- paste("Name: ", datasource$school_name,"<br/>", "Address: ",
               paste(datasource$street_mailing, datasource$city_mailing,
                     sep = ", "), "<br/>",
               "Level: ", datasource$school_level, "<br/>") %>%
  lapply(htmltools::HTML)

#leaflet code
leaflet() %>%
  addTiles() %>%
  setView(lat = 36.6987, lng = -78.9014, zoom = 10) %>%
  addPolylines(
    data = pops %>% filter(GEOID == "51083") %>% st_transform(crs = 4326),
    color = "red",
    opacity = 1,
    weight = 3
  ) %>%
  addCircleMarkers(
    data = datasource,
    lng = datasource$longitude,
    lat = datasource$latitude,
    label = label,
    weight = 2,
    fillOpacity = 0.6
  )


