
library(traveltime)

travel_api_key <- Sys.getenv("TRAVEL_API_KEY")
travel_api_id <- Sys.getenv("TRAVEL_API_ID")

## Read data
lihtc <- data.table::fread(here("data", "original", "Housing", "LIHTC_data", "lihtc_data_clean")) %>% as.data.frame()

## Subset to areas needed
lihtc_halifax <- lihtc %>% filter(GEOID == "51083")
lihtc_va <- lihtc %>% filter(str_detect(GEOID, "^(51)"))

## Get Isochrones
# iso_bike_8 <- traveltime_map(appId = travel_api_id,
#                              apiKey = travel_api_key,
#                              location = c(lihtc_halifax$latitude[1], lihtc_halifax$longitude[1]),
#                              traveltime = 480,
#                              type = "biking",
#                              departure = "2020-08-07T08:00:00+01:00")
# 
# iso_test_10 <- traveltime_map(appId = travel_api_id,
#                              apiKey = travel_api_key,
#                              location = c(lihtc_halifax$latitude[1], lihtc_halifax$longitude[1]),
#                              traveltime = 600,
#                              type = "biking",
#                              departure = "2020-08-07T08:00:00+01:00")
# 
# iso_test_12 <- traveltime_map(appId = travel_api_id,
#                              apiKey = travel_api_key,
#                              location = c(lihtc_halifax$latitude[1], lihtc_halifax$longitude[1]),
#                              traveltime = 720,
#                              type = "biking",
#                              departure = "2020-08-07T08:00:00+01:00")
# 
# iso_drive_8 <- traveltime_map(appId = travel_api_id,
#                              apiKey = travel_api_key,
#                              location = c(lihtc_halifax$latitude[1], lihtc_halifax$longitude[1]),
#                              traveltime = 480,
#                              type = "driving",
#                              departure = "2020-08-07T08:00:00+01:00")
# 
# iso_drive_10 <- traveltime_map(appId = travel_api_id,
#                               apiKey = travel_api_key,
#                               location = c(lihtc_halifax$latitude[1], lihtc_halifax$longitude[1]),
#                               traveltime = 600,
#                               type = "driving",
#                               departure = "2020-08-07T08:00:00+01:00")
# 
# iso_drive_12 <- traveltime_map(appId = travel_api_id,
#                               apiKey = travel_api_key,
#                               location = c(lihtc_halifax$latitude[1], lihtc_halifax$longitude[1]),
#                               traveltime = 720,
#                               type = "driving",
#                               departure = "2020-08-07T08:00:00+01:00")

## Map
leaflet() %>%
  addTiles() %>%
  addPolygons(data = iso_drive_8,
              fillColor = "red",
              fillOpacity = 0.2,
              opacity = 0) %>%
  addPolygons(data = iso_drive_10,
              fillColor = "orange",
              fillOpacity = 0.2,
              opacity = 0) %>%
  addPolygons(data = iso_drive_12,
              fillColor = "yellow",
              fillOpacity = 0.2,
              opacity = 0) %>%
  addCircleMarkers(data = lihtc_halifax,
                   lng = lihtc_halifax$longitude[1],
                   lat = lihtc_halifax$latitude[1],
                   label = lihtc_halifax$project,
                   weight = 2,
                   fillOpacity = 0.6)

