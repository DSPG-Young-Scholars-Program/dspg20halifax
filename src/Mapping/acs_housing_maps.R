
library(here)
library(dplyr)
library(sf)
library(leaflet)
library(geojsonio)

## Read GeoJSON in as sp, convert to sf
geog_mobility <- st_as_sf(geojson_read(here("data","original","acs_geog_mobility.geojson"), what = "sp"))

## Calculate rates for variables of interest
geog_mobility <- geog_mobility %>% 
  mutate(pct_stay = geog_mobility$estimate_total_same.house.1.year.ago_householder.lived.in.renter.occupied.housing.units_estimate / geog_mobility$estimate_total_householder.lived.in.renter.occupied.housing.units_estimate )

## Palette
fill_pal <- colorBin("BuPu", range(geog_mobility$pct_stay), bins = 9)

## Map
leaflet(geog_mobility) %>%
  addTiles() %>%
  addPolygons(
    weight = 1,
    fillColor = ~fill_pal(geog_mobility$pct_stay),
    fillOpacity = 1
  ) %>%
  addLegend(
    "bottomright", 
    pal = fill_pal, 
    values = ~pct_stay,
    title = "Pct of renters in <br>same household from 1 year ago",
    opacity = .8
  )

