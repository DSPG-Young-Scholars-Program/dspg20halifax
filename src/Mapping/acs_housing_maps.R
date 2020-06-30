
library(here)
library(dplyr)
library(sf)
library(leaflet)
library(geojsonio)

## Read GeoJSON in as sp, convert to sf
geog_mobility <- st_as_sf(geojson_read(here("data","original","Housing", "acs_geog_mobility.geojson"), what = "sp"))

## Various subsets for organizational and processing purposes
# geog_mob_income <- geog_mobility %>% select(c("GEOID", "NAME.x"), contains("individual.income") & contains("same.county"))
# geog_mob_poverty <- geog_mobility %>% select(c("GEOID", "NAME.x"), contains("poverty") & contains("same.county"))
# geog_mob_education <- geog_mobility %>% select(c("GEOID", "NAME.x"), contains("education") & contains("same.county"))
# geog_mob_age <- geog_mobility %>% select(c("GEOID", "NAME.x"), contains("age") & contains("same.county"))
geog_mob_sex <- geog_mobility %>% select(c("GEOID", "NAME.x"), contains("sex") & contains("same.county"))
# geog_mob_race <- geog_mobility %>% select(c("GEOID", "NAME.x"), contains("race") & contains("same.county"))

## Palette
fill_pal <- colorBin("BuPu", range(geog_mob_sex$estimate_moved..within.same.county_population.1.year.and.over_sex_male_estimate), bins = 9)

## Map
leaflet(geog_mob_sex) %>%
  addTiles() %>%
  addPolygons(
    weight = 1,
    fillColor = ~fill_pal(geog_mobility$estimate_moved..within.same.county_population.1.year.and.over_sex_male_estimate),
    fillOpacity = 1
  ) %>%
  addLegend(
    "bottomright", 
    pal = fill_pal, 
    values = ~estimate_moved..within.same.county_population.1.year.and.over_sex_male_estimate,
    title = "Prop. renters in <br>same household from 1 year ago",
    opacity = .8
  )


