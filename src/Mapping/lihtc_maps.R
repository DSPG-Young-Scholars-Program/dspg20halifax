
library(here)
library(leaflet)
library(stringr)
library(dplyr)
library(tidyr)
library(tigris)
library(sf)
library(tidycensus)

## Read data
lihtc <- data.table::fread(here("data", "original", "Housing", "LIHTC_data", "lihtc_data_clean")) %>% as.data.frame()

## Subset to areas needed
lihtc_halifax <- lihtc %>% filter(GEOID == "51083")
lihtc_va <- lihtc %>% filter(str_detect(GEOID, "^(51)"))

## Population data from ACS
pops <- get_acs(table = "B01003", geography = "county", year = 2018, state = "VA", survey = "acs5", geometry = TRUE)

#
#
# Point Locations of Projects ----------------------------------------------------------------------------
#
#

## Location of LIHTC projects in Halifax
halifax_border <- pops %>% filter(GEOID == "51083")

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = lihtc_halifax,
    lng = lihtc_halifax$longitude,
    lat = lihtc_halifax$latitude,
    label = lihtc_halifax$project,
    weight = 2,
    fillOpacity = 0.6
  )

#
#
# Projects by County ----------------------------------------------------------------------------
#
#

## Number of LIHTC projects per county, adjusted for population
lihtc_va_tmp <- lihtc_va %>%
  group_by(GEOID) %>%
  summarize(n = n(), med_year = median(yr_pis, na.rm = TRUE))

lihtc_va_summary <- left_join(pops, lihtc_va_tmp, by = "GEOID") %>% 
  st_transform(crs = 4326)

lihtc_va_summary[which(is.na(lihtc_va_summary$n)), "n"] <- 0

tmp <- lihtc_va_summary %>% mutate(n_per_cap = (n / estimate) * 10000)

pal <- colorBin("BuPu", range(tmp$n_per_cap, na.rm = TRUE), bins = 5)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = tmp,
    weight = 2,
    fillOpacity = 1,
    fillColor = ~pal(n_per_cap),
    label = ~round(n_per_cap, 2)
  ) %>%
  addPolylines(
    data = pops %>% filter(GEOID == "51083") %>% st_transform(crs = 4326),
    color = "red",
    opacity = 0.8,
    weight = 3
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = tmp$n_per_cap,
    title = "Projects per 10000 people"
  )

# -----

## Map of average year of project allocation
pal <- colorBin("YlGnBu", range(lihtc_va_summary$med_year, na.rm = TRUE), bins = 5)

leaflet() %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addPolygons(
    data = lihtc_va_summary,
    weight = 2,
    color = "#fafafa",
    opacity = 0.3,
    fillOpacity = 0.8,
    fillColor = ~pal(med_year),
    label = ~round(med_year)
  ) %>% addPolylines(
    data = pops %>% filter(GEOID == "51083") %>% st_transform(crs = 4326),
    color = "red",
    opacity = 1,
    weight = 3
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = lihtc_va_summary$med_year,
    title = "Median Year Placed in Service",
    labFormat = labelFormat(big.mark = "")
  )


#
#
# Allocation Dollars by County ----------------------------------------------------------------------------
#
#

## Allocation amounts
lihtc_va_tmp <- lihtc_va %>% group_by(GEOID) %>% summarize(med_alloc = median(allocamt, na.rm = TRUE), n = n())

lihtc_va_summary <- left_join(pops, lihtc_va_tmp, by = "GEOID") %>% 
  st_transform(crs = 4326)

tmp <- lihtc_va_summary %>% mutate(alloc_pop_adj = (med_alloc / estimate))

pal <- colorBin("BuPu", range(tmp$alloc_pop_adj, na.rm = TRUE), bins = 5)

## Per person
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = tmp,
    weight = 2,
    fillOpacity = 1,
    fillColor = ~pal(alloc_pop_adj),
    label = ~round(alloc_pop_adj, 2)
  ) %>%
  addPolylines(
    data = pops %>% filter(GEOID == "51083") %>% st_transform(crs = 4326),
    color = "red",
    opacity = 0.8,
    weight = 3
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = tmp$alloc_pop_adj,
    title = "Median allocation Dollars per Person"
  )

## Per Project
tmp <- lihtc_va_summary %>% mutate(alloc_proj_adj = (med_alloc / n))

pal <- colorBin("BuPu", range(tmp$alloc_proj_adj, na.rm = TRUE), bins = 5)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = tmp,
    weight = 2,
    fillOpacity = 1,
    fillColor = ~pal(alloc_proj_adj),
    label = ~round(alloc_proj_adj, 2)
  ) %>%
  addPolylines(
    data = pops %>% filter(GEOID == "51083") %>% st_transform(crs = 4326),
    color = "red",
    opacity = 0.8,
    weight = 3
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = tmp$alloc_proj_adj,
    title = "Median Allocation Dollars per Project"
  )






