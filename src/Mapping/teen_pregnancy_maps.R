
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(tidycensus)
library(stringr)
library(leaflet)
library(sf)

## Read birth rate data
birth_rates <- readr::read_csv(here::here("data", "original", "Teen_Pregnancy", "NCHS_-_Teen_Birth_Rates_for_Age_Group_15-19_in_the_United_States_by_County.csv"))

## Get population from census. Mainly just using this as a roundabout way to get polylines since tigris was giving weird polygons for the Eastern Shore
pops <- get_acs(table = "B01003", geography = "county", year = 2018, state = "VA", survey = "acs5", geometry = TRUE) %>% st_transform(crs = 4326)

## Get border for Halifax to highlight
halifax_border <- pops %>% filter(GEOID == "51083")

## Clean birth_rates data for merge
colnames(birth_rates) <- str_replace_all(colnames(birth_rates), " ", "_")
birth_rates <- birth_rates %>% rename(GEOID = Combined_FIPS_Code) %>% mutate(GEOID = as.character(GEOID))

## Filter to Virginia and merge on spatial data
va_birth_rates <- birth_rates %>% filter(State == "Virginia")
va_birth_rates_sp <- full_join(pops, va_birth_rates, by = "GEOID")

## Filter to current year for viewing
map_data <- va_birth_rates_sp %>% filter(Year == 2018)

## Palette
pal <- colorBin("BuPu", range(map_data$Birth_Rate, na.rm = TRUE), bins = 5)

## Map
leaflet(map_data) %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  addPolygons(
    weight = 2,
    color = "gray",
    fillOpacity = 1,
    fillColor = ~pal(Birth_Rate),
    label = ~round(Birth_Rate, 2)
  ) %>%
  addPolylines(
    data = halifax_border,
    color = "red",
    weight = 2,
    opacity = 1
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~Birth_Rate,
    title = "Births per 100,000 <br> Females Aged 15-19"
  )

