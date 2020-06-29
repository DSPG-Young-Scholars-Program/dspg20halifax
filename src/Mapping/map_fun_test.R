
library(geojsonio)

source(here::here("src", "Mapping", "map_template.R"))

geog_mobility <- st_as_sf(geojson_read(here::here("data","original","acs_geog_mobility.geojson"), what = "sp"))

geog_mobility <- geog_mobility %>% 
  mutate(pct_stay = geog_mobility$estimate_total_same.house.1.year.ago_householder.lived.in.renter.occupied.housing.units_estimate / geog_mobility$estimate_total_householder.lived.in.renter.occupied.housing.units_estimate )

create_map(geog_mobility, 
           variables = c("pct_stay"),
           group_names = c("Renters"),
           legend_name = "Mobility",
           label_name = "Pct in same house",
           scale_domain = c(0, 100),
           scale_breaks = c(0, 20, 40, 60, 80, 100),
           unstable_threshold = 2)
