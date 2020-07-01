source(here::here("src", "Mapping", "map_template.R"))

acs_transportation_county_sp <- st_read(here::here("data", "original", "acs_transportation_county.geojson"))

# moe calculated using https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofData2015.pdf

acs_transport_prop <- acs_transportation_county_sp %>%
  mutate(drove_alone_prop_estimate = round(workers_drove_alone_estimate / workers_total_estimate * 100, digits = 1),
         drove_alone_prop_moe = round(moe_prop(workers_drove_alone_estimate, workers_total_estimate, workers_drove_alone_moe, workers_total_moe) * 100, digits = 1),
         carpooled_prop_estimate = round(workers_carpooled_estimate / workers_total_estimate * 100, digits = 1),
         carpooled_prop_moe = round(moe_prop(workers_carpooled_estimate, workers_total_estimate, workers_carpooled_moe, workers_total_moe) * 100, digits = 1),
         transit_prop_estimate = round(workers_transit_estimate / workers_total_estimate * 100, digits = 1),
         transit_prop_moe = round(moe_prop(workers_transit_estimate, workers_total_estimate, workers_transit_moe, workers_total_moe) * 100, digits = 1))

create_map(acs_transport_prop,
           variables= c("drove_alone_prop"),
           group_names = NULL,
           legend_name = "Proportion of All Workers",
           label_name = "Proportion of All Workers",
           scale_domain = c(0, 100),
           scale_breaks = c(50, 75, 80, 85, 90, 100),
           unstable_threshold = 2)

