source(here::here("src", "Mapping", "map_template.R"))
library(tidycensus)


acs_mean_income_quantiles_county_sp <- st_read(here::here("data", "original", "acs_mean_income_quantiles_county.geojson"))

# moe calculated following https://www2.census.gov/programs-surveys/acs/tech_docs/accuracy/MultiyearACSAccuracyofData2015.pdf
top_bottom_difference <- acs_mean_income_quantiles_county_sp %>%
  mutate("top_bottom_difference_estimate" = estimate_quintile_means_highest_quintile_estimate - estimate_quintile_means_lowest_quintile_estimate,
         "top_bottom_difference_moe" = round(sqrt(estimate_quintile_means_highest_quintile_moe^2 + estimate_quintile_means_lowest_quintile_moe^2)))


create_map(top_bottom_difference,
           variables= c("top_bottom_difference"),
           group_names = NULL,
           legend_name = "Mean Income Difference",
           label_name = "Mean Income Difference",
           scale_domain = c(90000, 420000),
           scale_breaks = c(90000, 150000, 200000, 250000, 300000, 420000),
           unstable_threshold = 2)


gini_index <- tidycensus::get_acs(geography = "county",
                      year = 2018,
                      variables = c(gini_index = "B19083_001"),
                      state = "VA") %>%
  tidyr::pivot_wider(names_from = variable,
                     values_from = c(estimate, moe),
                     names_glue = "{variable}_{.value}")

tigris::counties(state = "VA", cb = TRUE, resolution = "20m", class = "sf") %>%
  st_transform(crs = 4326) %>%
  left_join(gini_index, by = "GEOID") %>%
  create_map(variables= c("gini_index"),
             group_names = NULL,
             legend_name = "Gini Index",
             label_name = "Gini Index",
             scale_domain = c(0.3, 0.6),
             scale_breaks = c(0.3,0.4, 0.42, 0.45, 0.5, 0.6),
             unstable_threshold = 2)
