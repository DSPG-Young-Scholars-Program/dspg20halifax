source(here::here("src", "Mapping", "map_template.R"))

acs_demographics_county_sp <- st_read(here::here("data", "original", "acs_age_sex_race_county.geojson"))

range(acs_demographics_county_sp$percent_estimate_hispanic_or_latino_and_race_total_population_not_hispanic_or_latino_white_alone_estimate)

create_map(acs_demographics_county_sp,
           variables= c("percent_estimate_hispanic_or_latino_and_race_total_population_not_hispanic_or_latino_white_alone"),
           group_names = NULL,
           legend_name = "Percent White",
           label_name = "Percent White",
           scale_domain = c(0, 100),
           scale_breaks = c(0, 20, 40, 60, 80, 100),
           unstable_threshold = 0)


BAMMtools::getJenksBreaks(acs_demographics_county_sp$percent_estimate_hispanic_or_latino_and_race_total_population_not_hispanic_or_latino_black_or_african_american_alone_estimate, 5)
hist(acs_demographics_county_sp$percent_estimate_hispanic_or_latino_and_race_total_population_not_hispanic_or_latino_black_or_african_american_alone_estimate)

create_map(acs_demographics_county_sp,
           variables= c("percent_estimate_hispanic_or_latino_and_race_total_population_not_hispanic_or_latino_black_or_african_american_alone"),
           group_names = NULL,
           legend_name = "Percent Black",
           label_name = "Percent Black",
           scale_domain = c(0, 80),
           scale_breaks = c(0, 5, 10, 20, 40, 80),
           unstable_threshold = 0)
