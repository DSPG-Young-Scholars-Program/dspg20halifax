
library(sf)
library(dplyr)
library(leaflet)
library(tigris)
library(ggplot2)
library(here)

source(here("src", "Mapping", "map_template.R"))

## Read in opportunity insights data and filter to Halifax
op_insights <- readr::read_csv(here::here("data", "original", "ACS", "tract_outcomes_simple.csv"))
halifax_op_insights <- op_insights %>% filter(state == 51, county == 83) %>% mutate(GEOID = paste0("51083", tract))

## Read in spatial data for Halifax
halifax_tracts <- tracts(state = "VA", county = "Halifax")
halifax_tracts <- st_as_sf(halifax_tracts)

## Join op insights data with polygons
halifax_sf <- left_join(halifax_op_insights, halifax_tracts) %>% 
  st_as_sf() %>% 
  st_transform(crs = 4326)

# #
# #
# # Visualizing Uncertainty --------------------------------------------------------------------------------------------------------------------
# #
# #
# 
# ## Just for exploration, create new columns where you add the SE to sub-median estimates and subtract a SE from above-median estimates
# ## Do this for both 1 SE and 2 SE
# halifax_sf <- halifax_sf %>%
#   mutate(se_adjusted_jail_p25_1 = case_when(jail_pooled_pooled_p25 < median(jail_pooled_pooled_p25) ~ jail_pooled_pooled_p25 + jail_pooled_pooled_p25_se,
#                                             jail_pooled_pooled_p25 >= median(jail_pooled_pooled_p25) ~ jail_pooled_pooled_p25 - jail_pooled_pooled_p25_se),
#          se_adjusted_jail_p25_2 =  case_when(jail_pooled_pooled_p25 < median(jail_pooled_pooled_p25) ~ jail_pooled_pooled_p25 + 2*jail_pooled_pooled_p25_se,
#                                              jail_pooled_pooled_p25 >= median(jail_pooled_pooled_p25) ~ jail_pooled_pooled_p25 - 2*jail_pooled_pooled_p25_se))
# 
# ## Fixed palette based on original estimates
# pal <- colorBin("BuPu", range(halifax_sf$jail_pooled_pooled_p25, na.rm = TRUE), bins = 5)
# 
# ## Add layers for estimates and the two SE-adjusted layers to display how much of a difference it makes
# leaflet(halifax_sf) %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(fillColor = ~pal(jail_pooled_pooled_p25),
#               fillOpacity = 0.5,
#               opacity = 0.3,
#               weight = 2,
#               color = "gray",
#               label = ~round(jail_pooled_pooled_p25, 2),
#               group = "Not Adjusted") %>%
#   addPolygons(fillColor = ~pal(se_adjusted_jail_p25_1),
#               fillOpacity = 0.5,
#               opacity = 0.3,
#               weight = 2,
#               color = "gray",
#               label = ~round(se_adjusted_jail_p25_1, 2),
#               group = "Adjusted 1") %>%
#   addPolygons(fillColor = ~pal(se_adjusted_jail_p25_2),
#               fillOpacity = 0.5,
#               opacity = 0.3,
#               weight = 2,
#               color = "gray",
#               label = ~round(se_adjusted_jail_p25_2, 2),
#               group = "Adjusted 2") %>%
#   addLayersControl(baseGroups = c("Not Adjusted", "Adjusted 1", "Adjusted 2")) %>%
#   addLegend("bottomright",
#             pal = pal,
#             values = halifax_sf$jail_pooled_pooled_p25)
# 
# ## This isn't the whole story though - I guess it ought to be a MVN distribution?
# ## Just artificially adding to the low ones and subtracting from the high ones really seems conservative
# 
# #
# #
# # Generating samples based on estimates and SEs recorded ------------------------------------------------------------------------------------
# #
# #
# 
# pal <- colorBin("BuPu", c(0, max(halifax_sf$jail_pooled_pooled_p25) + max(halifax_sf$jail_pooled_pooled_p25_se)), bins = 5, na.color = "gray")
# map_samples(data = halifax_sf, var = "jail_pooled_pooled_p25", se_var = "jail_pooled_pooled_p25_se", x = 10, palette = pal)


#
#
# Same but with more relevant variable:
#
#

source(here("src", "Mapping", "map_template.R"))

library(tidycensus)
library(tigris)
library(gganimate)
library(transformr)

## Variable names for ACS
## Variable names for ACS
vars_2018 <- load_variables(2018, dataset = "acs5", cache = TRUE)
profile_vars_2018 <- load_variables(2018, dataset = "acs5/profile", cache = TRUE)
subject_vars_2018 <- load_variables(2018, dataset = "acs5/subject", cache = TRUE)

## Polygons for counties and tracts
va_counties <- counties(state = "VA", class = "sf", cb = TRUE, resolution = "20m") %>%
  st_transform(crs = 4326)

halifax_tracts <- tracts(state = "VA", county = "Halifax", class = "sf", cb = TRUE) %>%
  st_transform(crs = 4326)

## ACS table IDs
# tables <- c("B07013", ## Geographic mobility
#             "S2507", ## Financial characteristics - no mortgage
#             "S2504", ## Physical characteristics
#             "S2502", ## Demographic characteristics
#             "DP04") ## General housing statistics

# ----- Geographic mobility ---- #

## County level geographic mobility variables
geog_mobility <- get_acs(geography = "county",
                         year = 2018,
                         table = "S0701",
                         state = "VA") %>%
  left_join(subject_vars_2018, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(";", "", gsub(",", "", gsub(" ", "_", gsub("!!", "_", label)))))) %>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

## Tract level geographic mobility variables
geog_mobility_tracts <- get_acs(geography = "tract",
                                year = 2018,
                                table = "S0701",
                                state = "VA",
                                county = "Halifax") %>%
  left_join(subject_vars_2018, by = c("variable" = "name")) %>%
  mutate(label = tolower(gsub(";", "", gsub(",", "", gsub(" ", "_", gsub("!!", "_", label))))))%>%
  select(-variable) %>%
  pivot_wider(names_from = label,
              values_from = c(estimate, moe),
              names_glue = "{label}_{.value}")

## Merge ACS on spatial data
geog_mobility_counties <- left_join(va_counties, geog_mobility, by = c("GEOID"))
geog_mobility_tracts <- left_join(halifax_tracts, geog_mobility_tracts, by = c("GEOID"))

## Plot county map
geog_mobility_counties %>%
  create_map(variable = "estimate_moved_within_same_county_population_1_year_and_over",
             group_names = "Intra-County Mobility",
             legend_name = "Moved within County",
             label_name = "Moved within County",
             scale_domain = range(geog_mobility_counties$`estimate_moved_within_same_county_population_1_year_and_over_estimate`),
             scale_breaks = c(0, 2, 4, 6, 8, 10, 12),
             unstable_threshold = 0)

## Plot tract map
geog_mobility_tracts %>%
  create_map(variable = "estimate_moved_within_same_county_population_1_year_and_over",
             group_names = "Intra-County Mobility",
             legend_name = "Moved within County",
             label_name = "Moved within County",
             scale_domain = range(geog_mobility_tracts$`estimate_moved_within_same_county_population_1_year_and_over_estimate`),
             scale_breaks = c(0, 2, 4, 6, 8, 10, 12),
             unstable_threshold = 0)

## Sample from independent normals at tract level and plot across layers to show uncertainty in estimates
pal <- colorBin("BuPu", c(geog_mobility_tracts$estimate_moved_within_same_county_population_1_year_and_over_estimate - geog_mobility_tracts$estimate_moved_within_same_county_population_1_year_and_over_moe, geog_mobility_tracts$estimate_moved_within_same_county_population_1_year_and_over_estimate + geog_mobility_tracts$estimate_moved_within_same_county_population_1_year_and_over_moe), bins = 10, na.color = "gray")

set.seed(4244)

map_samples(data = geog_mobility_tracts,
            var = "estimate_moved_within_same_county_population_1_year_and_over_estimate",
            se_var = "estimate_moved_within_same_county_population_1_year_and_over_moe",
            x = 10,
            moe = TRUE,
            palette = pal)



#
#
# Attempts at Animation. May not be possible on Rivanna ---------------------------------------------------------
#
#

# 
# mob_data_for_map <- geog_mobility_tracts %>%
#   rename(intra_county_move = estimate_moved_within_same_county_population_1_year_and_over_estimate,
#          intra_county_move_moe = estimate_moved_within_same_county_population_1_year_and_over_moe) %>%
#   select(intra_county_move, intra_county_move_moe)
# 
# sample_tracts <- function(data, x, var, se_var, moe = FALSE) {
#   
#   ## Create matrix of sample values
#   samps <- matrix(nrow = nrow(data.frame(data)), ncol = x)
#   colnames(samps) <- paste("Sample", seq(1:x), sep = "_")
#   
#   ## Sample based on estimates and SEs in the data
#   ## Assumes there is a column in the data labeled with the same name as the estimates plus an additional _se
#   for (i in seq(1, nrow(data))) {
#     
#     if (moe == TRUE) {
#       samp <- rnorm(x, mean = data[[i,var]], sd = data[[i,se_var]] / 1.64)
#       samps[i,] <- samp
#     } else {
#       samp <- rnorm(x, mean = data[[i,var]], sd = data[[i,se_var]])
#       samps[i,] <- samp
#     }
#     
#   }
#   
#   ## Add sampled data to sf object
#   samp_data <- samps %>% 
#     cbind(data) %>% 
#     as.data.frame() %>%
#     pivot_longer(cols = contains("Sample"), names_to = "Samples") %>%
#     st_as_sf() %>%
#     st_transform(crs = 4326)
#   
#   return(samp_data)
# }
# 
# 
# geog_mobility_samp <- sample_tracts(mob_data_for_map, x = 10, var = "intra_county_move", se_var = "intra_county_move_moe", moe = TRUE)
# 
# 
# 
# anim <- ggplot(geog_mobility_samp) +
#   geom_sf(aes(fill = value)) +
#   facet_wrap(~Samples, ncol = 3) +
#   theme_minimal() +
#   transition_states(Samples)
# 
# anim_save("test_anim.gif", animation = anim, here::here("data", "working"))
