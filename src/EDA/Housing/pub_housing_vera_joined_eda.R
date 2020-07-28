
library(ggplot2)
library(dplyr)
library(leaflet)
library(sf)
library(here)
library(stringr)

## Vera incarceration data
vera_data <- data.table::fread(here::here("data", "original", "Incarceration", "vera_incarceration_trends.csv")) %>% 
  as.data.frame() %>%
  mutate(fips = as.character(fips))

## Public housing data
pub_housing_summary <- vroom::vroom(here("data", "original", "Housing", "county_pub_housing_2013_2019.csv")) %>%
  replace("NA", NA) %>%
  mutate(across(everything(), function(col) ifelse(col == -1 | col == -4 | col == -5, NA, col))) %>% ## -1 = NA, -4 = Suppressed, -5 = % reporting too low
  mutate(name = str_extract(str_replace_all(str_to_lower(name), "[0-9]{3} ", ""), "^([^,])+"), ## County names have numbers for some years, include state name after a comma for some years. Clean these
         name = str_to_title(trimws(ifelse(str_detect(name, "city county"), str_replace_all(name, "county", ""), name)))) ## Clean duplicated counties that are labeled as both city and county

#adm_dis_data <- vera_data %>% filter(str_detect(fips, "^51[0-9]{3}"))

## Rural area admissions - discharges
rur_data <- vera_data %>% 
  filter(state == "VA") %>%
  filter(year > 1979.5, urbanicity == "rural") %>%
  mutate(adm_dis_diff = (total_jail_adm - total_jail_dis) / total_pop,
         isHalifax = ifelse(fips == "51083", "yes", "no")) %>%
  filter(!(fips %in% c("51159", "51091", "51193"))) ## extreme outliers in here?

## Isolate Halifax to highlight with rectangle
hal_data <- rur_data %>% filter(fips == "51083")
nodelist <- sort(levels(as.factor(rur_data$fips)))

## Difference in admissions to discharges scaled to total county population
ggplot() +
  geom_tile(data = rur_data, aes(x = year, y = as.factor(fips), fill = adm_dis_diff), color = "gray") +
  #geom_tile(data = outlier_data, aes(x = year, y = as.factor(fips), color = isOutlier), color = "gray") +
  #geom_tile(data = hal_data, aes(x = year, y = as.factor(fips), fill = total_jail_adm - total_jail_dis), color = alpha("blue", 1), size = 0.5) +
  geom_rect(data = rur_data, aes(xmin = 1979.5, xmax = 2017.5, ymin = which(nodelist=="51083")-0.49, ymax = which(nodelist=="51083")+0.49), color = "gray40", alpha = 0) +
  scale_fill_gradient2(low = "#053162", mid = "#fafafa", high = "#6a0120", na.value = "gray") +
  theme_minimal() +
  theme(panel.grid = element_blank())

## Just discharges - Halifax doesn't stand out much
ggplot(rur_data) +
  geom_tile(aes(x = year, y = as.factor(fips), fill = total_jail_dis / total_pop), color = "gray") +
  #geom_tile(data = outlier_data, aes(x = year, y = as.factor(fips), color = isOutlier), color = "gray") +
  #geom_tile(data = hal_data, aes(x = year, y = as.factor(fips), fill = total_jail_adm - total_jail_dis), color = alpha("blue", 1), size = 0.5) +
  geom_rect(aes(xmin = 1979.5, xmax = 2017.5, ymin = which(nodelist=="51083")-0.49, ymax = which(nodelist=="51083")+0.49), color = "red", alpha = 0) +
  theme_minimal() +
  theme(panel.grid = element_blank())

## -----------------------------------------------------------------------------------------------

## Public Housing Characteristics Data
# va_housing <- pub_housing_summary %>%
#   filter(year == 2017, program_label == "Summary of All HUD Programs", state == "VA")
# 
# ## Join on incarceration data by county
# joined_pub <- vera_data %>%
#   filter(state == "VA", year == 2017) %>%
#   full_join(va_housing, by = c("fips" = "code")) %>%
#   full_join(va_borders, by = c("fips" = "GEOID")) %>%
#   mutate(dis_to_units = total_jail_dis / total_units,
#          dis_to_pop = total_jail_dis / people_total) %>%
#   st_as_sf() %>%
#   st_transform(crs = 4326)
# 
# BAMMtools::getJenksBreaks(joined_pub$dis_to_pop, k = 5)
# pal <- colorBin("BuPu", domain = range(joined_pub$dis_to_pop, na.rm = TRUE), bins = c(0, 1, 5, 10, 25, 50))
# 
# ## Map of discharges relative to population
# leaflet() %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(data = joined_pub,
#               weight = 1,
#               fillColor = ~pal(dis_to_pop),
#               fillOpacity = 0.8,
#               label = ~paste(dis_to_pop),
#               group = "discharges") %>%
#   addLegend("bottomright", title = "Ratio of Jail Discharges <br>to Population: 2017", pal = pal, values = c(0, 6, 14, 27, 63))

## -----------------------------------------------------------------------------------------------

## Subset to overall HUD summary data
va_pub_housing <- pub_housing_summary %>%
  filter(program_label == "Summary of All HUD Programs", state == "VA")

## Join on incarceration data for recent years
va_housing_vera <- vera_data %>%
  filter(state == "VA", year > 2011) %>%
  full_join(va_pub_housing, by = c("fips" = "code", "year" = "year"))

## Dumbell plot for discharges to subsidized housing capacity
va_housing_vera %>%
  filter(year == 2017, !is.na(people_total), !is.na(total_jail_dis)) %>%
  mutate(isHalifax = ifelse(fips == "51083", "yes", "no"), capacity = ifelse(total_jail_dis > people_total, "above", "below")) %>%
  ggplot() +
  geom_segment(aes(x = reorder(fips,-people_total), xend = reorder(fips,-people_total), yend = people_total, y = total_jail_dis, alpha = isHalifax, color = capacity), size = 1.5) +
  geom_point(aes(x = reorder(fips,-people_total), y = total_jail_dis, alpha = isHalifax, color = capacity), size = 1.5) +
  geom_point(aes(x = reorder(fips,-people_total), y = people_total, alpha = isHalifax, color = capacity), size = 0.75) +
  scale_alpha_manual(values = c(0.2, 1)) +
  scale_color_manual(values = c("#d4373e", "#47518e")) +
  labs(x = "County\nby decreasing population", y = "Discharges in excess of subsidized housing capacity", title = "Jail Discharges Relative to Subsidized Housing Population") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")

## Difference in discharges and housing capacity plot
va_housing_vera %>%
  filter(year == 2017, !is.na(people_total), !is.na(total_jail_dis)) %>%
  mutate(isHalifax = ifelse(fips == "51083", "yes", "no"), 
         #capacity = 1 / (people_total / ((100 - pct_occupied) / 100)),
         capacity_color = ifelse(total_jail_dis > people_total, "above", "below"), 
         diff = total_jail_dis - people_total) %>%
  ggplot() +
  geom_segment(aes(x = reorder(fips,total_pop), xend = reorder(fips,total_pop), yend = 0, y = diff, alpha = isHalifax, color = capacity_color), size = 1.5) +
  geom_point(aes(x = reorder(fips,-diff), y = diff, alpha = isHalifax, color = capacity_color), size = 1) +
  #geom_point(aes(x = reorder(fips,-people_total), y = people_total, alpha = isHalifax, color = capacity), size = 0.75) +
  scale_alpha_manual(values = c(0.15, 1)) +
  scale_color_manual(values = c("#FC4444","#6b6385")) +
  labs(x = "County\nby increasing population", y = "Discharges in Excess of Subsidized Housing Population", title = "Capacity of Subsidized Housing System") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")

## Relative plot
va_housing_vera %>%
  filter(year == 2017, !is.na(people_total), !is.na(total_jail_dis)) %>%
  mutate(isHalifax = ifelse(fips == "51083", "yes", "no"), 
         #capacity = 1 / (people_total / ((100 - pct_occupied) / 100)),
         capacity_color = ifelse(total_jail_dis > people_total, "above", "below"), 
         diff = total_jail_dis / people_total) %>%
  ggplot() +
  geom_segment(aes(x = reorder(fips,diff), xend = reorder(fips,diff), yend = 1, y = diff, alpha = isHalifax, color = capacity_color), size = 1.5) +
  geom_point(aes(x = reorder(fips,-diff), y = diff, alpha = isHalifax, color = capacity_color), size = 1) +
  #geom_point(aes(x = reorder(fips,-people_total), y = people_total, alpha = isHalifax, color = capacity), size = 0.75) +
  scale_alpha_manual(values = c(0.15, 1)) +
  scale_color_manual(values = c("#FC4444","#6b6385")) +
  scale_y_continuous(breaks = c(1, 5, 10, 25, 50)) +
  labs(x = "County", y = "Relative Size of Jail Discharge Population to Subsidized Housing Capacity", title = "Capacity of Subsidized Housing System") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")

## Relative plot
va_housing_vera %>%
  filter(year == 2017, !is.na(people_total), !is.na(total_jail_dis)) %>%
  mutate(isHalifax = ifelse(fips == "51083", "yes", "no"), 
         dis_to_available_housing = total_jail_dis / people_total) %>%
  ggplot() +
  geom_segment(aes(x = reorder(fips, -dis_to_available_housing), xend = reorder(fips,-people_total), yend = 0, y = dis_to_available_housing, color = isHalifax), size = 1.5, alpha = 0.9) +
  #geom_point(aes(x = reorder(fips,people_total), y = total_jail_dis, color = capacity, alpha = isHalifax), size = 1) +
  #geom_point(aes(x = reorder(fips,people_total), y = people_total, alpha = isHalifax), color = "gray", size = 1) +
  #geom_path(aes( x = reorder(fips,-people_total), y = people_total, group = group), color = "gray", size = 1) +
  #geom_line(aes(x = reorder(fips,-people_total), y = people_total), color = "#6b6385", size = 3) +
  #gghighlight(total_jail_dis < people_total) +
  scale_alpha_manual(values = c(0.1, 1)) +
  scale_color_manual(values = c( "#6b6385","#FC4444")) +
  labs(x = "County", y = "", title = "Jail Discharges Relative to Affordable Housing Capacity") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")



va_housing_vera %>%
  filter(year == 2017, !is.na(people_total), !is.na(total_jail_dis)) %>%
  mutate(isHalifax = ifelse(fips == "51083", "yes", "no"), 
         #capacity = 1 / (people_total / ((100 - pct_occupied) / 100)),
         capacity_color = ifelse(total_jail_dis > people_total, "above", "below"), 
         diff = (total_jail_dis - people_total) / people_total) %>%
  filter(isHalifax == "yes") %>%
  select(total_jail_dis, people_total, pct_occupied, diff)
