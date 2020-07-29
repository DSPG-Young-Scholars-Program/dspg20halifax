library(devtools)
library(tidyr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(sf)
library(tigris)

#race mapping
new_data <- gather(halifax_decennial_data,"races","race_estimates", 3:8, na.rm = TRUE)

new_data$races <- factor(new_data$races, levels = c("white_race", "black_race", "hispanic_ethnicity", "asian_race", "native_race", "two_races"))
ggplot()+
  geom_col(data = new_data, aes(x = races, y = race_estimates))+
  labs(y = "population",
    title = "Race Breakdown in Halifax County, VA",
    caption = "Data source: 2018 ACS.\nData acquired with the R tidycensus package.")

#age mapping
d <- gather(halifax_decennial_data,"median_age","age_estimates", 29:31, na.rm = TRUE)
ggplot()+
  geom_col(data = d, position = "dodge", aes(x = median_age, y = age_estimates, col = "white"))+
  labs(title = "Age Breakdown in Halifax County",
       y = "Age",
       x = "Census Tract")

#gender mapping
gender <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "tidy",
                        variables = c(male_pop = "P012002", female_pop = "P012026"))
ggplot()+
  geom_col(data = gender , aes(x = variable, y = value))+
  scale_fill_brewer("Set2")+
  labs(x = "Gender",
       y = "population",
       title = "Gender Breakdown in Halifax County, VA",
       caption = "Data source: 2018 ACS.\nData acquired with the R tidycensus package.")

#household member mapping
ggplot()+
  geom_point(data = halifax_decennial_data, aes(x = NAME.x, y = one_household_members), col = "blue")+
  geom_point(data = halifax_decennial_data, aes(x = NAME.x, y = two_household_members), col = "red")+
  geom_point(data = halifax_decennial_data, aes(x = NAME.x, y = three_household_members), col = "orange")+
  geom_point(data = halifax_decennial_data, aes(x = NAME.x, y = four_household_members), col = "grey")+
  geom_point(data = halifax_decennial_data, aes(x = NAME.x, y = five_household_members), col = "#CAFF70")+
  geom_point(data = halifax_decennial_data, aes(x = NAME.x, y = six_household_members), col = "#BF3EFF")+
  geom_point(data = halifax_decennial_data, aes(x = NAME.x, y = seven_or_more_household_members), col = "#FFFF00")+
  labs(x = "Number of Household Members", y = "Census Tract", title = "Number of household members by Census Tract", subtitle = "Halifax County, VA", 
       caption = "Data source: 2018 ACS.\nData acquired with the R tidycensus package.")

#homeowner by race mapping
homeowner_by_race <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "tidy", geometry = TRUE,
                                   variables = c(white_owner= "H014003", black_owner = "H014004", white_renter = "H014011",
                                                 black_renter = "H014012"))
ggplot()+
  geom_point(data = homeowner_by_race, aes(x = NAME, y = value))+
  geom_point(data = homeowner_by_race, aes(x = NAME, y = value))+
  geom_point(data = homeowner_by_race, aes(x = NAME, y = value))+
  geom_point(data = homeowner_by_race, aes(x = NAME, y = value))+
  labs(x = "Census Tract", 
       y = "Type of Homeownership",
       title = "Type of Homeownership by race",
       subtitle = "Halifax County, VA",
       caption = "Data source: 2018 ACS.\nData acquired with the R tidycensus package.")

#graph on map in percent of renters
ggplot()+
  geom_sf(data = halifax_decennial_data, aes(fill = black_race))

halifax_decennial_data %>% 
  mutate(pct_renters = pop_in_renters_housing / total_pop_in_housing) %>%
ggplot()+
  geom_sf(aes(fill = pct_renters))+
  #facet_wrap(~variable)+
  scale_fill_viridis_c()

#graph on minor vs adut population
ggplot()+
  geom_sf(data = adult_or_juvenile, aes(fill = value))+
  facet_wrap(~variable)+
  scale_fill_viridis_c()

ggplot()+
  geom_sf(data = gender, aes(fill = value))+
  facet_wrap(~variable)+
  scale_fill_viridis_c()

ggplot()+
  geom_sf(data = household_status, aes(fill = value))+
  facet_wrap(~variable)+
  scale_fill_viridis_c()
ggplot()+
  geom_sf(data = race, aes(fill = value))+
  facet_wrap(~variable)+
  scale_fill_viridis_c()
ggplot()+
  geom_sf(data = median_age, aes(fill = value))+
  facet_wrap(~variable)+
  scale_fill_viridis_c()
ggplot()+
  geom_sf(data = group_quarters, aes(fill = value))+
  facet_wrap(~variable)+
  scale_fill_viridis_c()
ggplot()+
  geom_sf(data = institutionalized_pop, aes(fill = value))+
  facet_wrap(~variable)+
  scale_fill_viridis_c()

#teen birth mapping 
new_data <- Teen_Births_Southside %>% mutate(is_halifax = case_when(County == "Halifax" ~ "Yes",
                                                                    County != "Halifax" ~ "No"))
ggplot(new_data, aes(x = Birth_Rate, y = reorder(County, Birth_Rate), fill = is_halifax)) +
  geom_col(position = "dodge") +
  labs(title = "Teen Births in Virginia", subtitle = "Southside Region", fill = "Halifax", x = "Birth rate (per 1000 females aged 15-19)", y = "County",
       caption = "Data source: County Health Rankings, 2012 - 2018 measure") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 8))
