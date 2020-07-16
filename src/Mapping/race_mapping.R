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
homeowner_by_race <- get_decennial(geography = "tract", state = "VA", county = "Halifax", output = "tidy",
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

ggplot()+
  geom_sf(data = halifax_decennial_data, aes(fill = black_race))