library(tidycensus)
library(sp)
library(tidyverse)
library(sf)
library(mapview)
library(readr)
library(readxl)
library(choroplethr)
library(ggplot2)
library(ggspatial)
library(leaflet)
library(osmdata)
library(plotly)
library(tigris)
library(acs)

#point graph about teen births in VA 
ggplot(Teen_Births_VA, aes(x = County, y = Birth_Rate))+
geom_point()

#point graph about Teen Births in Southside VA
ggplot(Teen_Births_Southside, aes(x = Birth_Rate, y = County))+
  geom_col(fill = "blue", col = "black", position = "dodge")+
  labs(title = "Teen Births in Virginia", subtitle = "Southside Region",
       )

#creating a graph about no High school degree percentages in Southside Region
SS_no_HS_degree <- get_acs(geography = "county", variable = "DP02_0060P", 
                 year = 2018, geometry = TRUE, 
                   state = "VA", county = c("Amelia", "Amherst", "Appomattox", "Bedford County",
"Brunswick", "Buckingham", "Campbell", "Charlotte County", "Cumberland",
"Dinwiddie", "Franklin County", "Goochland", "Greensville", "Halifax",
"Henry", "Lunenburg", "Mecklenburg", "Nelson", "Nottoway", "Patrick",
"Pittsylvania", "Powhatan", "Prince Edward", "Danville City", "Lynchburg City", "Martinsville City"))

ggplot(SS_no_HS_degree, aes(fill= estimate))+
  geom_sf()+
  theme_minimal()+
  scale_fill_viridis()+
  labs(title = "Virginians with no HS degree",
       subtitle = "Southside Region",
       caption = "Birth Rate is per 1000 females aged 15 - 19")

#teen birth rates across country
ggplot(National_teen_births, aes(x = STATE, y = RATE))+
  geom_col()+
  labs(title = "Teen Birth rates across the country")