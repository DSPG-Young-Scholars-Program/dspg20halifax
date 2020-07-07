#Loading Packages into R
library(tidycensus)
library(sp)
library(tidyverse)
library(acs)
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
library(viridis)

median_income <- get_acs(geography = "tract", variable = "B06011_001", 
                         year = 2018, geometry = TRUE, 
                         state = "VA", county = "Halifax")

household_income <- get_acs(geography = "tract", variable = "B19001_001", 
                            year = 2018, geometry = TRUE, 
                            state = "VA", county = "Halifax")

poverty_income <- get_acs(geography = "tract", variable = "DP03_0119P", 
                          year = 2018, geometry = TRUE, 
                          state = "VA", county = "Halifax")
