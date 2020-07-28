library(rvest)
library(sf)
library(tigris)
library(leaflet)
library(tidycensus)
library(stringr)
library(dplyr)
library(here)
library(censusapi)
library(hablar)
library(BAMMtools)
library(ggplot2)

#load the csv file
datasource <- readr::read_csv(here::here("git", "TestDSPG", "Halifaxx", "data",
                                         "original", "Substance_Abuse",
                                         "Excessive Drinking and Alcohol-Impaired Driving Deaths - Halifax (1).csv"))

#access the column for 'Driving Deaths w/ Alcohol Involvement'
column <- datasource$`% Driving Deaths with Alcohol Involvement`
years <- c(2020, 2019, 2018, 2017, 2016, 2015, 2014)
#only select certain years from the data source
datasource <- filter(datasource, X1 %in% years)
#VA state data for 'Driving Deaths w/ Alcohol Involvement' for each corresponding year
va <- c(30, 31, 31, 31, 31, 31, 35)
datasource

#plot the data via ggplot
ggplot(data = datasource, aes(x = years)) +
  geom_line(aes(y = column, colour = "#CC6666")) +
  geom_line(aes(y = va, colour = "#9999CC")) +
  labs(x = 'Year', y = '% Driving Deaths w Alcohol Involvement')  +
  scale_x_continuous(breaks = seq(2014, 2020, 1), limits = c(2014, 2020)) +
  #set the values of the x axis
  scale_color_manual(labels = c("VA", "Halifax, VA"), values = c("#CC6666",  "#9999CC")) +
  guides(color=guide_legend("Location"))
  # geom_smooth(aes(y = column), method = "lm", formula = y ~ poly(x, 6), se = FALSE) +
  # geom_smooth(aes(y = va), method = "lm", formula = y ~ poly(x, 3), se = FALSE)
