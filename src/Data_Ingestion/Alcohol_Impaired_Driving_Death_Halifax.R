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
datasource <- readr::read_csv(here::here("git", "TestDSPG", "Halifaxx", "data",
                                         "original", "Substance_Abuse",
                                         "Excessive Drinking and Alcohol-Impaired Driving Deaths - Halifax (1).csv"))

column <- datasource$`% Driving Deaths with Alcohol Involvement`
years <- c(2020, 2019, 2018, 2017, 2016, 2015, 2014)
datasource <- filter(datasource, X1 %in% years)
va <- c(30, 31, 31, 31, 31, 31, 35)
datasource
ggplot(data = datasource, aes(x = X1)) +
  geom_line(aes(y = column, colour = "#CC6666")) +
  geom_line(aes(y = va, colour = "#9999CC")) +
  labs(x = 'Year', y = '% Driving Deaths w Alcohol Involvement')  +
  scale_x_continuous(breaks = seq(2014, 2020, 1), limits = c(2014, 2020)) +
  scale_color_manual(labels = c("VA", "Halifax, VA"), values = c("#CC6666",  "#9999CC")) +
  guides(color=guide_legend("Location"))
