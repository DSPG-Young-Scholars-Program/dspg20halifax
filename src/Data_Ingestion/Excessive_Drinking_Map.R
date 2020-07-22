install.packages("tigris")
install.packages('here')
install.packages('rvest')
install.packages('censusapi')
install.packages('hablar')
install.packages('BAMMtools')

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

census_api_key("1288a5a1e23422dbd03d06071f74b4cd50af12be", install = TRUE)
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

datasource <- readr::read_csv(here::here("git", "TestDSPG", "Halifaxx", "data",
                                         "original", "Substance_Abuse",
                                         "Excessive Drinking and Alcohol-Impaired Driving Deaths - Excessive Drinking.csv"))


create.map <- function(year) {
  datasource <- filter(datasource, Year == year)
}

mapping_2011 <- create.map('2011')
mapping_2012 <- create.map('2012')
mapping_2013 <- create.map('2013')
mapping_2014 <- create.map('2014')
mapping_2015 <- create.map('2015')
mapping_2016 <- create.map('2016')
mapping_2017 <- create.map('2017')
mapping_2018 <- create.map('2018')
mapping_2019 <- create.map('2019')


va_borders <- get_acs(table = "B01003", geography = "county", year = 2018, state = "VA",
                      survey = "acs5", geometry = TRUE, cache_table = TRUE) %>% st_transform(crs = 4326)

finish_map <- function(year_map, year) {
  new_tbl <- merge(va_borders, year_map, by.x = "GEOID", by.y = "FIPS")
  my_bins <- getJenksBreaks(new_tbl$`% Excessive Drinking`, k = 6)
  palette <- colorBin("Reds", domain = new_tbl$'% Excessive Drinking', bins = my_bins)
  my_label <- paste("County: ", new_tbl$NAME,"<br/>", "Rate: ", new_tbl$'% Excessive Drinking', "<br/>",
                    sep="") %>%
    lapply(htmltools::HTML)
  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = new_tbl, fillColor = ~palette(new_tbl$'% Excessive Drinking'),
                weight = 1, color = "#fafafa", fillOpacity = 0.8, group = year,
                label = my_label,
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "13px", direction = "auto")) %>%
    addLegend(pal = palette, values = my_bins,
              title = paste("Opiod Prescription Rate per 100 People in", year), position = "bottomright") %>%
    addLayersControl(baseGroups = c("2011", "2012", "2013", "2014", "2015", "2016", "2017",
                                    "2018", "2019", "2020"))

}

finish_map(mapping_2019, "2019")
finish_map(mapping_2018, "2018")
finish_map(mapping_2017, "2017")
finish_map(mapping_2016, "2016")
