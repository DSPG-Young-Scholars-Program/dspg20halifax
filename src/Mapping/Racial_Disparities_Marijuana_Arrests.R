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
                                         "Racial Disparities in Marijuana Arrests by VA County - 2018 (1).csv"))

va_borders <- get_acs(table = "B01003", geography = "county", year = 2018, state = "VA",
                      survey = "acs5", geometry = TRUE, cache_table = TRUE) %>% st_transform(crs = 4326)
datasource <- merge(va_borders, datasource, by.x = "GEOID", by.y = "FIPS Code")
unstable <- filter(datasource, `Meets Population & Reporting Thresholds` == "No")
stable <- filter(datasource, `Meets Population & Reporting Thresholds` == "Yes")

general_bins <- getJenksBreaks(datasource$`Times more likely Black person arrested in 2018`, k = 6)
general_bins <- sapply(general_bins, round)

general_palette <- colorBin("Reds", domain = datasource$`Times more likely Black person arrested in 2018`, bins = general_bins)
label <- paste("County: ", datasource$County,"<br/>", "Rate: ",
               paste(datasource$`Times more likely Black person arrested in 2018`, 'x',
                                                                       sep = ""), "<br/>",
              sep="") %>%
  lapply(htmltools::HTML)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = datasource, fillColor = ~general_palette(datasource$`Times more likely Black person arrested in 2018`),
              weight = 1, color = "#fafafa", fillOpacity = 0.8,
              label = label,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addLegend(pal = general_palette, values = general_bins,
            title = "Rates of Black vs White arrests for marijuana possession (100k people)", position = "bottomright")
