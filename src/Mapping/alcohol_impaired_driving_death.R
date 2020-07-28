install.packages("tigris")
install.packages('here')
install.packages('rvest')
install.packages('censusapi')
install.packages('hablar')
install.packages('BAMMtools')

#import libraries
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

# load census api key
census_api_key("1288a5a1e23422dbd03d06071f74b4cd50af12be", install = TRUE)
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

#read datasource and va borders data
datasource <- readr::read_csv(here::here("git", "TestDSPG", "Halifaxx", "data",
                                         "original", "Substance_Abuse",
                                         "Excessive Drinking and Alcohol-Impaired Driving Deaths - Alcohol Impaired Driving Deaths.csv"))
va_borders <- get_acs(table = "B01003", geography = "county", year = 2018, state = "VA",
                      survey = "acs5", geometry = TRUE, cache_table = TRUE) %>% st_transform(crs = 4326)

#create a map based on each year
create.map <- function(year) {
  datasource <- filter(datasource, Year == year)
  new_tbl <- merge(va_borders, datasource, by.x = "GEOID", by.y = "FIPS")
}

#call create.map for each year
mapping_2014 <- create.map("2014")
mapping_2015 <- create.map("2015")
mapping_2016 <- create.map("2016")
mapping_2017 <- create.map("2017")
mapping_2018 <- create.map("2018")
mapping_2019 <- create.map("2019")
mapping_2020 <- create.map("2020")

#generate label for each year and respective mapping
generate.label <- function(year_mapping) {
  my_label <- paste("County: ", year_mapping$County,"<br/>", "Rate: ", paste(year_mapping$'% Driving Deaths with Alcohol Involvement', '%',
                                                                             sep = ""), "<br/>",
                    sep="") %>%
    lapply(htmltools::HTML)
}

general_bins <- getJenksBreaks(datasource$`% Driving Deaths with Alcohol Involvement`, k = 6)
general_palette <- colorBin("Reds", domain = datasource$'`% Driving Deaths with Alcohol Involvement`', bins = general_bins)
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = mapping_2020, fillColor = ~general_palette(mapping_2020$'% Driving Deaths with Alcohol Involvement'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2020",
              label = generate.label(mapping_2020),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2019, fillColor = ~general_palette(mapping_2019$'% Driving Deaths with Alcohol Involvement'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2019",
              label = generate.label(mapping_2019),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2018, fillColor = ~general_palette(mapping_2018$'% Driving Deaths with Alcohol Involvement'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2018",
              label = generate.label(mapping_2018),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2017, fillColor = ~general_palette(mapping_2017$'% Driving Deaths with Alcohol Involvement'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2017",
              label = generate.label(mapping_2017),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2016, fillColor = ~general_palette(mapping_2016$'% Driving Deaths with Alcohol Involvement'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2016",
              label = generate.label(mapping_2016),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2015, fillColor = ~general_palette(mapping_2015$'% Driving Deaths with Alcohol Involvement'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2015",
              label = generate.label(mapping_2015),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2014, fillColor = ~general_palette(mapping_2014$'% Driving Deaths with Alcohol Involvement'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2014",
              label = generate.label(mapping_2014),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addLegend(pal = general_palette, values = general_bins,
            title = "Percentage of Driving Deaths with Alcohol Involvement", position = "bottomright") %>%
  addLayersControl(baseGroups = c("2014", "2015", "2016", "2017",
                                  "2018", "2019", "2020"),
                   options = layersControlOptions(collapsed = FALSE))
