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

#get csv to read in
datasource <- readr::read_csv(here::here("data",
                                         "original", "Substance_Abuse",
                                         "Excessive Drinking and Alcohol-Impaired Driving Deaths - Excessive Drinking.csv"))

#get VA border geographic data
va_borders <- get_acs(table = "B01003", geography = "county", year = 2018, state = "VA",
                      survey = "acs5", geometry = TRUE, cache_table = TRUE) %>% st_transform(crs = 4326)

#create map for each year (respectively) by filtering by year and merging with va_borders data
create.map <- function(year) {
  datasource <- filter(datasource, Year == year)
  new_tbl <- merge(va_borders, datasource, by.x = "GEOID", by.y = "FIPS")
}

#create relevant map for each year
mapping_2011 <- create.map('2011')
mapping_2012 <- create.map('2012')
mapping_2013 <- create.map('2013')
mapping_2014 <- create.map('2014')
mapping_2015 <- create.map('2015')
mapping_2016 <- create.map('2016')
mapping_2017 <- create.map('2017')
mapping_2018 <- create.map('2018')
mapping_2019 <- create.map('2019')
mapping_2020 <- create.map('2020')

#generate label on map for each respective year
generate.label <- function(year_mapping) {
  my_label <- paste("County: ", year_mapping$County,"<br/>", "Rate: ", paste(year_mapping$'% Excessive Drinking', '%',
                                                                           sep = ""), "<br/>",
                    sep="") %>%
    lapply(htmltools::HTML)
}

#generate bins using Jenks Breaks and palette for all the maps
general_bins <- getJenksBreaks(datasource$`% Excessive Drinking`, k = 6)
general_palette <- colorBin("Reds", domain = datasource$'`% Excessive Drinking`', bins = general_bins)

#leaflet code to plot all the mpas data
leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = mapping_2020, fillColor = ~general_palette(mapping_2020$'% Excessive Drinking'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2020",
              label = generate.label(mapping_2020),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
    addPolygons(data = mapping_2019, fillColor = ~general_palette(mapping_2019$'% Excessive Drinking'),
                weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2019",
                label = generate.label(mapping_2019),
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2018, fillColor = ~general_palette(mapping_2018$'% Excessive Drinking'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2018",
              label = generate.label(mapping_2018),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2017, fillColor = ~general_palette(mapping_2017$'% Excessive Drinking'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2017",
              label = generate.label(mapping_2017),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2016, fillColor = ~general_palette(mapping_2016$'% Excessive Drinking'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2016",
              label = generate.label(mapping_2016),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2015, fillColor = ~general_palette(mapping_2015$'% Excessive Drinking'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2015",
              label = generate.label(mapping_2015),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2014, fillColor = ~general_palette(mapping_2014$'% Excessive Drinking'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2014",
              label = generate.label(mapping_2014),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2013, fillColor = ~general_palette(mapping_2013$'% Excessive Drinking'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2013",
              label = generate.label(mapping_2013),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2012, fillColor = ~general_palette(mapping_2012$'% Excessive Drinking'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2012",
              label = generate.label(mapping_2012),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2011, fillColor = ~general_palette(mapping_2011$'% Excessive Drinking'),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2011",
              label = generate.label(mapping_2011),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
    addLegend(pal = general_palette, values = general_bins,
              title = "Percentage of Adults Reporting Heavy or Binge Drinking", position = "bottomright") %>%
    addLayersControl(baseGroups = c("2011", "2012", "2013", "2014", "2015", "2016", "2017",
                                    "2018", "2019", "2020"),
                     options = layersControlOptions(collapsed = FALSE))



