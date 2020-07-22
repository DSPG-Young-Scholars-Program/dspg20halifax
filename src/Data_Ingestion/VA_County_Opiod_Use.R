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
library(tidyr)
census_api_key("1288a5a1e23422dbd03d06071f74b4cd50af12be", install = TRUE)
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")


# ggplot(CDC.Opioid.Prescription.Data...2018, aes(x = V4)) + geom_histogram(stat = "count", binwidth = 50)
# column <- as.numeric(as.character(CDC.Opioid.Prescription.Data...2018$V4))
# hist <- hist(column, xlim = c(0, 200), main = "Distribution of 2018 VA Opiod Rates (CDC)",
#              xlab = "Opiod Prescription Rate Per 100 People")

va_borders <- get_acs(table = "B01003", geography = "county", year = 2018, state = "VA",
                      survey = "acs5", geometry = TRUE, cache_table = TRUE) %>% st_transform(crs = 4326)

create.map <- function(year) {
  link <- paste(paste("http://www.cdc.gov/drugoverdose/maps/rxcounty", year, sep = ""), ".html", sep = "")
  rate_for_year <- html(link)
  tables <- rate_for_year %>% html_table(fill = TRUE)
  table_for_year <- tables[[1]]
  rate_description <- paste(year, "Prescribing Rate")
  table_for_year <- filter(table_for_year, State == 'VA')
  new_tbl <- merge(va_borders, table_for_year, by.x = "GEOID", by.y = "FIPS County Code")
  column <- new_tbl[ ,8]
  column$geometry = NULL
  column <- as.numeric(sapply(column, noquote))
  new_tbl <- mutate(new_tbl, 'Rate' = column)
}

mapping_2006 <- create.map("2006")
mapping_2007 <- create.map("2007")
mapping_2008 <- create.map("2008")
mapping_2009 <- create.map("2009")
mapping_2010 <- create.map("2010")
mapping_2011 <- create.map("2011")
mapping_2012 <- create.map("2012")
mapping_2013 <- create.map("2013")
mapping_2014 <- create.map("2014")
mapping_2015 <- create.map("2015")
mapping_2016 <- create.map("2016")
mapping_2016
all_rates <- c(mapping_2006$Rate, mapping_2007$Rate, mapping_2008$Rate, mapping_2009$Rate,
               mapping_2010$Rate, mapping_2011$Rate, mapping_2012$Rate, mapping_2013$Rate,
               mapping_2014$Rate, mapping_2015$Rate, mapping_2016$Rate)

all_rates <- na.omit(all_rates)

generic_bins <-getJenksBreaks(all_rates, k = 6)
generic_bins <- sapply(generic_bins, round)
generic_palette <-colorBin("Reds", domain = all_rates, bins = generic_bins)

generate.label <- function(year_mapping) {
  my_label <- paste("County: ", year_mapping$County,"<br/>", "Rate: ", year_mapping$Rate, "<br/>",
                    sep="") %>%
    lapply(htmltools::HTML)
}


leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = mapping_2016, fillColor = ~generic_palette(mapping_2016$Rate),
                weight = 1, color = "#fafafa", fillOpacity = 0.8, group = '2016',
                label = generate.label(mapping_2016),
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2015, fillColor = ~generic_palette(mapping_2015$Rate),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = '2015',
              label = generate.label(mapping_2015),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2014, fillColor = ~generic_palette(mapping_2014$Rate),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = '2014',
              label = generate.label(mapping_2014),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2013, fillColor = ~generic_palette(mapping_2013$Rate),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = '2013',
              label = generate.label(mapping_2013),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2012, fillColor = ~generic_palette(mapping_2012$Rate),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = '2012',
              label = generate.label(mapping_2012),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2011, fillColor = ~generic_palette(mapping_2011$Rate),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = '2011',
              label = generate.label(mapping_2011),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2010, fillColor = ~generic_palette(mapping_2010$Rate),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = '2010',
              label = generate.label(mapping_2010),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2009, fillColor = ~generic_palette(mapping_2009$Rate),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = '2009',
              label = generate.label(mapping_2009),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2008, fillColor = ~generic_palette(mapping_2008$Rate),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = '2008',
              label = generate.label(mapping_2008),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2007, fillColor = ~generic_palette(mapping_2007$Rate),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = '2007',
              label = generate.label(mapping_2007),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
  addPolygons(data = mapping_2006, fillColor = ~generic_palette(mapping_2006$Rate),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = '2006',
              label = generate.label(mapping_2006),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
    addLegend(pal = generic_palette, values = generic_bins,
              title = paste("Opiod Prescription Rate per 100 People"), position = "bottomright") %>%
    addLayersControl(baseGroups = c("2006", "2007", "2008", "2009", "2010",
      "2011", "2012", "2013", "2014", "2015", "2016"))


