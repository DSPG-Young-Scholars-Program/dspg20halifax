install.packages("tigris")
install.packages('here')
install.packages('rvest')
install.packages('censusapi')
install.packages('hablar')

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

census_api_key("1288a5a1e23422dbd03d06071f74b4cd50af12be", install = TRUE)
# First time, reload your environment so you can use the key without restarting R.
readRenviron("~/.Renviron")
# You can check it with:
Sys.getenv("CENSUS_API_KEY")


# ggplot(CDC.Opioid.Prescription.Data...2018, aes(x = V4)) + geom_histogram(stat = "count", binwidth = 50)
# column <- as.numeric(as.character(CDC.Opioid.Prescription.Data...2018$V4))
# hist <- hist(column, xlim = c(0, 200), main = "Distribution of 2018 VA Opiod Rates (CDC)",
#              xlab = "Opiod Prescription Rate Per 100 People")
create.map <- function(year) {
  link <- paste(paste("http://www.cdc.gov/drugoverdose/maps/rxcounty", year, sep = ""), ".html", sep = "")
  rate_for_year <- html(link)
  tables <- rate_for_year %>% html_table(fill = TRUE)
  table_for_year <- tables[[1]]
  rate_description <- paste(year, "Prescribing Rate")
  table_for_year <- filter(table_for_year, State == 'VA')
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

va_borders <- get_acs(table = "B01003", geography = "county", year = 2018, state = "VA",
                      survey = "acs5", geometry = TRUE) %>% st_transform(crs = 4326)

finish_map <- function(year_map, year) {
  new_tbl <- merge(va_borders, year_map, by.x = "GEOID", by.y = "FIPS County Code")
  column <- new_tbl[ ,8]
  column$geometry = NULL
  column <- as.numeric(sapply(column, noquote))
  new_tbl <- mutate(new_tbl, 'Rate' = column)
  my_bins <- c(0, 50, 100, 150, 200, Inf)
  palette <- colorBin("Reds", domain = new_tbl$Rate, bins = my_bins)
  my_label <- paste("County: ", new_tbl$NAME,"<br/>", "Rate: ", new_tbl$Rate, "<br/>",
    sep="") %>%
    lapply(htmltools::HTML)
  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = new_tbl, fillColor = ~palette(new_tbl$Rate),
                weight = 1, color = "#fafafa", fillOpacity = 0.8, group = year,
                label = my_label,
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "13px", direction = "auto")) %>%
    addLegend(pal = palette, values = my_bins,
              title = paste("Opiod Prescription Rate per 100 People in", year), position = "bottomright") %>%
    addLayersControl(baseGroups = c("2006", "2007", "2008", "2009", "2010",
      "2011", "2012", "2013", "2014", "2015", "2016", "2017"))

}
finish_map(mapping_2006, "2006")
finish_map(mapping_2007, "2007")
finish_map(mapping_2008, "2008")
finish_map(mapping_2009, "2009")
finish_map(mapping_2010, "2010")
finish_map(mapping_2011, "2011")
finish_map(mapping_2012, "2012")
finish_map(mapping_2013, "2013")
finish_map(mapping_2014, "2014")
finish_map(mapping_2015, "2015")
finish_map(mapping_2016, "2016")

# new_tbl <- merge(va_borders, mapping_2016, by.x = "GEOID", by.y = "FIPS County Code")
# column <- new_tbl$`2016 Prescribing Rate`
# column <- as.numeric(sapply(column, noquote))
# new_tbl <- mutate(new_tbl, 'Rate' = column)
# sapply(new_tbl, class)
#
# my_bins <- c(0, 50, 100, 150, 200, Inf)
# palette <- colorBin("Reds", domain = new_tbl$Rate, bins = my_bins)
#
# my_label <- paste("County: ", new_tbl$NAME,"<br/>", "Rate: ", new_tbl$Rate, "<br/>",
#   sep="") %>%
#   lapply(htmltools::HTML)
#
# leaflet() %>%
#   addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(data = new_tbl, fillColor = ~palette(new_tbl$Rate),
#               weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2016",
#               label = my_label,
#               labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
#                                         textsize = "13px", direction = "auto")) %>%
#   addLegend(pal = palette, values = my_bins,
#             title = "Opiod Prescription Rate per 100 People in 2018", position = "bottomright") %>%
#   addLayersControl(baseGroups = c("2006", "2007", "2008", "2009", "2010",
#     "2011", "2012", "2013", "2014", "2015", "2016", "2017"))
