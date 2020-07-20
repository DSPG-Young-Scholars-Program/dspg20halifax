library(sf)
install.packages("tigris")
library(tigris)
library(leaflet)
library(tidycensus)
library(stringr)
library(dplyr)

# ggplot(CDC.Opioid.Prescription.Data...2018, aes(x = V4)) + geom_histogram(stat = "count", binwidth = 50)
# column <- as.numeric(as.character(CDC.Opioid.Prescription.Data...2018$V4))
# hist <- hist(column, xlim = c(0, 200), main = "Distribution of 2018 VA Opiod Rates (CDC)",
#              xlab = "Opiod Prescription Rate Per 100 People")
# column <- as.numeric(as.character(CDC.Opioid.Prescription.Data...2018$V4))
# column

# va_borders <- counties(state = "VA", year = 2018, cb = TRUE) %>%
#     st_as_sf() %>%
#   st_transform(crs= 4326)

va_borders <- get_acs(table = "B01003", geography = "county", year = 2018, state = "VA",
                      survey = "acs5", geometry = TRUE) %>% st_transform(crs = 4326)

datasource <- CDC.Opioid.Prescription.Data...2018
new_tbl <- merge(va_borders, datasource, by.x = "GEOID", by.y = "State.County.FIPS.Code")
new_tbl <- new_tbl %>% select("GEOID", "County", "Opiod.Prescription.Rate.per.100")
new_tbl$geometry = NULL

my_bins <- c(0, 50, 100, 150, 200, Inf)
palette <- colorBin("Reds", domain = new_tbl$Opiod.Prescription.Rate.per.100, bins = my_bins)
as.numeric(as.character)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = va_borders,
              fillColor = ~palette(new_tbl$Opiod.Prescription.Rate.per.100),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2011", label = new_tbl$County) %>%
  addLegend(pal = palette, values = my_bins,
            title = "Opiod Prescription Rate per 100 People in 2018", position = "bottomright") %>%
  addLayersControl(baseGroups = c("2011"))
