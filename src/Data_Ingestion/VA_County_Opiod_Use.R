library(sf)
install.packages("tigris")
library(tigris)
library(leaflet)
library(tidycensus)
library(stringr)
library(dplyr)
install.packages('here')
library(here)

# ggplot(CDC.Opioid.Prescription.Data...2018, aes(x = V4)) + geom_histogram(stat = "count", binwidth = 50)
# column <- as.numeric(as.character(CDC.Opioid.Prescription.Data...2018$V4))
# hist <- hist(column, xlim = c(0, 200), main = "Distribution of 2018 VA Opiod Rates (CDC)",
#              xlab = "Opiod Prescription Rate Per 100 People")

va_borders <- get_acs(table = "B01003", geography = "county", year = 2018, state = "VA",
                      survey = "acs5", geometry = TRUE) %>% st_transform(crs = 4326)
datasource <- readr::read_csv(here::here("git/TestDSPG/Halifaxx/data/original/Substance_Abuse/
                                         CDC Opioid Prescription Data - 2018.csv"))
new_tbl <- merge(va_borders, datasource, by.x = "GEOID", by.y = "State/County FIPS Code")
head(new_tbl)

my_bins <- c(0, 50, 100, 150, 200, Inf)
palette <- colorBin("Reds", domain = new_tbl$Opiod.Prescription.Rate.per.100, bins = my_bins)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = new_tbl, fillColor = ~palette(`Opiod Prescription Rate per 100`),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2011", label = ~County) %>%
  addLegend(pal = palette, values = my_bins,
            title = "Opiod Prescription Rate per 100 People in 2018", position = "bottomright") %>%
  addLayersControl(baseGroups = c("2011"))
