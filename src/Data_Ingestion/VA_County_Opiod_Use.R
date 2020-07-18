library(sf)
install.packages("tigris")
library(tigris)
library(leaflet)

va_borders <- counties(state = "VA", year = 2018, cb = TRUE) %>%
    st_as_sf() %>%
  st_transform(crs= 4326)

my_bins <- seq(0, 350, by = 50)
palette <- colorBin("Reds", domain = CDC.Opioid.Prescription.Data...2018$V4, bins = my_bins)
my_text <- CDC.Opioid.Prescription.Data...2018$V1

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = va_borders,
              fillColor = ~palette(as.numeric(CDC.Opioid.Prescription.Data...2018$V4)),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2011") %>%
  addLegend(pal = palette,
            values = my_bins, title = "Opiod Prescription Rate per 100 People in 2018", position = "bottomright",
            label = my_text) %>%
  addLayersControl(baseGroups = c("2011"))
