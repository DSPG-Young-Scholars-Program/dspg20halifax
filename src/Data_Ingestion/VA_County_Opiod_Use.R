library(sf)
install.packages("tigris")
library(tigris)
library(leaflet)

# ggplot(CDC.Opioid.Prescription.Data...2018, aes(x = V4)) + geom_histogram(stat = "count", binwidth = 50)
# column <- as.numeric(as.character(CDC.Opioid.Prescription.Data...2018$V4))
# hist <- hist(column, xlim = c(0, 200), main = "Distribution of 2018 VA Opiod Rates (CDC)",
#              xlab = "Opiod Prescription Rate Per 100 People")
# column <- as.numeric(as.character(CDC.Opioid.Prescription.Data...2018$V4))
# column

va_borders <- counties(state = "VA", year = 2018, cb = TRUE) %>%
    st_as_sf() %>%
  st_transform(crs= 4326)

my_bins <- c(0, 50, 100, 150, 200, Inf)
palette <- colorBin("Reds", domain = CDC.Opioid.Prescription.Data...2018$V4, bins = my_bins)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = va_borders,
              fillColor = ~palette(as.numeric(as.character(CDC.Opioid.Prescription.Data...2018$V4))),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2011", popup =CDC.Opioid.Prescription.Data...2018$V1) %>%
  addLegend(pal = palette,
            values = my_bins, title = "Opiod Prescription Rate per 100 People in 2018", position = "bottomright") %>%
  addLayersControl(baseGroups = c("2011"))
