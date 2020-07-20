install.packages("tigris")
install.packages('here')
install.packages('rvest')

library(rvest)
library(sf)
library(tigris)
library(leaflet)
library(tidycensus)
library(stringr)
library(dplyr)
library(here)

# ggplot(CDC.Opioid.Prescription.Data...2018, aes(x = V4)) + geom_histogram(stat = "count", binwidth = 50)
# column <- as.numeric(as.character(CDC.Opioid.Prescription.Data...2018$V4))
# hist <- hist(column, xlim = c(0, 200), main = "Distribution of 2018 VA Opiod Rates (CDC)",
#              xlab = "Opiod Prescription Rate Per 100 People")

rate_2011 <- html("http://www.cdc.gov/drugoverdose/maps/rxcounty2011.html")
tables <- rate_2011 %>% html_table(fill = TRUE)
first_table <- tables[[1]]
first_table <- filter(first_table, State == "VA" & '2011 Prescribing Rate' >= 0)
first_table

rate_2011 %>%
  html_nodes("table") %>%
  html_text() %>%


va_borders <- get_acs(table = "B01003", geography = "county", year = 2018, state = "VA",
                      survey = "acs5", geometry = TRUE) %>% st_transform(crs = 4326)
datasource <- readr::read_csv(here::here("git/TestDSPG/Halifaxx/data/original/Substance_Abuse/
                                         CDC Opioid Prescription Data - 2018.csv"))
new_tbl <- merge(va_borders, datasource, by.x = "GEOID", by.y = "State/County FIPS Code")
head(new_tbl)

my_bins <- c(0, 50, 100, 150, 200, Inf)
palette <- colorBin("Reds", domain = new_tbl$Opiod.Prescription.Rate.per.100, bins = my_bins)

my_label <- paste("County: ", new_tbl$NAME,"<br/>", "Rate: ", new_tbl$`Opiod Prescription Rate per 100`, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = new_tbl, fillColor = ~palette(`Opiod Prescription Rate per 100`),
              weight = 1, color = "#fafafa", fillOpacity = 0.8, group = "2011",
              label = my_label,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                        textsize = "13px", direction = "auto")) %>%
  addLegend(pal = palette, values = my_bins,
            title = "Opiod Prescription Rate per 100 People in 2018", position = "bottomright") %>%
  addLayersControl(baseGroups = c("2011"))
