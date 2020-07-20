library(leaflet)
library(dplyr)

range(hfl_white_race$white_race)

hist(hfl_white_race$white_race)
       
BAMMtools::getJenksBreaks(halifax_decennial_data$pct_white_race, 5)

Pct_colors <- colorBin("Blues", domain = c(0, 1), 
         bins = c(0, 0.15, 0.3, 0.45, 0.6, 0.75, 0.9, 1))

race_halifax <- halifax_decennial_data %>%
leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors(pct_white_race),
              fillOpacity = 0.75, group = "White Race",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("White People: ", (white_race))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_black_race),
              fillOpacity = 0.75, group = "Black Race",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Black People: ", (black_race))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_asian_race),
              fillOpacity = 0.75, group = "Asian Race",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Asian People: ", (asian_race))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_native_race),
              fillOpacity = 0.75, group = "Native Race",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Native People: ", (native_race))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_hispanic_ethnicity),
              fillOpacity = 0.75, group = "Hispanic Ethnicity",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Hispanic People: ", (hispanic_ethnicity))) %>%
  addLayersControl(overlayGroups = c("White Race", "Black Race", "Asian Race", 
                                     "Native Race", "Hipanic Ethnicity"))

saveWidget(race_halifax, file = "raceMapHalifax.html")

race_halifax <- halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors(pct_mortgage_owned_housing),
              fillOpacity = 0.75, group = "Mortgage Owned",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("Mortgage Owned: ", (pop_in_mortgage_owned_housing))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_black_race),
              fillOpacity = 0.75, group = "Black Race",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Black People: ", (black_race))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_asian_race),
              fillOpacity = 0.75, group = "Asian Race",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Asian People: ", (asian_race))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_native_race),
              fillOpacity = 0.75, group = "Native Race",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Native People: ", (native_race))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_hispanic_ethnicity),
              fillOpacity = 0.75, group = "Hispanic Ethnicity",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Hispanic People: ", (hispanic_ethnicity))) %>%
  addLayersControl(overlayGroups = c("White Race", "Black Race", "Asian Race", 
                                     "Native Race", "Hipanic Ethnicity"))
