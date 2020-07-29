library(sf)
library(sp)
library(leaflet)
library(dplyr)
library(BAMMtools)
library(here)
library(leaflet.extras)

#Counties_VA_geometry <- st_as_sf(VA_counties) %>%
#  mutate(County = NAME) %>%
#  select(GEOID, NAME, NAMELSAD, geometry)

#Teen_Births_VA <- Teen_Births_VA %>%
  #select(State, County, "BirthRate2018", "BirthRate2015", "BirthRate2012", "BirthRate2009", "BirthRate2006") %>%
  #mutate(Teen_Births_VA, GEOID = as.character(GEOID))

#Teen_Births_VA <- inner_join(Teen_Births_VA, Counties_VA_geometry ) %>%
#  mutate(is_halifax = case_when(County == "Halifax" ~ "Yes",
#                                County != "Halifax" ~ "No"))

#Teen_Births_VA <- st_as_sf(Teen_Births_VA)

st_read(here::here("src", "Data_Ingestion", "Teen_Births_VA.geojson"))

Teen_B_colors <- colorBin("YlGnBu", domain = c(0, 100), 
                              bins = c(0, 15, 30, 45, 60, 75, 90, 100))

Teen_Births_VA %>%
  leaflet(options = leafletOptions(minZoom = 7, maxZoom = 12)) %>%
  setView(-78.6569, 38, 7) %>%
  addTiles()%>%
  addResetMapButton() %>%
  addPolygons(fillColor = ~Teen_B_colors(BirthRate2018), color = "black", 
              fillOpacity = 1, group = "2018", opacity = 1, weight = 2,
              highlight = highlightOptions(color = "white",  
                                           bringToFront = TRUE),
              label = ~paste0("Birth Rate: ", (BirthRate2018))) %>%
  addPolygons(fillColor = ~Teen_B_colors(BirthRate2015), color = "black",
              fillOpacity = 1, group = "2015", weight = 2, opacity = 1,
              highlight = highlightOptions(color = "white",
                                           bringToFront = TRUE),
              label = ~paste0("Birth Rate: ", (BirthRate2015))) %>%
  addPolygons(fillColor = ~Teen_B_colors(BirthRate2012), color = "black",
              fillOpacity = 1, group = "2012", weight = 2, opacity = 1,
              highlight = highlightOptions(color = "white", 
                                           bringToFront = TRUE),
              label = ~paste0("Birth Rate: ", (BirthRate2012))) %>%
  addPolygons(fillColor = ~Teen_B_colors(BirthRate2009), color = "black",
              fillOpacity = 1, group = "2009", weight = 2, opacity = 1,
              highlight = highlightOptions(color = "white",
                                           bringToFront = TRUE),
              label = ~paste0("Birth Rate: ", (BirthRate2009))) %>%
  addPolygons(fillColor = ~Teen_B_colors(BirthRate2006), color = "black",
              fillOpacity = 1, group = "2006", weight = 2, opacity = 1,
              highlight = highlightOptions(color = "white",
                                           bringToFront = TRUE),
              label = ~paste0("Birth Rate: ", (BirthRate2006))) %>%
  addLayersControl(baseGroups = c("2018", "2015", "2012",
                                  "2009", "2006")) %>%
  addLegend(position = "bottomright", pal = Teen_B_colors, values = c(0,100), 
            title = "Birth Rate") 
