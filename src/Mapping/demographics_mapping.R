library(leaflet)
library(leaflet.extras)
library(dplyr)
library(BAMMtools)
library(sf)

#importing the Halifax Decennial Dataset onto this R file
halifax_decennial_data <- st_read(here::here("src", "Data_Ingestion", "halifax_decennial_data.geojson"))


#checking the range, histgram, and suggested breaks for selected variable
range(halifax_decennial_data$pct_asian_race)
hist(halifax_decennial_data$white_race)
BAMMtools::getJenksBreaks(halifax_decennial_data$pct_white_race, 5)

#created color palette separating values between percentages
Pct_colors <- colorBin("YlGnBu", domain = c(0, 1), 
         bins = c(0, 0.15, 0.35, 0.60, 0.85))

#Leaflet file on race in Halifax
 halifax_decennial_data %>%
  #limited dragging and set minimum and maximum zoom settings
leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  #added base tiles
  addTiles()%>%
  #added layers of chloropleth maps depicting each race
  addPolygons(fillColor = ~Pct_colors(pct_white_race),
              fillOpacity = 1, group = "White Race",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("White People: ", (round(pct_white_race, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_black_race),
              fillOpacity = 1, group = "Black Race",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Black People: ", (round(pct_black_race, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_asian_race),
              fillOpacity = 1, group = "Asian Race",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Asian People: ", (round(pct_asian_race, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_native_race),
              fillOpacity = 1, group = "Native Race",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Native People: ", (round(pct_native_race, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_hispanic_ethnicity),
              fillOpacity = 1, group = "Hispanic Ethnicity",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Hispanic People: ", (round(pct_hispanic_ethnicity)))) %>%
  #added Layer control to make each layer a seperate sheet
  addLayersControl(baseGroups = c("White Race", "Black Race", "Asian Race", 
                                     "Native Race", "Hipanic Ethnicity")) %>%
  #add legend
  addLegend(position = "bottomright", pal = Pct_colors, values = c(0,1), 
            title = "Proportion of Selected Race")  

#save Leaflet file as a Widget
saveWidget(race_halifax, file = "raceMapHalifax.html")

#Leaflet file on white vs non-white in Halifax
Pct_colors_WvNW <- colorBin("YlGnBu", domain = c(0, 1), 
                       bins = c(0, .2, 0.40, 0.60, 1))

halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors_WvNW(pct_white_race),
              fillOpacity = 1, group = "White Race",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("White People: ", (round(pct_white_race, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors_WvNW(pct_non_white),
              fillOpacity = 1, group = "Non White ",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Non White: ", (round(pct_black_race, digits = 2)))) %>%
  addLayersControl(baseGroups = c("White Race", "Non White")) %>%
  addLegend(position = "bottomright", pal = Pct_colors_WvNW, values = c(0,1), 
            title = "Proportion of Selected Race") 


#Leaflet map of types of housing ownership in Halifax
Pct_colors_housing <- colorBin("YlGnBu", domain = c(0, 1), 
                       bins = c(0.1, 0.25, 0.45, 0.60))

halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors_housing(pct_mortgage_owned_housing),
              fillOpacity = 1, group = "Mortgage Owned Housing",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("Mortgage Owned: ", (round(pct_mortgage_owned_housing, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors_housing(pct_full_owned_housing),
              fillOpacity = 1, group = "Full Owned Housing",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Full Owned Housing: ", (round(pct_full_owned_housing, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors_housing(pct_renters_housing),
              fillOpacity = 1, group = "Renters Housing",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Renters Housing: ", (round(pct_renters_housing, digits = 2)))) %>%
  addLayersControl(baseGroups = c("Mortgage Owned Housing", "Full Owned Housing", 
                                     "Renters Housing")) %>%
  addLegend(position = "bottomright", pal = Pct_colors_housing, values = c(0,1), 
            title = "Proportion of Ownership Type") 

#leaflet map on racial disparities in houshing types
Pct_colors_houseRace <- colorBin("YlGnBu", domain = c(190, 1700), 
                       bins = c(0, 450, 750, 1000, 1700 ))

halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors_houseRace(total_owner),
              fillOpacity = 1, group = "Total Population of Owners",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("Total Owners: ", (round(total_owner, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors_houseRace(white_owner),
              fillOpacity = 1, group = "White Population of Owners",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("White Owners: ", (round(white_owner, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors_houseRace(black_owner),
              fillOpacity = 1, group = "Black Population of Owners",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Black Owners: ", (round(black_owner, digits = 2)))) %>%
  addLayersControl(baseGroups = c("Total Population of Owners", "White Population of Owners", 
                                  "Black Population of Owners")) %>%
  addLegend(position = "bottomright", pal = Pct_colors_houseRace, values = c(0,1), 
            title = "Ownership by race") 


#Map on renting in Halifax based on Race
Pct_colors_rentRace <- colorBin("YlGnBu", domain = c(190, 1700), 
                                 bins = c(0, 250, 600, 1000))

halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors_rentRace(total_renter),
              fillOpacity = 1, group = "Total Population of Renters",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("Total Renters: ", (round(total_renter)))) %>%
  addPolygons(fillColor = ~Pct_colors_rentRace(white_renter),
              fillOpacity = 1, group = "White Population of Renters",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("White Renters: ", (round(white_renter)))) %>%
  addPolygons(fillColor = ~Pct_colors_rentRace(black_renter),
              fillOpacity = 1, group = "Black Population of Renters",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Black Renters: ", (round(black_renter)))) %>%
  addLayersControl(baseGroups = c("Total Population of Renters", "White Population of Renters", 
                                  "Black Population of Renters")) %>%
  addLegend(position = "bottomright", pal = Pct_colors_houseRace, values = c(0,1), 
            title = "Rentership by race") 


#Map on owning in Halifax based off of race
halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors(pct_white_owner),
              fillOpacity = 1, group = "White Owners",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("White OWners: ", (round(pct_white_owner, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_black_owner),
              fillOpacity = 1, group = "Black OWners",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Black Owners: ", (round(pct_black_owner, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_white_renter),
              fillOpacity = 1, group = "White Renters",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("White Renters: ", (round(pct_white_renter, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_black_renter),
              fillOpacity = 1, group = "Black Renters",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Black Renters: ", (round(pct_black_renter, digits = 2)))) %>%
  addLayersControl(baseGroups = c("White Owners", "Black Owners", 
                                  "White Renters", "Black Renters")) %>%
  addLegend(position = "bottomright", pal = Pct_colors, values = c(0,1), 
            title = "Proportion of Ownership Type by race") 


#Map on gender in Halifax
Pct_colors_gender <- colorBin("YlGnBu", domain = c(0, 1), 
                       bins = c(0.35, 0.45, 0.55, 0.65))

halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors_gender(pct_male_pop),
              fillOpacity = 1, group = "Male Population",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("Proportion of Male Population: ", (round(pct_male_pop, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors_gender(pct_female_pop),
              fillOpacity = 1, group = "Female Population",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Proportion of Female Population: ", (round(pct_female_pop, digits = 2)))) %>%
  addLayersControl(baseGroups = c("Male Population", "Female Population")) %>%
  addLegend(position = "bottomright", pal = Pct_colors_gender, values = c(0,1), 
            title = "Proportion of Gender") 


#Leaflet map on age in Halifax
Pct_colors_age <- colorBin("YlGnBu", domain = c(0, 1), 
                              bins = c(0.15, 0.40, 0.60, 0.8))

halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors_age(pct_under18),
              fillOpacity = 1, group = "Minor Population",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("Proportion of Minor Population: ", (round(pct_under18, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors_age(pct_adult),
              fillOpacity = 1, group = "Adult Population",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Proportion of Adult Population: ", (round(pct_adult, digits = 2)))) %>%
  addLayersControl(baseGroups = c("Minor Population", "Adult Population")) %>%
  addLegend(position = "bottomright", pal = Pct_colors_age, values = c(0,1), 
            title = "Proportion of Age") 


#Leaflet map on family structure in Halifax
Pct_colors_structure <- colorBin("YlGnBu", domain = c(0, 1), 
                           bins = c(0, 0.20, 0.40, 0.6))

halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors_structure(pct_husband_wife_household),
              fillOpacity = 1, group = "Husband and Wife Household",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("Proportion: ", (round(pct_husband_wife_household, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors_structure(pct_single_parent),
              fillOpacity = 1, group = "Household of Single Parents",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Proportion: ", (round(pct_single_parent, digits = 2)))) %>%
  addPolygons(fillColor = ~Pct_colors_structure(pct_lives_alone_household),
             fillOpacity = 1, group = "Households that live alove",
             highlight = highlightOptions(color = "white", fillColor = "red",
                                          bringToFront = TRUE),
             label = ~paste0("Proportion: ", (round(pct_lives_alone_household, digits = 2)))) %>%
  addLayersControl(baseGroups = c("Husband and Wife Household", 
                                  "Household of Single Parents", "Households that live alove")) %>%
  addLegend(position = "bottomright", pal = Pct_colors_structure, values = c(0,1), 
            title = "Family Household Structures") 


#Leaflet map on median age in Halifax
Pct_colors_med_age <- colorBin("YlGnBu", domain = c(0, 1), 
                                 bins = c(34, 40, 48, 53))

halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors_med_age(total_median_age),
              fillOpacity = 1, group = "Total Median Age",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("Age: ", (total_median_age))) %>%
  addPolygons(fillColor = ~Pct_colors_med_age(white_median_age),
              fillOpacity = 1, group = "White Median Age",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Age: ", (white_median_age))) %>%
  addPolygons(fillColor = ~Pct_colors_med_age(black_median_age),
              fillOpacity = 1, group = "Black Median Age",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Age: ", (black_median_age))) %>%
  addLayersControl(baseGroups = c("Total Median Age", "White Median Age", "Black Median Age")) %>%
  addLegend(position = "bottomright", pal = Pct_colors_med_age, values = c(0,1), 
            title = "Median Age") 
