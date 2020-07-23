library(leaflet)
library(leaflet.extras)
library(dplyr)
library(BAMMtools)
library(sp)

#importing the Halifax Decennial Dataset onto this R file
halifax_decennial_data <- st_read(here::here("src", "Data_Ingestion", "halifax_decennial_data.geojson"))

#checking the range, histgram, and suggested breaks for selected variable
range(halifax_decennial_data$pct_asian_race)
hist(halifax_decennial_data$white_race)
BAMMtools::getJenksBreaks(halifax_decennial_data$pct_white_race, 5)

#created color palette separating values between percentages
Pct_colors <- colorBin("YlGnBu", domain = c(0, 1), 
         bins = c(0, 0.01, 0.05, 0.15, 0.30, 0.45, 0.60, 0.8))

#Leaflet file on race in Halifax
race_halifax <- halifax_decennial_data %>%
  #limited dragging and set minimum and maximum zoom settings
leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  #added base tiles
  addTiles()%>%
  #added layers of chloropleth maps depicting each race
  addPolygons(fillColor = ~Pct_colors(pct_white_race),
              fillOpacity = 0.75, group = "White Race",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("White People: ", (pct_white_race))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_black_race),
              fillOpacity = 0.75, group = "Black Race",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Black People: ", (pct_black_race))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_asian_race),
              fillOpacity = 0.75, group = "Asian Race",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Asian People: ", (pct_asian_race))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_native_race),
              fillOpacity = 0.75, group = "Native Race",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Native People: ", (pct_native_race))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_hispanic_ethnicity),
              fillOpacity = 0.75, group = "Hispanic Ethnicity",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Hispanic People: ", (pct_hispanic_ethnicity))) %>%
  #added Layer control to make each layer a seperate sheet
  addLayersControl(baseGroups = c("White Race", "Black Race", "Asian Race", 
                                     "Native Race", "Hipanic Ethnicity")) %>%
  #add legend
  addLegend(position = "bottomright", pal = Pct_colors, values = c(0,1), 
            title = "Proportion of Selected Race")

#save Leaflet file as a Widget
saveWidget(race_halifax, file = "raceMapHalifax.html")


#Leaflet map of types of housing ownership in Halifax
housing_mapping <- halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors(pct_mortgage_owned_housing),
              fillOpacity = 0.75, group = "Mortgage Owned Housing",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("Mortgage Owned: ", (pct_mortgage_owned_housing))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_full_owned_housing),
              fillOpacity = 0.75, group = "Full Owned Housing",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Full Owned Housing: ", (pct_full_owned_housing))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_renters_housing),
              fillOpacity = 0.75, group = "Renters Housing",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Renters Housing: ", (pct_renters_housing))) %>%
  addLayersControl(baseGroups = c("Mortgage Owned Housing", "Full Owned Housing", 
                                     "Renters Housing")) %>%
  addLegend(position = "bottomright", pal = Pct_colors, values = c(0,1), 
            title = "Proportion of Ownership Type")

#leaflet map on racial disparities in houshing types
Pct_colors_houseRace <- colorBin("YlGnBu", domain = c(190, 1700), 
                       bins = c(190, 300, 450, 600, 750 , 850, 1000, 1400, 1700 ))
range(halifax_decennial_data$black_owner)
BAMMtools::getJenksBreaks(halifax_decennial_data$total_owner, 7)


race_owning_mapping <- halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors_houseRace(total_owner),
              fillOpacity = 0.75, group = "Total Population of Owners",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("Total Owners: ", (total_owner))) %>%
  addPolygons(fillColor = ~Pct_colors_houseRace(white_owner),
              fillOpacity = 0.75, group = "White Population of Owners",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("White Owners: ", (white_owner))) %>%
  addPolygons(fillColor = ~Pct_colors_houseRace(black_owner),
              fillOpacity = 0.75, group = "Black Population of Owners",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Black Owners: ", (black_owner))) %>%
  addLayersControl(baseGroups = c("Total Population of Owners", "White Population of Owners", 
                                  "Black Population of Owners")) %>%
  addLegend(position = "bottomright", pal = Pct_colors_houseRace(), values = c(0,1), 
            title = "Ownership by race")


#Map on renting in Halifax based on Race
race_renting_mapping <- halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors_houseRace(total_renter),
              fillOpacity = 0.75, group = "Total Population of Renters",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("Total Renters: ", (total_renter))) %>%
  addPolygons(fillColor = ~Pct_colors_houseRace(white_renter),
              fillOpacity = 0.75, group = "White Population of Renters",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("White Renters: ", (white_renter))) %>%
  addPolygons(fillColor = ~Pct_colors_houseRace(black_renter),
              fillOpacity = 0.75, group = "Black Population of Renters",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Black Renters: ", (black_renters))) %>%
  addLayersControl(baseGroups = c("Total Population of Renters", "White Population of Renters", 
                                  "Black Population of Renters")) %>%
  addLegend(position = "bottomright", pal = Pct_colors_houseRace(), values = c(0,1), 
            title = "Rentership by race")


#Map on owning in Halifax based off of race
owning_race_mapping <- halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors(pct_white_owner),
              fillOpacity = 0.75, group = "White Owners",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("White OWners: ", (pct_white_owner))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_black_owner),
              fillOpacity = 0.75, group = "Black OWners",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Black Owners: ", (pct_black_owner))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_white_renter),
              fillOpacity = 0.75, group = "White Renters",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("White Renters: ", (pct_white_renter))) %>%
  addPolygons(fillColor = ~Pct_colors(pct_black_renter),
              fillOpacity = 0.75, group = "Black Renters",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Black Renters: ", (pct_black_renter))) %>%
  addLayersControl(baseGroups = c("White Owners", "Black Owners", 
                                  "White Renters", "Black Renters")) %>%
  addLegend(position = "bottomright", pal = Pct_colors, values = c(0,1), 
            title = "Proportion of Ownership Type by race")


#Map on gender in Halifax
Pct_colors_gender <- colorBin("YlGnBu", domain = c(0, 1), 
                       bins = c(0.35, 0.40, 0.425, 0.45, 0.475, 0.50, 0.55, 0.6))

gender_mapping <- halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors_gender(pct_male_pop),
              fillOpacity = 0.75, group = "Male Population",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("Percent of Male Population: ", (pct_male_pop))) %>%
  addPolygons(fillColor = ~Pct_colors_gender(pct_female_pop),
              fillOpacity = 0.75, group = "Female Population",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Percent of Female Population: ", (pct_female_pop))) %>%
  addLayersControl(baseGroups = c("Male Population", "Female Population")) %>%
  addLegend(position = "bottomright", pal = Pct_colors_gender, values = c(0,1), 
            title = "Proportion of Gender")


#Leaflet map on age in Halifax
Pct_colors_age <- colorBin("YlGnBu", domain = c(0, 1), 
                              bins = c(0.15, 0.20, 0.40, 0.50, 0.60, 0.65, 0.70, 0.75, 0.8))

Age_mapping <- halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors_age(pct_under18),
              fillOpacity = 0.75, group = "Minor Population",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("Percent of Minor Population: ", (pct_under18))) %>%
  addPolygons(fillColor = ~Pct_colors_age(pct_adult),
              fillOpacity = 0.75, group = "Adult Population",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Percent of Adult Population: ", (pct_adult))) %>%
  addLayersControl(baseGroups = c("Minor Population", "Adult Population")) %>%
  addLegend(position = "bottomright", pal = Pct_colors_age, values = c(0,1), 
            title = "Proportion of Age")


#Leaflet map on family structure in Halifax
Pct_colors_structure <- colorBin("YlGnBu", domain = c(0, 1), 
                           bins = c(0.01, 0.05, 0.10, 0.20, 0.30, 0.40, 0.45, 0.50, 0.6))

Family_structure_mapping <- halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors_structure(pct_husband_wife_household),
              fillOpacity = 0.75, group = "Husband and Wife Household",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("Percent: ", (pct_husband_wife_household))) %>%
  addPolygons(fillColor = ~Pct_colors_structure(pct_male_no_wife_household),
              fillOpacity = 0.75, group = "Household with Male; no wife",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Percent: ", (pct_male_no_wife_household))) %>%
  addPolygons(fillColor = ~Pct_colors_structure(pct_female_no_husband_household),
              fillOpacity = 0.75, group = "Household with Female, no husband",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Percent: ", (pct_female_no_husband_household))) %>%
  addPolygons(fillColor = ~Pct_colors_structure(pct_lives_alone_household),
             fillOpacity = 0.75, group = "Households that live alove",
             highlight = highlightOptions(color = "white", fillColor = "red",
                                          bringToFront = TRUE),
             label = ~paste0("Percent: ", (pct_lives_alone_household))) %>%
  addLayersControl(baseGroups = c("Husband and Wife Household", "Household with Male; no wife", 
                                  "Household with Female, no husband", "Households that live alove")) %>%
  addLegend(position = "bottomright", pal = Pct_colors_structure, values = c(0,1), 
            title = "Family Household Structures")


#Leaflet map on median age in Halifax
Pct_colors_med_age <- colorBin("YlGnBu", domain = c(0, 1), 
                                 bins = c(34, 38, 40, 44, 46, 48, 50, 53))

Median_age_mapping <- halifax_decennial_data %>%
  leaflet(options = leafletOptions(dragging = FALSE, minZoom = 9, maxZoom = 12)) %>%
  addTiles()%>%
  addPolygons(fillColor = ~Pct_colors_med_age(total_median_age),
              fillOpacity = 0.75, group = "Total Median Age",
              highlight = highlightOptions(color = "white", fillColor = "red", 
                                           bringToFront = TRUE),
              label = ~paste0("Age: ", (total_median_age))) %>%
  addPolygons(fillColor = ~Pct_colors_med_age(white_median_age),
              fillOpacity = 0.75, group = "White Median Age",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Age: ", (white_median_age))) %>%
  addPolygons(fillColor = ~Pct_colors_med_age(black_median_age),
              fillOpacity = 0.75, group = "Black Median Age",
              highlight = highlightOptions(color = "white", fillColor = "red",
                                           bringToFront = TRUE),
              label = ~paste0("Age: ", (black_median_age))) %>%
  addLayersControl(baseGroups = c("Total Median Age", "White Median Age", "Black Median Age")) %>%
  addLegend(position = "bottomright", pal = Pct_colors_med_age, values = c(0,1), 
            title = "Median Age")
