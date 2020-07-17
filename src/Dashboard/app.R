

library(shiny)
library(shinydashboard)
library(shinybusy)
library(fresh) # for eventual styling
library(purrr)
library(sf)
library(leaflet)
library(dplyr)
library(ggplot2)
library(glue)


# modules loaded automatically from "R/" as of shiny 1.5.0

ui <- dashboardPage(
  dashboardHeader(title = "Halifax Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Social Determinants", tabName = "social_determinants",
               menuSubItem("Housing", tabName = "housing"),
               menuSubItem("Unemployment", tabName = "unemployment"),
               menuSubItem("Substance Abuse", tabName = "substance_abuse"),
               menuSubItem("Family Structure", tabName = "family_structure"),
               startExpanded = TRUE)
    )
  ),
  dashboardBody(
    add_busy_spinner(spin = "fading-circle",
                     height = "30px", width = "30px"),
    tabItems(
      # First tab content
      tabItem(tabName = "overview",
              overviewUI("overviewTab")
      ),

      # Second tab content
      tabItem(tabName = "housing",
              housingUI("housingTab")
      ),
      tabItem(tabName = "unemployment",
              unemploymentUI("unemploymentTab")
      ),
      tabItem(tabName = "substance_abuse",
              substanceAbuseUI("substanceAbuseTab")
      ),
      tabItem(tabName = "family_structure",
              familyStructureUI("familyStructureTab")
      )
    )
  )
)

server <- function(input, output, session) {
  overviewServer("overviewTab")
  housingServer("housingTab")
  unemploymentServer("unemploymentTab")
  substanceAbuseServer("substanceAbuseTab")
  familyStructureServer("familyStructureTab")

}

shinyApp(ui, server)
