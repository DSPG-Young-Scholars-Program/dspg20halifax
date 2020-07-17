#######################################################
# Module containing code to create Family Structure Pane
#######################################################


familyStructureUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      h2("Family Structure Tab"),
      box(plotOutput(ns("plot1"), height = 250)),

      box(
        title = "Controls",
        sliderInput(ns("slider"), "Number of observations:", 1, 100, 50)
      )
    )
  )
}

familyStructureServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      set.seed(122)
      histdata <- rnorm(500)
      output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
      })
    }
  )
}
