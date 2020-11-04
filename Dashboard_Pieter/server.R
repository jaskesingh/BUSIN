# # # # # # # # # #

# Libraries
library(shiny)
library(shinydashboard)

# Shiny Server
shinyServer(function(input, output){
  output$histogram <- renderPlot({
    hist(faithful$eruptions, breaks = input$bins)
  })
})
# # # # # # # # # #