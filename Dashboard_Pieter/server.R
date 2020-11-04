# # # # # # # # # #

# Libraries
library(shiny)
library(shinydashboard)

# Shiny Server
shinyServer(function(input, output){
  
  # Growth
  output$histogram_growth <- renderPlot({
    hist(faithful$eruptions, breaks = input$bins_growth)
  })
  
  # Loyalty
  output$histogram_loyalty <- renderPlot({
    hist(faithful$eruptions, breaks = input$bins_loyalty)
  })
  
})

# # # # # # # # # #