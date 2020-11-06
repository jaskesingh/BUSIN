# # # # # # # # # #

# Libraries
library(shiny)
library(shinydashboard)

# Static ggplot
loyalty_per_brand_1 %>%
arrange(desc(Percentage)) %>% 
ggplot(aes(x = Percentage,
           y = Merk)) + 
  geom_col()


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