# # # # # # # # # #

# Libraries
library(shiny)
library(shinydashboard)

# Static ggplots
# Loyalty
loyalty_per_brand_1
loyalty_per_brand_2 <-loyalty_per_brand_1 %>% arrange(desc(Percentage)) 
loyalty_per_brand_2


loyalty_per_brand_plot <- ggplot(loyalty_per_brand_1, aes(x = Percentage,
                                                       y = Merk)) + 
                          geom_col()
loyalty_per_brand_plot

loyalty_per_brand_luxury <- loyalty_per_brand_2 %>% filter(`Type of brand` == "Luxury")
loyalty_per_brand_luxury

loyalty_per_brand_luxury_plot <- loyalty_per_brand_luxury %>%
  ggplot(aes(x=Percentage,
             y=Merk)) +
  geom_col()
loyalty_per_brand_luxury_plot

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