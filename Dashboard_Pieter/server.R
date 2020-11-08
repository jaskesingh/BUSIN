# # # # # # # # # #

# Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)

# Load and clean data
# Next to read_xlsx, there is also a read.xlsx function, in case the ...
# ... big "growth" table needs some more functions.
## loyalty_per_brand_1 <- read_xlsx("Data/loyalty_per_brand_v2.xlsx", skip = 2)

#Inspect data
# View(loyalty_per_brand_1)
# dim(loyalty_per_brand_1)
# str(loyalty_per_brand_1)
# head(loyalty_per_brand_1)

# Static ggplots
# # Loyalty
# loyalty_per_brand_1
# loyalty_per_brand_2 <-loyalty_per_brand_1 %>% arrange(desc(Percentage)) 
# loyalty_per_brand_2
# 
# 
# loyalty_per_brand_plot <- ggplot(loyalty_per_brand_1, aes(x = Percentage,
#                                                        y = Merk)) +
#                           geom_col()
# loyalty_per_brand_plot
# 
# loyalty_per_brand_luxury <- loyalty_per_brand_2 %>% filter(`Type of brand` == "Luxury")
# loyalty_per_brand_luxury
# 
# loyalty_per_brand_luxury_plot <- loyalty_per_brand_luxury %>%
#   ggplot(aes(x=Percentage,
#              y=Merk)) +
#   geom_col()
# loyalty_per_brand_luxury_plot

# Shiny Server
shinyServer(function(input, output){
  
  # Growth template
  output$histogram_growth <- renderPlot({
    hist(faithful$eruptions, breaks = input$bins_growth)
  })
  
  # Real loyalty
  output$loyalty_col <- renderPlot({
    loyalty_per_brand_2 <-loyalty_per_brand_1 %>% arrange(desc(Percentage))
    loyalty_per_brand_plot <- ggplot(loyalty_per_brand_1, aes(x = Percentage,
                                                              y = Merk)) +
      geom_col()
    loyalty_per_brand_plot
  })
  
})

# # # # # # # # # #