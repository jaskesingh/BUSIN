# # # # # # # # # #

# Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(scales)

# Load and clean data
# Next to read_xlsx, there is also a read.xlsx function, in case the ...
# ... big "growth" table needs some more functions.
# Opmerking aan mezelf: als error dat path niet exist, niet dit uit comments halen, wel View(loyalty_per_brand_1 runnen)
# loyalty_per_brand_1 <- read_xlsx("Data/loyalty_per_brand_v2.xlsx", skip = 2)

# Was al een tible
loyalty_per_brand_tibble = as_tibble(loyalty_per_brand_1)
loyalty_per_brand_1
loyalty_per_brand_tibble


#Percentages gemaakt, maar dan wordt kolomtype characters
# loyalty_per_brand_tibble$Percentage <- percent(x = loyalty_per_brand_tibble$Percentage, scale = 100, accuracy = 0.1)
# loyalty_per_brand_tibble

# Was al numeric
loyalty_per_brand_tibble$Percentage <- as.numeric(loyalty_per_brand_tibble$Percentage)
loyalty_per_brand_tibble

# Clean names
colnames(loyalty_per_brand_tibble) <- c("Ranking", "Brand", "Percentage", "Classification")
loyalty_per_brand_tibble

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
  
  # Growth placeholder
  output$histogram_growth <- renderPlot({
    hist(faithful$eruptions, breaks = input$bins_growth)
  })
  
  # Loyalty
  output$loyalty_col <- renderPlot({
    loyalty_per_brand_2 <-loyalty_per_brand_tibble %>% arrange(desc(Percentage))
    loyalty_per_brand_2
    loyalty_per_brand_plot <- ggplot(loyalty_per_brand_2, aes(x = Percentage,
                                                              y = Brand)) +
      geom_col()
    loyalty_per_brand_plot
  })
  
})

# # # # # # # # # #