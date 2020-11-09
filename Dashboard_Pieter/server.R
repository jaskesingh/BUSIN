# # # # # # # # # #

# Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(scales)

# Load data
      
      # Loyalty
      # Next to read_xlsx, there is also a read.xlsx function, in case the ...
      # ... big "growth" table needs some more functions.
      # 
      # Opmerking aan mezelf: als error dat path niet exist, niet dit uit comments halen, wel View(loyalty_per_brand_1 runnen)
      # loyalty_per_brand_data <- read_xlsx("Data/loyalty_per_brand_v2.xlsx", skip = 2)


      # Growth comparison
      # growth_comp_data_3 <- read_xlsx("Data/growth_comparison_top15_2019_v3.xlsx")
      #View(growth_comp_data_3)

      # growth_comp_data_4 <- read_xlsx("Data/growth_comparison_top15_2019_v4.xlsx")
      # View(growth_comp_data_4)
      # str(growth_comp_data_4)



# Test script die later verwijderd wordt
# Loyalty
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
  output$loyalty_bar <- renderPlot({
    
    # Make tibble (already was, just to be sure)
    loyalty_per_brand_tibble = as_tibble(loyalty_per_brand_data)
    
    # Change to numeric (already was, but just to be sure)
    loyalty_per_brand_tibble$Percentage <- as.numeric(loyalty_per_brand_tibble$Percentage)
    
    #Percentages gemaakt, maar dan wordt kolomtype character. Daarna naar numeric werkt ook niet. 
    # loyalty_per_brand_tibble$Percentage <- percent(x = loyalty_per_brand_tibble$Percentage, scale = 100, accuracy = 0.1)
    # loyalty_per_brand_tibble
    
    # Clean names
    colnames(loyalty_per_brand_tibble) <- c("Ranking", "Brand", "Percentage", "Classification")
    
    # Reverse order (high to low)
    loyalty_per_brand_tibble <- loyalty_per_brand_tibble[order(loyalty_per_brand_tibble$Percentage), ]
    
    # To retain the order in the plot
    loyalty_per_brand_tibble$Brand <- factor(loyalty_per_brand_tibble$Brand,
                                             levels = loyalty_per_brand_tibble$Brand)
    
    # Set theme
    theme_set(theme_minimal())
    
    # Create plot
    loyalty_per_brand_plot <- ggplot(loyalty_per_brand_tibble,
                                     aes(x = Percentage,
                                         y = Brand)) +
      geom_bar(stat = "identity",
               fill = "tomato3") +
      theme(axis.text.y = element_text(vjust=0.6))
    
    # Te doen:
    # - Tesla in andere kleur (Puurder rood, rest mss in zachter rood, om toch in stijl te blijven)
    # - Percentages ipv. 0.6 enzo
    # - Percentages schaal tot 100%
    # - (Optioneel) Namen brands groter
    # - Ggplotly zodat je precieze percentage ook ziet. Dan kan mogelijk checkbox zelfs weg.(Want wil ...
    #   ... kunnen filteren op luxury/mass market of beiden). Mss voegt plotly ook toe dat merken kan ...
    #   ... kiezen, anders eventueel zelf toevoegen
    
    # Print plot
    loyalty_per_brand_plot
  })
  
})

# # # # # # # # # #