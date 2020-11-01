#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

revenue <- function(yearinput,df) {
  revenue <- df %>% filter(df$Year == yearinput)
  return(revenue)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  

   

    output$col <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- Revenue$Year
        Yearrev <- seq(min(x), max(x), length.out = input$Yearrev)
        

        # draw the histogram with the specified number of bins
       revenue(input$Yearrev, Revenue) %>% ggplot(aes(x = Quarter, y = `1000_revenue`, fill= Quarter))+ geom_col(position="dodge") + 
          labs(title = 'Yearly automotive Revenue', subtitle = 'Per quarter and in thousends', y = 'Automotive revenue')  

    })
    

})
