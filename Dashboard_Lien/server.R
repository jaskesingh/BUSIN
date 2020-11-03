#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(ggplot2)
library(tidyr)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(plotly)
Revenue <- read_xlsx("data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Revenues (automotive)", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_Margin <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross margin", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_profit <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross profit", col_types = c("numeric", "text", "numeric", "numeric", "numeric"))
Free_cashflow <- read_xlsx("Data/Tesla's free cash flow by quarter 2020 world wide.xlsx", skip = 3 , sheet = "Data", col_types = c("numeric", "text", "numeric"))

revenue <- function(yearinput,df) {
  revenue <- df %>% filter(df$Year == yearinput)
  return(revenue)
}
#jaartotaal <- function(jaar, kolnaam, minjaar, maxjaar, data) {
#  
 # return(jaartotaal)
#}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    output$revbox <- renderValueBox({
      valueBox(
        paste0(sum(Revenue$`1000_revenue`[Revenue$Year == input$Yearrev], na.rm = TRUE)*1000),
        subtitle= paste0("Omzet ", input$Yearrev), 
        icon = icon("dollar-sign")
      )
    })
    
    output$frcashbox <- renderValueBox({
      valueBox(
        paste0(sum(Free_cashflow$`free cash flow`[Free_cashflow$Year == input$Yearrev], na.rm = TRUE)),
        subtitle = paste0("Free cashflow ", input$Yearrev), 
        icon = icon("dollar-sign")
      )
    })
    
    output$grprbox <- renderValueBox({
      valueBox(
        paste0(sum(Gross_profit$`Automotive gross profit GAAP`[Gross_profit$Year == input$Yearrev], na.rm = TRUE)), 
        subtitle = paste0("Bruto winst ", input$Yearrev),  
        icon = icon("dollar-sign")
      )
    })
  
    output$colrev <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- Revenue$Year
        Yearrev <- seq(min(x), max(x), length.out = input$Yearrev)
        

        # draw the histogram with the specified number of bins
        revenue(input$Yearrev, Revenue) %>% ggplot(aes(x = Quarter, y = `1000_revenue`, fill= Quarter))+ geom_col(position="dodge") + 
          labs(y = 'Automotive revenue')  +
          scale_y_continuous(breaks = seq(0,8000, by= 1000))

    })
    
    output$colgrmar <- renderPlot({
      
      revenue(input$Yearrev, Gross_Margin) %>% 
        ggplot(aes(x = Quarter, y = `Gross margin Automotive GAAP`, fill= Quarter)) + 
        geom_col(position="dodge") + 
        labs(y= 'Gross margin') + 
        scale_y_continuous(breaks = seq(0, 30, by= 2))
    })
    
    output$colgrpr <- renderPlot({
      revenue(input$Yearrev, Gross_profit) %>% 
        ggplot(aes(x = Quarter, y = `Automotive gross profit GAAP`, fill= Quarter)) + 
        geom_col(position="dodge") + 
        labs(y= 'Gross profit') + 
        scale_y_continuous(breaks = seq(0,2500000, by= 500000))
    })
    
    output$colfrcash <- renderPlot({
      revenue(input$Yearrev, Free_cashflow) %>% 
        ggplot(aes(x= Quarter, y= `free cash flow`, fill = Quarter)) + 
        geom_col(position="dodge") + 
        labs(y = 'Free cash flow')
    })
    
    output$linerev <- renderPlot({
      x    <- Revenue$Year
      Yearrevline <- seq(min(x), max(x)) 
      
      
      
      # draw the histogram with the specified number of bins
      Revenue %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% 
        mutate("totaal" = sum(`1000_revenue`, na.rm = TRUE)) %>% select(Year, totaal)%>% distinct() %>% ggplot(aes(x = Year , y = totaal))+ geom_line() + 
        labs(y = 'Automotive revenue')  +
        scale_y_continuous(limits = c(0, 25000), breaks = seq(0,25000, by= 5000)) + scale_x_continuous(breaks = seq(min(input$Yearrevline), max(input$Yearrevline), by = 1))
    })
      
      

})
