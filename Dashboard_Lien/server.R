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
library(scales)
Revenue <- read_xlsx("data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Revenues (automotive)", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_Margin <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross margin", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_profit <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross profit", col_types = c("numeric", "text", "numeric", "numeric", "numeric"))
Free_cashflow <- read_xlsx("Data/Tesla's free cash flow by quarter 2020 world wide.xlsx", skip = 3 , sheet = "Data", col_types = c("numeric", "text", "numeric"))

revenue <- function(yearinput,df) {
  revenue <- df %>% filter(df$Year == yearinput)
  return(revenue)
  
  filterbox <- function() {
    if (input$Quarterly == FALSE) {
      data <- data%>% group_by((data$Year))
    }
  }
}
#jaartotaal <- function(jaar, kolnaam, minjaar, maxjaar, data) {
#  
 # return(jaartotaal)
#}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    sortofgraph <- reactive({input$Quarterly})
    
    output$revbox <- renderValueBox({
      if (sortofgraph() == TRUE) {
      valueBox(
        paste0(gsub("(?!^)(?=(?:\\d{3})+$)", ".",sum(Revenue$`1000_revenue`[Revenue$Year == input$Yearrev], na.rm = TRUE)*1000, perl=T)),
        subtitle= paste0("Omzet ", input$Yearrev), 
        icon = icon("dollar-sign")
      )
      }
      else {
        somjaren <- c(sum(Revenue$`1000_revenue`[Revenue$Year == min(input$Yearrevline)], na.rm = TRUE):
                        sum(Revenue$`1000_revenue`[Revenue$Year == max(input$Yearrevline)], na.rm = TRUE))
        valueBox(
          paste0(gsub("(?!^)(?=(?:\\d{3})+$)", ".",sum(somjaren, na.rm = TRUE)*1000, perl=T)),
          subtitle= paste0("Omzet van ", min(input$Yearrevline), " tot ", max(input$Yearrevline)), 
          icon = icon("dollar-sign")
        )
      }
    })
    
    output$frcashbox <- renderValueBox({
      if (sortofgraph() == TRUE) {
      valueBox(
        paste0(gsub("(?!^)(?=(?:\\d{3})+$)", ".",sum(Free_cashflow$`free cash flow`[Free_cashflow$Year == input$Yearrev], na.rm = TRUE), perl=T)),
        subtitle = paste0("Free cashflow ", input$Yearrev), 
        icon = icon("dollar-sign")
      )
      }
      else {
        somjaren <- c(sum(Free_cashflow$`free cash flow`[Free_cashflow$Year == min(input$Yearrevline)], na.rm = TRUE):
                        sum(Free_cashflow$`free cash flow`[Free_cashflow$Year == max(input$Yearrevline)], na.rm = TRUE))
        valueBox(
          paste0(gsub("(?!^)(?=(?:\\d{3})+$)", ".",sum(somjaren, na.rm = TRUE), perl=T)),
          subtitle = paste0("Free cashflow van ", min(input$Yearrevline), " tot ", max(input$Yearrevline)), 
          icon = icon("dollar-sign")
        )
      }
    })
    
    output$grprbox <- renderValueBox({
      if (sortofgraph() == TRUE) {
      valueBox(
        paste0(gsub("(?!^)(?=(?:\\d{3})+$)", ".",sum(Gross_profit$`Automotive gross profit GAAP`[Gross_profit$Year == input$Yearrev], na.rm = TRUE), perl=T)), 
        subtitle = paste0("Bruto winst ", input$Yearrev),  
        icon = icon("dollar-sign")
      )
      }
      else {
        somjaren <- c(sum(Gross_profit$`Automotive gross profit GAAP`[Gross_profit$Year == min(input$Yearrevline)], na.rm = TRUE):
                        sum(Gross_profit$`Automotive gross profit GAAP`[Gross_profit$Year == max(input$Yearrevline)], na.rm = TRUE))
        valueBox(
          paste0(gsub("(?!^)(?=(?:\\d{3})+$)", ".",sum(somjaren, na.rm = TRUE), perl=T)),
          subtitle = paste0("Bruto winst van ", min(input$Yearrevline), " tot ", max(input$Yearrevline)),  
          icon = icon("dollar-sign")
        )
      }
    })
    
    output$colrev <- renderPlot({
      if (sortofgraph() == TRUE) {

        # generate bins based on input$bins from ui.R
        x    <- Revenue$Year
        Yearrev <- seq(min(x), max(x), length.out = input$Yearrev)
        

        # draw the histogram with the specified number of bins
        revenue(input$Yearrev, Revenue) %>% ggplot(aes(x = Quarter, y = `1000_revenue`, fill= Quarter))+ geom_col(position="dodge") + 
          labs(y = 'Automotive revenue')  +
          scale_y_continuous(limits = c(0, 8000), breaks = seq(0,8000, by= 1000))
      }
      else {
        y    <- Revenue$Year
        Yearrevline <- seq(min(y), max(y)) 
        
        Revenue %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% 
          mutate("totaal" = sum(`1000_revenue`, na.rm = TRUE)) %>% select(Year, totaal)%>% distinct() %>% ggplot(aes(x = Year , y = totaal))+ geom_line() + 
          labs(y = 'Automotive revenue')  +
          scale_y_continuous(limits = c(0, 25000), breaks = seq(0,25000, by= 5000)) + 
          scale_x_continuous(breaks = seq(min(input$Yearrevline), max(input$Yearrevline), by = 1))
      }
    })
    
    output$colfrcash <- renderPlot({
      if (sortofgraph() == TRUE) {
      revenue(input$Yearrev, Free_cashflow) %>% 
        ggplot(aes(x= Quarter, y= `free cash flow`/1000, fill = Quarter)) + 
        geom_col(position="dodge") + 
        labs(y = 'Free cash flow') + 
        scale_y_continuous(limits = c(-1500, 1500), breaks = seq(-1500,1500, by = 500))
      }
      else {
        Free_cashflow %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% 
          mutate("totaal" = sum(`free cash flow`, na.rm = TRUE)/1000) %>% select(Year, totaal)%>% distinct() %>% ggplot(aes(x = Year , y = totaal))+ 
          geom_line() + 
          labs(y = 'Free cash flow')  + scale_y_continuous(limits = c(-4000, 1300), breaks = seq(-4000, 1300, by= 1000)) 
      }
    })
    
    output$colgrpr <- renderPlot({
      if (sortofgraph() == TRUE) {
      revenue(input$Yearrev, Gross_profit) %>% 
        ggplot(aes(x = Quarter, y = `Automotive gross profit GAAP`/1000, fill= Quarter)) + 
        geom_col(position="dodge") + 
        labs(y= 'Gross profit') + 
        scale_y_continuous(limits = c(0,2500), breaks = seq(0,2500, by= 500))
      }
      else {
        Gross_profit %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% 
          mutate("totaal" = sum(`Automotive gross profit GAAP`, na.rm = TRUE)/1000) %>% select(Year, totaal)%>% distinct() %>% ggplot(aes(x = Year , y = totaal))+ 
          geom_line() + 
          labs(y = "Gross profit")  + scale_y_continuous(limits = c(0, 5000), breaks = seq(0, 5000, by= 1000)) 
      }
    })
    
    output$colgrmar <- renderPlot({
      if (sortofgraph() == TRUE) {
      revenue(input$Yearrev, Gross_Margin) %>% 
        ggplot(aes(x = Quarter, y = `Gross margin Automotive GAAP`, fill= Quarter)) + 
        geom_col(position="dodge") + 
        labs(y= 'Gross margin') + 
        scale_y_continuous(limits = c(0,30), breaks = seq(0, 30, by= 5))
      }
      else {
        Gross_Margin %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% 
          mutate("totaal" = sum(`Gross margin Automotive GAAP`, na.rm = TRUE)) %>% select(Year, totaal)%>% distinct() %>% ggplot(aes(x = Year , y = totaal))+ 
          geom_line() + 
          labs(y = "Gross margin")  + scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by= 10)) 
      }
        })
    
})
