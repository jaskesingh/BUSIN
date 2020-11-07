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
library(maps)
library(RColorBrewer)


#financiele cijfers
Revenue <- read_xlsx("data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Revenues (automotive)", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_Margin <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross margin", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_profit <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross profit", col_types = c("numeric", "text", "numeric", "numeric", "numeric"))
Free_cashflow <- read_xlsx("Data/Tesla's free cash flow by quarter 2020 world wide.xlsx", skip = 3 , sheet = "Data", col_types = c("numeric", "text", "numeric"))
#uitbreiding europa
countriesafpassengercars <- read_xlsx("Data/Countries overview of af passenger cars.xlsx", skip = 2 , col_types = c("numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
countriesafinfrastructure <- read_xlsx("Data/countries overview of af infrastructure.xlsx", skip = 2 , col_types = c("numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))
#financiele cijfers, functies
revenue <- function(yearinput,df) {
  revenue <- df %>% filter(df$Year == yearinput)
  return(revenue)
}
somvoorbox <- function(df, jaar, inputjaar, colnaam) {
  somjaren <- df %>% filter(jaar >= min(inputjaar) & jaar <= max(inputjaar)) %>% group_by(jaar) %>% summarize(totaal = sum(colnaam, na.rm = TRUE))
  somresultaat <- format(round(sum(somjaren$totaal, na.rm = TRUE)/1000000, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)
  return(somresultaat)
}

#uitbreiding europa, data in juiste vorm krijgen
countriesafpassengercars <- countriesafpassengercars %>% gather('BEV', 'H2', 'CNG', 'LNG', 'PHEV', 'LPG', 'Total', key = 'Fuel', value = 'waardes')
countriesafpassengercars$Country[1:2457] <- c('Ausria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia',
                                        'Finland', 'France', 'Germany', 'Greece', 'Hungria', 'Ireland', 'Italy', 'Latvia', 'Lithuania',
                                        'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia',
                                        'Spain', 'Sweden')
countriesafinfrastructure <- countriesafinfrastructure %>% gather('Electricity', 'H2', 'Natural Gas', 'LPG', 'Total', key = 'Fuel', value = 'waardes')
countriesafinfrastructure$Country[1:1755] <- c('Ausria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia',
                                              'Finland', 'France', 'Germany', 'Greece', 'Hungria', 'Ireland', 'Italy', 'Latvia', 'Lithuania',
                                              'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia',
                                              'Spain', 'Sweden')

#wereldkaart

teslapercountrysales <- read_xlsx("Data/Verkoop landen tesla.xlsx", skip = 1, col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>% gather('2013', '2014', '2015', '2016', '2017', '2018', '2019', key = 'jaar', value = 'waarde')

some.eu.countries <- c('Ukraine', 'France', 'Spain', 'Sweden', 'Norway', 'Germany', 'Finland', 'Poland', 'Italy', 'UK', 'Romania', 'Belarus', 'Greece', 'Bulgaria', 'Iceland', 'Hungary', 'Portugal', 'Austria', 'Czech Republic', 'Serbia', 'Ireland', 'Lithuania', 'Latvia', 'Croatia', 'Bosnia and Herzegovina', 'Slovakia', 'Estonia', 'Denmark', 'Netherlands', 'Switzerland', 'Moldova', 'Belgium', 'Armenia', 'Albania', 'Macedonia', 'Turkey', 'Slovenia', 'Montenegro', 'Kosovo', 'Cyprus', 'Luxembourg', 'Georgia', 'Andorra', 'Malta', 'Liechtenstein', 'San Marino', 'Monaco', 'Vatican')

some.eu.map <- map_data("world", region = some.eu.countries)
tesla.eu.map <- left_join(some.eu.map, teslapercountrysales, by = "region")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #financieel tabblad
    sortofgraph <- reactive({input$Quarterly})
    
    output$revbox <- renderValueBox({
      if (sortofgraph() == TRUE) {
      valueBox(
        paste0(format(round(sum(Revenue$`Automotive Revenues Tesla`[Revenue$Year == input$Yearrev], na.rm = TRUE)/1000000, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle= paste0("Revenue ", input$Yearrev, " in million"), 
        icon = icon("dollar-sign"), color = 'red'
      )
      }
      else {
        
        somjaren <- Revenue %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% summarize(totaal = sum(`Automotive Revenues Tesla`, na.rm = TRUE))
        valueBox(
          paste0(format(round(sum(somjaren$totaal, na.rm = TRUE)/1000000, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
          subtitle= paste0("Revenue from ", min(input$Yearrevline), " until ", max(input$Yearrevline), " in million"), 
          icon = icon("dollar-sign"), color = 'red'
        )
      }
    })
    
    output$frcashbox <- renderValueBox({
      if (sortofgraph() == TRUE) {
      valueBox(
        paste0(format(round(sum(Free_cashflow$`free cash flow`[Free_cashflow$Year == input$Yearrev], na.rm = TRUE)/1000000, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle = paste0("Free cashflow ", input$Yearrev, " in million"), 
        icon = icon("dollar-sign"), color = 'red'
      )
      }
      else {
        somjaren <- Free_cashflow %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% summarize(totaal = sum(`free cash flow`, na.rm = TRUE))

        valueBox(
          paste0(format(round(sum(somjaren$totaal, na.rm = TRUE)/1000000, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
          subtitle = paste0("Free cashflow from ", min(input$Yearrevline), " until ", max(input$Yearrevline), " in million"), 
          icon = icon("dollar-sign"), color = 'red'
        )
      }
    })
    
    output$grprbox <- renderValueBox({
      if (sortofgraph() == TRUE) {
      valueBox(
        paste0(format(round(sum(Gross_profit$`Automotive gross profit GAAP`[Gross_profit$Year == input$Yearrev], na.rm = TRUE)/1000000, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)), 
        subtitle = paste0("Gross profit ", input$Yearrev, " in million"),  
        icon = icon("piggy-bank"), color = 'red'
      )
      }
      else {
        somjaren <- Gross_profit %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% summarize(totaal = sum(`Automotive gross profit GAAP`, na.rm = TRUE))
        valueBox(
          paste0(format(round(sum(somjaren$totaal, na.rm = TRUE)/1000000, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
          subtitle = paste0("Gross profit from ", min(input$Yearrevline), " until ", max(input$Yearrevline), " in million"),  
          icon = icon("piggy-bank"), color = 'red'
        )
      }
    })

    
    output$colrev <- renderPlot({
      if (sortofgraph() == TRUE) {

        # generate bins based on input$bins from ui.R
        x    <- Revenue$Year
        Yearrev <- seq(min(x), max(x), length.out = input$Yearrev)

        revvar <- revenue(input$Yearrev, Revenue)
        
         revvar %>% ggplot(aes(x = Quarter, y = `Automotive Revenues Tesla`/1000000, fill= Quarter))+ geom_col(position="dodge") + 
          labs(title = input$Yearrev, y = 'Automotive revenue')  + geom_text(aes(label = `Automotive Revenues Tesla`/1000000), vjust = -0.5 ) +
          scale_y_continuous(limits = c(0, 8000), breaks = seq(0,8000, by= 1000)) 
         
        
      }
      else {
        y    <- Revenue$Year
        Yearrevline <- seq(min(y), max(y))
        revvar <- Revenue %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% 
          mutate("totaal" = sum(`Automotive Revenues Tesla`, na.rm = TRUE)/1000000) %>% select(Year, totaal)%>% distinct()
        
        revvar %>% ggplot(aes(x = Year , y = totaal))+ geom_line() + geom_point() + geom_text(aes(label = format(round(totaal, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)), vjust = -0.5 ) +
          labs(y = 'Automotive revenue') +
          scale_y_continuous(limits = c(0, 21000), breaks = seq(0, 21000, by = 5000))
        
          
        
      }
    })
    
    output$colfrcash <- renderPlot({
      if (sortofgraph() == TRUE) {
        freecashvar <- revenue(input$Yearrev, Free_cashflow)
        
       
        freecashvar %>% ggplot(aes(x= Quarter, y= `free cash flow`/1000000, fill = Quarter)) + 
        geom_col(position="dodge") + geom_text(aes(label = `free cash flow`/1000000), vjust = -0.5 ) +
        labs(title = input$Yearrev, y = 'Free cash flow') + 
        scale_y_continuous(limits = c(-1500, 1500), breaks = seq(-1500,1500, by = 500))
       
      }
      else {
        freecashvar <-  Free_cashflow %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% 
          mutate("totaal" = sum(`free cash flow`, na.rm = TRUE)/1000000) %>% select(Year, totaal)%>% distinct()
        
       freecashvar %>% ggplot(aes(x = Year , y = totaal))+ 
          geom_line() + geom_point() + geom_text(aes(label = format(round(totaal, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)), vjust = -0.5 ) +
          labs(y = 'Free cash flow')  + scale_y_continuous(limits = c(-4000, 1300), breaks = seq(-4000, 1300, by= 1000))
       
      }
    })
    
    output$colgrpr <- renderPlot({
      if (sortofgraph() == TRUE) {
        grossprofitvar <- revenue(input$Yearrev, Gross_profit) 
        
        grossprofitvar %>% 
          ggplot(aes(x = Quarter, y = `Automotive gross profit GAAP`/1000000, fill= Quarter)) + 
          geom_col(position="dodge") +  geom_text(aes(label = `Automotive gross profit GAAP`/1000000), vjust = -0.5 ) +
          labs(title = input$Yearrev, y= 'Gross profit') + 
          scale_y_continuous(limits = c(0,2500), breaks = seq(0,2500, by= 500))
        
      }
      else {
        grossprofitvar <- Gross_profit %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% 
          mutate("totaal" = sum(`Automotive gross profit GAAP`, na.rm = TRUE)/1000000) %>% select(Year, totaal)%>% distinct() 
        
        grossprofitvar %>% ggplot(aes(x = Year , y = totaal)) + 
          geom_line() + geom_point() + geom_text(aes(label = format(round(totaal, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)), vjust = -0.5 ) +
          labs(y = "Gross profit")  + scale_y_continuous(limits = c(0, 5000), breaks = seq(0, 5000, by= 1000)) 
        
      }
    })
    
    output$colgrmar <- renderPlot({
      if (sortofgraph() == TRUE) {
        grossmarginvar <- revenue(input$Yearrev, Gross_Margin)
        
       grossmarginvar %>% 
        ggplot(aes(x = Quarter, y = `Gross margin Automotive GAAP`, fill= Quarter)) + 
        geom_col(position="dodge") + geom_text(aes(label = `Gross margin Automotive GAAP`), vjust = -0.5 ) +
        labs(title = input$Yearrev, y= 'Gross margin') + 
        scale_y_continuous(limits = c(0,30), breaks = seq(0, 30, by= 5))
       
      }
      else {
        grossmarginvar <- Gross_Margin %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% 
          mutate("totaal" = sum(`Gross margin Automotive GAAP`, na.rm = TRUE)) %>% select(Year, totaal)%>% distinct()
        
         grossmarginvar %>% ggplot(aes(x = Year , y = totaal)) + 
          geom_line() + geom_point() + geom_text(aes(label = format(round(totaal, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)), vjust = -0.5 ) +
          labs(y = "Gross margin")  + scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by= 10))
        
      }
        })
    
      #Uitbreiding naar de EU
    checkeurope <- reactive({input$Europe})
    output$colpascar <- renderPlot({
      if (checkeurope() == 2) {
        countriespasscarvar <- countriesafpassengercars %>% filter(Country == input$EUoptions, Fuel %in% input$EUcheck)
        
       countriespasscarvar %>% 
        ggplot(aes(x = Year, y = waardes, fill = Fuel)) + 
        geom_col(position = "dodge") + 
        labs(title = input$EUoptions, y = '')  + 
        scale_y_continuous(limits = c(0, 3600000), breaks = seq(0,4000000, by= 500000)) +
        scale_x_continuous(limits= c(min(countriesafpassengercars$Year), max(countriesafpassengercars$Year)) , breaks = seq(min(countriesafpassengercars$Year), max(countriesafpassengercars$Year), by = 1))
       
      }
      else {
        countriespasscarvar <- countriesafpassengercars %>% filter(Fuel %in% input$EUcheck, Year == input$YearEU)
        
         countriespasscarvar %>% 
          ggplot(aes(x = Country, y = waardes, fill = Fuel ))+ 
          geom_col(position = "dodge") + 
          labs(title = input$YearEU, y = '')  + 
          scale_y_continuous(limits = c(0, 3600000), breaks = seq(0,4000000, by= 500000)) + 
          coord_flip()
         
        }
    })
    output$colinfr <- renderPlot({
      if (checkeurope() == 2) {
        countriesinfrvar <- countriesafinfrastructure %>% filter(Country == input$EUoptions, Fuel %in% input$EUcheckinfr)
        
        countriesinfrvar %>% 
          ggplot(aes(x = Year, y = waardes, fill = Fuel))+ 
          geom_col(position = "dodge") + 
          labs(title = input$EUoptions, y = '')  + 
          scale_y_continuous(limits = c(0,65000), breaks = seq(0,65000, by= 5000)) +
          scale_x_continuous(limits = c(min(countriesafinfrastructure$Year), max(countriesafinfrastructure$Year)), breaks = seq(min(countriesafinfrastructure$Year), max(countriesafinfrastructure$Year), by = 1))
        
      }
      else {
        countriesinfrvar <- countriesafinfrastructure %>% filter(Fuel %in% input$EUcheckinfr, Year == input$YearEU)
        
         countriesinfrvar %>% 
          ggplot(aes(x = Country, y = waardes, fill = Fuel))+ 
          geom_col(position = "dodge") + 
          labs(title = input$YearEU, y = '')  + 
          scale_y_continuous(limits = c(0, 65000), breaks = seq(0,65000, by= 5000)) +
          coord_flip()
         
      }
    })
    output$distPlot <- renderPlot({
      teslamap <- function(inputjaar, df) {
        if (inputjaar == "2013") {
          teslamap <- df %>% filter(df$jaar == inputjaar, df$waarde > 0)
        }
        else {
          teslamap <- df %>% filter(df$jaar == c(inputjaar, (as.numeric(inputjaar) - 1)), df$waarde > 0)  
        }
        return(teslamap)
      }
      
      teslamapvar <- teslamap(input$teslajaar, tesla.eu.map)
        gg <- teslamapvar %>% ggplot() + geom_map(dat = tesla.eu.map, map = tesla.eu.map, aes(map_id = region), fill = "white", color="black")
      if (input$teslajaar == "2013") {
      gg <- gg + geom_map(map = tesla.eu.map, aes(map_id = region, fill = jaar), colour = "black")
      }
      else {
        gg <- gg + geom_map(map = tesla.eu.map, aes(map_id = region, fill = jaar), colour = "black")
      }
      gg <- gg + expand_limits(x = tesla.eu.map$long, y = tesla.eu.map$lat)
      gg <- gg + theme(axis.title = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.background = element_blank(),
                        legend.position = "none",
                        panel.border = element_blank(),
      strip.background = element_rect(fill = 'white', colour = 'white')) + scale_fill_manual( values = c("tomato", "skyblue"))
      gg
      
    })
    
})
