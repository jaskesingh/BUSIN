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
library(maps)
library(gghighlight)
library(basicTrendline)


#finance

Revenue <- read_xlsx("data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Revenues (automotive)", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_Margin <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross margin", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_profit <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross profit", col_types = c("numeric", "text", "numeric", "numeric", "numeric"))
Free_cashflow <- read_xlsx("Data/Tesla's free cash flow by quarter 2020 world wide.xlsx", skip = 3 , sheet = "Data", col_types = c("numeric", "text", "numeric"))

##cleaning
Revenuetabel <- Revenue %>% group_by(Year) %>% 
  mutate("Revenue " = sum(Revenue, na.rm = TRUE)/1000000)
Free_cashflow <- Free_cashflow %>% group_by(Year) %>% 
  mutate("Free cash flow " = sum(`Free cash flow`, na.rm = TRUE)/1000000)
Gross_profit <- Gross_profit %>% group_by(Year) %>% 
  mutate("Gross Profit " = sum(`Gross Profit`, na.rm = TRUE)/1000000)
Gross_Margin <- Gross_Margin %>% group_by(Year) %>% 
  mutate("Gross Margin " = sum(`Gross Margin`, na.rm = TRUE))

Revenuetabel <- Revenuetabel %>% unite(Year, Quarter, col = "Date", sep = " ") 
Gross_profit <- Gross_profit %>% unite(Year, Quarter, col = "Date", sep = " ") 
Free_cashflow <- Free_cashflow %>% unite(Year, Quarter, col = "Date", sep = " ") 
Gross_Margin <- Gross_Margin %>% unite(Year, Quarter, col = "Date", sep = " ") 


Revenuetabelnorm <- Revenuetabel %>% select(Date, Revenue)
Gross_profitnorm <- Gross_profit %>% select(Date, `Gross Profit`)
Gross_Marginnorm <- Gross_Margin %>% select(Date, `Gross Margin`) 
Free_cashflownorm <- Free_cashflow %>% select(Date, `Free cash flow`)

Revenuetabelsom <- Revenuetabel %>% select(Date, `Revenue `)
Gross_profitsom <- Gross_profit %>% select(Date, `Gross Profit `)
Gross_Marginsom <- Gross_Margin %>% select(Date, `Gross Margin `)
Free_cashflowsom <- Free_cashflow %>% select(Date, `Free cash flow `)

Financial_numbersnorm <- left_join(Revenuetabelnorm, Gross_profitnorm, by = "Date")
Financial_numbersnorm <- left_join(Financial_numbersnorm, Gross_Marginnorm, by = "Date")
Financial_numbersnorm <- left_join(Financial_numbersnorm, Free_cashflownorm, by = "Date")

Financial_numbersnorm <- Financial_numbersnorm %>% separate(Date, sep = " ", into = c("Year", "Quarter"))
Financial_numbersnorm$'Year' <- as.numeric(Financial_numbersnorm$'Year')
Financial_numbersnorm$'Revenue' <- as.numeric(Financial_numbersnorm$'Revenue')
Financial_numbersnorm$'Gross Profit' <- as.numeric(Financial_numbersnorm$'Gross Profit')
Financial_numbersnorm$'Gross Margin' <- as.numeric(Financial_numbersnorm$'Gross Margin')
Financial_numbersnorm$'Free cash flow' <- as.numeric(Financial_numbersnorm$'Free cash flow')

Financial_numbers_gather_norm <- Financial_numbersnorm %>% gather('Revenue', 'Gross Profit', 'Gross Margin', 'Free cash flow', key = 'Type', value = 'finvalue')

Financial_numberssom <- left_join(Revenuetabelsom, Gross_profitsom, by = "Date")
Financial_numberssom <- left_join(Financial_numberssom, Gross_Marginsom, by = "Date")
Financial_numberssom <- left_join(Financial_numberssom, Free_cashflowsom, by = "Date")

Financial_numberssom <- Financial_numberssom %>% separate(Date, sep = " ", into = c("Year", "Quarter"))
Financial_numberssom$'Year' <- as.numeric(Financial_numberssom$'Year')
Financial_numberssom$'Revenue ' <- as.numeric(Financial_numberssom$'Revenue ')
Financial_numberssom$'Gross Profit ' <- as.numeric(Financial_numberssom$'Gross Profit ')
Financial_numberssom$'Gross Margin ' <- as.numeric(Financial_numberssom$'Gross Margin ')
Financial_numberssom$'Free cash flow ' <- as.numeric(Financial_numberssom$'Free cash flow ')

Financial_numbers_gather_som <- Financial_numberssom %>% gather('Revenue ', 'Gross Profit ', 'Gross Margin ', 'Free cash flow ', key = 'Type', value = 'Total') %>% select(Year, Type, Total) %>% distinct()

##financiele cijfers, functies
financefunction <- function(yearinput,df) {
  financefunction <- df %>% filter(df$Year == yearinput)
  return(financefunction)
}

# #uitbreiding europa, data in juiste vorm krijgen
# countriesafpassengercars <- countriesafpassengercars %>% gather('BEV', 'H2', 'CNG', 'LNG', 'PHEV', 'LPG', 'Total', key = 'Fuel', value = 'waardes')
# countriesafpassengercars$Country[1:2457] <- c('Ausria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia',
#                                         'Finland', 'France', 'Germany', 'Greece', 'Hungria', 'Ireland', 'Italy', 'Latvia', 'Lithuania',
#                                         'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia',
#                                         'Spain', 'Sweden')
# countriesafinfrastructure <- countriesafinfrastructure %>% gather('Electricity', 'H2', 'Natural Gas', 'LPG', 'Total', key = 'Fuel', value = 'waardes')
# countriesafinfrastructure$Country[1:1755] <- c('Ausria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia',
#                                               'Finland', 'France', 'Germany', 'Greece', 'Hungria', 'Ireland', 'Italy', 'Latvia', 'Lithuania',
#                                               'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia',
#                                               'Spain', 'Sweden')
# 
# #wereldkaart
# 
# teslapercountrysales <- read_xlsx("Data/Verkoop landen tesla.xlsx", skip = 1, col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>% gather('2013', '2014', '2015', '2016', '2017', '2018', '2019', key = 'jaar', value = 'waarde')
# 
# some.eu.countries <- c('Ukraine', 'France', 'Spain', 'Sweden', 'Norway', 'Germany', 'Finland', 'Poland', 'Italy', 'UK', 'Romania', 'Belarus', 'Greece', 'Bulgaria', 'Iceland', 'Hungary', 'Portugal', 'Austria', 'Czech Republic', 'Serbia', 'Ireland', 'Lithuania', 'Latvia', 'Croatia', 'Bosnia and Herzegovina', 'Slovakia', 'Estonia', 'Denmark', 'Netherlands', 'Switzerland', 'Moldova', 'Belgium', 'Armenia', 'Albania', 'Macedonia', 'Turkey', 'Slovenia', 'Montenegro', 'Kosovo', 'Cyprus', 'Luxembourg', 'Georgia', 'Andorra', 'Malta', 'Liechtenstein', 'San Marino', 'Monaco', 'Vatican')
# 
# some.eu.map <- map_data("world", region = some.eu.countries)
# tesla.eu.map <- left_join(some.eu.map, teslapercountrysales, by = "region")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #financieel tabblad
  
  #financieel tabblad
  output$revbox <- renderValueBox({
    somjaar <- Financial_numbers_gather_som %>% filter(Year == input$Yearrev, Type == 'Revenue ') %>% group_by(Year) 
    
    valueBox(
      paste0(format(round(somjaar$Total[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
      subtitle= paste0("Revenue ", input$Yearrev, " in million"), 
      icon = icon("dollar-sign"), color = 'red'
    )
    
  })
  
  output$frcashbox <- renderValueBox({
    somjaar <- Financial_numbers_gather_som %>% filter(Year == input$Yearrev, Type == 'Free cash flow ') %>% group_by(Year)
    if(somjaar$Total[]>=0) {  
      valueBox(
        paste0(format(round(somjaar$Total[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle = paste0("Free cashflow ", input$Yearrev, " in million"), 
        icon = icon("dollar-sign"), color = 'red'
      )
    }
    else {
      valueBox(
        paste0(format(round(somjaar$Total[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle = paste0("Free Cashflow ", input$Yearrev, " in million"), 
        icon = icon("dollar-sign"), color = 'purple'
      )
    }
  })
  
  output$grprbox <- renderValueBox({
    somjaar <- Financial_numbers_gather_som %>% filter(Year == input$Yearrev, Type == 'Gross Profit ') %>% group_by(Year)
    valueBox(
      paste0(format(round(somjaar$Total[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)), 
      subtitle = paste0("Gross profit ", input$Yearrev, " in million"),  
      icon = icon("piggy-bank"), color = 'red'
    )
  })
  
  
  output$colfin <- renderPlotly({
    
    # generate bins based on input$bins from ui.R
    x    <- Financial_numbers_gather_norm$Year
    Yearrev <- seq(min(x), max(x), length.out = input$Yearrev)
    
    financevar <- financefunction(input$Yearrev, Financial_numbers_gather_norm) %>% filter(Type != "Gross Margin")
    Value <- financevar$finvalue/1000000
    
    financevarpcol <- financevar %>% ggplot(aes(x = Quarter, y = Value, fill = Type))+ geom_col(position = "dodge") + 
      labs(title = input$Yearrev, y = 'Value') + scale_fill_manual(values = c("green", "lightseagreen", "blue")) +  
      theme_minimal() + geom_hline(yintercept = 0, color = "black", size = 1.5)
    
    ggplotly(financevarpcol)
    
    
  })
  output$linefin <- renderPlotly({
    
    
    y    <- Financial_numbers_gather_som$Year
    Yearrevline <- seq(min(y), max(y))
    financevar <- Financial_numbers_gather_som %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline), Type != "Gross Margin ") %>% group_by(Year, Type) %>% 
      select(Year, Total, Type)%>% distinct()
    
    financevarpline <- financevar %>% ggplot(aes(x = Year , y = Total, color = Type))+ geom_line() +
      labs(y = 'Value') + scale_x_continuous(breaks = seq(min(Yearrevline), max(Yearrevline), by = 1)) +
      theme_minimal() + scale_color_manual(values = c("green","lightseagreen", "blue")) + geom_hline(yintercept = 0, color = "black", size = 1.5)
    ggplotly(financevarpline)
  })
  
  output$grossmargin <- renderPlotly({
    y <- Financial_numbers_gather_som$Year
    Yeargrossmargin <- seq(min(y), max(y))
    financevarmar <- Financial_numbers_gather_som %>% filter(Year >= min(input$Yeargrossmargin) & Year <= max(input$Yeargrossmargin), Type == "Gross Margin ") %>% group_by(Year, Type) %>% 
       select(Year, Total, Type) %>% distinct()
    
    financevarmarp <- financevarmar %>% ggplot(aes(x = Year, y = Total, color = Type)) + geom_line() + 
      labs(y = 'Value') + scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, by= 5)) + scale_x_continuous(breaks = seq(min(Yeargrossmargin), max(Yeargrossmargin), by = 1)) +
      theme_minimal() + scale_color_manual(values = c("purple", "red")) + stat_smooth(method = 'lm', se = FALSE, aes(color = 'Trend'))
    
    ggplotly(financevarmarp)
     
    
    
    
  })
  
    #   #Uitbreiding naar de EU
    # checkeurope <- reactive({input$Europe})
    # output$colpascar <- renderPlot({
    #   if (checkeurope() == 2) {
    #     countriespasscarvar <- countriesafpassengercars %>% filter(Country == input$EUoptions, Fuel %in% input$EUcheck)
    #     
    #    countriespasscarvar %>% 
    #     ggplot(aes(x = Year, y = waardes, fill = Fuel)) + 
    #     geom_col(position = "dodge") + 
    #     labs(title = input$EUoptions, y = '')  + 
    #     scale_y_continuous(limits = c(0, 3600000), breaks = seq(0,4000000, by= 500000)) +
    #     scale_x_continuous(limits= c(min(countriesafpassengercars$Year), max(countriesafpassengercars$Year)) , breaks = seq(min(countriesafpassengercars$Year), max(countriesafpassengercars$Year), by = 1))
    #    
    #   }
    #   else {
    #     countriespasscarvar <- countriesafpassengercars %>% filter(Fuel %in% input$EUcheck, Year == input$YearEU)
    #     
    #      countriespasscarvar %>% 
    #       ggplot(aes(x = Country, y = waardes, fill = Fuel ))+ 
    #       geom_col(position = "dodge") + 
    #       labs(title = input$YearEU, y = '')  + 
    #       scale_y_continuous(limits = c(0, 3600000), breaks = seq(0,4000000, by= 500000)) + 
    #       coord_flip()
    #      
    #     }
    # })
    # output$colinfr <- renderPlot({
    #   if (checkeurope() == 2) {
    #     countriesinfrvar <- countriesafinfrastructure %>% filter(Country == input$EUoptions, Fuel %in% input$EUcheckinfr)
    #     
    #     countriesinfrvar %>% 
    #       ggplot(aes(x = Year, y = waardes, fill = Fuel))+ 
    #       geom_col(position = "dodge") + 
    #       labs(title = input$EUoptions, y = '')  + 
    #       scale_y_continuous(limits = c(0,65000), breaks = seq(0,65000, by= 5000)) +
    #       scale_x_continuous(limits = c(min(countriesafinfrastructure$Year), max(countriesafinfrastructure$Year)), breaks = seq(min(countriesafinfrastructure$Year), max(countriesafinfrastructure$Year), by = 1))
    #     
    #   }
    #   else {
    #     countriesinfrvar <- countriesafinfrastructure %>% filter(Fuel %in% input$EUcheckinfr, Year == input$YearEU)
    #     
    #      countriesinfrvar %>% 
    #       ggplot(aes(x = Country, y = waardes, fill = Fuel))+ 
    #       geom_col(position = "dodge") + 
    #       labs(title = input$YearEU, y = '')  + 
    #       scale_y_continuous(limits = c(0, 65000), breaks = seq(0,65000, by= 5000)) +
    #       coord_flip()
    #      
    #   }
    # })
    # output$distPlot <- renderPlot({
    #   teslamap <- function(inputjaar, df) {
    #     if (inputjaar == "2013") {
    #       teslamap <- df %>% filter(df$jaar == inputjaar, df$waarde > 0)
    #     }
    #     else {
    #       teslamap <- df %>% filter(df$jaar == c(inputjaar, (as.numeric(inputjaar) - 1)), df$waarde > 0)  
    #     }
    #     return(teslamap)
    #   }
    #   
    #   teslamapvar <- teslamap(input$teslajaar, tesla.eu.map)
    #     gg <- teslamapvar %>% ggplot() + geom_map(dat = tesla.eu.map, map = tesla.eu.map, aes(map_id = region), fill = "white", color="black")
    #   if (input$teslajaar == "2013") {
    #   gg <- gg + geom_map(map = tesla.eu.map, aes(map_id = region, fill = jaar), colour = "black")
    #   }
    #   else {
    #     gg <- gg + geom_map(map = tesla.eu.map, aes(map_id = region, fill = jaar), colour = "black")
    #   }
    #   gg <- gg + expand_limits(x = tesla.eu.map$long, y = tesla.eu.map$lat)
    #   gg <- gg + theme(axis.title = element_blank(),
    #                     axis.text = element_blank(),
    #                     axis.ticks = element_blank(),
    #                     panel.grid.major = element_blank(),
    #                     panel.grid.minor = element_blank(),
    #                     panel.background = element_blank(),
    #                     legend.position = "none",
    #                     panel.border = element_blank(),
    #   strip.background = element_rect(fill = 'white', colour = 'white')) + scale_fill_manual( values = c("tomato", "skyblue"))
    #   gg
    #   
    # })
    
})
