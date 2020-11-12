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



#finance

Revenue <- read_xlsx("data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Revenues (automotive)", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_Margin <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross margin", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_profit <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross profit", col_types = c("numeric", "text", "numeric", "numeric", "numeric"))
Free_cashflow <- read_xlsx("Data/Tesla's free cash flow by quarter 2020 world wide.xlsx", skip = 3 , sheet = "Data", col_types = c("numeric", "text", "numeric"))

##cleaning
Revenuetabel <- Revenue %>% group_by(Year) %>% 
  mutate("totalrevenue" = sum(`Automotive Revenues Tesla`, na.rm = TRUE)/1000000)
Free_cashflow <- Free_cashflow %>% group_by(Year) %>% 
  mutate("totalfreecashflow" = sum(`free cash flow`, na.rm = TRUE)/1000000)
Gross_profit <- Gross_profit %>% group_by(Year) %>% 
  mutate("totalgrossprofit" = sum(`Automotive gross profit GAAP`, na.rm = TRUE)/1000000)
Gross_Margin <- Gross_Margin %>% group_by(Year) %>% 
  mutate("totalgrossmargin" = sum(`Gross margin Automotive GAAP`, na.rm = TRUE))

Revenuetabel <- Revenuetabel %>% unite(Year, Quarter, col = "Date", sep = " ") 
Gross_profit <- Gross_profit %>% unite(Year, Quarter, col = "Date", sep = " ") 
Free_cashflow <- Free_cashflow %>% unite(Year, Quarter, col = "Date", sep = " ") 
Gross_Margin <- Gross_Margin %>% unite(Year, Quarter, col = "Date", sep = " ") 

Revenuetabel <- rename(Revenuetabel, c("Automotive Revenues Tesla" = "Revenue"))
Gross_profit <- Gross_profit %>% rename(c("Automotive gross profit GAAP" = "Gross Profit"))
Gross_Margin <- Gross_Margin %>% rename( c("Gross margin Automotive GAAP" = "Gross Margin"))

Revenuetabelnorm <- Revenuetabel %>% select(Date, Revenue)
Gross_profitnorm <- Gross_profit %>% select(Date, `Gross Profit`)
Gross_Marginnorm <- Gross_Margin %>% select(Date, `Gross Margin`) 
Free_cashflownorm <- Free_cashflow %>% select(Date, `free cash flow`)

Revenuetabelsom <- Revenuetabel %>% select(Date, totalrevenue)
Gross_profitsom <- Gross_profit %>% select(Date, totalgrossprofit)
Gross_Marginsom <- Gross_Margin %>% select(Date, totalgrossmargin)
Free_cashflowsom <- Free_cashflow %>% select(Date, totalfreecashflow)

Financial_numbersnorm <- left_join(Revenuetabelnorm, Gross_profitnorm, by = "Date")
Financial_numbersnorm <- left_join(Financial_numbersnorm, Gross_Marginnorm, by = "Date")
Financial_numbersnorm <- left_join(Financial_numbersnorm, Free_cashflownorm, by = "Date")

Financial_numbersnorm <- Financial_numbersnorm %>% separate(Date, sep = " ", into = c("Year", "Quarter"))
Financial_numbersnorm$'Year' <- as.numeric(Financial_numbersnorm$'Year')
Financial_numbersnorm$'Revenue' <- as.numeric(Financial_numbersnorm$'Revenue')
Financial_numbersnorm$'Gross Profit' <- as.numeric(Financial_numbersnorm$'Gross Profit')
Financial_numbersnorm$'Gross Margin' <- as.numeric(Financial_numbersnorm$'Gross Margin')
Financial_numbersnorm$'free cash flow' <- as.numeric(Financial_numbersnorm$'free cash flow')

Financial_numbers_gather_norm <- Financial_numbersnorm %>% gather('Revenue', 'Gross Profit', 'Gross Margin', 'free cash flow', key = 'typenumber', value = 'finvalue')

Financial_numberssom <- left_join(Revenuetabelsom, Gross_profitsom, by = "Date")
Financial_numberssom <- left_join(Financial_numberssom, Gross_Marginsom, by = "Date")
Financial_numberssom <- left_join(Financial_numberssom, Free_cashflowsom, by = "Date")

Financial_numberssom <- Financial_numberssom %>% separate(Date, sep = " ", into = c("Year", "Quarter"))
Financial_numberssom$'Year' <- as.numeric(Financial_numberssom$'Year')
Financial_numberssom$'totalrevenue' <- as.numeric(Financial_numberssom$'totalrevenue')
Financial_numberssom$'totalgrossprofit' <- as.numeric(Financial_numberssom$'totalgrossprofit')
Financial_numberssom$'totalgrossmargin' <- as.numeric(Financial_numberssom$'totalgrossmargin')
Financial_numberssom$'totalfreecashflow' <- as.numeric(Financial_numberssom$'totalfreecashflow')

Financial_numbers_gather_som <- Financial_numberssom %>% gather('totalrevenue', 'totalgrossprofit', 'totalgrossmargin', 'totalfreecashflow', key = 'typenumber', value = 'finvalue') %>% select(Year, typenumber, finvalue) %>% distinct()


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
  
  output$revbox <- renderValueBox({
      somjaar <- Financial_numbers_gather_som %>% filter(Year == input$Yearrev, typenumber == 'totalrevenue') %>% group_by(Year) 
      
      valueBox(
        paste0(format(round(somjaar$finvalue[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle= paste0("Revenue ", input$Yearrev, " in million"), 
        icon = icon("dollar-sign"), color = 'red'
      )
    
  })
  
  output$frcashbox <- renderValueBox({
    somjaar <- Financial_numbers_gather_som %>% filter(Year == input$Yearrev, typenumber == 'totalfreecashflow') %>% group_by(Year)
      valueBox(
        paste0(format(round(somjaar$finvalue[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle = paste0("Free cashflow ", input$Yearrev, " in million"), 
        icon = icon("dollar-sign"), color = 'red'
      )
  })
  
  output$grprbox <- renderValueBox({
      somjaar <- Financial_numbers_gather_som %>% filter(Year == input$Yearrev, typenumber == 'totalgrossprofit') %>% group_by(Year)
      valueBox(
        paste0(format(round(somjaar$finvalue[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)), 
        subtitle = paste0("Gross profit ", input$Yearrev, " in million"),  
        icon = icon("piggy-bank"), color = 'red'
      )
  })
  
  
  output$colfin <- renderPlotly({
    
    # generate bins based on input$bins from ui.R
    x    <- Financial_numbers_gather_norm$Year
    Yearrev <- seq(min(x), max(x), length.out = input$Yearrev)
    
    financevar <- financefunction(input$Yearrev, Financial_numbers_gather_norm) %>% filter(typenumber != "Gross Margin")
    value <- financevar$finvalue/1000000
    
    financevarpcol <- financevar %>% ggplot(aes(x = Quarter, y = value, fill = typenumber))+ geom_col(position = "dodge") + 
      labs(title = input$Yearrev, y = 'Value') + scale_fill_manual(values = c("blue2", "royalblue1", "skyblue3")) +  
      scale_y_continuous(limits = c(-1500, 8000), breaks = seq(-1500,8000, by= 500)) +  
      theme_minimal() + geom_hline(yintercept = 0, color = "black", size = 1.5)
    
    ggplotly(financevarpcol)
    
    
  })
  output$linefin <- renderPlotly({
    
    y    <- Financial_numbers_gather_som$Year
    Yearrevline <- seq(min(y), max(y))
    financevar <- Financial_numbers_gather_som %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline), typenumber != "totalgrossmargin") %>% group_by(Year, typenumber) %>% 
      mutate("total" = sum(finvalue, na.rm = TRUE)) %>% select(Year, total, typenumber)%>% distinct()
    
    financevarpline <- financevar %>% ggplot(aes(x = Year , y = total, color = typenumber))+ geom_line() + geom_point() +
      labs(y = 'Value') + 
      theme_minimal() + scale_color_manual(values = c("blue2", "royalblue1", "skyblue3")) + geom_hline(yintercept = 0, color = "black", size = 1.5)
    ggplotly(financevarpline)
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
