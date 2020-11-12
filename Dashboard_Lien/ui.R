#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

Revenuetabel <- rename(Revenuetabel, c("Revenue" = "Automotive Revenues Tesla"))
Gross_profit <- Gross_profit %>% rename(c("Gross Profit"= "Automotive gross profit GAAP"))
Gross_Margin <- Gross_Margin %>% rename( c("Gross Margin" = "Gross margin Automotive GAAP"))

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


# #uitbreiding in europa tabblad
# countriesafpassengercars <- read_xlsx("Data/Countries overview of af passenger cars.xlsx", skip = 2 , col_types = c("numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
# teslajaareu <- tesla.eu.map %>% filter(jaar != "NA") %>% select(jaar)

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(title = 'Tesla', skin = 'red',
    dashboardHeader(title = "Menu"),
    dashboardSidebar(
      sidebarMenu(
        sidebarSearchForm("searchText", "buttonSearch", "Search"),
        menuItem("Finance", tabName = "Omzet"),
        menuItem("Expansion in Europe", tabName = "EU")
    )
    ),  
    dashboardBody(
      tabItems(
        #finance
        tabItem(tabName = "Omzet",
                h2("Financial numbers worldwide, based on automotive sector"),
                fluidRow(
                  valueBoxOutput("revbox"),
                  valueBoxOutput("frcashbox"),
                  valueBoxOutput("grprbox")
                ),    
                #Grafieken voor financiele cijfers
                fluidRow(
                  box(title = "Yearly",
                      "In million",
                      solidHeader = T, status="danger", plotlyOutput("linefin"),sliderInput(inputId = "Yearrevline", 
                                                                                            label = "Choose the range of years to appear",
                                                                                            min = min(Revenue$Year),
                                                                                            max = max(Revenue$Year),
                                                                                            value = c(min(Revenue$Year),max(Revenue$Year)))
                  ),
                  box(title = "Quarterly", 
                      "In million", solidHeader = T, status="danger", plotlyOutput("colfin"), sliderInput(inputId = "Yearrev", 
                                                                                                          label = "Choose year to give the quarters of",
                                                                                                          min = min(Revenue$Year),
                                                                                                          max = max(Revenue$Year),
                                                                                                          value = 2020))
                )
                )
                
      #   ),
      # tabItem(tabName = "EU",
      #         fluidRow(
      #           box(title = "AF passenger cars",
      #               "Total fleet of passenger cars per alternative fuel (AF)", solidHeader = T, status="danger", plotOutput("colpascar"),
      #               checkboxGroupInput("EUcheck", "Choose the fuels for Europe or per country", c('BEV', 'CNG', 'H2', 'LNG', 'LPG', 'PHEV', 'Total'), 
      #                                  selected = c('BEV', 'CNG', 'H2', 'LNG', 'LPG', 'PHEV'))),
      #           box(title = "AF infrastructure",
      #               "Total number of alternative fuel (AF) infrastructure per type of fuel",  solidHeader = T, status="danger",plotOutput("colinfr"),
      #               checkboxGroupInput("EUcheckinfr", "Choose the fuels for Europe or per country", c('Electricity', 'H2', 'Natural Gas', 'LPG', 'Total'), 
      #                                  selected = c('Electricity', 'H2', 'Natural Gas', 'LPG')))
      #           ),
      #         fluidRow(
      #         box(title = "Choose your options", solidHeader = T, status="danger", 
      #             radioButtons("Europe", label= "Choose what you want to see on the graph", choices = list("Europe" = 1, "Per chosen country" = 2)),
      #             sliderInput(inputId = "YearEU", 
      #                         label = "Choose year for the values in Europe",
      #                         min = min(countriesafpassengercars$Year),
      #                         max = max(countriesafpassengercars$Year),
      #                         value = 2020),
      #             
      #             selectInput("EUoptions", "Choose a country of Europe", 
      #                         c('Ausria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia',
      #                           'Finland', 'France', 'Germany', 'Greece', 'Hungria', 'Ireland', 'Italy', 'Latvia', 'Lithuania',
      #                           'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia',
      #                           'Spain', 'Sweden'), selected = "Belgium")
      #             ),
      #         
      #         box(title = "Tesla sales in Europe per year", solidHeader = T, status="danger", 
      #             selectInput(inputId = "teslajaar",
      #                         label = "choose the year you want to see (blue is new that year)",
      #                         choices = teslajaareu),
      #             plotOutput("distPlot")))
      # 
      #         
      # )
      
      
        
      ))))

