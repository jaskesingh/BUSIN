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

