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
#financieel tabblad
Revenue <- read_xlsx("data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Revenues (automotive)", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_Margin <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross margin", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_profit <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross profit", col_types = c("numeric", "text", "numeric", "numeric", "numeric"))
Free_cashflow <- read_xlsx("Data/Tesla's free cash flow by quarter 2020 world wide.xlsx", skip = 3 , sheet = "Data", col_types = c("numeric", "text", "numeric"))

#uitbreiding in europa tabblad
countriesafpassengercars <- read_xlsx("Data/Countries overview of af passenger cars.xlsx", skip = 2 , col_types = c("numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(title = 'Tesla',
    dashboardHeader(title = "Menu"),
    dashboardSidebar(
      sidebarMenu(
        sidebarSearchForm("searchText", "buttonSearch", "Search"),
        menuItem("Financial quarterly numbers", tabName = "Omzet"),
        menuItem("Expansion in Europe", tabName = "EU")
    )
    ),  
    dashboardBody(
      tabItems(
        #financiele tabblad
        tabItem(tabName = "Omzet",
                
                fluidRow(
                  valueBoxOutput("revbox"),
                  valueBoxOutput("frcashbox"),
                  valueBoxOutput("grprbox")
                ),    
            #Grafieken voor financiele cijfers
                fluidRow(
                  box(title = "Car revenue",
                       "In thousands",
                      solidHeader = T, status="primary", plotOutput("colrev")),
                  box(title = "Free cashflow", 
                      "In thousands", solidHeader = T, status = "primary", plotOutput(("colfrcash")))
                ),
                fluidRow(
                  box(title = "Gross profit", 
                      "In thousands", solidHeader = T, status = "primary", plotOutput("colgrpr")),
                  box(title = "Gross margin", 
                      "In percentage", solidHeader = T, status = "primary", plotOutput("colgrmar")),
              #aanpasbare waardes
                  box(title = "Make changes to the graphs (Quarterly)",
                      solidHeader = T, status = "primary", sliderInput(inputId = "Yearrev", 
                                                                       label = "Choose year to give the quarters of",
                                                                       min = min(Revenue$Year),
                                                                       max = max(Revenue$Year),
                                                                       value = 2020),
                      checkboxInput("Quarterly", "Quarterly overview", value = FALSE)
                      ),
                  box(title = "Make changes to the graph (Yearly)", solidHeader = T, status = "primary",
                      sliderInput(inputId = "Yearrevline", 
                                  label = "Choose the range of years to appear",
                                  min = min(Revenue$Year),
                                  max = max(Revenue$Year),
                                  value = c(min(Revenue$Year),max(Revenue$Year)))
                  )
                )
             
        ),
      tabItem(tabName = "EU",
              fluidRow(
                box(title = "AF passenger cars",
                    "Total fleet of passenger cars per alternative fuel", solidHeader = T, status = "primary", width = 12, plotOutput("colpascar"))
              ),
              fluidRow(
              box(title = "Choose your options", solidHeader = T, status = "primary", 
                  selectInput("EUoptions", "Choose a country of Europe", 
                              c('Ausria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia',
                                'Finland', 'France', 'Germany', 'Greece', 'Hungria', 'Ireland', 'Italy', 'Latvia', 'Lithuania',
                                'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia',
                                'Spain', 'Sweden')),
                  checkboxGroupInput("EUcheck", "Choose the fuels", c('BEV', 'CNG', 'H2', 'LNG', 'LPG', 'PHEV'), 
                                     selected = c('BEV', 'CNG', 'H2', 'LNG', 'LPG', 'PHEV')),
                  checkboxInput("Europe", "Europe", value = TRUE),
                  sliderInput(inputId = "YearEU", 
                              label = "Choose year for the values in Europe",
                              min = min(countriesafpassengercars$Year),
                              max = max(countriesafpassengercars$Year),
                              value = 2020)
                  ))
      # textInput("text_input", "Welk land", value = "Europa")
      )
      
      
        
      ))))

