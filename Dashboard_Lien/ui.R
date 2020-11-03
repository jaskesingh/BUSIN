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
Revenue <- read_xlsx("data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Revenues (automotive)", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_Margin <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross margin", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_profit <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross profit", col_types = c("numeric", "text", "numeric", "numeric", "numeric"))
Free_cashflow <- read_xlsx("Data/Tesla's free cash flow by quarter 2020 world wide.xlsx", skip = 3 , sheet = "Data", col_types = c("numeric", "text", "numeric"))
# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(title = 'Tesla',
    dashboardHeader(title = "Menu"),
    dashboardSidebar(
      sidebarMenu(
        sidebarSearchForm("searchText", "buttonSearch", "Search"),
        menuItem("Financiële cijfers", tabName = "Omzet"),
        menuItem("Jaarlijkse gegevens", tabName = "Jaarlijkse_gegevens"),
        menuItem("Uitbreiding naar EU", tabName = "EU")
    )),  
    dashboardBody(
      tabItems(
        tabItem(tabName = "Omzet",
                fluidRow(
                  valueBoxOutput("revbox"),
                  valueBoxOutput("frcashbox"),
                  valueBoxOutput("grprbox")
                ),            
                fluidRow(
                  box(title = "Auto-omzet",
                       "In duizenden, per kwartaal",
                      solidHeader = T, status="primary", plotOutput("colrev")),
                  box(title = "Free cashflow", br(), solidHeader = T, status = "primary", plotOutput(("colfrcash")))
                ),
                fluidRow(
                  box(title = "Bruto winst", br(), solidHeader = T, status = "primary", plotOutput("colgrpr")),
                  box(title = "Bruto winstmarge", 
                      "In percentage, per kwartaal", solidHeader = T, status = "primary", plotOutput("colgrmar")),
                  box(title = "Aanpassingen aan grafieken maken",
                      solidHeader = T, status = "primary", sliderInput(inputId = "Yearrev", 
                                                                       label = "Kies het jaar",
                                                                       min = min(Revenue$Year),
                                                                       max = max(Revenue$Year),
                                                                       value = 2020),
                      textInput("text_input", "Welk land", value = "Europa"))
                )
            ),
      tabItem(tabName = "Jaarlijkse_gegevens",
              fluidRow(
                box(title = "Auto-omzet",
                    "In duizenden, per kwartaal",
                    solidHeader = T, status="primary", plotOutput("linerev")),
                box(title = "Free cashflow", br(), solidHeader = T, status = "primary", plotOutput("linefrcash"))
              ),
              fluidRow(
                box(title = "Bruto winst", br(), solidHeader = T, status = "primary", plotOutput("linegrpr")),
                box(title = "Bruto winstmarge", 
                    "In percentage, per kwartaal", solidHeader = T, status = "primary", plotOutput("linegrmar")),
                box(title = "Aanpassingen aan grafieken maken", solidHeader = T, status = "primary",
                    sliderInput(inputId = "Yearrevline", 
                                label = "Kies het jaar",
                                min = min(Revenue$Year),
                                max = max(Revenue$Year),
                                value = c(min(Revenue$Year),max(Revenue$Year)))
                )
              )
        ),
      tabItem(tabName = "EU",
              h1("Europese unie"))
      
      
      
        
      ))))

