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
        menuItem("Financial quarterly numbers", tabName = "Omzet"),
        menuItem("Expansion in Europe", tabName = "EU")
    )
    ),  
    dashboardBody(
      tabItems(
        tabItem(tabName = "Omzet",
                
                fluidRow(
                  valueBoxOutput("revbox"),
                  valueBoxOutput("frcashbox"),
                  valueBoxOutput("grprbox")
                ),            
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
              h1("Europese unie"))
      # textInput("text_input", "Welk land", value = "Europa")
      
      
      
        
      ))))

