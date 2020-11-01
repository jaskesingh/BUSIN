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
Revenue <- read_xlsx("data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Revenues (automotive)", col_types = c("numeric", "text", "numeric", "numeric"))

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Tesla"),
    dashboardSidebar(
      sidebarMenu(
        sidebarSearchForm("searchText", "buttonSearch", "Search"),
      menuItem("Financiële cijfers", tabName = "Omzet"),
      menuItem("Uitbreiding naar EU"),
      textInput("text_input", "Welk land", value = "Europa")
    )),  
    dashboardBody(
      tabItems(
        tabItem(tabName = "Omzet",
                fluidRow(
                  infoBox("Omzet 2020 in duizenden", sum(Revenue$`1000_revenue`[Revenue$Year == "2020"], na.rm = TRUE), icon = icon("dollar-sign")),
                  infoBox("Gross profit 2020", icon = icon("dollar-sign")),
                  infoBox("Gross margin 2020", icon = icon("dollar-sign")),
                  ),                
                fluidRow(
                  box(title = "Auto-omzet", 
                       "In duizenden, per kwartaal",
                      solidHeader = T, status="primary", plotOutput("col"),
                      sliderInput(inputId = "Yearrev", 
                        label = "Kies het jaar",
                        min = min(Revenue$Year),
                        max = max(Revenue$Year),
                        value = 2020)),
                  box(title = "Gross profit", solidHeader = T, status = "warning"))
      ))
      
      
      
      
      
        
      )))
 # fluidPage(

    # Application title
  #  titlePanel("Financiële cijfers Tesla "),

    # Sidebar with a slider input for number of bins
   # sidebarLayout(
    #    sidebarPanel(
     #       sliderInput(inputId = "Yearrev", 
      #                  label = "Kies het jaar",
       #                 min = min(Revenue$Year),
        #                max = max(Revenue$Year),
        #                value = 2020)
        #),
        #
#
        # Show a plot of the generated distribution
        #mainPanel(
        #    plotOutput(outputId = "col")
#        )
 #   )
 # )
  #  )))
