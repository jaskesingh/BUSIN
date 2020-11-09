# # # # # # # # # #

# Libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(scales)


# Shiny UI
shinyUI(
  dashboardPage(skin = "red", 
    
    dashboardHeader(title = "Menu"),
    
    dashboardSidebar(
      
      
      sidebarMenu(
        # Pitch document: Growth --> Comparison of electric vehicles of different brands
        # Maybe add icons in final dashboard (video 3)
        menuItem("Growth (Later)", tabName =  "dashboard_growth"),
      
        # Pitch document: Customers --> Purchase history --> Loyalty
        menuItem("Brand Loyalty (Work-In-Progress)", tabName = "dashboard_loyalty")
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard_growth",
                fluidRow(
                  box(title = "Top 15 EV's of 2019 compared (Work-in-progress)",
                      status = "danger",
                      solidHeader = T,
                      plotOutput("growth_bar"),
                      selectInput(inputId = "growth_select_box",
                                  label = "Select parameter for comparison",
                                  choices = c("Sales In 2019",
                                              "Sales In 2018",
                                              "Change In Sales From 2018 To 2019 (%)",
                                              "Share In EV Market In 2019",
                                              "Share In EV Market In 2018",
                                              "Percent Of This Model That Was EV In 2019",
                                              "Percent Of This Model That Was EV In 2018",
                                              # De 7 dingen hierboven zouden eventueel ook ...
                                              # ... als pie chart kunnen, met best dan de ...
                                              # info per jaar via plotly (toch zeker % en sales)
                                              "Range",
                                              "Top speed (km/h)",
                                              "Acceleration (0-100 km/h)",
                                              "Horsepower",
                                              "Top Charging Speed (km/h)",
                                              "Price",
                                              "Trunk Space (Including Frunk If Applicable)",
                                              "Segment",
                                              "NCAP Stars",
                                              "NCAP Adult Occupant Score (%)",
                                              "NCAP Child Occupant Score (%)",
                                              "NCAP Vulnerable Road Users Score (%)",
                                              "NCAP Safety Assist Score (%)",
                                              "NCAP Average Score (%)"
                                              ),
                                  selected = "Sales in 2019 (absolute)"
                                  )
                      )
                  )
                ),
        
        tabItem(tabName = "dashboard_loyalty", 
                fluidRow(
                  box(title = "Loyalty per brand (Work-in-progress)",
                      "Percentage of car buyers that chose the same brand when buying a new car",
                      status = "danger",
                      solidHeader = T,
                      plotOutput("loyalty_bar"),
                      checkboxGroupInput(inputId = "loyalty_checkboxes",
                                         label = "Choose class(es)",
                                         choices = c("Luxury", "Mass market"),
                                         selected = c("Luxury", "Mass market")
                                        )
                      )
                        )
                ) 
        )
    )
      
  )
)


# # # # # # # # # #