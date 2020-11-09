
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(readr)
library(plotly)
library(DT)

eusurvey <- read.csv("data/hev1.csv")

shinyUI(
  dashboardPage(
    skin = 'red',
    
    dashboardHeader(
      title = "TEST",
      dropdownMenu(
        type = "tasks",
        taskItem(
          text = "Not finished yet",
          value = 19
          )
        )
      ),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem(
          "EU Survey 2018", 
          tabName = "survey"
          ),
        menuItem(
          "Sales",
          tabName = "sales",
          menuSubItem(
            "Competition",
            tabName = "competition"
          )
        )
        )
      ),
    
    dashboardBody(
      tabItems(
          tabItem(
            tabName = "survey",
            fluidRow(
              valueBoxOutput("surveytotal"),
              valueBoxOutput("totalcountries")
              ),
            fluidRow(
              box(
                title = "Per country",
                tabPanel(" ", 
                         selectInput(inputId = "country",
                                     label = "Choose country",
                                     choices = unique(eusurvey$Country),
                                     selected = "Belgium",
                                     multiple = T
                         ),
                         dataTableOutput("country")
                ), 
                width = 14
              ),
              tabBox(
                title = "Based on",
                tabPanel("Income", 
                         selectInput(inputId = "incountry",
                                     label = "choose Country",
                                     choices = unique(eusurvey$Country),
                                     selected = "Belgium"
                         ),
                         selectInput(inputId = "incomegr",
                                     label = "choose income group",
                                     choices = unique(eusurvey$Income_group),
                                     multiple = T,
                                     selected = "middle"
                         ),
                         plotlyOutput("view")
                ),
                tabPanel("Employment status", 
                         selectInput(inputId = "estatus",
                                     label = "Choose employment status",
                                     choices = unique(eusurvey$Employment_status),
                                     selected = "Studying",
                                     multiple = T
                         ),
                         plotlyOutput("employ")
                         ),
                tabPanel("Gender",
                       selectInput(inputId = "gcountry",
                                   label = "Choose country",
                                   choices = unique(eusurvey$Country),
                                   multiple = T,
                                   selected = "Belgium"
                       ),
                       plotlyOutput("ggcountry")
                       ),
                tabPanel("Plan to buy car",
                         selectInput(inputId = "carplancountry",
                                     label = "Choose country",
                                     choices = unique(eusurvey$Country),
                                     selected = "Belgium"),
                         plotlyOutput("plan")),
                width = 14
              ),
              box(
                title = "Proportion of people willing to buy ev",
                  plotlyOutput("propev"),
                width = 14
                            )
            )
            ),
          tabItem(
            tabName = "competition",
            fluidRow(
              
            )
          )
      )
    )
  )
)
