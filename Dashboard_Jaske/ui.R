
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(readr)
library(plotly)
library(DT)

eusurvey <- read.csv('data/hev1.csv')

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
              tabBox(
                title = "Based on gender",
                tabPanel("Female", 
                         plotOutput("efemale")
                         ),
                tabPanel("Male", 
                         plotOutput("emale")
                         ),
                tabPanel("Country based",
                         selectInput(inputId = "gcountry",
                                     label = "Choose country",
                                     choices = levels(eusurvey$Country),
                                     multiple = T,
                                     selected = "Belgium"
                                     ),
                         plotOutput("gcountry")
                         )
                ),
              
              box(
                title = "Per country",
                tabPanel(" ", 
                         selectInput(inputId = "country",
                                     label = "Choose country",
                                     choices = levels(eusurvey$Country
                                     ),
                                     selected = "Belgium",
                                     multiple = T
                         ),
                         dataTableOutput("country")
                ), 
                width = 8
              ),
              
              tabBox(
                title = "Based on income",
                tabPanel("tab1", 
                         selectInput(inputId = "incountry",
                                     label = "choose Country",
                                     choices = levels(eusurvey$Country),
                                     selected = "Belgium"
                         ),
                         selectInput(inputId = "incomegr",
                                     label = "choose income group",
                                     choices = levels(eusurvey$Income_group),
                                     selected = "middle"
                         ),
                         plotOutput("view")
                ),
                tabPanel("tab2", 
                         selectInput(inputId = "estatus",
                                     label = "Choose employment status",
                                     choices = levels(eusurvey$Employment_status),
                                     selected = "Studying",
                                     multiple = T
                         ),
                         plotlyOutput("employ")
              )
              )
            )
            )
      )
    )
  )
)
