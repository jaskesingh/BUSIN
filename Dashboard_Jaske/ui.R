
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(readr)

eusurvey <- read.csv('data/hev1.csv')

shinyUI(
  dashboardPage(
    dashboardHeader(title = "TEST",
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
        menuItem("Klanten", tabName = "klanten",
                 menuItem("Test", tabName = "test"),
                 menuItem("Test1", tabName = "test1"))
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "test",
                tabBox(
                  title = "based on gender",
                  tabPanel("Female", plotOutput("efemale")),
                  tabPanel("Male", plotOutput("emale")),
                  tabPanel("Country based",
                           selectInput(inputId = "gcountry",
                                       label = "Choose country",
                                       choices = levels(hev1$Country),
                                       multiple = T,
                                       selected = "Belgium"),
                           plotOutput("gcountry"))
            
                  ),
                tabItem(tabName = "test1",
                tabBox(
                  title = "per country",
                  tabPanel(" ", selectInput(inputId = "country",
                                  label = "Choose country",
                                  choices = levels(hev1$Country),
                                  multiple = T,
                                  selected = "Belgium")),
                  dataTableOutput("country"))
    )
    )
    )
  )
)
)