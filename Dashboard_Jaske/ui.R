
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
    skin = 'red',
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
                 menuSubItem("Test", tabName = "test"),
                 menuSubItem("Test1", tabName = "test1"),
                 menuSubItem("Income", tabName = "income"))
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
                                       choices = levels(eusurvey$Country),
                                       multiple = T,
                                       selected = "Belgium"),
                           plotOutput("gcountry"))
            
                  )),
        tabItem(tabName = "test1",
                tabBox(
                  title = "per country",
                  tabPanel(" ", selectInput(inputId = "country",
                                  label = "Choose country",
                                  choices = levels(eusurvey$Country),
                                  selected = "Belgium",
                                  multiple = T),
                           dataTableOutput("country")), width = 8)
                ),
        tabItem(tabName = "income",
                tabBox(
                  title = "based on income",
                  tabPanel("tab1", selectInput(inputId = "incountry",
                                               label = "choose Country",
                                               choices = levels(eusurvey$Country),
                                               selected = "Belgium"),
                           selectInput(inputId = "incomegr",
                                       label = "choose income group",
                                       choices = levels(eusurvey$Income_group),
                                       selected = "middle"),
                           plotOutput("view"))
                ))
    )
    )
  )
)
