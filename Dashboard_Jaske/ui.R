
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
        menuItem("Klanten", tabName = "klanten")
      )
    ),
    
    dashboardBody(
      tabBox(
        title = "based on gender",
        tabPanel("Female", plotOutput("efemale")),
        tabPanel("Male", plotOutput(("emale")))
      )
      )
    )
    )