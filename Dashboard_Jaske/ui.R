
library(shiny)
library(shinydashboard)
library(shinyjs)
library(htmltools)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(readr)
library(plotly)
library(DT)
library(tidyquant)
library(quantmod)

eusurvey <- read.csv("data/hev1.csv")

logotsla <- tags$a(href='https://cdn.freelogovectors.net/wp-content/uploads/2014/05/tesla_motors-logo.png',
                 tags$img(src='tesla_motors-logo.png', height='60', width='50'),
                 'Menu')

shinyUI(
  dashboardPage(
    skin = 'red',
    
    dashboardHeader(
      title = logotsla, titleWidth = 600,
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
        sidebarSearchForm("searchText", "buttonSearch", "Search"),
        menuItem(
          "EU Survey 2018", 
          tabName = "survey"
          ),
        menuItem(
          "TSLA stock",
          tabName = "stock",
          badgeLabel = "New", badgeColor = "green"
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
            tabName = "stock",
            fluidRow(
              box(
                dateInput(inputId = "st", label = "start date",
                          value = "2020-01-01"),
                dateInput(inputId = "en", label = "end date"),
                plotlyOutput("tslastock")
              )
              
            )
          )
      )
    )
  )
)
