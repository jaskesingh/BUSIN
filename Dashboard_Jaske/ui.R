
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

logotsla <- tags$a(href='https://www.google.com',
                 tags$img(src="data/tesla-logo2.jpg", height='50', width='50'),
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
        menuItem("Growth", tabName = "Growth", newTab = T, icon = icon('chart-line'), 
                 menuSubItem("Sales per segment", tabName = "Salespersegment", icon = icon('search-dollar')), 
                 menuSubItem("Sales per fuel type", tabName = "fueltype", icon = icon('funnel-dollar')),
                 menuSubItem("Best selling EV's compared", tabName = "best_selling_evs_compared", icon = icon('medal'))
        ),
        menuItem("Customers", tabName = "Customers", newTab = T, icon = icon('users'),
                 menuSubItem("Purchase process", tabName = "Purchaseprocess", icon = icon('wallet')),
                 menuSubItem("Brand loyalty", tabName = "dashboard_loyalty", icon = icon('grin-hearts') ),
                 menuSubItem("EV popularity", tabName = "survey",icon = icon('grin-stars'))
        ),
        menuItem("Sales", tabName = "Sales", icon = icon('dollar-sign')),
        menuItem("Finance", tabName = "Finance", icon = icon('file-invoice-dollar')),
        menuItem("Superchargers", tabName = "Superchargers", newTab = T, icon = icon('bolt'),
                 menuSubItem("Map", tabName = "Map", icon = icon('map-marked-alt')), 
                 menuSubItem("Statistics", tabName = "Statistics", icon = icon('chart-bar')), 
                 menuSubItem("Competition", tabName = "Competition", icon = icon('fist-raised'))),
        menuItem("Expansion in Europe", tabName = "Expansion_in_Europe", icon = icon('expand-arrows-alt')),
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
      ),
      useShinyjs()
    )
  )
)
