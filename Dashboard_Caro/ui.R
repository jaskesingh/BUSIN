library(readxl)
library(maps)
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(ggmap)
library(mapdata)
library(mapproj)
library(sf)
library(rnaturalearth)
library(hrbrthemes)
library(grid)
library(rworldmap)
library(shiny)
library(plotly)
library(leaflet)
library(shinydashboard)

#Map + table01 + infoboxen
superchargers <- read_xlsx("Data/Superchargers.xlsx")
superchargers <- superchargers %>% separate(GPS, sep = ",", into = c("Latitude", "Longitude"))
superchargers$Longitude <- as.double(superchargers$Longitude)
superchargers$Latitude <- as.double(superchargers$Latitude)
superchargers <- data.frame(superchargers)

#Histogram01
verkoo <- read_xlsx("Data/Yearly Tesla Sales Country Split (Europe).xlsx")

# Define UI for application that draws a map
shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Tesla'),
    dashboardSidebar(
      sidebarMenu(
        sidebarSearchForm("searchText", "buttonSearch", "Search"),
        menuItem("Superchargers", tabName = "Superchargers", menuSubItem("Map", tabName = "Map"), menuSubItem("Statistics", tabName = "Statistics"))
     )),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "Map",
          leafletOutput("mymap"), dataTableOutput("table01")),
        tabItem(
          tabName = "Statistics",
          fluidRow(
            valueBoxOutput("totbox"),
            valueBoxOutput("openbox"),
            valueBoxOutput("buildbox"),
            valueBoxOutput("permitbox"),
            valueBoxOutput("pclosedbox"),
            valueBoxOutput("tclosedbox")
          ),
          fluidRow(
            box(
              title = "Teslas/supercharger",
              solidHeader = T, status = "primary", plotOutput("hist01"),
              sliderInput(inputId = "Year",
                          label = "Choose year",
                          min = 2013,
                          max = 2019,
                          value = 2019),
              selectInput(inputId = "Country",
                          label = "Choose country",
                          choices = verkoo$Country,
                          multiple = TRUE
                          )
            )
          )
          ))
      
)))
    


# Application title
    #titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    #sidebarLayout(
       # sidebarPanel(
            #sliderInput("bins",
                       # "Number of bins:",
                       # min = 1,
                       # max = 50,
                       # value = 30)
        #),

        # Show a plot of the generated distribution
       # mainPanel(
           # plotOutput("distPlot")
     #   )
  #  )
#))
