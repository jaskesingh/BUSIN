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

#Groei: verkoop alle merken per segment
VPS <- read_xlsx("Data/New cars sold in the EU by segment in million units.xlsx")

# Define UI for application that draws a map
shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Menu'),
    dashboardSidebar(
      sidebarMenu(
        sidebarSearchForm("searchText", "buttonSearch", "Search"),
        menuItem("Superchargers", tabName = "Superchargers", newTab = T, menuSubItem("Map", tabName = "Map"), menuSubItem("Statistics", tabName = "Statistics"), 
                 menuSubItem("Competition", tabName = "Competition")),
        menuItem("Growth", tabName = "Growth", newTab = T, menuSubItem("Sales per segment", tabName = "Sales"))
     )),
    dashboardBody(
      tabItems(
          tabItem(
            tabName = "Map",  
            leafletOutput("mymap"), dataTableOutput("table01")),
          tabItem(
            tabName = "Statistics",
            fluidRow(
              box(title = "Info on Tesla superchargers in Europe",
                solidHeader = T, status = "danger", width = 12,
                valueBoxOutput("totbox"),
                valueBoxOutput("openbox"),
                valueBoxOutput("buildbox"),
                valueBoxOutput("permitbox"),
                valueBoxOutput("pclosedbox"),
                valueBoxOutput("tclosedbox")
                )
              ),
            fluidRow(
              box(
                title = "Teslas/supercharger", width = 12,
                solidHeader = T, status = "danger", plotOutput("hist01"),
                sliderInput(inputId = "Year",
                          label = "Choose year",
                          min = 2013,
                          max = 2019,
                          value = 2019),
                selectInput(inputId = "Country",
                          label = "Choose country",
                          choices = verkoo$Country,
                          multiple = TRUE,
                          selected = "Belgium")
                )
              )
        ),
        tabItem(
            tabName = "Competition",
            tabBox(
              title ="Number of superchargers per country", height = 12,
              tabPanel("Tab1", plotOutput("hist02")),
              tabPanel("Tab2", plotOutput("hist03")),
              selectInput(inputId = "Country2",
                        label = "Choose country",
                        choices = superchargers$Country,
                        multiple = TRUE,
                        selected = "Belgium")
              ),
            box(
              title = "Superchargers market share", height = 12,
              solidHeader = T, status = 'danger', plotOutput("pie01")
              )
        ),
        tabItem(
            tabName = "Sales",
            fluidRow(
              box(
                title = "New cars sold in the EU by segment in million units over the years", width = 12,
                solidHeader = T, status = 'danger', plotOutput("line01"),
                selectInput(inputId = "Segment",
                        label = "Choose segment",
                        choices = VPS$Segment,
                        multiple = TRUE,
                        selected = "SUV")
              )
            ),
            fluidRow(
              box(
                title = "New cars sold in the EU by segment in million units for each year", width = 12,
                solidHeader = T, status = 'danger', plotOutput("hist04"),
                sliderInput(inputId = "Year2",
                        label = "Choose year",
                        min = 2008,
                        max = 2019,
                        value = c(2008, 2019)),
                selectInput(inputId = "Segment2",
                        label = "Choose segment",
                        choices = VPS$Segment,
                        multiple = TRUE,
                        selected = "SUV")
                )
            )
        )
      )
    )
  )
)


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
