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

superchargers <- read_xlsx("Data/Superchargers.xlsx")
superchargers <- superchargers %>% separate(GPS, sep = ",", into = c("Latitude", "Longitude"))
superchargers$Longitude <- as.double(superchargers$Longitude)
superchargers$Latitude <- as.double(superchargers$Latitude)
superchargers <- data.frame(superchargers)
aantal <- plyr::count(superchargers, "Status")

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
            infoBox("Total number of superchargers", value = sum(aantal$freq)),
            infoBox("Number of open superchargers", value = aantal$freq[aantal$Status == "OPEN"]),
            infoBox("Number of building superchargers", value = aantal$freq[aantal$Status == "CONSTRUCTION"]),
            infoBox("Number of permit superchargers",value = aantal$freq[aantal$Status == "PERMIT"]),
            infoBox("Number of permantly closed superchargers", value = aantal$freq[aantal$Status == "CLOSED_PERM"]),
            infoBox("Number of temporarly closed superchargers", value = aantal$freq[aantal$Status == "CLOSED_TEMP"])
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
