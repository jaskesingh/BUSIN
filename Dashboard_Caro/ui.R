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
        menuItem("Snellaadpalen", tabName = "Snellaadpalen", menuSubItem("Kaart", tabName = "Kaart"), menuSubItem("Gegevens", tabName = "Gegevens"))
     )),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "Kaart",
          leafletOutput("mymap"), dataTableOutput("table01")),
        tabItem(
          tabName = "Gegevens",
          fluidRow(
            infoBox("Totaal aaantal snellaadpalen", value = sum(aantal$freq), icon = icon("dollar-sign")),
            infoBox("Aantal open snellaadpalen", value = aantal$freq[aantal$Status == "OPEN"], icon = icon("dollar-sign")),
            infoBox("Aantal bouwende snellaadpalen", value = aantal$freq[aantal$Status == "CONSTRUCTION"], icon = icon("dollar-sign")),
            infoBox("Aantal toegestane snellaadpalen",value = aantal$freq[aantal$Status == "PERMIT"]),
            infoBox("Aantal permanent gesloten snellaadpalen", value = aantal$freq[aantal$Status == "CLOSED_PERM"]),
            infoBox("Aantal tijdelijk gesloten snellaadpalen", value = aantal$freq[aantal$Status == "CLOSED_TEMP"])
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
