library(readxl)
library(maps)
library(ggplot2)
library(tidyr)
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

superchargers <- read_xlsx("Data/Superchargers.xlsx")
superchargers <- superchargers %>% separate(GPS, sep = ",", into = c("Latitude", "Longitude"))
superchargers$Longitude <- as.double(superchargers$Longitude)
superchargers$Latitude <- as.double(superchargers$Latitude)
superchargers <- data.frame(superchargers)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    output$mymap <- renderLeaflet({
        leaflet() %>% addTiles() %>% addMarkers(data = superchargers, lng = superchargers$Longitude, lat = superchargers$Latitude, label = 'Supercharger', popup = 'Stalls')
    })
   })
    
    #function(input, output) {

    #output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
       # x    <- faithful[, 2]
       # bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
       # hist(x, breaks = bins, col = 'darkgray', border = 'white')

   # })

#})
