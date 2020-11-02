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
library(DT)
library(rvest)

#Map + table01
superchargers <- read_xlsx("Data/Superchargers.xlsx")
superchargers <- superchargers %>% separate(GPS, sep = ",", into = c("Latitude", "Longitude"))
superchargers$Longitude <- as.double(superchargers$Longitude)
superchargers$Latitude <- as.double(superchargers$Latitude)
superchargers$id <- seq.int(nrow(superchargers))
superchargers$Year <- format(superchargers$'Open Date', format="%Y")
superchargers <- data.frame(superchargers)

#Histogram01
verkoop <- read_xlsx("Data/Yearly Tesla Sales Country Split (Europe).xlsx")
Verkoop <- verkoop %>% gather('2013':'2019', key=  "Year", value="Sales")
verkoop$Year <- as.integer(verkoop$Year)
verkoop$Sales <- as.integer(verkoop$Sales)


# Define server logic required to draw a map
shinyServer(function(input, output, session) {
    
    #table tesla superchargers
    output$table01 <- renderDataTable({
        
        DT::datatable(superchargers, selection = "single",options=list(stateSave = TRUE))
    })
    
    # to keep track of previously selected row
    prev_row <- reactiveVal()
    
    # new icon style
    my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
    
    observeEvent(input$table01_rows_selected, {
        row_selected = superchargers[input$table01_rows_selected,]
        proxy <- leafletProxy('mymap')
        proxy %>%
            addAwesomeMarkers(popup=as.character(row_selected$mag),
                              layerId = as.character(row_selected$id),
                              lng=row_selected$Longitude, 
                              lat=row_selected$Latitude,
                              icon = my_icon)
        # Reset previously selected marker
        if(!is.null(prev_row()))
        {
            proxy %>%
                addMarkers(popup=as.character(prev_row()$Street.Address), 
                           layerId = as.character(prev_row()$id),
                           lng=prev_row()$Longitude, 
                           lat=prev_row()$Latitude)
        }
        # set new value to reactiveVal 
        prev_row(row_selected)
    })
    #map tesla superchargers
    output$mymap <- renderLeaflet({
        leaflet() %>% addTiles() %>% addMarkers(data = superchargers, layerId = as.character(superchargers$id))
    })
    
    observeEvent(input$mymap_marker_click, {
        clickId <- input$mymap_marker_click$id
        dataTableProxy("table01") %>%
            selectRows(which(superchargers$id == clickId)) %>%
            selectPage(which(input$table01_rows_all == clickId) %/% 
                           input$table01_state$length+1)})
    
    #histogram: vergelijken met teslaverkoop 
    output$hist01 <- renderPlot({
        verkoopC <- verkoop %>% filter(Country %in% input$Country, Year == input$Year)
        superchargersC <- superchargers %>% filter(Year < input$Year+1, Status == 'OPEN', Country %in% input$Country)
        superchargersC <- count(superchargersC, "Country")
        ratio <- full_join(superchargersC, verkoopC, by = 'Country')
        ratio$freq <- as.integer(ratio$freq)
        ratio$Country <- as.factor(ratio$Country)
        ratio <- ratio %>% mutate(Teslas_per_Supercharger = Sales/freq)
        ratio$Teslas_per_Supercharger <- as.double(ratio$Teslas_per_Supercharger)
        ratio %>% ggplot(aes(x= Country, y = Teslas_per_Supercharger)) + geom_col() + labs(title = "Teslas/supercharger in 2019.") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
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
