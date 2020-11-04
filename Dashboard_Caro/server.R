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
library(stringr)

#Map + table01 + infoboxen
superchargers <- read_xlsx("Data/Superchargers.xlsx")
superchargers <- superchargers %>% separate(GPS, sep = ",", into = c("Latitude", "Longitude"))
superchargers$Longitude <- as.double(superchargers$Longitude)
superchargers$Latitude <- as.double(superchargers$Latitude)
superchargers$id <- seq.int(nrow(superchargers))
superchargers$Year <- format(superchargers$'Open Date', format="%Y")
superchargers <- data.frame(superchargers)
aantal <- plyr::count(superchargers, "Status")

#Histogram01
verkoo <- read_xlsx("Data/Yearly Tesla Sales Country Split (Europe).xlsx")
verkoo$'2013' <- as.numeric(verkoo$'2013')
verkoo$'2014' <- as.numeric(verkoo$'2014')
verkoo$'2015' <- as.numeric(verkoo$'2015')
verkoo$'2016' <- as.numeric(verkoo$'2016')
verkoo$'2017' <- as.numeric(verkoo$'2017')
verkoo$'2018' <- as.numeric(verkoo$'2018')
verkoo$'2019' <- as.numeric(verkoo$'2019')
verkoo <- verkoo %>% gather('2013':'2019',key = "Year", value = "Sales")
verkoo$Year <- as.integer(verkoo$Year)       
verkoo$Sales <- as.integer(verkoo$Sales)
verkoo <- data.frame(verkoo)

#Concurrentie
ionity <- read_xlsx("Data/ionity_locations.xlsx")
ionity$Supercharger <- ionity$name
ionity$Stalls <- ionity$charger_count
ionity$Open.Date <- ionity$golive
ionity$Latitude <- ionity$coords.lat
ionity$Longitude <- ionity$coords.lng
ionity$Status <- ionity$description
ionity$Description <- ionity$title
ionity$Country <- ionity$geo_state.country
ionity$City <- ionity$city
ionity$State <- ionity$geo_state.name_en
ionity <- ionity %>% filter(Status != 'now building' | Status != 'Now building')
ionity <- ionity %>% select(Supercharger, Stalls, Latitude, Longitude, Description, City, State, Country, Open.Date)
teslapalen <- superchargers %>% mutate(Description = 'Tesla') %>% filter(Status == 'OPEN') %>% select(Supercharger, Stalls, Latitude, Longitude, Description, City, State, Country, Open.Date)
ionity$Open.Date <- as.POSIXct(ionity$Open.Date, format = "%Y-%m-%d %H:%M")
laadpalen <- bind_rows(ionity, teslapalen)
laadpalen$Country <- as.factor(laadpalen$Country)
taart <- plyr::count(laadpalen, "Description")
laadpalen <- plyr::count(laadpalen, c("Description", "Country"))
taart <- taart %>% dplyr::mutate(ratio = round(freq/sum(freq)*100))

#Groei: verkoop alle merken per segment
VPS <- read_xlsx("Data/New cars sold in the EU by segment in million units.xlsx")
VPS <- VPS %>% gather('2008':'2019', key=  "Year", value="Sales")
VPS$Year <- as.numeric(VPS$Year)
VPS$Sales <- as.double(VPS$Sales)

#Groei: aandeel elektrische auto's op belgische en eu markt
nieuw <- read_xlsx("Data/Verkoop per brandstof (België) met market share.xlsx", sheet = "Nieuw")
tweedehands <- read_xlsx("Data/Verkoop per brandstof (België) met market share.xlsx", sheet = "Tweedehands")
eu <- read_xlsx("Data/% share of new passenger cars by fuel type in the EU.xlsx")
NieuwMS <- nieuw %>% gather(MS12, MS13, MS14, MS15, MS16, MS17, MS18, MS19,key = "Year", value = "Market.Share",na.rm = FALSE, convert = FALSE, factor_key = FALSE)
NieuwMS$Year <- recode(NieuwMS$Year, MS12 = "2012", MS13 = "2013", MS14 = "2014", MS15 = "2015", MS16 = "2016", MS17 = "2017", MS18 = "2018", MS19 = "2019" )
Nieuw <- nieuw %>% gather('2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', key = "Year", value = "Cars sold",na.rm = FALSE, convert = FALSE, factor_key = FALSE)
Nieuw$Year <- as.integer(Nieuw$Year)
NieuwMS$Year <- as.integer(NieuwMS$Year)
NieuwMS$Market.Share <- as.double(NieuwMS$Market.Share)
TweedehandsMS <- tweedehands %>% gather(MS12, MS13, MS14, MS15, MS16, MS17, MS18, MS19,key = "Year", value = "Market.Share",na.rm = FALSE, convert = FALSE, factor_key = FALSE)
TweedehandsMS$Year <- recode(TweedehandsMS$Year, MS12 = "2012", MS13 = "2013", MS14 = "2014", MS15 = "2015", MS16 = "2016", MS17 = "2017", MS18 = "2018", MS19 = "2019" )
Tweedehands <- tweedehands %>% gather('2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', key = "Year", value = "Cars sold",na.rm = FALSE, convert = FALSE, factor_key = FALSE)
Tweedehands$Year <- as.integer(Tweedehands$Year)
TweedehandsMS$Year <- as.integer(TweedehandsMS$Year)
TweedehandsMS$Market.Share <- as.double(TweedehandsMS$Market.Share)
EuMS <- eu %>% gather('2016', '2017', '2018', '2019',key = "Year", value = "Market.Share",na.rm = FALSE, convert = FALSE, factor_key = FALSE)
EuMS$Year <- as.integer(EuMS$Year)
EuMS$Market.Share <- as.double(EuMS$Market.Share)

#Klanten: aankoopproces
aankoopproces <- read_xlsx("Data/Online.xlsx")
aankoopproces <- aankoopproces %>% gather(`Not at all interested/not very interested`:`Somewhat interested/very interested`, key = "Interest", value="Percentage")

#Verkoop: periodieke tesla verkoop
data <- read_xlsx("Data/Monthly Tesla Vehicle Sales.xlsx")
Data <- data %>% gather(January:December, key=  "Month", value="Sales") %>% mutate(Month = str_replace(Month, "January", "1"), Month = str_replace(Month, "February", "2"), Month = str_replace(Month, "March", "3"), Month = str_replace(Month, "April", "4"), Month = str_replace(Month, "May", "5"), Month = str_replace(Month, "June", "6"), Month = str_replace(Month, "July", "7"), Month = str_replace(Month, "August", "8"), Month = str_replace(Month, "September", "9"), Month = str_replace(Month, "October", "10"), Month = str_replace(Month, "November", "11"), Month = str_replace(Month, "December", "12"))
Data$Month <- as.integer(Data$Month)
Data$Year <- as.factor(Data$Year) 

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
    
    #infoboxen
    output$totbox <- renderValueBox({
        valueBox(
            paste0(sum(aantal$freq)),
            subtitle= "Total number of superchargers", 
        )})
    output$openbox <- renderValueBox({
        valueBox(
            paste0(aantal$freq[aantal$Status == "OPEN"]),
            subtitle= "Number of open superchargers", 
        )})
    output$buildbox <- renderValueBox({
        valueBox(
            paste0(aantal$freq[aantal$Status == "CONSTRUCTION"]),
            subtitle= "Number of building superchargers", 
        )})
    output$permitbox <- renderValueBox({
        valueBox(
            paste0(aantal$freq[aantal$Status == "PERMIT"]),
            subtitle= "Number of permit superchargers", 
        )})
    output$pclosedbox <- renderValueBox({
        valueBox(
            paste0(aantal$freq[aantal$Status == "CLOSED_PERM"]),
            subtitle= "Number of permantly closed superchargers", 
        )})
    output$tclosedbox <- renderValueBox({
        valueBox(
            paste0(aantal$freq[aantal$Status == "CLOSED_TEMP"]),
            subtitle= "Number of temporarly closed superchargers", 
        )})
    
    #histogram: vergelijken met teslaverkoop 
    output$hist01 <- renderPlot({
        verkooC <- verkoo %>% dplyr::filter(Country %in% input$Country, Year == input$Year)
        superchargersC <- superchargers %>% dplyr::filter(Year < input$Year+1, Status == 'OPEN', Country %in% input$Country)
        superchargersC <- plyr::count(superchargersC, "Country")
        ratio <- full_join(superchargersC, verkooC, by = 'Country')
        ratio$freq <- as.integer(ratio$freq)
        ratio$Country <- as.factor(ratio$Country)
        ratio <- ratio %>% mutate(Teslas_per_Supercharger = Sales/freq)
        ratio$Teslas_per_Supercharger <- as.double(ratio$Teslas_per_Supercharger)
        ratio %>% ggplot(aes(x= Country, y = Teslas_per_Supercharger)) + geom_col() + labs(title = paste0("Teslas/supercharger in", input$Year)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
    })
    
    #histogram: concurrentie snellaadpalen
    output$hist02 <- renderPlot({
        laadpalenC <- laadpalen %>% filter(Country %in% input$Country2)
        laadpalenC %>% ggplot(aes(x = Description, y = freq)) + geom_col() + labs(title = "Superchargers per country") + facet_wrap(Country~.)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))})
    output$hist03 <- renderPlot({
        laadpalenC <- laadpalen %>% filter(Country %in% input$Country2)
        laadpalenC %>% ggplot(aes(x = Country, y = freq)) + geom_col(aes(fill = Description)) + labs(title = "Superchargers per country") + theme(axis.text.x = element_text(angle = 45, hjust = 1))})
    
    #taartdiagram: concurrentie snellaadpalen
    output$pie01 <- renderPlot({
    taart %>% ggplot(aes(x="", y = ratio, fill = Description)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + labs(title = "Superchargers market share")})
    
    #lijngrafiek: Groei: verkoop alle merken per segment
    output$line01 <- renderPlot({
        VPSC2 <- VPS %>% filter(Segment %in% input$Segment)
        VPSC2 %>% ggplot(aes(x=Year, y=Sales)) + geom_line(aes(color = Segment)) + labs(title = "New cars sold in the EU by segment in million units over the years.") + 
        scale_x_continuous(breaks = c(2008:2019)) + scale_y_continuous(breaks= seq(0,6, by = 1)) })
    output$hist04 <- renderPlot({
        VPSC <- VPS %>% filter(Segment %in% input$Segment2, Year >= min(input$Year2) & Year <= max(input$Year2))
        VPSC %>% ggplot(aes(x = Segment, y = Sales)) + geom_col() + facet_wrap(Year~.) + 
            labs(title = "New cars sold in the EU by segment in million units for each year.") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    #lijn nieuw: groei: aandeel elektrische auto's op belgische en eu markt
    output$line02 <- renderPlot({
        NieuwC <- Nieuw %>% filter(Year >= min(input$Year3) & Year <= max(input$Year3), Fuel %in% input$Fuel)
        NieuwC %>% ggplot(aes(x = Year, y = `Cars sold`)) + geom_line(aes(color = Fuel), size = 1) + labs(title = "Number of new cars sold in Belgium over the years")})
    #taart nieuw: groei: aandeel elektrische auto's op belgische en eu markt
    output$pie02 <- renderPlot({
        NieuwMSC <- NieuwMS %>% filter(Year == input$Year5)
        bar1 <- NieuwMSC %>% ggplot(aes(x ="", y = Market.Share*100, fill = Fuel)) + geom_bar(width = 1, stat = "identity") + xlab("") + labs(title = "Market share of new cars by fuel type in Belgium in", input$Year5) 
        bar1 + coord_polar("y", start = 0)
    })
    
    #lijn tweedehands: groei: aandeel elektrische auto's op belgische en eu markt
    output$line03 <- renderPlot({
        TweedehandsC <- Tweedehands %>% filter(Year >= min(input$Year4) & Year <= max(input$Year4), Fuel %in% input$Fuel2)
        TweedehandsC %>% ggplot(aes(x = Year, y = `Cars sold`)) + geom_line(aes(color = Fuel), size = 1) + labs(title = "Number of second hand cars sold in Belgium over the years")
    })
    #taart tweedehands: groei: aandeel elektrische auto's op belgische en eu markt
    output$pie03 <- renderPlot({
        TweedehandsMSC <- TweedehandsMS %>% filter(Year == input$Year6)
        bar <- TweedehandsMSC %>% ggplot(aes(x ="", y = Market.Share*100, fill = Fuel)) + geom_bar(width = 1, stat = "identity") + xlab("") + labs(title = "Market share of second hand cars by fuel type in Belgium in", input$Year6) 
        bar + coord_polar("y", start = 0)
    })
    
    #taart eu: groei: aandeel elektrische auto's op belgische en eu markt
    output$pie04 <- renderPlot({
        EuMSC <- EuMS %>% filter(Year == input$Year7)
        bareu <- EuMSC %>% ggplot(aes(x ="", y = Market.Share, fill = Fuel)) + geom_bar(width = 1, stat = "identity") + xlab("") + labs(title = "Market share of new cars by fuel type in the EU in", input$Year7)
        bareu + coord_polar("y", start = 0)
    })
    
    #Hist eu: groei: aandeel elektrische auto's op belgische en eu markt
    output$hist05 <- renderPlot({
        EuMSC2 <- EuMS %>% filter(Year >= min(input$Year8) & Year <= max(input$Year8), Fuel == input$Fuel3)
        EuMSC2 %>% ggplot(aes(x = Year, y = Market.Share)) + geom_col() + labs(title = "Market Share of new", input$Fuel3,"cars in the EU over the years")
    })
    
    #HistMS klanten: aankoopproces
    output$hist06 <- renderPlot({
        aankoopprocesC <- aankoopproces %>% filter(Country %in% input$Country3)
        aankoopprocesC %>% ggplot(aes(x = Country, y = Percentage)) + geom_col(aes(fill = Interest)) + labs(title = "Share of Europeans interested in online vehicle purchasing in 2018" )
    })
    
    #Hist klanten: aankoopproces
    output$hist07 <- renderPlot({
        aankoopprocesC2 <- aankoopproces %>% filter(Country %in% input$Country4, Interest %in% input$Interest)
        aankoopprocesC2 %>% ggplot(aes(x = Country, y = Percentage)) + geom_col() + facet_wrap(Interest~.) + labs(title = "Share of Europeans interested in online vehicle purchasing in 2018" ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    #line verkoop: periodieke tesla verkoop
    output$line04 <- renderPlot({
        DataC <- Data %>% filter(Month >= min(input$Month) & Month <= max(input$Month), Year %in% input$Year9)
        DataC %>% ggplot(aes(x= Month, y = Sales, na.rm = T)) + geom_line(aes(color = Year)) + scale_x_continuous(breaks = seq(0,12, by = 1))
    })
    
    #hist verkoop: periodieke tesla verkoop
    output$hist08 <- renderPlot({
        DataC2 <- Data %>% filter(Month >= min(input$Month) & Month <= max(input$Month), Year %in% input$Year9)
        DataC2 %>% ggplot(aes(x = Month, y = Sales, na.rm = T)) + geom_col() + facet_wrap(Year~.) + labs(title = "Periodic Tesla sales over the years.") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_continuous(breaks = seq(0,12, by = 1))
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
