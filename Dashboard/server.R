library(readxl)
library(shinydashboard)
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
library(shinydashboard)
library(plotly)
library(leaflet)
library(DT)
library(rvest)
library(stringr)
library(tidyverse)

#Caro

#Map + table01 + infoboxen
superchargers <- read_xlsx("data/Superchargers.xlsx")
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

#Lien

#financiele cijfers
Revenue <- read_xlsx("data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Revenues (automotive)", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_Margin <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross margin", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_profit <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross profit", col_types = c("numeric", "text", "numeric", "numeric", "numeric"))
Free_cashflow <- read_xlsx("Data/Tesla's free cash flow by quarter 2020 world wide.xlsx", skip = 3 , sheet = "Data", col_types = c("numeric", "text", "numeric"))
#uitbreiding europa
countriesafpassengercars <- read_xlsx("Data/Countries overview of af passenger cars.xlsx", skip = 2 , col_types = c("numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
countriesafinfrastructure <- read_xlsx("Data/countries overview of af infrastructure.xlsx", skip = 2 , col_types = c("numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))
#financiele cijfers, functies
revenue <- function(yearinput,df) {
  revenue <- df %>% filter(df$Year == yearinput)
  return(revenue)
}


#uitbreiding europa, data in juiste vorm krijgen
countriesafpassengercars <- countriesafpassengercars %>% gather('BEV', 'H2', 'CNG', 'LNG', 'PHEV', 'LPG', 'Total', key = 'Fuel', value = 'waardes')
countriesafpassengercars$Country[1:2457] <- c('Ausria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia',
                                              'Finland', 'France', 'Germany', 'Greece', 'Hungria', 'Ireland', 'Italy', 'Latvia', 'Lithuania',
                                              'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia',
                                              'Spain', 'Sweden')
countriesafinfrastructure <- countriesafinfrastructure %>% gather('Electricity', 'H2', 'Natural Gas', 'LPG', 'Total', key = 'Fuel', value = 'waardes')
countriesafinfrastructure$Country[1:1755] <- c('Ausria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia',
                                               'Finland', 'France', 'Germany', 'Greece', 'Hungria', 'Ireland', 'Italy', 'Latvia', 'Lithuania',
                                               'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia',
                                               'Spain', 'Sweden')

#wereldkaart

teslapercountrysales <- read_xlsx("Data/Verkoop landen tesla.xlsx", skip = 1, col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>% gather('2013', '2014', '2015', '2016', '2017', '2018', '2019', key = 'jaar', value = 'waarde')

some.eu.countries <- c('Ukraine', 'France', 'Spain', 'Sweden', 'Norway', 'Germany', 'Finland', 'Poland', 'Italy', 'UK', 'Romania', 'Belarus', 'Greece', 'Bulgaria', 'Iceland', 'Hungary', 'Portugal', 'Austria', 'Czech Republic', 'Serbia', 'Ireland', 'Lithuania', 'Latvia', 'Croatia', 'Bosnia and Herzegovina', 'Slovakia', 'Estonia', 'Denmark', 'Netherlands', 'Switzerland', 'Moldova', 'Belgium', 'Armenia', 'Albania', 'Macedonia', 'Turkey', 'Slovenia', 'Montenegro', 'Kosovo', 'Cyprus', 'Luxembourg', 'Georgia', 'Andorra', 'Malta', 'Liechtenstein', 'San Marino', 'Monaco', 'Vatican')

some.eu.map <- map_data("world", region = some.eu.countries)
tesla.eu.map <- left_join(some.eu.map, teslapercountrysales, by = "region")


#Pieter

# Customers: loyalty

  # Keep for now
  # loyalty_per_brand_data <- read_xlsx("Data/loyalty_per_brand_v2.xlsx", skip = 2)
  
  # New
  loyalty_per_brand_data <- read_xlsx("Data/loyalty_per_brand_v3.xlsx", skip = 2)
  
  # Make tibble (already was, just to be sure)
  loyalty_per_brand_tibble = as_tibble(loyalty_per_brand_data)
  
  # Change to numeric (already was, but just to be sure)
  loyalty_per_brand_tibble$Percentage <- as.numeric(loyalty_per_brand_tibble$Percentage)
  
  # Clean names
  colnames(loyalty_per_brand_tibble) <- c("Ranking", "Brand", "Percentage", "Classification")
  
  # Reverse order (high to low)
  loyalty_per_brand_tibble <- loyalty_per_brand_tibble[order(loyalty_per_brand_tibble$Percentage), ]
  
  # To retain the order in the plot
  loyalty_per_brand_tibble$Brand <- factor(loyalty_per_brand_tibble$Brand,
                                       levels = loyalty_per_brand_tibble$Brand)
  
# Growth: Comparison
  
  growth_comp_data_5 <- read_xlsx("Data/growth_comparison_v5.xlsx")
  
  # Placeholder for presentation 10-11-20
    # Select
    growth_comp_sales_2019_1 <- growth_comp_data_5 %>% 
                                  select(c("Submodel", "2019")) %>%
                                  drop_na("2019") %>%
                                  # Drop others and segment total
                                  drop_na("Submodel")
    
    # To retain the order in the plot
    growth_comp_sales_2019_1$"2019" <- factor(growth_comp_sales_2019_1$"2019",
                                       levels = growth_comp_sales_2019_1$"2019")
    
  
#jaske

eusurvey <- read.csv("data/hev1.csv")

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
    leaflet() %>% addTiles() %>% addMarkers(data = superchargers, layerId = as.character(superchargers$id) )
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
      subtitle= "Total number of supercharger stations", color = "red"
    )})
  output$openbox <- renderValueBox({
    valueBox(
      paste0(aantal$freq[aantal$Status == "OPEN"]),
      subtitle= "Number of open supercharger stations", color = "red"
    )})
  output$buildbox <- renderValueBox({
    valueBox(
      paste0(aantal$freq[aantal$Status == "CONSTRUCTION"]),
      subtitle= "Number of building supercharger stations", color = "red"
    )})
  output$permitbox <- renderValueBox({
    valueBox(
      paste0(aantal$freq[aantal$Status == "PERMIT"]),
      subtitle= "Number of permit supercharger stations", color = "red"
    )})
  output$pclosedbox <- renderValueBox({
    valueBox(
      paste0(aantal$freq[aantal$Status == "CLOSED_PERM"]),
      subtitle= "Number of permantly closed supercharger stations", color = "red"
    )})
  output$tclosedbox <- renderValueBox({
    valueBox(
      paste0(aantal$freq[aantal$Status == "CLOSED_TEMP"]),
      subtitle= "Number of temporarly closed supercharger stations", color = "red"
    )})
  
  #histogram: vergelijken met teslaverkoop 
  output$hist01 <- renderPlotly({
    verkooC <- verkoo %>% dplyr::filter(Country %in% input$Country, Year == input$Year)
    superchargersC <- superchargers %>% dplyr::filter(Year < input$Year+1, Status == 'OPEN', Country %in% input$Country)
    superchargersC <- plyr::count(superchargersC, "Country")
    ratio <- full_join(superchargersC, verkooC, by = 'Country')
    ratio$freq <- as.integer(ratio$freq)
    ratio$Country <- as.factor(ratio$Country)
    ratio <- ratio %>% mutate(Teslas_per_Supercharger = Sales/freq)
    ratio$Teslas_per_Supercharger <- as.double(ratio$Teslas_per_Supercharger)
    h1 <- ratio %>% ggplot(aes(x= Country, y = Teslas_per_Supercharger)) + geom_col() + labs(title = paste0("Teslas/supercharger station in ", input$Year)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits = c(0, 1400), breaks = seq(0,1400, by= 200)) + ylab(label = "Teslas per supercharger station" ) + theme_minimal()
    ggplotly(h1)
  })
  
  #histogram: concurrentie snellaadpalen
  output$hist02 <- renderPlotly({
    laadpalenC <- laadpalen %>% filter(Country %in% input$Country2)
    h2 <- laadpalenC %>% ggplot(aes(x = Description, y = freq)) + geom_col() + facet_wrap(Country~.)+ theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0,100, by= 20)) + ylab("Number of supercharger stations") + xlab("Brand") + theme_minimal()
    ggplotly(h2)})
  output$hist03 <- renderPlotly({
    laadpalenC <- laadpalen %>% filter(Country %in% input$Country2)
    h3 <- laadpalenC %>% ggplot(aes(x = Country, y = freq)) + geom_col(aes(fill = Description)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits = c(0, 200), breaks = seq(0,200, by= 50)) + ylab("Number of supercharger stations") + theme_minimal()
    ggplotly(h3)})
  
  #taartdiagram: concurrentie snellaadpalen
  output$pie01 <- renderPlotly({
    fig <- plot_ly(taart, labels = ~Description, values = ~ratio, type = 'pie')
    fig <- fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))})
  
  
  #lijngrafiek: Groei: verkoop alle merken per segment
  output$line01 <- renderPlotly({
    VPSC2 <- VPS %>% filter(Segment %in% input$Segment)
    p <- VPSC2 %>% ggplot(aes(x=Year, y=Sales)) + geom_line(aes(color = Segment)) + labs(title = "New cars sold in the EU by segment in million units over the years.") + 
      scale_x_continuous(breaks = c(2008:2019)) + scale_y_continuous(breaks= seq(0,6, by = 1)) + ylab("Cars sold") + theme_minimal()
    ggplotly(p)})
  #histogram: groei: verkoop alle merken per segment
  output$hist04 <- renderPlotly({
    VPSC <- VPS %>% filter(Segment %in% input$Segment2, Year >= min(input$Year2) & Year <= max(input$Year2))
    h4 <- VPSC %>% ggplot(aes(x = Segment, y = Sales)) + geom_col() + facet_wrap(Year~., ncol = 6, nrow = 2) + 
      labs(title = "New cars sold in the EU by segment in million units for each year.") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits = c(0,6), breaks = seq(0,6, by= 1)) + ylab("Cars sold") 
    ggplotly(h4)})
  
  #lijn nieuw: groei: aandeel elektrische auto's op belgische en eu markt
  checkregion <- reactive({input$Region})
  output$line02 <- renderPlotly({
    if(checkregion() == 1) {
      NieuwC <- Nieuw %>% filter(Year >= min(input$Year3) & Year <= max(input$Year3), Fuel %in% input$Fuel)
      p2 <- NieuwC %>% ggplot(aes(x = Year, y = `Cars sold`)) + geom_line(aes(color = Fuel), size = 1) + labs(title = "Number of new cars sold in Belgium over the years") + theme_minimal()
      ggplotly(p2)
    }
    else{
      #lijn tweedehands: groei: aandeel elektrische auto's op belgische en eu markt
      TweedehandsC <- Tweedehands %>% filter(Year >= min(input$Year3) & Year <= max(input$Year3), Fuel %in% input$Fuel)
      p3 <- TweedehandsC %>% ggplot(aes(x = Year, y = `Cars sold`)) + geom_line(aes(color = Fuel), size = 1) + labs(title = "Number of second hand cars sold in Belgium over the years") + theme_minimal()
      ggplotly(p3)
    }
  })
  
  #taart nieuw: groei: aandeel elektrische auto's op belgische en eu markt
  checktype <- reactive({input$Region2})
  output$pie02 <- renderPlotly({
    if(checktype() == 1){
      NieuwMSC <- NieuwMS %>% filter(Year == input$Year5)
      fig1 <- plot_ly(NieuwMSC, labels = ~Fuel, values = ~Market.Share, type = 'pie')
      fig1 <- fig1 %>% layout(title = "Market share of new cars by fuel type in Belgium",
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    else{
      #taart tweedehands: groei: aandeel elektrische auto's op belgische en eu markt
      TweedehandsMSC <- TweedehandsMS %>% filter(Year == input$Year5)
      fig2 <- plot_ly(TweedehandsMSC, labels = ~Fuel, values = ~Market.Share, type = 'pie')
      fig2 <- fig2 %>% layout(title = "Market share of second hand cars by fuel type in Belgium",
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
  })
  
  #taart eu: groei: aandeel elektrische auto's op belgische en eu markt
  output$pie04 <- renderPlotly({
    EuMSC <- EuMS %>% filter(Year == input$Year7)
    fig3 <- plot_ly(EuMSC, labels = ~Fuel, values = ~Market.Share, type = 'pie')
    fig3 <- fig3 %>% layout(title = "Market share of new cars by fuel type in the EU",
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))})
  
  #Hist eu: groei: aandeel elektrische auto's op belgische en eu markt
  output$hist05 <- renderPlotly({
    EuMSC2 <- EuMS %>% filter(Year >= min(input$Year8) & Year <= max(input$Year8), Fuel == input$Fuel3)
    h5 <- EuMSC2 %>% ggplot(aes(x = Year, y = Market.Share)) + geom_col() + labs(title = "Market Share of new cars in the EU over the years", input$Fuel3,"cars in the EU over the years") +
      scale_y_continuous(limits = c(0, 60), breaks = seq(0,60, by= 10)) + theme_minimal()
    ggplotly(h5)})
  
  #HistMS klanten: aankoopproces
  output$hist06 <- renderPlotly({
    aankoopprocesC <- aankoopproces %>% filter(Country %in% input$Country3)
    h6 <- aankoopprocesC %>% ggplot(aes(x = Country, y = Percentage)) + geom_col(aes(fill = Interest)) + labs(title = "Share of Europeans interested in online vehicle purchasing in 2018" ) + theme_minimal()
    ggplotly(h6)})
  
  #Hist klanten: aankoopproces
  output$hist07 <- renderPlotly({
    aankoopprocesC2 <- aankoopproces %>% filter(Country %in% input$Country4, Interest %in% input$Interest)
    h7 <- aankoopprocesC2 %>% ggplot(aes(x = Country, y = Percentage)) + geom_col() + facet_wrap(Interest~.) + labs(title = "Share of Europeans interested in online vehicle purchasing in 2018" ) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits = c(0, 70), breaks = seq(0,70, by= 10)) + theme_minimal()
    ggplotly(h7)})
  
  #line verkoop: periodieke tesla verkoop
  output$line04 <- renderPlotly({
    DataC <- Data %>% filter(Month >= min(input$Month) & Month <= max(input$Month), Year %in% input$Year9)
    p4 <- DataC %>% ggplot(aes(x= Month, y = Sales, na.rm = T)) + geom_line(aes(color = Year)) + scale_x_continuous(breaks = seq(0,12, by = 1))
    ggplotly(p4)
  })
  
  #hist verkoop: periodieke tesla verkoop
  output$hist08 <- renderPlotly({
    DataC2 <- Data %>% filter(Month >= min(input$Month) & Month <= max(input$Month), Year %in% input$Year9)
    h8 <- DataC2 %>% ggplot(aes(x = Month, y = Sales, na.rm = T)) + geom_col() + facet_wrap(Year~.) + labs(title = "Periodic Tesla sales over the years.") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_continuous(breaks = seq(0,12, by = 1)) +
      scale_y_continuous(limits = c(0, 25000), breaks = seq(0,25000, by= 5000)) + ylab("Sales") + theme_minimal()
    ggplotly(h8)})
  
  
  #financieel tabblad
  sortofgraph <- reactive({input$Quarterly})
  
  output$revbox <- renderValueBox({
    if (sortofgraph() == TRUE) {
      valueBox(
        paste0(format(round(sum(Revenue$`Automotive Revenues Tesla`[Revenue$Year == input$Yearrev], na.rm = TRUE)/1000000, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle= paste0("Revenue ", input$Yearrev, " in million"), 
        icon = icon("dollar-sign"), color = 'red'
      )
    }
    else {
      
      somjaren <- Revenue %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% summarize(totaal = sum(`Automotive Revenues Tesla`, na.rm = TRUE))
      valueBox(
        paste0(format(round(sum(somjaren$totaal, na.rm = TRUE)/1000000, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle= paste0("Revenue from ", min(input$Yearrevline), " until ", max(input$Yearrevline), " in million"), 
        icon = icon("dollar-sign"), color = 'red'
      )
    }
  })
  
  output$frcashbox <- renderValueBox({
    if (sortofgraph() == TRUE) {
      valueBox(
        paste0(format(round(sum(Free_cashflow$`free cash flow`[Free_cashflow$Year == input$Yearrev], na.rm = TRUE)/1000000, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle = paste0("Free cashflow ", input$Yearrev, " in million"), 
        icon = icon("dollar-sign"), color = 'red'
      )
    }
    else {
      somjaren <- Free_cashflow %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% summarize(totaal = sum(`free cash flow`, na.rm = TRUE))
      
      valueBox(
        paste0(format(round(sum(somjaren$totaal, na.rm = TRUE)/1000000, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle = paste0("Free cashflow from ", min(input$Yearrevline), " until ", max(input$Yearrevline), " in million"), 
        icon = icon("dollar-sign"), color = 'red'
      )
    }
  })
  
  output$grprbox <- renderValueBox({
    if (sortofgraph() == TRUE) {
      valueBox(
        paste0(format(round(sum(Gross_profit$`Automotive gross profit GAAP`[Gross_profit$Year == input$Yearrev], na.rm = TRUE)/1000000, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)), 
        subtitle = paste0("Gross profit ", input$Yearrev, " in million"),  
        icon = icon("piggy-bank"), color = 'red'
      )
    }
    else {
      somjaren <- Gross_profit %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% summarize(totaal = sum(`Automotive gross profit GAAP`, na.rm = TRUE))
      valueBox(
        paste0(format(round(sum(somjaren$totaal, na.rm = TRUE)/1000000, 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle = paste0("Gross profit from ", min(input$Yearrevline), " until ", max(input$Yearrevline), " in million"),  
        icon = icon("piggy-bank"), color = 'red'
      )
    }
  })
  
  
  output$colrev <- renderPlotly({
    if (sortofgraph() == TRUE) {
      
      # generate bins based on input$bins from ui.R
      x    <- Revenue$Year
      Yearrev <- seq(min(x), max(x), length.out = input$Yearrev)
      
      revvar <- revenue(input$Yearrev, Revenue)
      Automotive_Revenue <- revvar$`Automotive Revenues Tesla`/1000000
      
      revvarp <- revvar %>% ggplot(aes(x = Quarter, y = Automotive_Revenue))+ geom_col() + 
        labs(title = input$Yearrev, y = 'Automotive revenue')  +  scale_y_continuous(limits = c(0, 8000), breaks = seq(0,8000, by= 1000)) +
        theme_minimal()
      
    }
    else {
      y    <- Revenue$Year
      Yearrevline <- seq(min(y), max(y))
      revvar <- Revenue %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% 
        mutate("total" = sum(`Automotive Revenues Tesla`, na.rm = TRUE)/1000000) %>% select(Year, total)%>% distinct()
      
      revvarp <- revvar %>% ggplot(aes(x = Year , y = total))+ geom_line() + geom_point() +
        labs(y = 'Automotive revenue') + scale_y_continuous(limits = c(0, 21000), breaks = seq(0, 21000, by = 5000)) +
        theme_minimal()
    }
    ggplotly(revvarp)
  })
  
  output$colfrcash <- renderPlotly({
    if (sortofgraph() == TRUE) {
      freecashvar <- revenue(input$Yearrev, Free_cashflow)
      free_cash_flow <- freecashvar$`free cash flow`/1000000
      
      freecashvarp <- freecashvar %>% ggplot(aes(x= Quarter, y= free_cash_flow)) + 
        geom_col() + 
        labs(title = input$Yearrev, y = 'Free cash flow') +  scale_y_continuous(limits = c(-1500, 1500), breaks = seq(-1500,1500, by = 500)) + 
        theme_minimal()
      
    }
    else {
      freecashvar <-  Free_cashflow %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% 
        mutate("total" = sum(`free cash flow`, na.rm = TRUE)/1000000) %>% select(Year, total)%>% distinct()
      
      freecashvarp <- freecashvar %>% ggplot(aes(x = Year , y = total))+ 
        geom_line() + geom_point() + 
        labs(y = 'Free cash flow')  + scale_y_continuous(limits = c(-4000, 1300), breaks = seq(-4000, 1300, by= 1000)) + 
        theme_minimal()
      
    }
    ggplotly(freecashvarp)
  })
  
  output$colgrpr <- renderPlotly({
    if (sortofgraph() == TRUE) {
      grossprofitvar <- revenue(input$Yearrev, Gross_profit) 
      gross_profit <-grossprofitvar$`Automotive gross profit GAAP`/1000000
     
      grossprofitvarp <- grossprofitvar %>% 
        ggplot(aes(x = Quarter, y = gross_profit)) + 
        geom_col() +  
        labs(title = input$Yearrev, y= 'Gross profit') + 
        scale_y_continuous(limits = c(0,2500), breaks = seq(0,2500, by= 500)) + theme_minimal()
      
    }
    else {
      grossprofitvar <- Gross_profit %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% 
        mutate("total" = sum(`Automotive gross profit GAAP`, na.rm = TRUE)/1000000) %>% select(Year, total)%>% distinct() 
      
      grossprofitvarp <-grossprofitvar %>% ggplot(aes(x = Year , y = total)) + 
        geom_line() + geom_point() + 
        labs(y = "Gross profit")  + scale_y_continuous(limits = c(0, 5000), breaks = seq(0, 5000, by= 1000)) + 
        theme_minimal()
      
    }
    ggplotly(grossprofitvarp)
  })
  
  output$colgrmar <- renderPlotly({
    if (sortofgraph() == TRUE) {
      grossmarginvar <- revenue(input$Yearrev, Gross_Margin)
      gross_margin <- grossmarginvar$`Gross margin Automotive GAAP`
      
      grossmarginvarp <- grossmarginvar %>% 
        ggplot(aes(x = Quarter, y = gross_margin)) + 
        geom_col() + 
        labs(title = input$Yearrev, y= 'Gross margin') + 
        scale_y_continuous(limits = c(0,30), breaks = seq(0, 30, by= 5)) + theme_minimal()
      
    }
    else {
      grossmarginvar <- Gross_Margin %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline)) %>% group_by(Year) %>% 
        mutate("total" = sum(`Gross margin Automotive GAAP`, na.rm = TRUE)) %>% select(Year, total)%>% distinct()
      
      grossmarginvarp <- grossmarginvar %>% ggplot(aes(x = Year , y = total)) + 
        geom_line() + geom_point() + 
        labs(y = "Gross margin")  + scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by= 10)) + 
        theme_minimal()
      
    }
    ggplotly(grossmarginvarp)
  })
  
  #Uitbreiding naar de EU
  checkeurope <- reactive({input$Europe})
  output$colpascar <- renderPlotly({
    if (checkeurope() == 2) {
      countriespasscarvar <- countriesafpassengercars %>% filter(Country == input$EUoptions, Fuel %in% input$EUcheck)
      value <- countriespasscarvar$waardes
      
      
      countriespasscarvarp <- countriespasscarvar %>% 
        ggplot(aes(x = Year, y = value, fill = Fuel)) + 
        geom_col(position = "dodge") + 
        labs(title = input$EUoptions, y = '')  + 
        scale_x_continuous(limits= c(min(countriesafpassengercars$Year), max(countriesafpassengercars$Year)) , 
                           breaks = seq(min(countriesafpassengercars$Year), max(countriesafpassengercars$Year), by = 1)) +
        theme_minimal()
      
    }
    else { 
      countriespasscarvar <- countriesafpassengercars %>% filter(Fuel %in% input$EUcheck, Year == input$YearEU)
      value <- countriespasscarvar$waardes
      
      
      
      countriespasscarvarp <- countriespasscarvar %>% 
        ggplot(aes(x = Country, y = value, fill = Fuel ))+ 
        geom_col(position = "dodge") + 
        labs(title = input$YearEU, y = '')  + scale_y_continuous(limits = c(0, 3600000), breaks = seq(0,4000000, by= 500000)) +
        coord_flip() + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    }
    ggplotly(countriespasscarvarp)
  })
  output$colinfr <- renderPlotly({
    if (checkeurope() == 2) {
      countriesinfrvar <- countriesafinfrastructure %>% filter(Country == input$EUoptions, Fuel %in% input$EUcheckinfr)
      value <- countriesinfrvar$waardes
      
      countriesinfrvarp <- countriesinfrvar %>% 
        ggplot(aes(x = Year, y = value, fill = Fuel))+ 
        geom_col(position = "dodge") + 
        labs(title = input$EUoptions, y = '')  + 
        scale_x_continuous(limits = c(min(countriesafinfrastructure$Year), max(countriesafinfrastructure$Year)), 
                           breaks = seq(min(countriesafinfrastructure$Year), max(countriesafinfrastructure$Year), by = 1)) + 
        theme_minimal()
      
    }
    else {
      countriesinfrvar <- countriesafinfrastructure %>% filter(Fuel %in% input$EUcheckinfr, Year == input$YearEU)
      value <- countriesinfrvar$waardes
      
      countriesinfrvarp <- countriesinfrvar %>% 
        ggplot(aes(x = Country, y = value, fill = Fuel))+ 
        geom_col(position = "dodge") + 
        labs(title = input$YearEU, y = '')  + scale_y_continuous(limits = c(0, 65000), breaks = seq(0,65000, by= 5000)) +
        coord_flip() + theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    }
    ggplotly(countriesinfrvarp)
  })
  output$distPlot <- renderPlot({
    teslamap <- function(inputjaar, df) {
      if (inputjaar == "2013") {
        teslamap <- df %>% filter(df$jaar == inputjaar, df$waarde > 0)
      }
      else {
        teslamap <- df %>% filter(df$jaar == c(inputjaar, (as.numeric(inputjaar) - 1)), df$waarde > 0)  
      }
      return(teslamap)
    }
    
    teslamapvar <- teslamap(input$teslajaar, tesla.eu.map)
    gg <- teslamapvar %>% ggplot() + geom_map(dat = tesla.eu.map, map = tesla.eu.map, aes(map_id = region), fill = "white", color="black") + coord_map(ylim = c(35, 71))
    if (input$teslajaar == "2013") {
      gg <- gg + geom_map(map = tesla.eu.map, aes(map_id = region, fill = jaar), colour = "black")
    }
    else {
      gg <- gg + geom_map(map = tesla.eu.map, aes(map_id = region, fill = jaar), colour = "black")
    }
    gg <- gg + expand_limits(x = tesla.eu.map$long, y = tesla.eu.map$lat)
    gg <- gg + theme(axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     legend.position = "none",
                     panel.border = element_blank(),
                     strip.background = element_rect(fill = 'white', colour = 'white')) + scale_fill_manual( values = c("tomato", "skyblue"))
            
    gg
  })
    
    output$histogram_growth <- renderPlot({
      hist(faithful$eruptions, breaks = input$bins_growth)
    })
    
    # Loyalty
    output$loyalty_bar <- renderPlot({
      
      # Create plot
      loyalty_per_brand_plot <- ggplot(loyalty_per_brand_tibble,
                                       aes(x = Percentage,
                                           y = Brand)) +
        geom_bar(stat = "identity",
                 fill = "tomato3") +
        theme(axis.text.y = element_text(vjust=0.6)) + theme_minimal()
      
      # Te doen:
      # - Tesla in andere kleur (Puurder rood, rest mss in zachter rood, om toch in stijl te blijven)
      # - Percentages ipv. 0.6 enzo
      # - Percentages schaal tot 100%
      # - (Optioneel) Namen brands groter
      # - Ggplotly zodat je precieze percentage ook ziet. Dan kan mogelijk checkbox zelfs weg.(Want wil ...
      #   ... kunnen filteren op luxury/mass market of beiden). Mss voegt plotly ook toe dat merken kan ...
      #   ... kiezen, anders eventueel zelf toevoegen
      
      # Print plot
      loyalty_per_brand_plot
    })
    
    output$ggcountry <- renderPlotly({
      f2 <- eusurvey %>% filter(Country %in% input$gcountry)
      p2 <- f2 %>% ggplot(aes(Gender)) + 
        geom_bar(aes(fill = buy_electric), position = "dodge") +
        facet_wrap(~Country) + 
        labs(y = "Number of respondents", fill = "Buy EV")
      ggplotly(p2) %>% 
        layout( 
          xaxis = list(automargin=TRUE), 
          yaxis = list(automargin=TRUE)
        )
    })
    
    output$country <- renderDataTable({
      t1 <- eusurvey %>% filter(Country == input$country) %>%
        group_by(Country, Gender, buy_electric) %>% 
        summarise(median(Age), n = n()) %>% 
        arrange(desc(n))
      datatable(t1, filter = "top")
    })
    
    output$view <- renderPlotly({
      f3 <- eusurvey %>% filter(Country == input$incountry, Income_group == input$incomegr)
      p3 <- f3 %>% ggplot(aes(Income_group)) + 
        geom_bar(aes(fill = buy_electric), position = "dodge") +
        labs(y = "Number of respondents", fill = "Buy EV") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
      ggplotly(p3)
    })
    
    output$employ <- renderPlotly({
      f1 <- eusurvey %>% filter(Employment_status %in% input$estatus)
      p1 <- f1 %>% ggplot(aes(Employment_status)) +
        geom_bar(aes(fill = buy_electric), position = "dodge") +
        labs(y = "Number of respondents", fill = "Buy EV") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
      ggplotly(p1) %>% 
        layout( 
          xaxis = list(automargin=TRUE), 
          yaxis = list(automargin=TRUE)
        )
    })
    
    output$plan <- renderPlotly({
      f4 <- eusurvey %>% filter (Country %in% input$carplancountry)
      p4 <- f4 %>% ggplot(aes(Plan_to_purchase_vehicle)) + 
        geom_bar(aes(fill = buy_electric), position = "dodge") + 
        labs(y = "Number of respondents", x = "Plan to buy car", fill = "Buy EV") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
      ggplotly(p4)
      
    })
    
    output$propev <- renderPlotly({
      teslacountries <- c("Austria", "Belgium", "Czech Republic", "Denmark", 
                          "Finland", "France", "Germany", "Ireland", "Italy", 
                          "Luxembourg", "Netherlands", "Norway", "Slovenia", 
                          "Spain", "Sweden", "Switzerland")
      
      eusurvey$tesla_sold <- ifelse(eusurvey$Country %in% teslacountries, 1, 0) 
      eusurvey <- eusurvey %>% mutate(tesla_sold = as.logical(tesla_sold))
      
      ggplotly(eusurvey %>% group_by(Country, tesla_sold) %>% summarize(n=n(),prop=sum(buy_electric==1)/n()) %>%
                 ggplot(aes(Country, prop)) + geom_point(aes(color = tesla_sold)) + 
                 labs(y = "Percentage of people willing to buy ev", x = "Countries", color = "Tesla sold") +
                 theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
                 scale_color_manual(values=c("black", "red"))
      )
    })
    
    output$surveytotal <- renderValueBox({
      valueBox(
        nrow(eusurvey), subtitle = "Number of respondents", icon = icon("user-alt"), color = 'red'
      )
    })
    
    output$totalcountries <- renderValueBox({
      valueBox(
        length(unique(eusurvey$Country)), subtitle = "Number of countries",
        icon = icon("globe-europe"), color = 'red'
      )
    })
  
})

  

