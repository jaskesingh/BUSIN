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
library(gghighlight)

# Toegevoegd op 13/11
library(scales)
library(ggExtra)

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
aankoopproces$Interest <- ordered(aankoopproces$Interest, levels = c("Not at all interested/not very interested", "Neutral", "Somewhat interested/very interested"))

#Verkoop: periodieke tesla verkoop
data <- read_xlsx("Data/Monthly Tesla Vehicle Sales.xlsx")
Data <- data %>% gather(January:December, key=  "Month", value="Sales") %>% mutate(Month = str_replace(Month, "January", "1"), Month = str_replace(Month, "February", "2"), Month = str_replace(Month, "March", "3"), Month = str_replace(Month, "April", "4"), Month = str_replace(Month, "May", "5"), Month = str_replace(Month, "June", "6"), Month = str_replace(Month, "July", "7"), Month = str_replace(Month, "August", "8"), Month = str_replace(Month, "September", "9"), Month = str_replace(Month, "October", "10"), Month = str_replace(Month, "November", "11"), Month = str_replace(Month, "December", "12"))
Data$Month <- as.integer(Data$Month)
Data$Year <- as.factor(Data$Year) 

#Lien

#finance

Revenue <- read_xlsx("data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Revenues (automotive)", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_Margin <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross margin", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_profit <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross profit", col_types = c("numeric", "text", "numeric", "numeric", "numeric"))
Free_cashflow <- read_xlsx("Data/Tesla's free cash flow by quarter 2020 world wide.xlsx", skip = 3 , sheet = "Data", col_types = c("numeric", "text", "numeric"))

##cleaning
Revenuetabel <- Revenue %>% group_by(Year) %>% 
  mutate("totalrevenue" = sum(Revenue, na.rm = TRUE)/1000000)
Free_cashflow <- Free_cashflow %>% group_by(Year) %>% 
  mutate("totalfreecashflow" = sum(`free cash flow`, na.rm = TRUE)/1000000)
Gross_profit <- Gross_profit %>% group_by(Year) %>% 
  mutate("totalgrossprofit" = sum(`Gross Profit`, na.rm = TRUE)/1000000)
Gross_Margin <- Gross_Margin %>% group_by(Year) %>% 
  mutate("totalgrossmargin" = sum(`Gross Margin`, na.rm = TRUE))

Revenuetabel <- Revenuetabel %>% unite(Year, Quarter, col = "Date", sep = " ") 
Gross_profit <- Gross_profit %>% unite(Year, Quarter, col = "Date", sep = " ") 
Free_cashflow <- Free_cashflow %>% unite(Year, Quarter, col = "Date", sep = " ") 
Gross_Margin <- Gross_Margin %>% unite(Year, Quarter, col = "Date", sep = " ") 


Revenuetabelnorm <- Revenuetabel %>% select(Date, Revenue)
Gross_profitnorm <- Gross_profit %>% select(Date, `Gross Profit`)
Gross_Marginnorm <- Gross_Margin %>% select(Date, `Gross Margin`) 
Free_cashflownorm <- Free_cashflow %>% select(Date, `free cash flow`)

Revenuetabelsom <- Revenuetabel %>% select(Date, totalrevenue)
Gross_profitsom <- Gross_profit %>% select(Date, totalgrossprofit)
Gross_Marginsom <- Gross_Margin %>% select(Date, totalgrossmargin)
Free_cashflowsom <- Free_cashflow %>% select(Date, totalfreecashflow)

Financial_numbersnorm <- left_join(Revenuetabelnorm, Gross_profitnorm, by = "Date")
Financial_numbersnorm <- left_join(Financial_numbersnorm, Gross_Marginnorm, by = "Date")
Financial_numbersnorm <- left_join(Financial_numbersnorm, Free_cashflownorm, by = "Date")

Financial_numbersnorm <- Financial_numbersnorm %>% separate(Date, sep = " ", into = c("Year", "Quarter"))
Financial_numbersnorm$'Year' <- as.numeric(Financial_numbersnorm$'Year')
Financial_numbersnorm$'Revenue' <- as.numeric(Financial_numbersnorm$'Revenue')
Financial_numbersnorm$'Gross Profit' <- as.numeric(Financial_numbersnorm$'Gross Profit')
Financial_numbersnorm$'Gross Margin' <- as.numeric(Financial_numbersnorm$'Gross Margin')
Financial_numbersnorm$'free cash flow' <- as.numeric(Financial_numbersnorm$'free cash flow')

Financial_numbers_gather_norm <- Financial_numbersnorm %>% gather('Revenue', 'Gross Profit', 'Gross Margin', 'free cash flow', key = 'typenumber', value = 'finvalue')

Financial_numberssom <- left_join(Revenuetabelsom, Gross_profitsom, by = "Date")
Financial_numberssom <- left_join(Financial_numberssom, Gross_Marginsom, by = "Date")
Financial_numberssom <- left_join(Financial_numberssom, Free_cashflowsom, by = "Date")

Financial_numberssom <- Financial_numberssom %>% separate(Date, sep = " ", into = c("Year", "Quarter"))
Financial_numberssom$'Year' <- as.numeric(Financial_numberssom$'Year')
Financial_numberssom$'totalrevenue' <- as.numeric(Financial_numberssom$'totalrevenue')
Financial_numberssom$'totalgrossprofit' <- as.numeric(Financial_numberssom$'totalgrossprofit')
Financial_numberssom$'totalgrossmargin' <- as.numeric(Financial_numberssom$'totalgrossmargin')
Financial_numberssom$'totalfreecashflow' <- as.numeric(Financial_numberssom$'totalfreecashflow')

Financial_numbers_gather_som <- Financial_numberssom %>% gather('totalrevenue', 'totalgrossprofit', 'totalgrossmargin', 'totalfreecashflow', key = 'typenumber', value = 'finvalue') %>% select(Year, typenumber, finvalue) %>% distinct()

##financiele cijfers, functies
financefunction <- function(yearinput,df) {
  financefunction <- df %>% filter(df$Year == yearinput)
  return(financefunction)
}

#uitbreiding europa
countriesafpassengercars <- read_xlsx("Data/Countries overview of af passenger cars.xlsx", skip = 2 , col_types = c("numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
countriesafinfrastructure <- read_xlsx("Data/countries overview of af infrastructure.xlsx", skip = 2 , col_types = c("numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric"))
countriesafinfrastructure <- countriesafinfrastructure %>% mutate("Electricity (BEV + PHEV)" = Electricity,  "Natural Gas (CNG + LNG)" = `Natural Gas`)
countriesafinfrastructure <- countriesafinfrastructure %>% select("Year", "Country", "Electricity (BEV + PHEV)", "H2", "Natural Gas (CNG + LNG)", "LPG", "Total")

#uitbreiding europa, data in juiste vorm krijgen
countriesafpassengercars <- countriesafpassengercars %>% gather('BEV', 'H2', 'CNG', 'LNG', 'PHEV', 'LPG', 'Total', key = 'Fuel', value = 'waardes')
countriesafpassengercars$Country[1:2457] <- c('Ausria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia',
                                              'Finland', 'France', 'Germany', 'Greece', 'Hungria', 'Ireland', 'Italy', 'Latvia', 'Lithuania',
                                              'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia',
                                              'Spain', 'Sweden')
countriesafinfrastructure <- countriesafinfrastructure %>% gather('Electricity (BEV + PHEV)', 'H2', 'Natural Gas (CNG + LNG)', 'LPG', 'Total', key = 'Fuel', value = 'waardes')
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
  
  # Load data
  loyalty_per_brand_data <- read_xlsx("Data/loyalty_per_brand_v4.xlsx", skip = 2)
  
  # Make tibble (already was, but just to be sure)
  loyalty_per_brand_tibble = as_tibble(loyalty_per_brand_data)
  
  # Change to numeric (already was, but just to be sure)
  loyalty_per_brand_tibble$Percentage <- as.numeric(loyalty_per_brand_tibble$Percentage)
  
  # Clean names
  colnames(loyalty_per_brand_tibble) <- c("Ranking", "Brand", "Percentage", "Classification")
  
  # Select columns that will be needed
  loyalty_per_brand_tibble <- loyalty_per_brand_tibble %>%
                              select(-Ranking)
  
  # Select row with Tesla to later add to both luxury and mass market
  loyalty_per_brand_Tesla <- loyalty_per_brand_tibble %>% filter(Brand == "Tesla")
  
  
# # Growth: Comparison
# 
#   # growth_comp_data_5 <- read_xlsx("Dashboard/Data/growth_comparison_v5.xlsx")
#   # View(growth_comp_data_5)
# 
#   # Ik denk correctere versie
#   # growth_comp_data_5 <- read_xlsx("Data/growth_comparison_v5.xlsx")
# 
#   
#   # Placeholder for presentation 10-11-20
#     # Select
#     growth_comp_sales_2019_1 <- growth_comp_data_5 %>% 
#                                   select(c("Submodel", "2019")) %>%
#                                   drop_na("2019") %>%
#                                   # Drop others and segment total
#                                   drop_na("Submodel")
#     
#     # To retain the order in the plot
#     growth_comp_sales_2019_1$"2019" <- factor(growth_comp_sales_2019_1$"2019",
#                                        levels = growth_comp_sales_2019_1$"2019")
    
  
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
  
  
  #infobox best verkocht segment: groei: verkoop alle merken per segment
  output$bestsoldsegment <- renderValueBox({
    VPSC2 <- VPS %>% filter(Segment %in% input$Segment, Year == max(input$Year2))
    valueBox(
      paste0(VPSC2$Segment[VPSC2$Sales == max(VPSC2$Sales)]),
      subtitle= paste("Best sold segment in ", max(input$Year2)), color = "red"
    )})
  
  #lijngrafiek: Groei: verkoop alle merken per segment
  output$line01 <- renderPlotly({
    VPSC2 <- VPS %>% filter(Segment %in% input$Segment, Year >= min(input$Year2) & Year <= max(input$Year2))
    p <- VPSC2 %>% ggplot(aes(x=Year, y=Sales)) + geom_line(aes(color = Segment)) + labs(title = "New cars sold in the EU by segment in million units over the years.") + 
      scale_x_continuous(breaks = c(2008:2019)) + scale_y_continuous(breaks= seq(0,6, by = 1)) + ylab("Cars sold") + theme_minimal()
    ggplotly(p)})
  
  #infobox best verkocht brandstof: groei: aandeel elektrische auto's op belgische en eu markt
  output$bestsoldfuel <- renderValueBox({
    if(checkregion() == 1){
      NieuwC <- Nieuw %>% filter(Fuel %in% input$Fuel, Year == max(input$Year3))
      valueBox(
        paste0(NieuwC$Fuel[NieuwC$`Cars sold` == max(NieuwC$`Cars sold`)]),
        subtitle= paste("Best sold type of car in Belgium in ", max(input$Year3)), color = "red"
      )}
    else{
      TweedehandsC <- Tweedehands %>% filter(Fuel %in% input$Fuel, Year == max(input$Year3))
      valueBox(
        paste0(TweedehandsC$Fuel[TweedehandsC$`Cars sold` == max(TweedehandsC$`Cars sold`)]),
        subtitle= paste("Best sold type of second hand car in Belgium in ", max(input$Year3)), color = "red"
      )
    }})
  
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
    MeanSales <- DataC %>% group_by(Month) %>% summarise(Sales = mean(Sales, na.rm = T) )
    p4 <- DataC %>% ggplot(aes(x= Month, y = Sales, na.rm = T)) + geom_line(aes(color = Year)) + geom_line(data = MeanSales, color = 'black') + scale_x_continuous(breaks = seq(0,12, by = 1))
    ggplotly(p4)
  })
  
  #hist verkoop: periodieke tesla verkoop
  output$hist08 <- renderPlotly({
    DataC2 <- Data %>% filter(Month >= min(input$Month) & Month <= max(input$Month), Year %in% input$Year9)
    h8 <- DataC2 %>% ggplot(aes(x = Month, y = Sales, na.rm = T)) + geom_col() + facet_wrap(Year~.) + labs(title = "Periodic Tesla sales over the years.") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_x_continuous(breaks = seq(0,12, by = 1)) +
      scale_y_continuous(limits = c(0, 25000), breaks = seq(0,25000, by= 5000)) + ylab("Sales") + theme_minimal()
    ggplotly(h8)})
  
  
  #financieel tabblad
  output$revbox <- renderValueBox({
    somjaar <- Financial_numbers_gather_som %>% filter(Year == input$Yearrev, typenumber == 'totalrevenue') %>% group_by(Year) 
    
    valueBox(
      paste0(format(round(somjaar$finvalue[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
      subtitle= paste0("Revenue ", input$Yearrev, " in million"), 
      icon = icon("dollar-sign"), color = 'red'
    )
    
  })
  
  output$frcashbox <- renderValueBox({
    somjaar <- Financial_numbers_gather_som %>% filter(Year == input$Yearrev, typenumber == 'totalfreecashflow') %>% group_by(Year)
    if(somjaar$finvalue[]>=0) {  
      valueBox(
        paste0(format(round(somjaar$finvalue[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle = paste0("Free cashflow ", input$Yearrev, " in million"), 
        icon = icon("dollar-sign"), color = 'red'
      )
    }
    else {
      valueBox(
        paste0(format(round(somjaar$finvalue[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle = paste0("Free cashflow ", input$Yearrev, " in million"), 
        icon = icon("dollar-sign"), color = 'purple'
      )
    }
  })
  
  output$grprbox <- renderValueBox({
    somjaar <- Financial_numbers_gather_som %>% filter(Year == input$Yearrev, typenumber == 'totalgrossprofit') %>% group_by(Year)
    valueBox(
      paste0(format(round(somjaar$finvalue[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)), 
      subtitle = paste0("Gross profit ", input$Yearrev, " in million"),  
      icon = icon("piggy-bank"), color = 'red'
    )
  })
  
  
  output$colfin <- renderPlotly({
    
    # generate bins based on input$bins from ui.R
    x    <- Financial_numbers_gather_norm$Year
    Yearrev <- seq(min(x), max(x), length.out = input$Yearrev)
    
    financevar <- financefunction(input$Yearrev, Financial_numbers_gather_norm) %>% filter(typenumber != "Gross Margin")
    value <- financevar$finvalue/1000000
    
    financevarpcol <- financevar %>% ggplot(aes(x = Quarter, y = value, fill = typenumber))+ geom_col(position = "dodge") + 
      labs(title = input$Yearrev, y = 'Value') + scale_fill_manual(values = c("blue2", "royalblue1", "skyblue3")) +  
      scale_y_continuous(limits = c(-1500, 8000), breaks = seq(-1500,8000, by= 500)) +  
      theme_minimal() + geom_hline(yintercept = 0, color = "black", size = 1.5)
    
    ggplotly(financevarpcol)
    
    
  })
  output$linefin <- renderPlotly({
    
    
    y    <- Financial_numbers_gather_som$Year
    Yearrevline <- seq(min(y), max(y))
    financevar <- Financial_numbers_gather_som %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline), typenumber != "totalgrossmargin") %>% group_by(Year, typenumber) %>% 
      mutate("total" = sum(finvalue, na.rm = TRUE)) %>% select(Year, total, typenumber)%>% distinct()
    
    financevar$col = cut(financevar$total, c(-Inf, 0, Inf))
    
    financevarpline <- financevar %>% ggplot(aes(x = Year , y = total, color = typenumber))+ geom_line() +
      labs(y = 'Value') + 
      theme_minimal() + scale_color_manual(values = c("blue2", "royalblue1", "skyblue3")) + geom_hline(yintercept = 0, color = "black", size = 1.5) + 
      gghighlight(total >= 0) 
    ggplotly(financevarpline)
  })
  
  output$grossmargin <- renderPlotly({
    y <- Financial_numbers_gather_som$Year
    Yeargrossmargin <- seq(min(y), max(y))
    financevarmar <- Financial_numbers_gather_som %>% filter(Year >= min(input$Yeargrossmargin) & Year <= max(input$Yeargrossmargin), typenumber == "totalgrossmargin") %>% group_by(Year, typenumber) %>% 
      mutate("total" = sum(finvalue, na.rm = TRUE)) %>% select(Year, total, typenumber) %>% distinct()
    
    financevarmarp <- financevarmar %>% ggplot(aes(x = Year, y = total, color = typenumber)) + geom_line() + 
      labs(y = 'Value') + scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, by= 5)) +
      theme_minimal() + scale_color_manual(values = c("midnightblue")) + theme(legend.position = "none")
    ggplotly(financevarmarp)
  })
  
  #Uitbreiding naar de EU
  checkeurope <- reactive({input$Europe})
  output$colpascar <- renderPlotly({
    
    if (checkeurope() == 2) {
      countriespasscarvar <- countriesafpassengercars %>% filter(Country == input$EUoptions)
      value <- countriespasscarvar$waardes
      
      
      countriespasscarvarp <- countriespasscarvar %>% 
        ggplot(aes(x = Year, y = value, fill = Fuel)) + 
        geom_col(position = "dodge") + 
        labs(title = input$EUoptions, y = '')  + 
        scale_x_continuous(limits= c(min(countriesafpassengercars$Year), max(countriesafpassengercars$Year)) , 
                           breaks = seq(min(countriesafpassengercars$Year), max(countriesafpassengercars$Year), by = 1)) +
        theme_minimal() + scale_fill_manual(values = c("red", "orange", "green", "lightseagreen", "blue", "purple", "maroon1"))
        
      
    }
    else { 
      countriespasscarvar <- countriesafpassengercars %>% filter(Year == input$YearEU)
      value <- countriespasscarvar$waardes
      
      
      
      countriespasscarvarp <- countriespasscarvar %>% 
        ggplot(aes(x = Country, y = value, fill = Fuel ))+ 
        geom_col(position = "dodge") + 
        labs(title = input$YearEU, y = '')  + scale_y_continuous(limits = c(0, 3600000), breaks = seq(0,4000000, by= 500000)) +
        coord_flip() + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        scale_fill_manual(values = c("red", "orange", "green", "lightseagreen", "blue", "purple", "maroon1"))
      
    }
    ggplotly(countriespasscarvarp)
  })
  output$colinfr <- renderPlotly({
    if (checkeurope() == 2) {
      countriesinfrvar <- countriesafinfrastructure %>% filter(Country == input$EUoptions)
      value <- countriesinfrvar$waardes
      
      countriesinfrvarp <- countriesinfrvar %>% 
        ggplot(aes(x = Year, y = value, fill = Fuel))+ 
        geom_col(position = "dodge") + 
        labs(title = input$EUoptions, y = '')  + 
        scale_x_continuous(limits = c(min(countriesafinfrastructure$Year), max(countriesafinfrastructure$Year)), 
                           breaks = seq(min(countriesafinfrastructure$Year), max(countriesafinfrastructure$Year), by = 1)) + 
        theme_minimal() + 
      scale_fill_manual(values = c("purple", "green", "blue", "orange", "maroon1"))
      
    }
    else {
      countriesinfrvar <- countriesafinfrastructure %>% filter(Year == input$YearEU)
      value <- countriesinfrvar$waardes
      
      countriesinfrvarp <- countriesinfrvar %>% 
        ggplot(aes(x = Country, y = value, fill = Fuel))+ 
        geom_col(position = "dodge") + 
        labs(title = input$YearEU, y = '')  + scale_y_continuous(limits = c(0, 65000), breaks = seq(0,65000, by= 5000)) +
        coord_flip() + theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  + 
        scale_fill_manual(values = c("purple", "green", "blue", "orange", "maroon1"))
      
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

########################################################################################################################
####################################### Temporary mark to quickly find code back ####################################### 
########################################################################################################################     
        
    # Loyalty
    
      # KPI's
      output$loyalty_percentage_of_tesla <- renderValueBox({
        
        # Select percentage
        loyalty_perc_of_tesla <- loyalty_per_brand_Tesla$Percentage
        
        # Convert to percentage
        loyalty_perc_of_tesla <-percent(loyalty_perc_of_tesla,
                                        accuracy = 0.1)
        # print(loyalty_perc_of_tesla)

        # Display Valuebox
        valueBox(
          loyalty_perc_of_tesla,
          subtitle = "Loyalty percentage of Tesla",
          color = "red"
        )
        
      })
    
      # Graph
      output$loyalty_bar <- renderPlot({
        
        # Filter based on input, ...
        loyalty_per_brand_chosen_class <- loyalty_per_brand_tibble %>% filter(Classification %in% input$loyalty_checkboxes)
        
        # ... however, we want to make sure Tesla is always shown. So, we remove Tesla (even if it's not there) ...
        loyalty_per_brand_chosen_class <- loyalty_per_brand_chosen_class %>% filter(!Brand %in% c("Tesla"))
        
        # ... and then add it in each case. (Yes, this are probably shorter ways to do this, but it works :-) )
        loyalty_per_brand_chosen_class <- loyalty_per_brand_chosen_class %>% add_row(Brand = loyalty_per_brand_Tesla$Brand,
                                                                                     Percentage = loyalty_per_brand_Tesla$Percentage,
                                                                                     Classification = loyalty_per_brand_Tesla$Classification,
                                                                                     )
  
        # Reverse order (so the barplot shows the values from high to low)
        loyalty_per_brand_chosen_class <- loyalty_per_brand_chosen_class[order(loyalty_per_brand_chosen_class$Percentage), ]
        
        # Make sure we retain the order in the plot
        loyalty_per_brand_chosen_class$Brand <- factor(loyalty_per_brand_chosen_class$Brand,
                                                 levels = loyalty_per_brand_chosen_class$Brand)
        
        
        
        # Create the plot
        loyalty_per_brand_plot <- ggplot(loyalty_per_brand_chosen_class,
                                         aes(x = Percentage,
                                             y = Brand,
                                             fill = factor(ifelse(Brand == "Tesla", "Highlighted", "Normal")))) +
          geom_col() + 
          theme_minimal() +
          scale_fill_manual(name = "Hidden_legend", 
                            values = c("red2", "coral2")) +
          scale_x_continuous(breaks = seq(0, 1, 0.1),
                             limits = c(0, 1),
                             labels = percent_format(accuracy = 1),
                             expand = expansion(mult = c(0, 0.01))
                             ) +
          removeGridY() +
          theme(axis.text = element_text(size = 12),
                axis.title = element_text(size = 15),
                legend.position = "none") 
          
        # Display plot
        loyalty_per_brand_plot
      
      
      
      # Te doen:
      # - KPI: Rank
      # - KPI 2: Percentage (80%)
      # - Ggplotly zodat je precieze percentage ook ziet. Dan kan mogelijk checkbox zelfs weg.(Want wil ...
      #   ... kunnen filteren op luxury/mass market of beiden). Mss voegt plotly ook toe dat merken kan ...
      #   ... kiezen
      

    })
    
    # Growth comparisons
    output$growth_comparison_bar <- renderPlot({
      
      
      # Create plot
      # growth_comp_plot <- ggplot(growth_comp_sales_2019_1,
      #                            aes(x = year_2019_sales,
      #                                y = submodel)) +
      #   geom_bar(stat = "identity",
      #            fill = "tomato3") +
      #   theme(axis.text.y = element_text(angle = 65, vjust=0.6)) +
      #   theme_minimal()
      # 
      # growth_comp_plot
      
      
      # Ook dit leidt tot niks.
      # growth_comp_sales_2019_1 <- growth_comp_sales_2019_1 %>%
      #                             mutate(year_2019_sales = fct_reorder(year_2019_sales, submodel))
      # View(growth_comp_sales_2019_1)
      # str(growth_comp_sales_2019_1)
      # growth_comp_sales_2019_1$year_2019_sales <- as.numeric(growth_comp_sales_2019_1$year_2019_sales)
      # 
      # growth_comp_plot <- ggplot(growth_comp_sales_2019_1,
      #                            aes(x = year_2019_sales,
      #                                y = submodel)) +
      #   geom_col(fill = "tomato3") +
      #   theme_minimal() +
      #   theme(axis.text.x = element_text(angle = 65, vjust=0.6))
      # 
      # growth_comp_plot
      
      # Te doen:
      # - Op einde: code opruimen, oude datasets weggooien.
      
    })

########################################################################################################################
####################################### Temporary mark to quickly find code back ####################################### 
########################################################################################################################    
        
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

  

