library(shiny)
library(readxl)
library(readr)
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
library(plotly)
library(leaflet)
library(shinydashboard)
library(stringr)
library(DT)
library(tidyverse)

#Caro

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

#Groei: aandeel elektrische auto's op Belgische en EU markt
nieuw <- read_xlsx("Data/Verkoop per brandstof (België) met market share.xlsx", sheet = "Nieuw")
Nieuw <- nieuw %>% gather('2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', key = "Year", value = "Cars sold",na.rm = FALSE, convert = FALSE, factor_key = FALSE)
eu <- read_xlsx("Data/% share of new passenger cars by fuel type in the EU.xlsx")

#Klanten: aankoopproces
aankoopproces <- read_xlsx("Data/Online.xlsx")
aankoopproces <- aankoopproces %>% gather(`Not at all interested/not very interested`:`Somewhat interested/very interested`, key = "Interest", value="Percentage")

#Verkoop: periodieke tesla verkopen
data <- read_xlsx("Data/Monthly Tesla Vehicle Sales.xlsx")
Data <- data %>% gather(January:December, key=  "Month", value="Sales") %>% mutate(Month = str_replace(Month, "January", "1"), Month = str_replace(Month, "February", "2"), Month = str_replace(Month, "March", "3"), Month = str_replace(Month, "April", "4"), Month = str_replace(Month, "May", "5"), Month = str_replace(Month, "June", "6"), Month = str_replace(Month, "July", "7"), Month = str_replace(Month, "August", "8"), Month = str_replace(Month, "September", "9"), Month = str_replace(Month, "October", "10"), Month = str_replace(Month, "November", "11"), Month = str_replace(Month, "December", "12"))
Data$Month <- as.integer(Data$Month)
Data$Year <- as.factor(Data$Year) 


#Lien

#financieel tabblad
Revenue <- read_xlsx("data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Revenues (automotive)", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_Margin <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross margin", col_types = c("numeric", "text", "numeric", "numeric"))
Gross_profit <- read_xlsx("Data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Gross profit", col_types = c("numeric", "text", "numeric", "numeric", "numeric"))
Free_cashflow <- read_xlsx("Data/Tesla's free cash flow by quarter 2020 world wide.xlsx", skip = 3 , sheet = "Data", col_types = c("numeric", "text", "numeric"))

#uitbreiding in europa tabblad
countriesafpassengercars <- read_xlsx("Data/Countries overview of af passenger cars.xlsx", skip = 2 , col_types = c("numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
teslapercountrysales <- read_xlsx("Data/Verkoop landen tesla.xlsx", skip = 1, col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>% gather('2013', '2014', '2015', '2016', '2017', '2018', '2019', key = 'jaar', value = 'waarde')

#jaske
eusurvey <- read.csv("data/hev1.csv")

# Pieter

  # Load growth comparison (groco) data
  # groco_data <- read_xlsx("Data/growth_comparison_v7.xlsx")
  # View(groco_data)
  # str(groco_data)
  
  # Clean it
    
    # Convert to numerics
    # groco_data$`Change In Sales From 2018 To 2019 (%)`  <- as.numeric(groco_data$`Change In Sales From 2018 To 2019 (%)`)
    
    # Round to correct for weird numbers
      
# Define UI for application that draws a map
shinyUI(
  dashboardPage(skin = 'red',
                dashboardHeader(title = 'Menu'),
                dashboardSidebar(
                  sidebarMenu(
                    sidebarSearchForm("searchText", "buttonSearch", "Search"),
                    menuItem("Growth", tabName = "Growth", newTab = T, 
                             menuSubItem("Sales per segment", tabName = "Salespersegment"), 
                             menuSubItem("Sales per fuel type", tabName = "fueltype"),
                             menuSubItem("Best selling EV's compared", tabName = "dashboard_growth")
                             ),
                    menuItem("Customers", tabName = "Customers", newTab = T, 
                             menuSubItem("Purchase process", tabName = "Purchaseprocess"),
                             menuSubItem("Brand loyalty", tabName = "dashboard_loyalty"),
                             menuSubItem("Survey", tabName = "survey")
                             ),
                    menuItem("Sales", tabName = "Sales", newTab =T, menuSubItem("Periodic analysis", tabName = "Periodic")),
                    menuItem("Finance", tabName = "Omzet"),
                    menuItem("Superchargers", tabName = "Superchargers", newTab = T, menuSubItem("Map", tabName = "Map"), menuSubItem("Statistics", tabName = "Statistics"), 
                             menuSubItem("Competition", tabName = "Competition")),
                    menuItem("Expansion in Europe", tabName = "EU")
                  )),
                dashboardBody(
                  tabItems(
                    #Caro
                    tabItem(
                      tabName = "Map",  
                      leafletOutput("mymap"), dataTableOutput("table01")),
                    tabItem(
                      tabName = "Statistics",
                      h2("Info on Tesla supercharger stations in Europe"),
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
                          title = "Number of Teslas per supercharger station", width = 12,
                          solidHeader = T, status = "danger", plotlyOutput("hist01"),
                          sliderInput(inputId = "Year",
                                      label = "Choose year",
                                      min = 2013,
                                      max = 2019,
                                      value = 2019,
                                      sep = ""),
                          selectInput(inputId = "Country",
                                      label = "Choose country",
                                      choices = verkoo$Country,
                                      multiple = TRUE,
                                      selected = c("Belgium", "Austria", "Czech Republic", "Denmark", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands", "Norway", "Portugal", "Romania", "Slovenia", "Spain", "Sweden", "Switzerland"))
                        )
                      )
                    ),
                    tabItem(
                      tabName = "Competition",
                      fluidRow(
                        box(
                          title = "Superchargers market share", width = 12, 
                          solidHeader = T, status = 'danger', plotlyOutput("pie01")
                        )
                      ),
                      fluidRow(
                        box(
                            title ="Number of supercharger stations per country", width = 12,
                            solidHeader = T, status = "danger",
                            plotlyOutput("hist02"),
                            selectInput(inputId = "Country2",
                                    label = "Choose country",
                                    choices = superchargers$Country,
                                    multiple = TRUE,
                                    selected = c("Belgium", "Norway", "Italy", "Germany", "France", "Netherlands", "United Kingdom", "Switzerland", "Portugal", "Spain", "Iceland", "Denmark", "Poland", "Serbia", "Bulgaria", "Sweden", "Hungary", "Czech Republic", "Slovakia", "Finland", "Austria", "Croatia", "Ireland", "Russia", "Liechtenstein", "Slovenia", "Luxembourg"))
                      )),
                     ),
                    tabItem(
                      tabName = "Salespersegment",
                      fluidRow(
                        valueBoxOutput("bestsoldsegment")
                      ),
                      fluidRow(
                        box(
                          title = "New cars sold in the EU by segment in million units over the years", 
                          width = 12,
                          solidHeader = T, status = 'danger', plotlyOutput("line01"),
                          selectInput(inputId = "Segment",
                                      label = "Choose segment",
                                      choices = VPS$Segment,
                                      multiple = TRUE,
                                      selected = c("Lower Medium (C)", "Luxury (E+F)", "MPV", "Small (A+B)", "SUV", "Upper Medium (D)")),
                          sliderInput(inputId = "Year2",
                                      label = "Choose year",
                                      min = 2008,
                                      max = 2019,
                                      value = c(2008, 2019),
                                      sep = "")
                        )
                      )
                    ),
                    tabItem(
                      tabName = "fueltype",
                      fluidRow(
                        valueBoxOutput("bestsoldfuel"),
                        valueBoxOutput("bestsoldfueleu")
                      ),
                      fluidRow(
                        box(
                          title = "Number of cars sold in Belgium", solidHeader = T, status = 'danger', plotlyOutput("line02"),
                          radioButtons("Region", label= "Choose which kind of cars you want to see on the graph", choices = list("New" = 1, "Second hand" = 2)),
                          sliderInput(inputId = "Year3",
                                      label = "Choose year",
                                      min = 2012,
                                      max = 2019,
                                      value = c(2012, 2019),
                                      sep = ""),
                          selectInput(inputId = "Fuel",
                                      label = "Choose fuel type",
                                      choices = nieuw$Fuel,
                                      multiple = TRUE,
                                      selected = c("Electric", "Petrol", "Diesel", "Alternative fuels", "Hybrid"))
                          
                        ),
                        box(
                          title = "Market share of cars by fuel type in Belgium", solidHeader = T, status = 'danger', plotlyOutput("pie02"),
                          radioButtons("Region2", label= "Choose which kind of cars you want to see on the graph", choices = list("New" = 1, "Second hand" = 2)),
                          sliderInput(inputId = "Year5",
                                      label = "Choose year",
                                      min = 2012,
                                      max = 2019,
                                      value = 2019,
                                      sep = "")
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Market Share of new cars in the EU over the years", plotlyOutput("hist05"), solidHeader = T, status = 'danger',
                          sliderInput(inputId = "Year8",
                                      label = "Choose year",
                                      min = 2016,
                                      max = 2019,
                                      value = c(2016, 2019),
                                      sep = ""),
                          selectInput(inputId = "Fuel3",
                                      label = "Choose fuel type",
                                      choices = eu$Fuel,
                                      selected = "Electrically-chargeable")
                        ),
                        box(
                          title = "Market share of new cars by fuel type in the EU", plotlyOutput("pie04"), solidHeader = T, status = 'danger',
                          sliderInput(inputId = "Year7",
                                      label = "Choose year",
                                      min = 2016,
                                      max = 2019,
                                      value = 2019,
                                      sep = "")
                        )
                      )
                    ),
                    tabItem(
                      tabName = "Purchaseprocess",
                      tabBox(
                        title = "Share of Europeans interested in online vehicle purchasing in 2018", height = 12, width =12,
                        tabPanel("Tab1", plotlyOutput("hist06"),
                                 selectInput(inputId = "Country3",
                                             label = "Choose country",
                                             choices = aankoopproces$Country,
                                             multiple = TRUE,
                                             selected = c("Belgium", "Germany", "France", "UK", "Italy"))),
                        tabPanel("Tab2", plotlyOutput("hist07"),
                                 selectInput(inputId = "Country4",
                                             label = "Choose country",
                                             choices = aankoopproces$Country,
                                             multiple = TRUE,
                                             selected = c("Belgium", "Germany", "France", "UK", "Italy")),
                                 selectInput(inputId = "Interest",
                                             label = "Choose level of interest",
                                             choices = aankoopproces$Interest,
                                             multiple = TRUE,
                                             selected = c("Not at all interested/not very interested", "Neutral", "Somewhat interested/very interested")))
                      )
                    ),
                    tabItem(
                      tabName = "Periodic",
                      box(
                        title = "Periodic Tesla sales over the years.", "Black line is the mean sales of all the selected years", plotlyOutput("line04"),
                        height = 12, width = 12, solidHeader = T, status = 'danger',
                        sliderInput(inputId = "Month",
                                    label = "Choose month",
                                    min = 1,
                                    max = 12,
                                    value = c(1, 12),
                                    sep = ""),
                        selectInput(inputId = "Year9",
                                    label = "Choose year",
                                    choices = Data$Year,
                                    multiple = TRUE,
                                    selected = c("2016", "2017", "2018", "2019","2020"))
                      )
                    ),
                    #finance
                    tabItem(tabName = "Omzet",
                            h2("Financial numbers worldwide, based on automotive sector"),
                            fluidRow(
                              valueBoxOutput("revbox"),
                              valueBoxOutput("frcashbox"),
                              valueBoxOutput("grprbox")
                            ),    
                            #Grafieken voor financiele cijfers
                            fluidRow(
                              box(title = "Yearly",
                                  "In million",
                                  solidHeader = T, status="danger", plotlyOutput("linefin"),sliderInput(inputId = "Yearrevline", 
                                                                                                        label = "Choose the range of years to appear",
                                                                                                        min = min(Revenue$Year),
                                                                                                        max = max(Revenue$Year),
                                                                                                        value = c(min(Revenue$Year),max(Revenue$Year)),
                                                                                                        sep = "")
                              ),
                              box(title = "Quarterly", 
                                  "In million", solidHeader = T, status="danger", plotlyOutput("colfin"), sliderInput(inputId = "Yearrev", 
                                                                                                                      label = "Choose year to give the quarters of",
                                                                                                                      min = min(Revenue$Year),
                                                                                                                      max = max(Revenue$Year),
                                                                                                                      value = 2020, 
                                                                                                                      sep = ""))
                            ),
                            fluidRow(
                              box(title = "Gross Margin",
                                     "In percentage",
                                     solidHeader = T, status = "danger", plotlyOutput("grossmargin"), sliderInput(inputId = "Yeargrossmargin", 
                                                                                                                  label = "Choose the range of years to appear",
                                                                                                                  min = min(Revenue$Year),
                                                                                                                  max = max(Revenue$Year),
                                                                                                                  value = c(min(Revenue$Year),max(Revenue$Year)),
                                                                                                                  sep = ""))
                            )
                            
                    ),
                    tabItem(tabName = "EU",
                            fluidRow(
                              box(title = "AF passenger cars",
                                  "Total fleet of passenger cars per alternative fuel (AF)", solidHeader = T, status="danger", plotlyOutput("colpascar"),
                                  ),
                              box(title = "AF infrastructure",
                                  "Total number of alternative fuel (AF) infrastructure per type of fuel", solidHeader = T, status="danger", plotlyOutput("colinfr"),
                                  )
                            ),
                            fluidRow(
                              box(title = "Choose your options", solidHeader = T, status="danger", 
                                  radioButtons("Europe", label= "Choose what you want to see on the graph", choices = list("Europe" = 1, "Per chosen country" = 2)),
                                  sliderInput(inputId = "YearEU", 
                                              label = "Choose year for the values in Europe",
                                              min = min(countriesafpassengercars$Year),
                                              max = max(countriesafpassengercars$Year),
                                              value = 2020,
                                              sep = ""),
                                  
                                  selectInput("EUoptions", "Choose a country of Europe", 
                                              c('Ausria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia',
                                                'Finland', 'France', 'Germany', 'Greece', 'Hungria', 'Ireland', 'Italy', 'Latvia', 'Lithuania',
                                                'Luxembourg', 'Malta', 'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia',
                                                'Spain', 'Sweden'), selected = "Belgium")
                              ),
                              box(title = "Tesla sales in Europe per year", solidHeader = T, status="danger", 
                                  selectInput(inputId = "teslajaar",
                                              label = "choose the year you want to see (blue is new that year)",
                                              choices = unique(teslapercountrysales$jaar),
                                              selected = 2013),
                                  plotOutput("distPlot")))
                          ),
                    
                    tabItem(tabName = "dashboard_growth",
                            fluidRow(
                              box(title = "Top 15 EV's of 2019 compared (Work-in-progress)",
                                  status = "danger",
                                  solidHeader = T,
                                  plotOutput("growth_comparison_bar"),
                                  selectInput(inputId = "growth_select_box",
                                              label = "Select parameter for comparison",
                                              choices = c("Sales In 2019",
                                                          "Sales In 2018",
                                                          "Change In Sales From 2018 To 2019 (%)",
                                                          "Share In EV Market In 2019",
                                                          "Share In EV Market In 2018",
                                                          "Proportion Of Sales Of This Model That Was EV In 2019 (%)",
                                                          "Proportion Of Sales Of This Model That Was EV In 2018 (%)",
                                                          "Range",
                                                          "Top Speed (km/h)",
                                                          "Acceleration (0-100 km/h)",
                                                          "Horsepower",
                                                          "Top Charging Speed (km/h)",
                                                          "Price",
                                                          "Trunk Space (Including Frunk If Applicable)",
                                                          "Segment",
                                                          "NCAP Stars",
                                                          "NCAP Adult Occupant Score (%)",
                                                          "NCAP Child Occupant Score (%)",
                                                          "NCAP Vulnerable Road Users Score (%)",
                                                          "NCAP Safety Assist Score (%)",
                                                          "NCAP Average Score (%)"
                                              ),
                                              selected = "Sales in 2019"
                                  )
                              )
                            )
                    ),
                    
########################################################################################################################
####################################### Temporary mark to quickly find code back ####################################### 
######################################################################################################################## 

                    tabItem(tabName = "dashboard_loyalty", 
                            fluidRow(
                              valueBoxOutput("loyalty_percentage_of_tesla"),
                              valueBoxOutput("loyalty_rank_of_tesla")
                            ),
                            fluidRow(
                              box(title = "Loyalty per brand",
                                  "Percentage of car buyers that chose the same brand when buying a new car",
                                  width = 12,
                                  status = "danger",
                                  solidHeader = T,
                                  plotOutput("loyalty_bar",
                                             height = "500px"),
                                  checkboxGroupInput(inputId = "loyalty_checkboxes",
                                                     label = "Choose class(es) to compare Tesla with",
                                                     choices = c("Luxury", "Mass market"),
                                                     selected = c("Luxury", "Mass market")
                                  )
                                  
                              )
                            )
                    ),
                    
########################################################################################################################
####################################### Temporary mark to quickly find code back ####################################### 
######################################################################################################################## 

                      tabItem(
                        tabName = "survey",
                        h2("Survey taken in 2018 in EU-countries"),
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
                        tabName = "competition",
                        fluidRow(
                          
                        )
                      )
                            
                            
                    )  
                  )
                )
  )
