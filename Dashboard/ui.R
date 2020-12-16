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
library(shinyjs)
library(htmltools)
library(stringr)
library(DT)
library(tidyverse)
library(tidyquant)
library(quantmod)
library(RSQLite)

#Establish connection with the database
con <- dbConnect(drv = SQLite(), "Tesla_database.sqlite")

#Read tables
VPS <- dbReadTable(con, "VPS")
Nieuw <- dbReadTable(con, "Nieuw")
aankoopproces <- dbReadTable(con, "aankoopproces")
Data <- dbReadTable(con, "Data")
eusurvey <- dbReadTable(con, "eusurvey")
Financial_numbers_gather_norm <- dbReadTable(con, "Financial_numbers_gather_norm")
superchargers <- dbReadTable(con, "superchargers")
verkoo <- dbReadTable(con, "verkoo")
countriesafpassengercars <- dbReadTable(con, "countriesafpassengercars")
countriesafinfrastructure <- dbReadTable(con, "countriesafinfrastructure")

#Close connection with the database
dbDisconnect(con)

#Define UI for application that draws a map
shinyUI(
  dashboardPage(skin = 'red',
                dashboardHeader(title = 'Menu'),
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Growth", tabName = "Growth", newTab = T, icon = icon('chart-line'), 
                             menuSubItem("Sales per segment", tabName = "Salespersegment", icon = icon('search-dollar')), 
                             menuSubItem("Sales per fuel type", tabName = "fueltype", icon = icon('funnel-dollar')),
                             menuSubItem("Best selling EV's compared", tabName = "best_selling_evs_compared", icon = icon('medal'))
                             ),
                    menuItem("Customers", tabName = "Customers", newTab = T, icon = icon('users'),
                             menuSubItem("Purchase process", tabName = "Purchaseprocess", icon = icon('wallet')),
                             menuSubItem("Brand loyalty", tabName = "dashboard_loyalty", icon = icon('grin-hearts') ),
                             menuSubItem("EV popularity", tabName = "survey",icon = icon('grin-stars'))
                             ),
                    menuItem("Sales", tabName = "Sales", icon = icon('dollar-sign')),
                    menuItem("Finance", tabName = "Finance", icon = icon('file-invoice-dollar')),
                    menuItem("Superchargers", tabName = "Superchargers", newTab = T, icon = icon('bolt'),
                             menuSubItem("Map", tabName = "Map", icon = icon('map-marked-alt')), 
                             menuSubItem("Statistics", tabName = "Statistics", icon = icon('chart-bar')), 
                             menuSubItem("Competition", tabName = "Competition", icon = icon('fist-raised'))),
                    menuItem("Expansion in Europe", tabName = "Expansion_in_Europe", icon = icon('expand-arrows-alt'))
                  )),
                dashboardBody(
                  tabItems(
                    
                #Growth
                    tabItem(
                      tabName = "Salespersegment",
                      fluidRow(
                        valueBoxOutput("bestsoldsegment", width = 6),
                        valueBoxOutput("populairst", width = 6)
                      ),
                      fluidRow(
                        box(
                          title = "New cars sold in the EU by segment over the years", "In million units", 
                          width = 12,
                          solidHeader = T, status = 'danger', plotlyOutput("line01"),
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
                        column( width = 12,
                                valueBoxOutput("bestsoldfuel", width = 6),
                                valueBoxOutput("populairstfuel", width = 6)),
                        column( width = 12,
                                valueBoxOutput("bestsoldfueleu", width = 6),
                                valueBoxOutput("populairstfueleu", width = 6))
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
                                      sep = "")
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
                                      sep = "")
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
                    tabItem(tabName = "best_selling_evs_compared",
                            fluidRow(
                              valueBoxOutput("teslas"),
                              valueBoxOutput("teslax"),
                              valueBoxOutput("tesla3")
                            ),
                            fluidRow(
                              box(title = "Top 15 best sold EV's of 2019 compared by different characteristics",
                                  status = "danger",
                                  solidHeader = T,
                                  width = 12,
                                  plotlyOutput("growth_comparison_bar",
                                               height = "580px"),
                                  selectInput(inputId = "growth_select_box",
                                              label = "Select parameter for comparison",
                                              choices = c("Sales In 2019 (units)",
                                                          "Sales In 2018 (units)",
                                                          "Change In Sales From 2018 To 2019 (%)",
                                                          "Share In EV Market In 2019 (%)",
                                                          "Share In EV Market In 2018 (%)",
                                                          "Proportion Of Sales Of This Model That Was EV In 2019 (%)",
                                                          "Proportion Of Sales Of This Model That Was EV In 2018 (%)",
                                                          "Range (km)",
                                                          "Top Speed (km/h)",
                                                          "Acceleration (0-100 km/h)",
                                                          "Horsepower (units)",
                                                          "Top Charging Speed (km/h)",
                                                          "Price (EUR)",
                                                          "Trunk Space (Including Frunk If Applicable) (l)",
                                                          "NCAP Stars (0-5)",
                                                          "NCAP Adult Occupant Score (%)",
                                                          "NCAP Child Occupant Score (%)",
                                                          "NCAP Vulnerable Road Users Score (%)",
                                                          "NCAP Safety Assist Score (%)",
                                                          "NCAP Average Score (%)"
                                              ),
                                              selected = "Sales in 2019 (units)"
                                  )
                              )
                            )
                    ),
                    
                #Customers
                    tabItem(
                      tabName = "Purchaseprocess",
                      fluidRow(
                        box(
                          title = "Share of Europeans interested in online vehicle purchasing in 2018", width =12, solidHeader = T, status = 'danger',
                          plotlyOutput("hist07"),
                          selectInput(inputId = "Country4",
                                      label = "Choose country",
                                      choices = aankoopproces$Country,
                                      multiple = TRUE,
                                      selected = c("Belgium", "Germany", "France", "UK", "Italy")),
                          selectInput(inputId = "Interest",
                                      label = "Choose level of interest",
                                      choices = aankoopproces$Interest,
                                      multiple = TRUE,
                                      selected = c("Not at all interested/not very interested", "Neutral", "Somewhat interested/very interested"))
                        ))
                    ),
                    tabItem(
                      tabName = "dashboard_loyalty", 
                        fluidRow(
                          valueBoxOutput("loyalty_percentage_of_tesla", width = 6),
                          valueBoxOutput("loyalty_rank_of_tesla", width = 6)
                        ),
                        fluidRow(
                          box(title = "Loyalty per brand",
                              "Percentage of car buyers that chose the same brand when buying a new car",
                              width = 12,
                              status = "danger",
                              solidHeader = T,
                              plotlyOutput("loyalty_bar",
                                         height = "580px"),
                              checkboxGroupInput(inputId = "loyalty_checkboxes",
                                                 label = "Choose class(es) to compare Tesla with",
                                                 choices = c("Luxury", "Mass market"),
                                                 selected = c("Luxury", "Mass market")
                              )
                              
                          )
                        )
                    ),
                   tabItem(
                    tabName = "survey",
                      h2("Survey taken in 2018 in EU-countries"),
                        fluidRow(
                          valueBoxOutput("surveytotal", width = 6),
                          valueBoxOutput("totalcountries", width = 6)
                        ),
                        fluidRow(
                          box(
                            title = "Per country",
                              tabPanel(" ", 
                               selectInput(inputId = "ccountry",
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
                            tabPanel("Gender",
                                     selectInput(inputId = "gcountry",
                                                 label = "Choose country",
                                                 choices = unique(eusurvey$Country),
                                                 multiple = T,
                                                 selected = "Belgium"
                                     ),
                                     plotlyOutput("ggcountry")
                          ),
                            tabPanel("Income", 
                               selectInput(inputId = "incountry",
                                           label = "Choose Country",
                                           choices = unique(eusurvey$Country),
                                           selected = "Belgium"
                               ),
                               selectInput(inputId = "incomegr",
                                           label = "Choose income group",
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
                    
                    
                    
                    #Sales
                    tabItem(
                      tabName = "Sales",
                      fluidPage(
                      box(title = "Periodic Tesla sales over the years", 
                          "Black line is the mean sales of all the selected years", plotlyOutput("line04"),
                          width = 12, solidHeader = T, status = 'danger',
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
                      ))
                    ),
                
                
                #finance
                    tabItem(tabName = "Finance",
                            h2("Financial numbers worldwide, based on automotive sector from Tesla"),
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
                                                                                                        min = min(Financial_numbers_gather_norm$Year),
                                                                                                        max = max(Financial_numbers_gather_norm$Year),
                                                                                                        value = c(min(Financial_numbers_gather_norm$Year),max(Financial_numbers_gather_norm$Year)),
                                                                                                        sep = "")
                              ),
                              box(title = "Quarterly", 
                                  "In million", solidHeader = T, status="danger", plotlyOutput("colfin"), sliderInput(inputId = "Yearrev", 
                                                                                                                      label = "Choose year to give the quarters of",
                                                                                                                      min = min(Financial_numbers_gather_norm$Year),
                                                                                                                      max = max(Financial_numbers_gather_norm$Year),
                                                                                                                      value = 2020, 
                                                                                                                      sep = ""))
                            ),
                            fluidRow(
                              box(title = "Gross Margin",
                                     "In percentage",
                                     solidHeader = T, status = "danger", plotlyOutput("grossmargin"), sliderInput(inputId = "Yeargrossmargin", 
                                                                                                                  label = "Choose the range of years to appear",
                                                                                                                  min = min(Financial_numbers_gather_norm$Year),
                                                                                                                  max = max(Financial_numbers_gather_norm$Year),
                                                                                                                  value = c(min(Financial_numbers_gather_norm$Year),max(Financial_numbers_gather_norm$Year)),
                                                                                                                  sep = "")),
                              box(
                                title = "Tesla performance on the Stock Market",
                                solidHeader = T, status = "danger", plotlyOutput("tslastock"),
                                dateInput(inputId = "st", label = "From",
                                            value = "2020-01-01"),
                                  dateInput(inputId = "en", label = "To")
                                  )
                            )
                            
                    ),
                    
                #Superchargers
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
                          solidHeader = T, status = "danger", verbatimTextOutput("brush_info"),
                          plotOutput("hist01", height = "600px", click = "plot1_click",
                                                                         brush = brushOpts(
                                                                           id = "plot1_brush")),
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
                          plotlyOutput("hist02", height = "600px"),
                          selectInput(inputId = "Country2",
                                      label = "Choose country",
                                      choices = superchargers$Country,
                                      multiple = TRUE,
                                      selected = c("Belgium", "Norway", "Italy", "Germany", "France", "Netherlands", "United Kingdom", "Switzerland", "Portugal", "Spain", "Iceland", "Denmark", "Poland", "Serbia", "Bulgaria", "Sweden", "Hungary", "Czech Republic", "Slovakia", "Finland", "Austria", "Croatia", "Ireland", "Russia", "Liechtenstein", "Slovenia", "Luxembourg"))
                        ))
                    ),
                    
                #Expansion in Europe
                    tabItem(tabName = "Expansion_in_Europe",
                            fluidRow(
                              box(title = "AF passenger cars",
                                  "Total fleet of passenger cars per alternative fuel (AF)", solidHeader = T, status="danger", plotlyOutput("colpascar", height = "650px"),
                                  ),
                              box(title = "AF infrastructure",
                                  "Total number of alternative fuel (AF) infrastructure per type of fuel", solidHeader = T, status="danger", plotlyOutput("colinfr", height = "650px"),
                                  )
                            ),
                            fluidRow(
                              box(title = "Tesla sales in Europe per year", solidHeader = T, status="danger", 
                                  selectInput(inputId = "teslajaar",
                                              label = "Choose the year you want to see (green is new that year)",
                                              choices = unique(teslapercountrysales$jaar),
                                              selected = 2013),
                                  plotOutput("distPlot", height = "650px")),
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
                                                'Spain', 'Sweden'), selected = "Belgium"),
                                  dataTableOutput("europemaptable")
                              )
                              )
                          )
                
                      )
                            
                            
                    ) 
                  )
                )

