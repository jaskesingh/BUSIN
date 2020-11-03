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
library(stringr)

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

# Define UI for application that draws a map
shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Menu'),
    dashboardSidebar(
      sidebarMenu(
        sidebarSearchForm("searchText", "buttonSearch", "Search"),
        menuItem("Superchargers", tabName = "Superchargers", newTab = T, menuSubItem("Map", tabName = "Map"), menuSubItem("Statistics", tabName = "Statistics"), 
                 menuSubItem("Competition", tabName = "Competition")),
        menuItem("Growth", tabName = "Growth", newTab = T, menuSubItem("Sales per segment", tabName = "Salespersegment"), menuSubItem("Sales per fuel type", tabName = "fueltype")),
        menuItem("Customers", tabName = "Customers", newTab = T, menuSubItem("Purchase process", tabName = "Purchaseprocess")),
        menuItem("Sales", tabName = "Sales", newTab =T, menuSubItem("Periodic analysis", tabName = "Periodic"))
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
            tabName = "Salespersegment",
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
        ),
        tabItem(
          tabName = "fueltype",
          box( width = 12,
            title = "Belgium", solidHeader = T, status = 'danger',
            fluidRow(
              box(
                title = "Number of new cars sold", solidHeader = T, status = 'danger', plotOutput("line02"),
                sliderInput(inputId = "Year3",
                            label = "Choose year",
                            min = 2012,
                            max = 2019,
                            value = c(2012, 2019)),
                selectInput(inputId = "Fuel",
                            label = "Choose fuel type",
                            choices = nieuw$Fuel,
                            multiple = TRUE,
                            selected = "elektrisch")
                
              ),
              box(
                title = "Market share of new cars by fuel type", solidHeader = T, status = 'danger', plotOutput("pie02"),
                sliderInput(inputId = "Year5",
                            label = "Choose year",
                            min = 2012,
                            max = 2019,
                            value = 2019)
            )
          ),
          fluidRow(
            box(
              title = "Number of second hand cars sold", solidHeader = T, status = 'danger', plotOutput("line03"),
              sliderInput(inputId = "Year4",
                          label = "Choose year",
                          min = 2012,
                          max = 2019,
                          value = c(2012, 2019)),
              selectInput(inputId = "Fuel2",
                          label = "Choose fuel type",
                          choices = nieuw$Fuel,
                          multiple = TRUE,
                          selected = "elektrisch")
              
            ),
            box(
              title = "Market share of second hand cars by fuel type", solidHeader = T, status = 'danger', plotOutput("pie03"),
              sliderInput(inputId = "Year6",
                          label = "Choose year",
                          min = 2012,
                          max = 2019,
                          value = 2019)
            )
          )
        ),
        box(
          title = "EU", solidHeader = T, status = 'danger', width = 12,
          fluidRow(
            box(
              title = "Market Share of new cars in the EU over the years", plotOutput("hist05"), solidHeader = T, status = 'danger',
              sliderInput(inputId = "Year8",
                          label = "Choose year",
                          min = 2016,
                          max = 2019,
                          value = c(2016, 2019)),
              selectInput(inputId = "Fuel3",
                          label = "Choose fuel type",
                          choices = eu$Fuel,
                          selected = "Electrically-chargeable")
            ),
            box(
              title = "Market share of new cars by fuel type in the EU", plotOutput("pie04"), solidHeader = T, status = 'danger',
              sliderInput(inputId = "Year7",
                          label = "Choose year",
                          min = 2016,
                          max = 2019,
                          value = 2019)
            )
          )
        )
      ),
      tabItem(
        tabName = "Purchaseprocess",
        tabBox(
          title = "Share of Europeans interested in online vehicle purchasing in 2018", height = 12, width =12,
          tabPanel("Tab1", plotOutput("hist06"),
                   selectInput(inputId = "Country3",
                               label = "Choose country",
                               choices = aankoopproces$Country,
                               multiple = TRUE,
                               selected = "Belgium")),
          tabPanel("Tab2", plotOutput("hist07"),
          selectInput(inputId = "Country4",
                      label = "Choose country",
                      choices = aankoopproces$Country,
                      multiple = TRUE,
                      selected = "Belgium"),
          selectInput(inputId = "Interest",
                      label = "Choose level of interest",
                      choices = aankoopproces$Interest,
                      multiple = TRUE,
                      selected = "Somewhat interested/very interested"))
        )
      ),
      tabItem(
        tabName = "Periodic",
        tabBox(
          title = "Periodic Tesla sales over the years.", height = 12, width = 12,
          tabPanel("Tab1", plotOutput("line04")),
          tabPanel("Tab2", plotOutput("hist08")),
          sliderInput(inputId = "Month",
                      label = "Choose month",
                      min = 1,
                      max = 12,
                      value = c(1, 12)),
          selectInput(inputId = "Year9",
                      label = "Choose year",
                      choices = Data$Year,
                      multiple = TRUE,
                      selected = c("2019","2020"))
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
