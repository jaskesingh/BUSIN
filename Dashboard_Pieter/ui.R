# # # # # # # # # #

# Libraries
library(shiny)
library(shinydashboard)

# Shiny UI
shinyUI(
  dashboardPage(
    
    dashboardHeader(title = "Menu"),
    
    dashboardSidebar(
      

      
      
      
      sidebarMenu(
        # Pitch document: Growth --> Comparison of electric vehicles of different brands
        menuItem("Growth (Later)"),
      
        # Pitch document: Customers --> Purchase history --> Loyalty
        menuItem("Brand Loyalty (Work-In-Progress)", tabName = "dashboard")
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard", 
                fluidRow(
                  box(plotOutput("histogram")),
                  box(sliderInput("bins", "Number of Breaks", 1, 100, 50))
                )
          
        )
      )
      
    )
  )
)

# # # # # # # # # #