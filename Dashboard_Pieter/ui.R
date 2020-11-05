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
        # Maybe add icons in final dashboard (video 3)
        menuItem("Growth (Later)", tabName =  "dashboard_growth"),
      
        # Pitch document: Customers --> Purchase history --> Loyalty
        menuItem("Brand Loyalty (Work-In-Progress)", tabName = "dashboard_loyalty")
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard_growth",
                fluidRow(
                  box(plotOutput("histogram_growth")),
                  box(sliderInput("bins_growth", "Number of Breaks", 1, 100, 10))
                  )
                ),
        
        tabItem(tabName = "dashboard_loyalty", 
                fluidRow(
                  box(title = "Loyalty per brand", status = "primary", solidHeader = T, plotOutput("histogram_loyalty")),
                  box(status = "primary", sliderInput("bins_loyalty", "Number of Breaks", 1, 100, 50))
                  )
                )
          
        )
    )
      
  )
)


# # # # # # # # # #