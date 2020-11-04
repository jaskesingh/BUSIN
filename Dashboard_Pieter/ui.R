

# Libraries
library(shiny)
library(shinydashboard)

# Shiny UI
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Menu"),
    dashboardSidebar(
      menuItem("Growth (Later)"),
      # Pitch document: Growth --> Comparison of electric vehicles of different brands
      menuItem("Brand Loyalty (Work-In-Progress)")
      # Pitch document: Customers --> Purchase history --> Loyalty
    ),
    dashboardBody()
  )
)