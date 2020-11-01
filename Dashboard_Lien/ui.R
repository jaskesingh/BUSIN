#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
Revenue <- read_xlsx("data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Revenues (automotive)", col_types = c("text", "text", "numeric", "numeric"))

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Financiële cijfers Telsa "),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "Yearrev", 
                        label = "Kies het jaar",
                        min = 2008,
                        max = 2020,
                        value = 2020)
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "col")
        )
    )
))
