#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(ggplot2)
library(tidyr)
Revenue <- read_xlsx("data/Revenue-gross margin-gross profit worldwide 2015-2020.xlsx", sheet = "Revenues (automotive)", col_types = c("numeric", "text", "numeric", "numeric"))

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Financiële cijfers Telsa "),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "Yearrev", 
                        label = "Kies het jaar",
                        min = min(Revenue$Year),
                        max = max(Revenue$Year),
                        value = 2020)
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "col")
        )
    )
))
