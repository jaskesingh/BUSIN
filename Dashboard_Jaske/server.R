
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(readr)

eusurvey <- read.csv("data/hev1.csv")

shinyServer(function(input, output, session) {
    
    output$efemale <- renderPlot({
        hev1 %>% ggplot(aes(Gender)) + geom_bar(aes(fill = buy_electric), position = "dodge")
    })
})
   
#selectInput(inputId = "gender",
#label = "choose gender",
#choices = c("M", "F"))