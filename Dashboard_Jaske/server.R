
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
        hev1 %>% filter(Gender == "F") %>% 
            ggplot(aes(Gender)) + 
            geom_bar(aes(fill = buy_electric), position = "dodge")  +
            scale_y_continuous(limits = c(0, 6000))
    })
    
    output$emale <- renderPlot({
        hev1 %>% filter(Gender == "M") %>% 
            ggplot(aes(Gender)) + 
            geom_bar(aes(fill = buy_electric), position = "dodge") +
            scale_y_continuous(limits = c(0, 6000))
    })
    
    output$country <- renderDataTable({
        hev1 %>% filter(Country == input$country) %>% 
            group_by(Country, Gender, buy_electric) %>% 
            summarise(n = n(), median(Age)) %>% 
            arrange(desc(n))
    })
})
   
#selectInput(inputId = "gender",
#label = "choose gender",
#choices = c("M", "F"))