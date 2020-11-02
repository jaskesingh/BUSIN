
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
        eusurvey %>% filter(Gender == "F") %>% 
            ggplot(aes(Gender)) + 
            geom_bar(aes(fill = buy_electric), position = "dodge")  +
            scale_y_continuous(limits = c(0, 6000))
    })
    
    output$emale <- renderPlot({
        eusurvey %>% filter(Gender == "M") %>% 
            ggplot(aes(Gender)) + 
            geom_bar(aes(fill = buy_electric), position = "dodge") +
            scale_y_continuous(limits = c(0, 6000))
    })
    
    output$gcountry <- renderPlot({
        eusurvey %>% filter(Country %in% input$gcountry) %>% 
            ggplot(aes(Gender)) + 
            geom_bar(aes(fill = buy_electric), position = "dodge") +
            scale_y_continuous(limits = c(0, 500)) + facet_wrap(~Country)
        
    })
    
    output$country <- renderDataTable({
        eusurvey %>% filter(Country == input$country) %>% 
            group_by(Country, Gender, buy_electric) %>% 
            summarise(n = n(), median(Age)) %>% 
            arrange(desc(n))
    })
    
    output$view <- renderPlot({
        eusurvey %>% 
            filter(Country == input$incountry, Income_group == input$incomegr) %>% 
            ggplot(aes(Income_group)) + 
            geom_bar(aes(fill = buy_electric), position = "dodge")
    })
})
   
#selectInput(inputId = "gender",
#label = "choose gender",
#choices = c("M", "F"))