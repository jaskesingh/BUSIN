
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(readr)
library(plotly)

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
            scale_y_continuous(limits = c(0, 500)) + facet_wrap(~Country) + 
            labs(y = "Number of respondents", fill = "Buy EV")
        
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
    
    output$employ <- renderPlotly({
        f1 <- eusurvey %>% filter(Employment_status %in% input$estatus)
        p1 <- f1 %>% ggplot(aes(Employment_status)) +
            geom_bar(aes(fill = buy_electric), position = "dodge") +
            scale_y_continuous(limits = c(0, 6000))
        ggplotly(p1)
    })
    
    output$surveytotal <- renderValueBox({
        valueBox(
            nrow(eusurvey), subtitle = "Number of Survey respondents"
        )
    })
})
   
#selectInput(inputId = "gender",
#label = "choose gender",
#choices = c("M", "F"))