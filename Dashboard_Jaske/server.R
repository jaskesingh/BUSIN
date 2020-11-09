
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(readr)
library(plotly)
library(DT)

eusurvey <- read.csv("data/hev1.csv")

shinyServer(function(input, output) {
    
    output$ggcountry <- renderPlotly({
        f2 <- eusurvey %>% filter(Country %in% input$gcountry)
        p2 <- f2 %>% ggplot(aes(Gender)) + 
            geom_bar(aes(fill = buy_electric), position = "dodge") +
            facet_wrap(~Country) + 
            labs(y = "Number of respondents", fill = "Buy EV")
        ggplotly(p2) %>% 
            layout( 
                xaxis = list(automargin=TRUE), 
                yaxis = list(automargin=TRUE)
            )
    })
    
    output$country <- renderDataTable({
        t1 <- eusurvey %>% filter(Country == input$country) %>%
            group_by(Country, Gender, buy_electric) %>% 
            summarise(median(Age), n = n()) %>% 
            arrange(desc(n))
        datatable(t1, filter = "top")
    })
    
    output$view <- renderPlotly({
        f3 <- eusurvey %>% filter(Country == input$incountry, Income_group == input$incomegr)
        p3 <- f3 %>% ggplot(aes(Income_group)) + 
            geom_bar(aes(fill = buy_electric), position = "dodge") +
            labs(y = "Number of respondents", fill = "Buy EV") +
            theme(axis.text.x = element_text(angle = 60, hjust = 1))
        ggplotly(p3)
    })
    
    output$employ <- renderPlotly({
        f1 <- eusurvey %>% filter(Employment_status %in% input$estatus)
        p1 <- f1 %>% ggplot(aes(Employment_status)) +
            geom_bar(aes(fill = buy_electric), position = "dodge") +
            labs(y = "Number of respondents", fill = "Buy EV") +
            theme(axis.text.x = element_text(angle = 60, hjust = 1))
        ggplotly(p1) %>% 
            layout( 
                xaxis = list(automargin=TRUE), 
                yaxis = list(automargin=TRUE)
            )
    })
    
    output$plan <- renderPlotly({
        f4 <- eusurvey %>% filter (Country == input$carplancountry)
        p4 <- f4 %>% ggplot(aes(Plan_to_purchase_vehicle)) + 
                     geom_bar(aes(fill = buy_electric), position = "dodge") + 
                     labs(y = "Number of respondents", x = "Plan to buy car", fill = "Buy EV") +
                     theme(axis.text.x = element_text(angle = 60, hjust = 1))
        ggplotly(p4)
                 
    })
    
    output$propev <- renderPlotly({
        ggplotly(eusurvey %>% group_by(Country) %>% summarize(n=n(),prop=sum(buy_electric==1)/n()) %>%
        ggplot(aes(Country, prop)) + geom_point() + 
            labs(y = "Percentage of people willing to buy ev", x = "Countries") +
            theme(axis.text.x = element_text(angle = 60, hjust = 1))
        )
    })
    
    output$surveytotal <- renderValueBox({
        valueBox(
            nrow(eusurvey), subtitle = "Number of respondents", icon = icon("user-alt")
        )
    })
    
    output$totalcountries <- renderValueBox({
        valueBox(
            length(unique(eusurvey$Country)), subtitle = "Number of countries",
            icon = icon("globe-europe")
        )
    })
})