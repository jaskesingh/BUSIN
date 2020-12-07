
library(shiny)
library(shinydashboard)
library(shinyjs)
library(htmltools)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(lubridate)
library(readr)
library(plotly)
library(DT)
library(tidyquant)
library(quantmod)

eusurvey <- read.csv("data/hev1.csv")

shinyServer(function(input, output) {
    
    output$ggcountry <- renderPlotly({
        f2 <- eusurvey %>% filter(Country == input$gcountry)
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
        teslacountries <- c("Austria", "Belgium", "Czech Republic", "Denmark", 
                            "Finland", "France", "Germany", "Ireland", "Italy", 
                            "Luxembourg", "Netherlands", "Norway", "Slovenia", 
                            "Spain", "Sweden", "Switzerland")
        
        eusurvey$tesla_sold <- ifelse(eusurvey$Country %in% teslacountries, 1, 0) 
        eusurvey <- eusurvey %>% mutate(tesla_sold = as.logical(tesla_sold))
        
        ggplotly(eusurvey %>% group_by(Country, tesla_sold) %>% summarize(n=n(),prop=sum(buy_electric==1)/n()) %>%
        ggplot(aes(Country, prop)) + geom_point(aes(color = tesla_sold)) + 
            labs(y = "Percentage of people willing to buy ev", x = "Countries", color = "Tesla sold") +
            theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
            scale_color_manual(values=c("black", "red"))
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
    
    output$tslastock <- renderPlotly({
        
        TSLA <- tq_get("TSLA", get = "stock.prices", from = input$st, to = input$en)
        
        p <- TSLA %>% ggplot(aes(date , close)) + geom_line() +
            labs(title = "TSLA stock evolution", y = "Closing Price", x = "") + 
            theme_tq()
        
        ggplotly(p)
    })
    
    runjs({'
        var el2 = document.querySelector(".skin-blue");
        el2.className = "skin-blue sidebar-mini";
        var clicker = document.querySelector(".sidebar-toggle");
        clicker.id = "switchState";
    '})
    
    onclick('switchState', runjs({'
        var title = document.querySelector(".logo")
        if (title.style.visibility == "hidden") {
          title.style.visibility = "visible";
        } else {
          title.style.visibility = "hidden";
        }
  '}))
    
})