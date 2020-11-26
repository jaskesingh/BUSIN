library(readxl)
library(shinydashboard)
library(maps)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggmap)
library(mapdata)
library(mapproj)
library(sf)
library(rnaturalearth)
library(hrbrthemes)
library(grid)
library(rworldmap)
library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(DT)
library(rvest)
library(stringr)
library(tidyverse)
library(gghighlight)
library(scales)
library(ggExtra)
library(toOrdinal)
library(tidyquant)
library(quantmod)
library(ggflags) #geef dit in om te installeren: devtools::install_github("rensa/ggflags")
library(RSQLite)

# Establish connection with the database
con <- dbConnect(drv = SQLite(), "Tesla_database.sqlite")

#Read tables
VPS <- dbReadTable(con, "VPS")
Nieuw <- dbReadTable(con, "Nieuw")
NieuwMS <- dbReadTable(con, "NieuwMS")
Tweedehands <- dbReadTable(con, "Tweedehands")
TweedehandsMS <- dbReadTable(con, "TweedehandsMS")
EuMS <- dbReadTable(con, "EuMS")
groco_data_gather <- dbReadTable(con, "groco_data_gather")
aankoopproces <- dbReadTable(con, "aankoopproces")
loyalty_per_brand_ranked_tibble <- dbReadTable(con, "loyalty_per_brand_ranked_tibble")
eusurvey <- dbReadTable(con, "eusurvey")
Data <- dbReadTable(con, "Data")
Financial_numbers_gather_norm <- dbReadTable(con, "Financial_numbers_gather_norm")
Financial_numbers_gather_som <- dbReadTable(con, "Financial_numbers_gather_som")
aantal <- dbReadTable(con, "aantal")
superchargers <- dbReadTable(con, "superchargers")
verkoo <- dbReadTable(con, "verkoo")
taart <- dbReadTable(con, "taart")
laadpalen <- dbReadTable(con, "laadpalen")
countriesafpassengercars <- dbReadTable(con, "countriesafpassengercars")
countriesafinfrastructure <- dbReadTable(con, "countriesafinfrastructure")
tesla.eu.map <- dbReadTable(con, "tesla.eu.map")

dbDisconnect(con)

##financiele cijfers, functies
financefunction <- function(yearinput,df) {
  financefunction <- df %>% filter(df$Year == yearinput)
  return(financefunction)
}

# Define server logic required to draw a map
shinyServer(function(input, output, session) {
  
  #Growth
  ## Sales per segment
  ### Infoboxes 
    output$bestsoldsegment <- renderValueBox({
      VPSC2 <- VPS %>% filter(Year == max(input$Year2))
      valueBox(
        paste0(VPSC2$Segment[VPSC2$Sales == max(VPSC2$Sales)]),
        subtitle= paste("Best sold segment in ", max(input$Year2)), icon = icon('car-side'), color = "red")
    })
  
  
    output$populairst <- renderValueBox({
      VPSC2 <- VPS %>% filter(Year == max(input$Year2) | Year == min(input$Year2))
      VPSC2 <- VPSC2 %>% group_by(Segment) %>% mutate(Difference = (Sales[Year == max(Year)] - Sales[Year == min(Year)]))
      valueBox(
        paste0(VPSC2$Segment[VPSC2$Difference == max(VPSC2$Difference) & VPSC2$Year == max(input$Year2)]),
        subtitle= paste("Segment that has augmented the most between ", min(input$Year2), " and ", max(input$Year2)), icon = icon('car-side'), color = "red")
    })
  
  ### Graph
  output$line01 <- renderPlotly({
    VPSC2 <- VPS %>% filter(Year >= min(input$Year2) & Year <= max(input$Year2))
    p <- VPSC2 %>% ggplot(aes(x=Year, y=Sales)) + geom_line(aes(color = Segment)) + labs(title = "New cars sold in the EU by segment in million units over the years.") + 
      scale_x_continuous(breaks = c(2008:2019)) + scale_y_continuous(breaks= seq(0,6, by = 1)) + ylab("Cars sold") + theme_minimal() + scale_color_manual(values = c("red", "orange", "green", "lightseagreen", "blue", "purple"))
    ggplotly(p)})
  
  
  ## Sales per fuel type
  ### Infoboxes
    output$bestsoldfuel <- renderValueBox({
      if(checkregion() == 1){
        NieuwC <- Nieuw %>% filter(Year == max(input$Year3))
        valueBox(
          paste0(NieuwC$Fuel[NieuwC$Cars.sold == max(NieuwC$Cars.sold)]),
          subtitle= paste("Best sold type of car in Belgium in ", max(input$Year3)), icon = icon('gas-pump'), color = "red"
      )}
      else{
        TweedehandsC <- Tweedehands %>% filter(Year == max(input$Year3))
        valueBox(
          paste0(TweedehandsC$Fuel[TweedehandsC$Cars.sold == max(TweedehandsC$Cars.sold)]),
          subtitle= paste("Best sold type of second hand car in Belgium in ", max(input$Year3)), icon = icon('gas-pump'), color = "red"
        )
      }
    })
  
    output$populairstfuel <- renderValueBox({
      if(checkregion() == 1){
        NieuwC <- Nieuw %>% filter(Year == max(input$Year3) | Year == min(input$Year3))
        NieuwC <- NieuwC %>% group_by(Fuel) %>% mutate(Difference = (Cars.sold[Year == max(Year)] - Cars.sold[Year == min(Year)]))
        valueBox(
          paste0(NieuwC$Fuel[NieuwC$Difference == max(NieuwC$Difference) & NieuwC$Year == max(input$Year3)]),
          subtitle= paste("Type of car that has augmented the most in Belgium between ", min(input$Year3), " and ", max(input$Year3)), icon = icon('gas-pump'), color = "red"
        )}
      else{
        TweedehandsC <- Tweedehands %>% filter(Year == max(input$Year3) | Year == min(input$Year3))
        TweedehandsC <- TweedehandsC %>% group_by(Fuel) %>% mutate(Difference = (Cars.sold[Year == max(Year)] - Cars.sold[Year == min(Year)]))
        valueBox(
          paste0(TweedehandsC$Fuel[TweedehandsC$Difference == max(TweedehandsC$Difference) & TweedehandsC$Year == max(input$Year3)]),
          subtitle= paste("Type of second hand car that has augmented the most in Belgium between ", min(input$Year3), " and ", max(input$Year3)), icon = icon('gas-pump'), color = "red"
        )
      }
    })
    
    output$bestsoldfueleu <- renderValueBox({
      EuMSC <- EuMS %>% filter(Year == input$Year7)
      valueBox(
        paste0(EuMSC$Fuel[EuMSC$Market.Share == max(EuMSC$Market.Share)]),
        subtitle= paste("Best sold type of car in the EU in ", input$Year7), icon = icon('gas-pump'), color = "red"
      )
    })
    
    output$populairstfueleu <- renderValueBox({
      EUMSC2 <- EuMS %>% filter(Year == max(input$Year8) | Year == min(input$Year8))
      EUMSC2 <- EUMSC2 %>% group_by(Fuel) %>% mutate(Difference = (Market.Share[Year == max(Year)] - Market.Share[Year == min(Year)]))
      valueBox(
        paste0(EUMSC2$Fuel[EUMSC2$Difference == max(EUMSC2$Difference) & EUMSC2$Year == max(input$Year8)]),
        subtitle= paste("Type of car that has augmented the most in the EU between ", min(input$Year8), " and ", max(input$Year3)), icon = icon('gas-pump'), color = "red"
      )
    })
  
  ### Graphs
    checkregion <- reactive({input$Region})
    output$line02 <- renderPlotly({
      if(checkregion() == 1) {
        NieuwC <- Nieuw %>% filter(Year >= min(input$Year3) & Year <= max(input$Year3))
        p2 <- NieuwC %>% ggplot(aes(x = Year, y = Cars.sold)) + geom_line(aes(color = Fuel)) + labs(title = "Number of new cars sold in Belgium over the years") + theme_minimal() +
          scale_color_manual(values = c("purple", "orange", "red", "green", "blue")) + ylab("Cars sold")
        ggplotly(p2)
      }
      else{
        #lijn tweedehands: groei: aandeel elektrische auto's op belgische en eu markt
        TweedehandsC <- Tweedehands %>% filter(Year >= min(input$Year3) & Year <= max(input$Year3))
        p3 <- TweedehandsC %>% ggplot(aes(x = Year, y = Cars.sold)) + geom_line(aes(color = Fuel)) + labs(title = "Number of second hand cars sold in Belgium over the years") + theme_minimal() +
          scale_color_manual(values = c("purple", "orange", "red", "green", "blue")) + ylab("Cars sold")
        ggplotly(p3)
      }
    })
    
    checktype <- reactive({input$Region2})
    output$pie02 <- renderPlotly({
      if(checktype() == 1){
        NieuwMSC <- NieuwMS %>% filter(Year == input$Year5)
        fig1 <- plot_ly(NieuwMSC, labels = ~Fuel, values = ~Market.Share, type = 'pie')
        fig1 <- fig1 %>% layout(title = "Market share of new cars by fuel type in Belgium",
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      }
      else{
        #taart tweedehands: groei: aandeel elektrische auto's op belgische en eu markt
        TweedehandsMSC <- TweedehandsMS %>% filter(Year == input$Year5)
        fig2 <- plot_ly(TweedehandsMSC, labels = ~Fuel, values = ~Market.Share, type = 'pie')
        fig2 <- fig2 %>% layout(title = "Market share of second hand cars by fuel type in Belgium",
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      }
    })
    
    output$pie04 <- renderPlotly({
      EuMSC <- EuMS %>% filter(Year == input$Year7)
      fig3 <- plot_ly(EuMSC, labels = ~Fuel, values = ~Market.Share, type = 'pie')
      fig3 <- fig3 %>% layout(title = "Market share of new cars by fuel type in the EU",
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    output$hist05 <- renderPlotly({
      EuMSC2 <- EuMS %>% filter(Year >= min(input$Year8) & Year <= max(input$Year8))
      h5 <- EuMSC2 %>% ggplot(aes(x = Year, y = Market.Share)) + geom_line(aes(color = Fuel)) + labs(title = "Market Share of new cars in the EU over the years", input$Fuel3,"cars in the EU over the years") +
        scale_y_continuous(limits = c(0, 60), breaks = seq(0,60, by= 10)) + theme_minimal() + scale_color_manual(values = c("purple", "orange", "red", "green", "blue")) + ylab("Market share")
      ggplotly(h5)
    })
  
  
  ## Best selling EV's compared
    
    # KPI ???
    
    output$growth_comparison_bar <- renderPlotly({
    
      groco_filtered_data <- groco_data_gather %>% filter(!is.na(Submodel), Type == input$growth_select_box, !is.na(Value)) %>% 
        mutate(isvalue = (Submodel %in% c("Tesla Model 3","Tesla Model 3 Standard Range Plus","Tesla Model 3 Long Range","Tesla Model 3 Performance","Tesla Model S",
                                          "Tesla Model S Long Range","Tesla Model S Performance","Tesla Model X","Tesla Model X Long Range","Tesla Model X Performance"))) %>%
                                                   select(Submodel, Value, isvalue)

      groco_filtered_data <- groco_filtered_data[order(groco_filtered_data$Value), ]

      groco_filtered_data$Submodel <- factor(groco_filtered_data$Submodel,
                                                     levels = groco_filtered_data$Submodel)
      
      
      groco_plot <-  groco_filtered_data  %>% 
                        ggplot(
                             aes(x = Value,
                                 y = Submodel,
                                 fill = isvalue
                             )) +
                        geom_col(position = "dodge") +
                        theme_minimal() +
                        scale_fill_manual(name = "Hidden_legend",
                                          values = c("coral2", "red2")) +
                        removeGridY() +
                        theme(legend.position = "none") 
      
      ggplotly(groco_plot, tooltip = c("x", "y"))
  
    })
  
  
  
  #Customers
  ## Purchase process

    output$hist07 <- renderPlotly({
      aankoopprocesC2 <- aankoopproces %>% filter(Country %in% input$Country4, Interest %in% input$Interest)
      aankoopprocesC2 <- aankoopprocesC2 %>% group_by(Interest) %>% mutate(Level = ifelse(Percentage == max(Percentage), "Highest", ifelse(Percentage == min(Percentage), "Lowest", "Between")))
      h7 <- aankoopprocesC2 %>% ggplot(aes(x = Country, y = Percentage, fill = Level)) + geom_col() + facet_wrap(Interest~.) + labs(title = "Share of Europeans interested in online vehicle purchasing in 2018" ) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(limits = c(0, 70), breaks = seq(0,70, by= 10)) + theme_minimal() + scale_fill_manual(values = c("red", "green", "blue")) 
      ggplotly(h7)})
  
  ##Brand loyalty
  ### Infoboxes  
  
    output$loyalty_percentage_of_tesla <- renderValueBox({
      loyalty_per_brand_ranked_Tesla <- loyalty_per_brand_ranked_tibble %>%
        filter(Brand == "Tesla")
      loyalty_perc_of_tesla <- loyalty_per_brand_ranked_Tesla$Percentage
      loyalty_perc_of_tesla <- percent(loyalty_perc_of_tesla,
                                       accuracy = 0.1)
      valueBox(
        loyalty_perc_of_tesla,
        subtitle = "Loyalty of Tesla's customers",
        icon = icon('hand-holding-heart'),
        color = "red"
      )
      
    })
    
    output$loyalty_rank_of_tesla <- renderValueBox({
      loyalty_per_brand_ranked_Tesla <- loyalty_per_brand_ranked_tibble %>%
        filter(Brand == "Tesla") 
      loyalty_rank_tesla_number <- loyalty_per_brand_ranked_Tesla$Rank
      loyalty_ordinal_rank_tesla <- toOrdinal(loyalty_rank_tesla_number)
      valueBox(
        loyalty_ordinal_rank_tesla,
        subtitle = "Place of Tesla in loyalty ranking",
        icon = icon('award'), #keuze uit award, crown of trophy
        color = "red"
      )
      
    })
    
    
  ### Graph
    output$loyalty_bar <- renderPlotly({

      loyalty_per_brand_chosen_class <- loyalty_per_brand_ranked_tibble %>% filter(Classification %in% input$loyalty_checkboxes)

      loyalty_per_brand_ranked_Tesla <- loyalty_per_brand_ranked_tibble %>%
        filter(Brand == "Tesla") 

      loyalty_per_brand_chosen_class <- loyalty_per_brand_chosen_class %>% filter(!Brand %in% c("Tesla"))

      loyalty_per_brand_chosen_class <- loyalty_per_brand_chosen_class %>% add_row(Brand = loyalty_per_brand_ranked_Tesla$Brand,
                                                                                   Percentage = loyalty_per_brand_ranked_Tesla$Percentage,
                                                                                   Classification = loyalty_per_brand_ranked_Tesla$Classification,
                                                                                   Rank = loyalty_per_brand_ranked_Tesla$Rank
      )

      loyalty_per_brand_chosen_class <- loyalty_per_brand_chosen_class[order(loyalty_per_brand_chosen_class$Percentage), ]

      loyalty_per_brand_chosen_class$Brand <- factor(loyalty_per_brand_chosen_class$Brand,
                                                     levels = loyalty_per_brand_chosen_class$Brand)

      loyalty_per_brand_plot <- ggplot(loyalty_per_brand_chosen_class,
                                       aes(x = Percentage,
                                           y = Brand,
                                           fill = factor(ifelse(Brand == "Tesla", "Highlighted", "Normal"))
                                       )
                              ) +
                                geom_col() + 
                                theme_minimal() +
                                scale_fill_manual(name = "Hidden_legend", 
                                                  values = c("red2", "coral2")) +
                                scale_x_continuous(breaks = seq(0, 1, 0.1),
                                                   limits = c(0, 1),
                                                   labels = percent_format(accuracy = 1),
                                                   expand = expansion(mult = c(0, 0.01))
                                ) +
                                removeGridY() +
                                theme(axis.text = element_text(size = 12),
                                      axis.title = element_text(size = 15),
                                      legend.position = "none") 

      ggplotly(loyalty_per_brand_plot, tooltip = c("x", "y"))

      
    })
  
  ## Survey
  ### Infoboxes
    
    output$surveytotal <- renderValueBox({
      valueBox(
        nrow(eusurvey), subtitle = "Number of respondents", icon = icon("user-alt"), color = 'red'
      )
    })
    
    output$totalcountries <- renderValueBox({
      valueBox(
        length(unique(eusurvey$Country)), subtitle = "Number of countries",
        icon = icon("globe-europe"), color = 'red'
      )
    })
    
  ### Table
    
    output$country <- renderDataTable({
      t1 <- eusurvey %>% filter(Country == input$ccountry) %>%
        group_by(Country, as.factor(Gender), as.logical(buy_electric)) %>% 
        summarise(median(Age), n = n()) %>% rename("Gender" = "as.factor(Gender)",
                                                   "Average age" = "median(Age)",
                                                   "Buy electric" = "as.logical(buy_electric)",
                                                   "Number of respondents" = "n")
      datatable(t1, filter = "top")
    })
    
  ### Graph
    
    output$view <- renderPlotly({
      f3 <- eusurvey %>% filter(Country == input$incountry, Income_group == input$incomegr)
      p3 <- f3 %>% ggplot(aes(Income_group)) + 
        geom_bar(aes(fill = as.logical(buy_electric)), position = "dodge") +
        labs(y = "Number of respondents", fill = "Buy EV") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))  + theme_minimal() + 
        scale_fill_manual(values = c("red", "lightseagreen"))
      ggplotly(p3)
    })
    
    output$employ <- renderPlotly({
      f1 <- eusurvey %>% filter(Employment_status %in% input$estatus)
      p1 <- f1 %>% ggplot(aes(Employment_status)) +
        geom_bar(aes(fill = as.logical(buy_electric)), position = "dodge") +
        labs(y = "Number of respondents", fill = "Buy EV") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme_minimal() + 
        scale_fill_manual(values = c("red", "lightseagreen"))
      ggplotly(p1) %>% 
        layout( 
          xaxis = list(automargin=TRUE), 
          yaxis = list(automargin=TRUE)
        )
    })
    
    output$ggcountry <- renderPlotly({
      f2 <- eusurvey %>% filter(Country %in% input$gcountry)
      p2 <- f2 %>% ggplot(aes(Gender)) + 
        geom_bar(aes(fill = as.logical(buy_electric)), position = "dodge") +
        facet_wrap(~Country) + 
        labs(y = "Number of respondents", fill = "Buy EV") + theme_minimal() + 
        scale_fill_manual(values = c("red", "lightseagreen"))
      ggplotly(p2) %>% 
        layout( 
          xaxis = list(automargin=TRUE), 
          yaxis = list(automargin=TRUE)
        )
    })
    
    output$plan <- renderPlotly({
      f4 <- eusurvey %>% filter (Country %in% input$carplancountry)
      p4 <- f4 %>% ggplot(aes(Plan_to_purchase_vehicle)) + 
        geom_bar(aes(fill = as.logical(buy_electric)), position = "dodge") + 
        labs(y = "Number of respondents", x = "Plan to buy car", fill = "Buy EV") + theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +  
        scale_fill_manual(values = c("red", "lightseagreen"))
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
                 labs(y = "Percentage of people willing to buy ev", x = "Countries", color = "Tesla sold") + theme_minimal() +
                 theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
                 scale_color_manual(values=c("black", "red"))
      )
    })

  
  
  
  #Sales
  ## Periodic analysis
    
    output$line04 <- renderPlotly({
      DataC <- Data %>% filter(Month >= min(input$Month) & Month <= max(input$Month), Year %in% input$Year9)
      MeanSales <- DataC %>% group_by(Month) %>% summarise(Sales = mean(Sales, na.rm = T) )
      p4 <- DataC %>% ggplot(aes(x= Month, y = Sales, na.rm = T)) + geom_line(aes(color = Year)) + geom_line(data = MeanSales, color = 'black') + scale_x_continuous(breaks = seq(0,12, by = 1)) + theme_minimal() + scale_color_manual(values = c("red", "orange", "green", "lightseagreen", "blue"))
      ggplotly(p4, tooltip = c("x", "y", "color"))
    })
    
    
  
  #Finance
  ### Infoboxes
    
    output$revbox <- renderValueBox({
      somjaar <- Financial_numbers_gather_som %>% filter(Year == input$Yearrev, Type == 'Revenue ') %>% group_by(Year) 
      
      valueBox(
        paste0(format(round(somjaar$Total[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle= paste0("Revenue ", input$Yearrev, " in million"), 
        icon = icon("dollar-sign"), color = 'red'
      )
      
    })
    
    output$frcashbox <- renderValueBox({
      somjaar <- Financial_numbers_gather_som %>% filter(Year == input$Yearrev, Type == 'Free cash flow ') %>% group_by(Year)
      if(somjaar$Total[]>=0) {  
        valueBox(
          paste0(format(round(somjaar$Total[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
          subtitle = paste0("Free cashflow ", input$Yearrev, " in million"), 
          icon = icon("dollar-sign"), color = 'red'
        )
      }
      else {
        valueBox(
          paste0(format(round(somjaar$Total[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
          subtitle = paste0("Free Cashflow ", input$Yearrev, " in million"), 
          icon = icon("dollar-sign"), color = 'purple'
        )
      }
    })
    
    output$grprbox <- renderValueBox({
      somjaar <- Financial_numbers_gather_som %>% filter(Year == input$Yearrev, Type == 'Gross Profit ') %>% group_by(Year)
      valueBox(
        paste0(format(round(somjaar$Total[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)), 
        subtitle = paste0("Gross profit ", input$Yearrev, " in million"),  
        icon = icon("piggy-bank"), color = 'red'
      )
    })
    
  ### Graph
    
    output$colfin <- renderPlotly({
      x    <- Financial_numbers_gather_norm$Year
      Yearrev <- seq(min(x), max(x), length.out = input$Yearrev)
      
      financevar <- financefunction(input$Yearrev, Financial_numbers_gather_norm) %>% filter(Type != "Gross Margin")
      Value <- financevar$finvalue/1000000
      
      financevarpcol <- financevar %>% ggplot(aes(x = Quarter, y = Value, fill = Type))+ geom_col(position = "dodge") + 
        labs(title = input$Yearrev, y = 'Value') + scale_fill_manual(values = c("green", "lightseagreen", "blue")) +  
        theme_minimal() + geom_hline(yintercept = 0, color = "black", size = 1.5)
      
      ggplotly(financevarpcol)
    })
    
    output$linefin <- renderPlotly({
      y    <- Financial_numbers_gather_som$Year
      Yearrevline <- seq(min(y), max(y))
      financevar <- Financial_numbers_gather_som %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline), Type != "Gross Margin ") %>% group_by(Year, Type) %>% 
        select(Year, Total, Type)%>% distinct()
      
      financevarpline <- financevar %>% ggplot(aes(x = Year , y = Total, color = Type))+ geom_line() +
        labs(y = 'Value') + scale_x_continuous(breaks = seq(min(Yearrevline), max(Yearrevline), by = 1)) +
        theme_minimal() + scale_color_manual(values = c("green","lightseagreen", "blue")) + geom_hline(yintercept = 0, color = "black", size = 1.5)
      ggplotly(financevarpline)
    })
    
    output$grossmargin <- renderPlotly({
      y <- Financial_numbers_gather_som$Year
      Yeargrossmargin <- seq(min(y), max(y))
      financevarmar <- Financial_numbers_gather_som %>% filter(Year >= min(input$Yeargrossmargin) & Year <= max(input$Yeargrossmargin), Type == "Gross Margin ") %>% group_by(Year, Type) %>% 
        select(Year, Total, Type) %>% distinct()
      
      financevarmarp <- financevarmar %>% ggplot(aes(x = Year, y = Total, color = Type)) + geom_line() + 
        labs(y = 'Value') + scale_y_continuous(limits = c(50,100), breaks = seq(50, 100, by= 5)) + scale_x_continuous(breaks = seq(min(Yeargrossmargin), max(Yeargrossmargin), by = 1)) +
        theme_minimal() + scale_color_manual(values = c("purple", "red")) + stat_smooth(method = 'lm', se = FALSE, aes(color = 'Trend'))
      
      ggplotly(financevarmarp, tooltip = c("x", "y", "color"))
    })
    
    output$tslastock <- renderPlotly({
      
      TSLA <- tq_get("TSLA", get = "stock.prices", from = input$st, to = input$en)
      
      p <- TSLA %>% ggplot(aes(date , close)) + geom_line() +
        labs(title = "TSLA stock evolution", y = "Closing Price", x = "") + 
        stat_smooth(method = 'lm', se = FALSE, aes(color = 'Trend')) +
        theme_tq() + scale_color_manual(values = c("red"))
      
      ggplotly(p, tooltip = c("x", "y"))
    })
    
  
  #Superchargers
  ## Map
  ### Table
    output$table01 <- renderDataTable({
      DT::datatable(superchargers, selection = "single",options=list(stateSave = TRUE))
    })
    
    # to keep track of previously selected row
    prev_row <- reactiveVal()
    
    # new icon style
    my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
    
    observeEvent(input$table01_rows_selected, {
      row_selected = superchargers[input$table01_rows_selected,]
      proxy <- leafletProxy('mymap')
      proxy %>%
        addAwesomeMarkers(popup=as.character(row_selected$mag),
                          layerId = as.character(row_selected$id),
                          lng=row_selected$Longitude, 
                          lat=row_selected$Latitude,
                          icon = my_icon)
      
      # Reset previously selected marker
      if(!is.null(prev_row()))
      {
        proxy %>%
          addMarkers(popup=as.character(prev_row()$Street.Address), 
                     layerId = as.character(prev_row()$id),
                     lng=prev_row()$Longitude, 
                     lat=prev_row()$Latitude)
      }
      
      # set new value to reactiveVal 
      prev_row(row_selected)
    })
    
  ### Map Graph
    output$mymap <- renderLeaflet({
      leaflet() %>% addTiles() %>% addMarkers(data = superchargers, layerId = as.character(superchargers$id) )
    })
    
    observeEvent(input$mymap_marker_click, {
      clickId <- input$mymap_marker_click$id
      dataTableProxy("table01") %>%
        selectRows(which(superchargers$id == clickId)) %>%
        selectPage(which(input$table01_rows_all == clickId) %/% 
                     input$table01_state$length+1)})
  
  ## Statistics
  ### Infoboxes
    
    output$totbox <- renderValueBox({
      valueBox(
        paste0(sum(aantal$freq)),
        subtitle= "Total number of supercharger stations", icon = icon('charging-station'), color = "red"
      )
    })
    output$openbox <- renderValueBox({
      valueBox(
        paste0(aantal$freq[aantal$Status == "OPEN"]),
        subtitle= "Number of open supercharger stations", icon = icon('charging-station'), color = "red"
      )
    })
    output$buildbox <- renderValueBox({
      valueBox(
        paste0(aantal$freq[aantal$Status == "CONSTRUCTION"]),
        subtitle= "Number of building supercharger stations", icon = icon('tools'),color = "red"
      )
    })
    output$permitbox <- renderValueBox({
      valueBox(
        paste0(aantal$freq[aantal$Status == "PERMIT"]),
        subtitle= "Number of permit supercharger stations", icon = icon('scroll'), color = "red"
      )
    })
    output$pclosedbox <- renderValueBox({
      valueBox(
        paste0(aantal$freq[aantal$Status == "CLOSED_PERM"]),
        subtitle= "Number of permantly closed supercharger stations", icon = icon('lock'), color = "red"
      )})
    output$tclosedbox <- renderValueBox({
      valueBox(
        paste0(aantal$freq[aantal$Status == "CLOSED_TEMP"]),
        subtitle= "Number of temporarly closed supercharger stations", icon = icon('unlock'), color = "red"
      )
    })
    
  ### Graph 
    output$hist01 <- renderPlot({
      verkooC <- verkoo %>% dplyr::filter(Country %in% input$Country, Year == input$Year)
      superchargersC <- superchargers %>% dplyr::filter(Year < input$Year+1, Status == 'OPEN', Country %in% input$Country)
      superchargersC <- plyr::count(superchargersC, "Country")
      ratio <- full_join(superchargersC, verkooC, by = 'Country')
      ratio$freq <- as.integer(ratio$freq)
      ratio[is.na(ratio)] = 0
      ratio$Country <- as.factor(ratio$Country)
      #h1 <- ratio %>% ggplot(aes(x= freq, y = Sales, label = Country)) + geom_point() + geom_text(aes(label = Country), check_overlap = TRUE, vjust = "outward", hjust = "inward") + labs(title = paste0("Teslas/supercharger station in ", input$Year)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        #scale_y_continuous(limits = c(0, 31000), breaks = seq(0,31000, by= 5000)) + scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, by = 10)) + ylab(label = "Number of Teslas sold" ) + xlab(label = "Number of supercharger stations") + theme_minimal()
      ratio %>% ggplot(aes(x= freq, y= Sales, country = Countries, size= freq)) + geom_flag() + scale_country() + scale_size(range = c(0, 15)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        ylab(label = "Number of Teslas sold" ) + xlab(label = "Number of supercharger stations") + theme_minimal()
      #ggplotly(h1)
    })
    
    selected_points <- reactiveVal()
    
    # update the reactiveVal whenever input$plot1_brush changes, i.e. new points are selected.
    observeEvent(input$plot1_brush,{
      selected_points( brushedPoints(ratio, input$plot1_brush))
    })
    
    output$brush_info <- renderPrint({
      selected_points()
    })
    
    output$click_info <- renderPrint({
      nearPoints(ratio, input$plot1_click, addDist = TRUE)
    })
    
    #output$table02 <- renderDataTable({
      #verkooC <- verkoo %>% dplyr::filter(Country %in% input$Country, Year == input$Year)
      #superchargersC <- superchargers %>% dplyr::filter(Year < input$Year+1, Status == 'OPEN', Country %in% input$Country)
      #superchargersC <- plyr::count(superchargersC, "Country")
      #ratio <- full_join(superchargersC, verkooC, by = 'Country')
      #ratio$freq <- as.integer(ratio$freq)
      #ratio[is.na(ratio)] = 0
      #ratio$Country <- as.factor(ratio$Country)
      #DT::datatable(ratio, selection = "single", options=list(stateSave = TRUE))
    #})
  
  ## Competition
  ### Graph
   
    output$hist02 <- renderPlotly({
      laadpalenC <- laadpalen %>% filter(Country %in% input$Country2)
      laadpalenC <- laadpalenC %>% group_by(Country) %>% mutate(Level = ifelse(freq[Description == "Tesla"] == max(freq), "Highest", ifelse(freq[Description == "Tesla"] == min(freq), "Lowest", "Between")))
      h2 <- laadpalenC %>% ggplot(aes(x = Description, y = freq, fill = Level)) + geom_col() + gghighlight(Description == "Tesla", calculate_per_facet = T, use_direct_label = F) + facet_wrap(Country~., nrow = 3, ncol = 9) + theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(limits = c(0, 100), breaks = seq(0,100, by= 20)) + ylab("Number of supercharger stations") + xlab("Brand") + scale_fill_manual(values = c("green", "red")) + scale_x_discrete(labels = c("Ionity", "Tesla"))
      #h2 <- laadpalenC %>% ggplot(aes(x = Country, y = freq, fill = Description)) + geom_col(position = "dodge") + theme_minimal() + scale_y_continuous(limits = c(0, 100), breaks = seq(0,100, by= 20)) + ylab("Number of supercharger stations") + coord_flip() + scale_fill_manual(values = c("orange", "blue"))
      ggplotly(h2, tooltip = c("x", "y"))
    })
    

    output$pie01 <- renderPlotly({
      fig <- plot_ly(taart, labels = ~Description, values = ~ratio, type = 'pie')
      fig <- fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    
  
  # Expansion in Europe
  ### Graph
    checkeurope <- reactive({input$Europe})
    output$colpascar <- renderPlotly({
      
      if (checkeurope() == 2) {
        countriespasscarvar <- countriesafpassengercars %>% filter(Country == input$EUoptions)
        value <- countriespasscarvar$waardes
        
        
        countriespasscarvarp <- countriespasscarvar %>% 
          ggplot(aes(x = Year, y = value, fill = Fuel)) + 
          geom_col(position = "dodge") + 
          labs(title = input$EUoptions, y = '')  + 
          scale_x_continuous(limits = c(min(countriesafpassengercars$Year), max(countriesafpassengercars$Year+1)),
                             breaks = seq(min(countriesafpassengercars$Year), max(countriesafpassengercars$Year), by = 1)) +
          theme_minimal() + scale_fill_manual(values = c("red", "orange", "green", "lightseagreen", "blue", "purple", "maroon1"))
      }
      
      else { 
        countriespasscarvar <- countriesafpassengercars %>% filter(Year == input$YearEU)
        value <- countriespasscarvar$waardes

        countriespasscarvarp <- countriespasscarvar %>% 
          ggplot(aes(x = Country, y = value, fill = Fuel ))+ 
          geom_col(position = "dodge") + 
          labs(title = input$YearEU, y = '')  + scale_y_continuous(limits = c(0, 3600000), breaks = seq(0,4000000, by= 500000)) +
          coord_flip() + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
          scale_fill_manual(values = c("red", "orange", "green", "lightseagreen", "blue", "purple", "maroon1"))
      }
      ggplotly(countriespasscarvarp)
    })
    
    output$colinfr <- renderPlotly({
      if (checkeurope() == 2) {
        countriesinfrvar <- countriesafinfrastructure %>% filter(Country == input$EUoptions)
        value <- countriesinfrvar$waardes
        
        countriesinfrvarp <- countriesinfrvar %>% 
          ggplot(aes(x = Year, y = value, fill = Fuel))+ 
          geom_col(position = "dodge") + 
          labs(title = input$EUoptions, y = '')  + 
          scale_x_continuous(limits = c(min(countriesafinfrastructure$Year), max(countriesafinfrastructure$Year+1)), 
                             breaks = seq(min(countriesafinfrastructure$Year), max(countriesafinfrastructure$Year), by = 1)) + 
          theme_minimal() + 
          scale_fill_manual(values = c("purple", "green", "blue", "orange", "maroon1"))
      }
      
      else {
        countriesinfrvar <- countriesafinfrastructure %>% filter(Year == input$YearEU)
        value <- countriesinfrvar$waardes
        
        countriesinfrvarp <- countriesinfrvar %>% 
          ggplot(aes(x = Country, y = value, fill = Fuel))+ 
          geom_col(position = "dodge") + 
          labs(title = input$YearEU, y = '')  + scale_y_continuous(limits = c(0, 65000), breaks = seq(0,65000, by= 5000)) +
          coord_flip() + theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))  + 
          scale_fill_manual(values = c("purple", "green", "blue", "orange", "maroon1"))
      }
      ggplotly(countriesinfrvarp)
    })
  
  ### Table
    output$europemaptable <- renderDataTable({
      
      teslamaptablevarshow <- tesla.eu.map %>% filter(jaar <= input$teslajaar & waarde >= 1) %>% 
        group_by(region) %>% 
        summarise(Year = min(jaar, na.rm = T)) %>% 
        select(Year, region) %>% 
        distinct() %>% 
        arrange(desc(Year))
      datatable(teslamaptablevarshow)
    })
    
  ### Graph map
    output$distPlot <- renderPlot({
      teslamap <- function(inputjaar, df) {
        if (inputjaar == "2013") {
          teslamap <- df %>% filter(df$jaar == inputjaar, df$waarde > 0)
        }
        else {
          teslamap <- df %>% filter(df$jaar == c(inputjaar, (as.numeric(inputjaar) - 1)), df$waarde > 0)  
        }
        return(teslamap)
      }
      
      teslamapvar <- teslamap(input$teslajaar, tesla.eu.map)
      gg <- teslamapvar %>% ggplot() + geom_map(dat = tesla.eu.map, map = tesla.eu.map, aes(map_id = region), fill = "white", color="black") + coord_map(ylim = c(35, 71))
      if (input$teslajaar == "2013") {
        gg <- gg + geom_map(map = tesla.eu.map, aes(map_id = region, fill = jaar), colour = "black")
      }
      else {
        gg <- gg + geom_map(map = tesla.eu.map, aes(map_id = region, fill = jaar), colour = "black")
      }
      gg <- gg + expand_limits(x = tesla.eu.map$long, y = tesla.eu.map$lat)
      gg <- gg + theme(axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(),
                       legend.position = "none",
                       panel.border = element_blank(),
                       strip.background = element_rect(fill = 'white', colour = 'white')) + scale_fill_manual( values = c("red", "green"))
      
      gg
    })
}) 

    

  

