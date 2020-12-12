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
library(shinyjs)
library(htmltools)
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
library(ggflags) #Installation: devtools::install_github("rensa/ggflags")
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

## Financial numbers, functions
###Function filters on the year selected by the user]
financefunction <- function(yearinput,df) {
  financefunction <- df %>% filter(df$Year == yearinput)
  return(financefunction)
}

# Define server 
shinyServer(function(input, output, session) {
  
  # Growth
  ## Sales per segment
  ### Infoboxes
  #### Shows best sold segment of the last year that is selected
    output$bestsoldsegment <- renderValueBox({
      VPSC2 <- VPS %>% filter(Year == max(input$Year2))
      valueBox(
        paste0(VPSC2$Segment[VPSC2$Sales == max(VPSC2$Sales)]),
        subtitle= paste("Best sold segment in ", max(input$Year2)), icon = icon('car-side'), color = "red")
    })
  
  #### Shows the segment that has augmented the most between the selected years
    output$populairst <- renderValueBox({
      VPSC2 <- VPS %>% filter(Year == max(input$Year2) | Year == min(input$Year2))
      VPSC2 <- VPSC2 %>% group_by(Segment) %>% mutate(Difference = (Sales[Year == max(Year)] - Sales[Year == min(Year)]))
      valueBox(
        paste0(VPSC2$Segment[VPSC2$Difference == max(VPSC2$Difference) & VPSC2$Year == max(input$Year2)]),
        subtitle= paste("Segment that has augmented the most between ", min(input$Year2), " and ", max(input$Year2)), icon = icon('car-side'), color = "red")
    })
  
  ### Graph
  #### Shows interactive line graph of new cars sold in the Eu by segment in million units over the years
  output$line01 <- renderPlotly({
    VPSC2 <- VPS %>% filter(Year >= min(input$Year2) & Year <= max(input$Year2))
    p <- VPSC2 %>% ggplot(aes(x=Year, y=Sales)) + geom_line(aes(color = Segment)) + 
      scale_x_continuous(breaks = c(2008:2019)) + scale_y_continuous(breaks= seq(0,6, by = 1)) + ylab("Cars sold") + theme_minimal() + scale_color_manual(values = c("red", "orange", "green", "lightseagreen", "blue", "purple"))
    ggplotly(p)})
  
  
  ## Sales per fuel type
  ### Infoboxes
  #### Shows the best sold type of car in Belgium in the last selected year
    output$bestsoldfuel <- renderValueBox({
  ##### Checks if new (1) or second hand (0) is selected    
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
  
  #### Shows the type of car that has augmented the most in Belgium between the selected years  
    output$populairstfuel <- renderValueBox({
  ##### Checks if new (1) or second hand (0) is selected    
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
    
  #### Shows the best sold type of car in the EU in the last selected year 
    output$bestsoldfueleu <- renderValueBox({
      EuMSC <- EuMS %>% filter(Year == input$Year7)
      valueBox(
        paste0(EuMSC$Fuel[EuMSC$Market.Share == max(EuMSC$Market.Share)]),
        subtitle= paste("Best sold type of car in the EU in ", input$Year7), icon = icon('gas-pump'), color = "red"
      )
    })
    
  #### Shows the type of car that has augmented the most in the EU between the selected years  
    output$populairstfueleu <- renderValueBox({
      EUMSC2 <- EuMS %>% filter(Year == max(input$Year8) | Year == min(input$Year8))
      EUMSC2 <- EUMSC2 %>% group_by(Fuel) %>% mutate(Difference = (Market.Share[Year == max(Year)] - Market.Share[Year == min(Year)]))
      valueBox(
        paste0(EUMSC2$Fuel[EUMSC2$Difference == max(EUMSC2$Difference) & EUMSC2$Year == max(input$Year8)]),
        subtitle= paste("Type of car that has augmented the most in the EU between ", min(input$Year8), " and ", max(input$Year3)), icon = icon('gas-pump'), color = "red"
      )
    })
  
  ### Graphs
  #### Checks which graphs it has to show: new or second hand  
    checkregion <- reactive({input$Region})
  ##### Shows interactive line graph of the number of cars sold in Belgium over the selected years
    output$line02 <- renderPlotly({
  ###### Checks if new (1) or second hand (0) is selected 
      if(checkregion() == 1) {
        NieuwC <- Nieuw %>% filter(Year >= min(input$Year3) & Year <= max(input$Year3))
        p2 <- NieuwC %>% ggplot(aes(x = Year, y = Cars.sold)) + geom_line(aes(color = Fuel)) + labs(title = "New cars") + theme_minimal() +
          scale_color_manual(values = c("purple", "orange", "red", "green", "blue")) + ylab("Cars sold")
        ggplotly(p2)
      }
      else{
        TweedehandsC <- Tweedehands %>% filter(Year >= min(input$Year3) & Year <= max(input$Year3))
        p3 <- TweedehandsC %>% ggplot(aes(x = Year, y = Cars.sold)) + geom_line(aes(color = Fuel)) + labs(title = "Second hand cars") + theme_minimal() +
          scale_color_manual(values = c("purple", "orange", "red", "green", "blue")) + ylab("Cars sold")
        ggplotly(p3)
      }
    })
    
  #### Checks which graphs it has to show: new or second hand    
    checktype <- reactive({input$Region2})
  #### Shows interactive pie chart of the market share of cars by fuel type in Belgium in the selected year
    output$pie02 <- renderPlotly({
  ###### Checks if new (1) or second hand (0) is selected 
      if(checktype() == 1){
        NieuwMSC <- NieuwMS %>% filter(Year == input$Year5)
        fig1 <- plot_ly(NieuwMSC, labels = ~Fuel, values = ~Market.Share, type = 'pie')
        fig1 <- fig1 %>% layout(title = "New cars",
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      }
      else{
        TweedehandsMSC <- TweedehandsMS %>% filter(Year == input$Year5)
        fig2 <- plot_ly(TweedehandsMSC, labels = ~Fuel, values = ~Market.Share, type = 'pie')
        fig2 <- fig2 %>% layout(title = "Second hand cars",
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      }
    })
    
  #### Shows interactive pie chart of the market share of new cars by fuel type in the EU in the selected year
    output$pie04 <- renderPlotly({
      EuMSC <- EuMS %>% filter(Year == input$Year7)
      fig3 <- plot_ly(EuMSC, labels = ~Fuel, values = ~Market.Share, type = 'pie')
      fig3 <- fig3 %>% layout(title = "New cars",
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
  #### Shows interactive line graph of the market share of new cars by fuel type in the EU over the selected years
    output$hist05 <- renderPlotly({
      EuMSC2 <- EuMS %>% filter(Year >= min(input$Year8) & Year <= max(input$Year8))
      h5 <- EuMSC2 %>% ggplot(aes(x = Year, y = Market.Share)) + geom_line(aes(color = Fuel)) + labs(title = "New cars") +
        scale_y_continuous(limits = c(0, 60), breaks = seq(0,60, by= 10)) + theme_minimal() + scale_color_manual(values = c("purple", "orange", "red", "green", "blue")) + ylab("Market share")
      ggplotly(h5)
    })
  
  
  ## Best selling EV's compared
  ###Infoboxes
  #### Shows sales of Tesla Model S in 2019
    output$teslas <- renderValueBox({
      groco_data_gather_4 <- groco_data_gather %>% filter(Submodel == "Tesla Model S", Type == "Sales In 2019")
      valueBox(
        paste0(format(groco_data_gather_4$Value, decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle= paste("Tesla model S sales 2019"), icon = icon('car-side'), color = "red")
    })
  #### Shows sales of Tesla Model S in 2019
    output$teslax <- renderValueBox({
      groco_data_gather_3 <- groco_data_gather %>% filter(Submodel == "Tesla Model X", Type == "Sales In 2019")
        valueBox(
          paste0(format(groco_data_gather_3$Value, decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
          subtitle= paste("Tesla model X sales 2019"), icon = icon('car-side'), color = "red")
    })
  #### Shows sales of Tesla Model S in 2019
    output$tesla3 <- renderValueBox({
      groco_data_gather_2 <- groco_data_gather %>% filter(Submodel == "Tesla Model 3", Type == "Sales In 2019")
      valueBox(
        paste0(format(groco_data_gather_2$Value, decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle= paste("Tesla model 3 sales 2019"), icon = icon('car-side'), color = "red")
    })
    
  ###Graph
    output$growth_comparison_bar <- renderPlotly({
    
  #### Use the inputs to filter the data so that only those submodels that have values are retained + select all Tesla models (to later color them using red2 instead of coral2)
      groco_filtered_data <- groco_data_gather %>% filter(!is.na(Submodel), Type == input$growth_select_box, !is.na(Value)) %>% 
        mutate(isvalue = (Submodel %in% c("Tesla Model 3","Tesla Model 3 Standard Range Plus","Tesla Model 3 Long Range","Tesla Model 3 Performance","Tesla Model S",
                                          "Tesla Model S Long Range","Tesla Model S Performance","Tesla Model X","Tesla Model X Long Range","Tesla Model X Performance"))) %>%
                                                   select(Submodel, Value, isvalue)

  #### Order the values
      groco_filtered_data <- groco_filtered_data[order(groco_filtered_data$Value), ]

  #### This makes sure the order is actually retained and properly displayed in the graph
      groco_filtered_data$Submodel <- factor(groco_filtered_data$Submodel,
                                                     levels = groco_filtered_data$Submodel)
      
      
  #### Creates the graph
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
  
  
  
  # Customers
  ## Purchase process
  ### Graph
  #### Shows interactive bar chart of the share of Europeans interested in online vehicle purchasing in 2018  
    output$hist07 <- renderPlotly({
      aankoopprocesC2 <- aankoopproces %>% filter(Country %in% input$Country4, Interest %in% input$Interest)
      aankoopprocesC2 <- aankoopprocesC2 %>% group_by(Interest) %>% mutate(Level = ifelse(Percentage == max(Percentage), "Highest", ifelse(Percentage == min(Percentage), "Lowest", "Between")))
      h7 <- aankoopprocesC2 %>% ggplot(aes(x = Country, y = Percentage, fill = Level)) + geom_col() + facet_wrap(Interest~.) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(limits = c(0, 70), breaks = seq(0,70, by= 10)) + theme_minimal() + scale_fill_manual(values = c("red", "green", "blue")) 
      ggplotly(h7)})
  
  ## Brand loyalty
  ### Infoboxes
  #### Shows loyalty of Tesla customers as percentage
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
    
  #### Shows place of Tesla in loyalty ranking  
    output$loyalty_rank_of_tesla <- renderValueBox({
      loyalty_per_brand_ranked_Tesla <- loyalty_per_brand_ranked_tibble %>%
        filter(Brand == "Tesla") 
      loyalty_rank_tesla_number <- loyalty_per_brand_ranked_Tesla$Rank
      loyalty_ordinal_rank_tesla <- toOrdinal(loyalty_rank_tesla_number)
      valueBox(
        loyalty_ordinal_rank_tesla,
        subtitle = "Place of Tesla in loyalty ranking",
        icon = icon('award'), 
        color = "red"
      )
      
    })
    
    
  ### Graph
  #### Shows the loyalty ranking
    output$loyalty_bar <- renderPlotly({
  
  #### Filters such that only the selected inputs are shown, which can be "luxury", "mass market" or both
      loyalty_per_brand_chosen_class <- loyalty_per_brand_ranked_tibble %>% filter(Classification %in% input$loyalty_checkboxes)
 
  #### Tesla is classified as a luxury brand in our dataset, but we want to make sure that regardless of the selected classification, Tesla is shown. 
  #### That's why we first save Tesla in a separate variable....
      loyalty_per_brand_ranked_Tesla <- loyalty_per_brand_ranked_tibble %>%
        filter(Brand == "Tesla") 

  #### .... and then delete it from the variable with the selection ("luxury", "mass market" or both) applied....
      loyalty_per_brand_chosen_class <- loyalty_per_brand_chosen_class %>% filter(!Brand %in% c("Tesla"))

  #### .... and then we add Tesla again. 
      loyalty_per_brand_chosen_class <- loyalty_per_brand_chosen_class %>% add_row(Brand = loyalty_per_brand_ranked_Tesla$Brand,
                                                                                   Percentage = loyalty_per_brand_ranked_Tesla$Percentage,
                                                                                   Classification = loyalty_per_brand_ranked_Tesla$Classification,
                                                                                   Rank = loyalty_per_brand_ranked_Tesla$Rank
      )
  #### Orders the values
      loyalty_per_brand_chosen_class <- loyalty_per_brand_chosen_class[order(loyalty_per_brand_chosen_class$Percentage), ]

  #### This makes sure the order is actually retained and properly displayed in the graph
      loyalty_per_brand_chosen_class$Brand <- factor(loyalty_per_brand_chosen_class$Brand,
                                                     levels = loyalty_per_brand_chosen_class$Brand)

  #### Create plot
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
  #### Shows number of total surveyrespondents  
    output$surveytotal <- renderValueBox({
      valueBox(
        paste0(format(nrow(eusurvey), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)), 
        subtitle = "Number of respondents", icon = icon("user-alt"), color = 'red'
      )
    })
  #### Shows number of countries where survey was taken  
    output$totalcountries <- renderValueBox({
      valueBox( paste0(format(length(unique(eusurvey$Country)), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle = "Number of countries",
        icon = icon("globe-europe"), color = 'red'
      )
    })
    
  ### Table
  #### Shows country, gender, average age and number of respondents that would buy or not buy an ev
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
  #### Shows the number of respondents per different income group and whether they are willing to buy an ev  
    output$view <- renderPlotly({
      f3 <- eusurvey %>% filter(Country %in% input$incountry, Income_group %in% input$incomegr)
      p3 <- f3 %>% ggplot(aes(x = Income_group, fill = as.logical(buy_electric), 
                              text = paste('Buy EV: ', as.logical(buy_electric)))) + 
        geom_bar(position = "dodge") +
        labs(y = "Number of respondents", fill = "Buy EV") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme_minimal() + 
        scale_fill_manual(values = c("red", "lightseagreen"))
      ggplotly(p3, tooltip = c("count", "x", "text"))
    })
  #### Shows the number of respondents per different employment status and whether they are willing to buy an ev
    output$employ <- renderPlotly({
      f1 <- eusurvey %>% filter(Employment_status %in% input$estatus)
      p1 <- f1 %>% ggplot(aes(x = Employment_status, 
                              text = paste('Buy EV: ', as.logical(buy_electric)))) +
        geom_bar(aes(fill = as.logical(buy_electric)), position = "dodge") +
        labs(y = "Number of respondents", fill = "Buy EV") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme_minimal() + 
        scale_fill_manual(values = c("red", "lightseagreen"))
      ggplotly(p1, tooltip = c("count", "x", "text")) %>% 
        layout( 
          xaxis = list(automargin=TRUE), 
          yaxis = list(automargin=TRUE)
        )
    })
  #### Shows the number of respondents per gender and country and whether they are willing to buy an ev  
    output$ggcountry <- renderPlotly({
      f2 <- eusurvey %>% filter(Country %in% input$gcountry)
      p2 <- f2 %>% ggplot(aes(x = Gender, 
                              text = paste('Buy EV: ', as.logical(buy_electric),
                                           "<br>Country: ", Country))) + 
        geom_bar(aes(fill = as.logical(buy_electric)), position = "dodge") +
        facet_wrap(~Country) + 
        labs(y = "Number of respondents", fill = "Buy EV") + theme_minimal() + 
        scale_fill_manual(values = c("red", "lightseagreen"))
      ggplotly(p2, tooltip = c("count", "x", "Country", "text")) %>% 
        layout( 
          xaxis = list(automargin=TRUE), 
          yaxis = list(automargin=TRUE)
        )
    })
  #### Shows the number of respondents that are planning to purchase a vehicle and whether they are willing to buy an ev  
    output$plan <- renderPlotly({
      f4 <- eusurvey %>% filter (Country %in% input$carplancountry)
      p4 <- f4 %>% ggplot(aes(x = Plan_to_purchase_vehicle, 
                              text = paste('Buy EV: ', as.logical(buy_electric),
                                           '<br>Country: ', Country))) + 
        geom_bar(aes(fill = as.logical(buy_electric)), position = "dodge") + 
        labs(y = "Number of respondents", x = "Plan to buy car", fill = "Buy EV") + theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) +  
        scale_fill_manual(values = c("red", "lightseagreen"))
      ggplotly(p4, tooltip = c("count", "x", "text"))
      
    })
  #### Shows the percentage of respondents that are willing to buy an ev per country and whether Tesla already sells it's ev's in that country  
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

  
  
  
  # Sales
  ## Graph
  ### Shows interactive line graph of the periodic Tesla sales of the selected years  
    output$line04 <- renderPlotly({
      DataC <- Data %>% filter(Month >= min(input$Month) & Month <= max(input$Month), Year %in% input$Year9)
      MeanSales <- DataC %>% group_by(Month) %>% summarise(Sales = mean(Sales, na.rm = T) )
      p4 <- DataC %>% ggplot(aes(x= Month, y = Sales, na.rm = T)) + geom_line(aes(color = Year)) + geom_line(data = MeanSales, color = 'black') + scale_x_continuous(breaks = seq(0,12, by = 1)) + theme_minimal() + scale_color_manual(values = c("red", "orange", "green", "lightseagreen", "blue"))
      ggplotly(p4, tooltip = c("x", "y", "color"))
    })
    
    
  
  #Finance
  ### Infoboxes
  #### Selects the revenue for the chosen year  
    output$revbox <- renderValueBox({
      somjaar <- Financial_numbers_gather_som %>% filter(Year == input$Yearrev, Type == 'Revenue ') %>% group_by(Year) 
      
  #### Shows the revenue for the chosen year     
      valueBox(
        paste0(format(round(somjaar$Total[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
        subtitle= paste0("Revenue ", input$Yearrev, " in million"), 
        icon = icon("dollar-sign"), color = 'red'
      )
    })
  #### Selects the Free cashflow for the chosen year  
    output$frcashbox <- renderValueBox({
      somjaar <- Financial_numbers_gather_som %>% filter(Year == input$Yearrev, Type == 'Free cash flow ') %>% group_by(Year)
      
  #### Shows the free cashflow for the chosen year if the free cashflow is bigger or equal than zero, the box colors red 
      if(somjaar$Total[]>=0) {  
        valueBox(
          paste0(format(round(somjaar$Total[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
          subtitle = paste0("Free cashflow ", input$Yearrev, " in million"), 
          icon = icon("dollar-sign"), color = 'red'
        )
      }
      
  #### Shows the free cashflow for the chosen year if the free cashflow is lower than zero, the box colors purple
      else {
        valueBox(
          paste0(format(round(somjaar$Total[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)),
          subtitle = paste0("Free Cashflow ", input$Yearrev, " in million"), 
          icon = icon("dollar-sign"), color = 'purple'
        )
      }
    })
    
  #### Selects the Gross profit for the chosen year  
    output$grprbox <- renderValueBox({
      somjaar <- Financial_numbers_gather_som %>% filter(Year == input$Yearrev, Type == 'Gross Profit ') %>% group_by(Year)
      
  #### Shows the Gross profit for the chosen year    
      valueBox(
        paste0(format(round(somjaar$Total[], 2), decimal.mark = ",", big.mark = " ", small.mark = " ", small.interval = 3)), 
        subtitle = paste0("Gross profit ", input$Yearrev, " in million"),  
        icon = icon("piggy-bank"), color = 'red'
      )
    })
    
  ### Graph
  #### Filter on the chosen year period and select revenue, free cashflow and gross profit    
    output$linefin <- renderPlotly({
      y    <- Financial_numbers_gather_som$Year
      Yearrevline <- seq(min(y), max(y))
      financevar <- Financial_numbers_gather_som %>% filter(Year >= min(input$Yearrevline) & Year <= max(input$Yearrevline), Type != "Gross Margin ") %>% group_by(Year, Type) %>% 
        select(Year, Total, Type)%>% distinct()
      
  #### Shows interactive line graph of the revenue, free cashflow and gross profit of the selected years. The 0 axis is a bigger black line.      
      financevarpline <- financevar %>% ggplot(aes(x = Year , y = Total, color = Type))+ geom_line() +
        labs(y = 'Value') + scale_x_continuous(breaks = seq(min(Yearrevline), max(Yearrevline), by = 1)) +
        theme_minimal() + scale_color_manual(values = c("green","lightseagreen", "blue")) + geom_hline(yintercept = 0, color = "black", size = 1.5)
      ggplotly(financevarpline)
    })
    
  #### Use the function to filter on the chosen year and select revenue, free cashflow and gross profit    
    output$colfin <- renderPlotly({
      x    <- Financial_numbers_gather_norm$Year
      Yearrev <- seq(min(x), max(x), length.out = input$Yearrev)
      financevar <- financefunction(input$Yearrev, Financial_numbers_gather_norm) %>% filter(Type != "Gross Margin")
      
  #### Make the value smaller by dividing by 1 000 000
      Value <- financevar$finvalue/1000000
      
  #### Shows interactive bar graph of the revenue, free cashflow and gross profit of the selected year per quarter      
      financevarpcol <- financevar %>% ggplot(aes(x = Quarter, y = Value, fill = Type))+ geom_col(position = "dodge") + 
        labs(title = input$Yearrev, y = 'Value') + scale_fill_manual(values = c("green", "lightseagreen", "blue")) +  
        theme_minimal() + geom_hline(yintercept = 0, color = "black", size = 1.5)
      ggplotly(financevarpcol)
    })

  #### Filter on the chosen year period and select gross margin     
    output$grossmargin <- renderPlotly({
      y <- Financial_numbers_gather_som$Year
      Yeargrossmargin <- seq(min(y), max(y))
      financevarmar <- Financial_numbers_gather_som %>% filter(Year >= min(input$Yeargrossmargin) & Year <= max(input$Yeargrossmargin), Type == "Gross Margin ") %>% group_by(Year, Type) %>% 
        select(Year, Total, Type) %>% distinct()
      
  #### Shows interactive line graph of the gross margin of the selected years, and gives a trendline.    
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
        theme_tq() + scale_color_manual(values = c("red")) + theme(legend.title = element_blank())
      
      ggplotly(p, tooltip = c("x", "y"))
    })
    
  
  # Superchargers
  ## Map
  ### Table
    output$table01 <- renderDataTable({
      DT::datatable(superchargers, selection = "single",options=list(stateSave = TRUE))
    })
    
  #### Keeps track of previously selected row
    prev_row <- reactiveVal()
    
  #### Creates new icon style to locate the selected supercharger
    my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
  
  #### Shows selected row on map   
    observeEvent(input$table01_rows_selected, {
      row_selected = superchargers[input$table01_rows_selected,]
      proxy <- leafletProxy('mymap')
      proxy %>%
        addAwesomeMarkers(popup=as.character(row_selected$mag),
                          layerId = as.character(row_selected$id),
                          lng=row_selected$Longitude, 
                          lat=row_selected$Latitude,
                          icon = my_icon)
      
  #### Resets previously selected marker
      if(!is.null(prev_row()))
      {
        proxy %>%
          addMarkers(popup=as.character(prev_row()$Street.Address), 
                     layerId = as.character(prev_row()$id),
                     lng=prev_row()$Longitude, 
                     lat=prev_row()$Latitude)
      }
      
  #### sets new value to reactiveVal 
      prev_row(row_selected)
    })
    
  ### Map Graph
    output$mymap <- renderLeaflet({
      leaflet() %>% addTiles() %>% addMarkers(data = superchargers, layerId = as.character(superchargers$id) )
    })
    
  #### Shows selected supercharger in table  
    observeEvent(input$mymap_marker_click, {
      clickId <- input$mymap_marker_click$id
      dataTableProxy("table01") %>%
        selectRows(which(superchargers$id == clickId)) %>%
        selectPage((which(input$table01_rows_all == clickId)-1) %/% 
                     input$table01_state$length+1)})
  
  ## Statistics
  ### Infoboxes
  #### Shows the total number of supercharger stations 
    output$totbox <- renderValueBox({
      valueBox(
        paste0(sum(aantal$freq)),
        subtitle= "Total number of supercharger stations", icon = icon('charging-station'), color = "red"
      )
    })
  #### Shows the number of open supercharger stations
    output$openbox <- renderValueBox({
      valueBox(
        paste0(aantal$freq[aantal$Status == "OPEN"]),
        subtitle= "Number of open supercharger stations", icon = icon('charging-station'), color = "red"
      )
    })
  #### Shows the number of building supercharger stations
    output$buildbox <- renderValueBox({
      valueBox(
        paste0(aantal$freq[aantal$Status == "CONSTRUCTION"]),
        subtitle= "Number of building supercharger stations", icon = icon('tools'),color = "red"
      )
    })
  #### Shows the number of permit supercharger stations
    output$permitbox <- renderValueBox({
      valueBox(
        paste0(aantal$freq[aantal$Status == "PERMIT"]),
        subtitle= "Number of permit supercharger stations", icon = icon('scroll'), color = "red"
      )
    })
  #### Shows the number of permanently closed supercharger stations
    output$pclosedbox <- renderValueBox({
      valueBox(
        paste0(aantal$freq[aantal$Status == "CLOSED_PERM"]),
        subtitle= "Number of permanently closed supercharger stations", icon = icon('lock'), color = "red"
      )})
  #### Shows the number of temporarly closed supercharger stations
    output$tclosedbox <- renderValueBox({
      valueBox(
        paste0(aantal$freq[aantal$Status == "CLOSED_TEMP"]),
        subtitle= "Number of temporarly closed supercharger stations", icon = icon('unlock'), color = "red"
      )
    })
    
  ### Graph 
  #### Shows scatter plot of the number of Teslas per supercharger station in the selected year
    output$hist01 <- renderPlot({
      verkooC <- verkoo %>% dplyr::filter(Country %in% input$Country, Year == input$Year)
      superchargersC <- superchargers %>% dplyr::filter(Year < input$Year+1, Status == 'OPEN', Country %in% input$Country)
      
  ##### Counts the number of superchargers per country
      superchargersC <- plyr::count(superchargersC, "Country")
      ratio <- full_join(superchargersC, verkooC, by = 'Country')
      ratio$freq <- as.integer(ratio$freq)
      
  ##### Gives the value 0 to countries with no superchargers
      ratio[is.na(ratio)] = 0
      ratio$Country <- as.factor(ratio$Country)
      ratio %>% ggplot(aes(x= freq, y= Sales, country = Countries, size= freq)) + geom_flag() + scale_country() + scale_size(range = c(0, 15)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        ylab(label = "Number of Teslas sold" ) + xlab(label = "Number of supercharger stations") + theme_minimal()
    })
    
  ### Table
    selected_points <- reactiveVal()
    
  #### Updates the reactiveVal whenever input$plot1_brush changes, i.e. new points are selected.
    observeEvent(input$plot1_brush,{
      selected_points( brushedPoints(ratio, input$plot1_brush))
    })
  ##### Shows the information of the selected points 
    output$brush_info <- renderPrint({
      selected_points()
    })
    
    output$click_info <- renderPrint({
      nearPoints(ratio, input$plot1_click, addDist = TRUE)
    })
  
  ## Competition
  ### Graphs
  #### Shows interactive bar chart of the number of supercharger stations per selected country 
    output$hist02 <- renderPlotly({
      laadpalenC <- laadpalen %>% filter(Country %in% input$Country2)
      laadpalenC <- laadpalenC %>% group_by(Country) %>% mutate(Level = ifelse(freq[Description == "Tesla"] == max(freq), "Highest", ifelse(freq[Description == "Tesla"] == min(freq), "Lowest", "Between")))
      h2 <- laadpalenC %>% ggplot(aes(x = Description, y = freq, fill = Level)) + geom_col() + gghighlight(Description == "Tesla", calculate_per_facet = T, use_direct_label = F) + facet_wrap(Country~., nrow = 3) + theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(limits = c(0, 100), breaks = seq(0,100, by= 20)) + ylab("Number of supercharger stations") + xlab("Brand") + scale_fill_manual(values = c("green", "red")) + scale_x_discrete(labels = c("Ionity", "Tesla"))
      ggplotly(h2, tooltip = c("x", "y"))
    })
    
  #### Shows interactive pie chart of superchargers market share
    output$pie01 <- renderPlotly({
      fig <- plot_ly(taart, labels = ~Description, values = ~ratio, type = 'pie')
      fig <- fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    
  
  # Expansion in Europe
  ### Graph
  #### Check whether the graph should show Europe or per country
    checkeurope <- reactive({input$Europe})
    output$colpascar <- renderPlotly({
  #### The graph gives the chosen country   
      if (checkeurope() == 2) {
  #### Select the chosen country and give waardes the name value
        countriespasscarvar <- countriesafpassengercars %>% filter(Country == input$EUoptions)
        value <- countriespasscarvar$waardes
        
  #### Shows interactive bar chart of the amount of car passengers of the selected country for the different fuel types
        countriespasscarvarp <- countriespasscarvar %>% 
          ggplot(aes(x = Year, y = value, fill = Fuel)) + 
          geom_col(position = "dodge") + 
          labs(title = input$EUoptions, y = '')  + 
          scale_x_continuous(limits = c(min(countriesafpassengercars$Year), max(countriesafpassengercars$Year+1)),
                             breaks = seq(min(countriesafpassengercars$Year), max(countriesafpassengercars$Year), by = 1)) +
          theme_minimal() + scale_fill_manual(values = c("red", "orange", "green", "lightseagreen", "blue", "purple", "maroon1"))
      }
      
  #### The graph gives the countries from Europe given in the dataset      
      else { 
  #### Select the chosen year and give waardes the name value
        countriespasscarvar <- countriesafpassengercars %>% filter(Year == input$YearEU)
        value <- countriespasscarvar$waardes
        
  #### Shows interactive bar chart of the amount of car passengers of all the countries in the dataset for the different fuel types
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
  #### The graph gives the chosen country
      if (checkeurope() == 2) {
  #### Select the chosen country and give waardes the name value
        countriesinfrvar <- countriesafinfrastructure %>% filter(Country == input$EUoptions)
        value <- countriesinfrvar$waardes
        
  #### Shows interactive bar chart of the amount of infrastructure per different fuel type for the selected country        
        countriesinfrvarp <- countriesinfrvar %>% 
          ggplot(aes(x = Year, y = value, fill = Fuel))+ 
          geom_col(position = "dodge") + 
          labs(title = input$EUoptions, y = '')  + 
          scale_x_continuous(limits = c(min(countriesafinfrastructure$Year), max(countriesafinfrastructure$Year+1)), 
                             breaks = seq(min(countriesafinfrastructure$Year), max(countriesafinfrastructure$Year), by = 1)) + 
          theme_minimal() + 
          scale_fill_manual(values = c("purple", "green", "blue", "orange", "maroon1"))
      }
  #### The graph gives the countries from Europe given in the dataset     
      else {
  #### Select the chosen year and give waardes the name value
        countriesinfrvar <- countriesafinfrastructure %>% filter(Year == input$YearEU)
        value <- countriesinfrvar$waardes
        
  #### Shows interactive bar chart of the amount of infrastructure per different fuel type for all the countries in the dataset  
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
  

    
  ### Graph map
    output$distPlot <- renderPlot({
  #### The function looks for the year, if it is 2013 the graph will be different. If it is higher than 2013, the selected year and the year below will be taken in consideration for the graph.
      teslamap <- function(inputjaar, df) {
        if (inputjaar == "2013") {
          teslamap <- df %>% filter(df$jaar == inputjaar, df$waarde > 0)
        }
        else {
          teslamap <- df %>% filter(df$jaar == c(inputjaar, (as.numeric(inputjaar) - 1)), df$waarde > 0)  
        }
        return(teslamap)
      }
      
  ###↨# Draw a basic map of Europe, which is non interactive
      teslamapvar <- teslamap(input$teslajaar, tesla.eu.map)
      gg <- teslamapvar %>% ggplot() + geom_map(dat = tesla.eu.map, map = tesla.eu.map, aes(map_id = region), fill = "white", color="black") + coord_map(ylim = c(35, 71))
  
  #### The year is 2013, so color all the countries with a Telsa in 2013 red
      if (input$teslajaar == "2013") {
        gg <- gg + geom_map(map = tesla.eu.map, aes(map_id = region, fill = jaar), colour = "black")
      }
  #### The year is higher than 2013, so color countries with a Tesla for the selected year green and for the year below red to see which ones are new.
      else {
        gg <- gg + geom_map(map = tesla.eu.map, aes(map_id = region, fill = jaar), colour = "black")
      }
      
  #### Adjust the map to get an optimal view
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
  ### Table
  #### Gives the countries that have a Tesla per year of first appearance of Tesla. The newest countries can be found at the top of the table
    output$europemaptable <- renderDataTable({
      teslamaptablevarshow <- tesla.eu.map %>% filter(jaar <= input$teslajaar & waarde >= 1) %>% 
        group_by(region) %>% 
        summarise(Year = min(jaar, na.rm = T)) %>% 
        select(Year, region) %>% 
        distinct() %>% 
        arrange(desc(Year))
      datatable(teslamaptablevarshow)
    })
    
}) 

    

  

