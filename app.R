library(shiny)
library(jsonlite)
library(rjson)
library(stringr)
require(reshape2)
library(dplyr)
library(tidyverse)
require(reshape2)
library(readxl)
library(lubridate)
library(zoo)
library(shinyalert)
library(DT)
library(plotly)
library(rhandsontable)
library(kableExtra)
library(reactable)
library(glue)
library(shinyjs)
library(shinycssloaders)
source("DButils.R")
source("functions.R")

ui <- fluidPage(includeCSS("www/design.css"),includeScript("www/design.js"),
  br(),
  # uiOutput("getCom"),
  tabsetPanel(
    type = "tabs",
    tabPanel("Summary",   br(), fluidRow(
        column(3,uiOutput("getDiff")),
        column(3,uiOutput("getDHP")),
        column(3,uiOutput("getEPS")),
        column(3,uiOutput("getPrice")),
      br()),
      fluidRow(
        column(3,uiOutput("getID")),
        column(3,uiOutput("get5D"))),
        br(),
        reactableOutput("details")%>% withSpinner(color="#0dc5c1")
      ),
        tabPanel("Profit Prediction", br(), fluidRow(span(column(3,(div(style="margin-left:5%", numericInput("investment", "Investment :", value = 100000)))),
                                                          column(3,div(numericInput("returns", "No of days to get return :", value = 1, max = 10, min = 1))))), br(),
                 fluidRow(column(6, br(), br(), br(), div(plotlyOutput("prof_plot")%>% withSpinner(color="#0dc5c1"))),
                          column(6, div(reactableOutput("prof_details")%>% withSpinner(color="#0dc5c1"))))))
)

server <- function(input, output, session) {
  
  storage <- reactiveValues()
  storage$modelData <- data.frame()
  # storage$company_info <- data.frame()
  # storage$real_Time_price <- data.frame()
  # storage$Stock_price_Change <- data.frame()
  main_data <- do.call(rbind.data.frame, lapply(rjson::fromJSON(file = get_Links('To get latest day prices', TRUE)), nullToNA))
  main_company_info <- do.call(rbind.data.frame, lapply(rjson::fromJSON(file = get_Links('To get Company Information', TRUE)), nullToNA))
  main_real_Time_price <- do.call(rbind.data.frame, lapply(rjson::fromJSON(file = get_Links('To get Real Time Price', TRUE)), nullToNA))
  main_Stock_price_Change <- do.call(rbind.data.frame, lapply(rjson::fromJSON(file = get_Links('To get Stock Price Change', TRUE)), nullToNA))
  historical_Price <- lapply(rjson::fromJSON(file = gsub("2021-09-24", Sys.Date(), gsub("2021-08-24", Sys.Date()-20, get_Links('To get historical prices', TRUE)))), nullToNA) %>% pluck("historicalStockList") %>%  map_dfr(~ as_tibble(.x) %>% unnest_wider(historical))
  main_data['yearHigh - dayHigh'] <- round(main_data['yearHigh'],0) - round(main_data['dayHigh'], 0)
  main_data['eps'] <- round(main_data['eps'],2)
  

  company_info_Columns <- c('description', 'industry', 'sector', 'isin', 'isActivelyTrading', 'fullTimeEmployees')
  real_Time_price_Columns <- c('prevClose', 'low')
  Stock_price_Change_Columns <- c('X1D', 'X5D')
    
  main_data <- inner_join(main_data, main_company_info[c(company_info_Columns, 'symbol')], by='symbol')
  main_data <- inner_join(main_data, main_real_Time_price[c(real_Time_price_Columns, 'symbol')], by='symbol')
  main_data <- inner_join(main_data, main_Stock_price_Change[c(Stock_price_Change_Columns, 'symbol')], by='symbol')

  required_Columns <- c('symbol', 'name', 'price', 'yearHigh - dayHigh', 'dayHigh', 'eps', 'marketCap', 'pe', 'open', 'volume', 'priceAvg50', 'priceAvg200', company_info_Columns,real_Time_price_Columns,Stock_price_Change_Columns)
  hidden_Columns <- c("symbol", "marketCap", "pe", "description", "industry", "sector", "isin", "prevClose", "low", "open", "pc", "X1D", "X5D", "volume", "isActivelyTrading", "priceAvg50", "priceAvg200") 
    
    
  output$details <- renderReactable({
  req(input$getDiff_id, input$getDHP_id)
      
  # browser()    
  main_data <- main_data[required_Columns][order(main_data['yearHigh - dayHigh'], decreasing = FALSE),] %>% mutate_at(c('fullTimeEmployees'),
                                                                                                                      as.numeric) %>% filter(low > 0, 
                                                                                                                   marketCap > 0, 
                                                                                                                   volume > 0,
                                                                                                                   isActivelyTrading == TRUE,
                                                                                                                   between(`yearHigh - dayHigh`, input$getDiff_id[1], input$getDiff_id[2]),
                                                                                                                   between(dayHigh, input$getDHP_id[1], input$getDHP_id[2]),
                                                                                                                   between(X1D, input$getID_id[1], input$getID_id[2]),
                                                                                                                   between(X5D, input$get5D_id[1], input$get5D_id[2]))
  main_data$pc <- do.call(paste, c(main_data[Stock_price_Change_Columns], sep = '_'))
  main_data[['name']] <- sapply(paste0(
                                           main_data[['name']],
                                       "_",main_data[['isin']],
                                       "_",main_data[['pc']],
                                       "_",main_data[['priceAvg50']],
                                       "_",main_data[['priceAvg200']]),function(x) htmltools::HTML(unlist(strsplit(x, "_"))[1],
                                                                                                '<span class="a">','ISIN : ', unlist(strsplit(x, "_"))[2],'</span>&nbsp',
                                                                                                price_Change("1D", unlist(strsplit(x, "_"))[3]),
                                                                                                price_Change("5D", unlist(strsplit(x, "_"))[4]),
                                                                                                '<sup style="font-size: smaller">','50D avg : ', round(as.numeric(unlist(strsplit(x, "_"))[5]),2),'</sup>&nbsp',
                                                                                                '<sup style="font-size: smaller">',' /&nbsp&nbsp200D avg : ', round(as.numeric(unlist(strsplit(x, "_"))[6]),2),'</sup>&nbsp'
                                                                                                )
      )
  
  
      
#   main_data$ht <- '<html>
# <script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.4/Chart.js"></script>
# <body>
# <canvas id="myChart" style="width:100%;max-width:600px"></canvas>
# 
# <script>
# const xValues = [50,60,70,80,90,100,110,120,130,140,150];
# const yValues = [7,8,8,9,9,9,10,11,14,14,15];
# 
# new Chart("myChart", {
#   type: "line",
#   data: {
#     labels: xValues,
#     datasets: [{
#       fill: false,
#       lineTension: 0,
#       backgroundColor: "rgba(0,0,255,1.0)",
#       borderColor: "rgba(0,0,255,0.1)",
#       data: yValues
#     }]
#   },
#   options: {
#     legend: {display: false},
#     scales: {
#       yAxes: [{ticks: {min: 6, max:16}}],
#     }
#   }
# });
# </script>
# 
# </body>
# </html>'   
  # storage$modelData <- main_data     
  reactable(class = "rt", rownames = F, defaultPageSize = 15, columns = c(list(name = colDef(html = TRUE,width = 1000,
                                                                       filterable = TRUE)),lapply(setNames(hidden_Columns, hidden_Columns), function(x){x = colDef(show =F)})),
        defaultColDef = colDef(show = T), main_data, details = function(index) {
        htmltools::div(tags$style("table {
  font-family: arial, sans-serif;
  border-collapse: collapse;
  margin: 40px;
}

td, th {
  border: 1px solid #dddddd;
  text-align: left;
  padding: 2px;
}


tr:nth-child(even) {
}"),tags$table(tags$tr(
                  tags$th('Symbol')
                 ,tags$th('Sector')
                 ,tags$th('Industry')
                 ,tags$th('Company Info')
                 ,tags$th('PE')
                 ,tags$th('Market Cap')
                 ,tags$th('prevClose')
                 ,tags$th('low')
                 ,tags$th('open')
                 )
                 ,tags$tr(
                  tags$td(main_data[index,][['symbol']])
                 ,tags$td(main_data[index,][['sector']])
                 ,tags$td(main_data[index,][['industry']])
                 ,tags$td(main_data[index,][['description']])
                 ,tags$td(main_data[index,][['pe']])
                 ,tags$td(format(main_data[index,][['marketCap']],scientific = FALSE))
                 ,tags$td(main_data[index,][['prevClose']])
                 ,tags$td(main_data[index,][['low']])
                 ,tags$td(main_data[index,][['open']])
                 )))
        
      })
})    

  
  output$getDiff <- renderUI({div(class = "label-left",sliderInput("getDiff_id", "Difference", min = min_num(main_data['yearHigh - dayHigh']), max = max_num(main_data['yearHigh - dayHigh']), value = c(min_num(main_data['yearHigh - dayHigh']),max_num(main_data['yearHigh - dayHigh']))))})
  output$getDHP <- renderUI({div(class = "label-left",sliderInput("getDHP_id", "Day High Price", min = round_of_two(min_num(main_data['dayHigh'])), max = round_of_two(max_num(main_data['dayHigh'])), value = c(min_num(main_data['dayHigh']),max_num(main_data['dayHigh']))))})
  output$getEPS <- renderUI({div(class = "label-left",sliderInput("getEPS_id", "EPS", min = round_of_two(min_num(main_data['eps'])), max = round_of_two(max_num(main_data['eps'])), value = min_num(main_data['eps'])))})
  output$getPrice <- renderUI({div(class = "label-left",sliderInput("getPrice_id", "Price", min = round_of_two(min_num(main_data['price'])), max = round_of_two(max_num(main_data['price'])), value = min_num(main_data['price'])))})
  output$getID <- renderUI({div(class = "label-left",sliderInput("getID_id", "1D Change", min = min_num(main_data['X1D']), max = max_num(main_data['X1D']), value = c(min_num(main_data['X1D']),max_num(main_data['X1D']))))})
  output$get5D <- renderUI({div(class = "label-left",sliderInput("get5D_id", "5D Change", min = min_num(main_data['X5D']), max = max_num(main_data['X5D']), value = c(min_num(main_data['X5D']),max_num(main_data['X5D']))))})
  
  
  main_data_selected <- lapply(rjson::fromJSON(file = gsub("2021-09-24", Sys.Date(), gsub("2021-08-24", Sys.Date()-60, gsub("companies_to_be_passed", capture.output(cat(as.character(main_data[which(main_data$`yearHigh - dayHigh` == 0),]$symbol), sep = ",")), get_Links('To get historical prices', FALSE))))), nullToNA) %>% pluck("historicalStockList") %>%  map_dfr(~ as_tibble(.x) %>% unnest_wider(historical))
  # browser()
  if(nrow(main_data_selected) > 1){
    output$prof_plot <- renderPlotly({
      profit_function_plot(main_data_selected, symbol, open, input$returns, input$investment)
    })
    
    output$prof_details <- renderReactable({ 
      reactable(class = "rt", rownames = F, as.data.frame(profit_function(main_data_selected, symbol, open, input$returns, input$investment))[c(1:6,15)],
                columns = c(list(symbol = colDef(html = TRUE,
                                               filterable = TRUE))))
    })
  }
  

}




shinyApp(ui, server)