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
library(data.table)
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
  
  main_data <- sqlQuery(conn,paste0("select * from Price_Table"))
  
  hidden_Columns <- c("symbol", "marketCap", "pe", "description", "industry", "sector", "isin", "prevClose", "low", "open", "pc", "1D", "5D", "volume", "isActivelyTrading", "priceAvg50", "priceAvg200") 
  
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
    
  output$details <- renderReactable({
  # req(input$getDiff_id, input$getDHP_id)
      
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

  
  # output$getDiff <- renderUI({div(class = "label-left",sliderInput("getDiff_id", "Difference", min = min_num(main_data['yearHigh - dayHigh']), max = max_num(main_data['yearHigh - dayHigh']), value = c(min_num(main_data['yearHigh - dayHigh']),max_num(main_data['yearHigh - dayHigh']))))})
  # output$getDHP <- renderUI({div(class = "label-left",sliderInput("getDHP_id", "Day High Price", min = round_of_two(min_num(main_data['dayHigh'])), max = round_of_two(max_num(main_data['dayHigh'])), value = c(min_num(main_data['dayHigh']),max_num(main_data['dayHigh']))))})
  # output$getEPS <- renderUI({div(class = "label-left",sliderInput("getEPS_id", "EPS", min = round_of_two(min_num(main_data['eps'])), max = round_of_two(max_num(main_data['eps'])), value = min_num(main_data['eps'])))})
  # output$getPrice <- renderUI({div(class = "label-left",sliderInput("getPrice_id", "Price", min = round_of_two(min_num(main_data['price'])), max = round_of_two(max_num(main_data['price'])), value = min_num(main_data['price'])))})
  # output$getID <- renderUI({div(class = "label-left",sliderInput("getID_id", "1D Change", min = min_num(main_data['1D']), max = max_num(main_data['1D']), value = c(min_num(main_data['1D']),max_num(main_data['1D']))))})
  # output$get5D <- renderUI({div(class = "label-left",sliderInput("get5D_id", "5D Change", min = min_num(main_data['5D']), max = max_num(main_data['5D']), value = c(min_num(main_data['5D']),max_num(main_data['5D']))))})
  # 
  # 
  # main_data_selected <- lapply(rjson::fromJSON(file = gsub("2021-09-24", Sys.Date(), gsub("2021-08-24", Sys.Date()-60, gsub("companies_to_be_passed", capture.output(cat(as.character(main_data[which(main_data$`yearHigh - dayHigh` == 0),]$symbol), sep = ",")), get_Links('To get historical prices', FALSE))))), nullToNA) %>% pluck("historicalStockList") %>%  map_dfr(~ as_tibble(.x) %>% unnest_wider(historical))
  
  md <- main_data %>% filter(low > 0, marketCap > 0, volume > 0, isActivelyTrading == TRUE) %>% pull(symbol)
  # browser() 
  
  
  #  main_data_selected <- as.data.frame(main_Data_Get_Historical(md, 'To get historical prices', from = 1, to = 5))
  # if(nrow(main_data_selected) > 1){
  #   output$prof_plot <- renderPlotly({
  #     profit_function_plot(main_data_selected, symbol, open, input$returns, input$investment)
  #   })
  # 
  #   output$prof_details <- renderReactable({
  #     reactable(class = "rt", rownames = F, as.data.frame(profit_function(main_data_selected, symbol, open, input$returns, input$investment))[c(1:6,15)],
  #               columns = c(list(symbol = colDef(html = TRUE,filterable = TRUE))))
  #   })
  # }
  

}




shinyApp(ui, server)