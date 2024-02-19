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
source("DButils.R")
source("functions.R")

ui <- fluidPage(includeCSS("www/design.css"),includeScript("www/design.js"),
  br(),
  # uiOutput("getCom"),
  fluidRow(
            column(3,uiOutput("getDiff")),
            column(3,uiOutput("getDHP")),
            column(3,uiOutput("getEPS")),
            column(3,uiOutput("getPrice"))),
  fluidRow(
    column(3,uiOutput("getID")),
    column(3,uiOutput("get5D"))),
  br(),
  reactableOutput("details")
)

server <- function(input, output, session) {
  
  storage <- reactiveValues()
  # storage$data <- data.frame()
  # storage$company_info <- data.frame()
  # storage$real_Time_price <- data.frame()
  # storage$Stock_price_Change <- data.frame()
  main_data <- do.call(rbind.data.frame, lapply(rjson::fromJSON(file = get_Links('To get latest day prices', TRUE)), nullToNA))
  main_company_info <- do.call(rbind.data.frame, lapply(rjson::fromJSON(file = get_Links('To get Company Information', TRUE)), nullToNA))
  main_real_Time_price <- do.call(rbind.data.frame, lapply(rjson::fromJSON(file = get_Links('To get Real Time Price', TRUE)), nullToNA))
  main_Stock_price_Change <- do.call(rbind.data.frame, lapply(rjson::fromJSON(file = get_Links('To get Stock Price Change', TRUE)), nullToNA))
  main_data['yearHigh - dayHigh'] <- round(main_data['yearHigh'],0) - round(main_data['dayHigh'], 0)
  main_data['eps'] <- round(main_data['eps'],2)
  

  company_info_Columns <- c('description', 'industry', 'sector','isin')
  real_Time_price_Columns <- c('prevClose', 'low')
  Stock_price_Change_Columns <- c('X1D', 'X5D')
    
    # for (i in company_info_Columns){
    #   main_data[i] <- main_company_info[[i]][match(main_company_info[['symbol']],main_data[['symbol']])]
    # }
    # 
    # for (i in real_Time_price_Columns){
    #   main_data[i] <- main_real_Time_price[[i]][match(main_real_Time_price[['symbol']],main_data[['symbol']])]
    # }
    # 
    # for (i in Stock_price_Change_Columns){
    #   main_data[i] <- main_Stock_price_Change[[i]][match(main_Stock_price_Change[['symbol']],main_data[['symbol']])]
    # }
  
  main_data <- inner_join(main_data, main_company_info[c(company_info_Columns, 'symbol')], by='symbol')
  main_data <- inner_join(main_data, main_real_Time_price[c(real_Time_price_Columns, 'symbol')], by='symbol')
  main_data <- inner_join(main_data, main_Stock_price_Change[c(Stock_price_Change_Columns, 'symbol')], by='symbol')

  required_Columns <- c('name', 'price', 'yearHigh - dayHigh', 'dayHigh', 'eps', 'marketCap', 'pe', company_info_Columns,real_Time_price_Columns,Stock_price_Change_Columns)
    
    
    

  print(colnames(main_data))

  output$details <- renderReactable({
  req(input$getDiff_id, input$getDHP_id)
      
  # browser()    
  main_data <- main_data[required_Columns][order(main_data['yearHigh - dayHigh'], decreasing = FALSE),] %>% filter(between(`yearHigh - dayHigh`, input$getDiff_id[1], input$getDiff_id[2]),
                                                                                                                   between(dayHigh, input$getDHP_id[1], input$getDHP_id[2]),
                                                                                                                   between(X1D, input$getID_id[1], input$getID_id[2]),
                                                                                                                   between(X5D, input$get5D_id[1], input$get5D_id[2]))
  main_data$pc <- do.call(paste, c(main_data[Stock_price_Change_Columns], sep = '_'))
  main_data[['name']] <- sapply(paste0(main_data[['name']],"_",main_data[['isin']],"_",main_data[['pc']]),function(x) htmltools::HTML(unlist(strsplit(x, "_"))[1],'<span class="a">', 
                                                                                                'ISIN : ', unlist(strsplit(x, "_"))[2],'</span>&nbsp',
                                                                                                price_Change("1D", unlist(strsplit(x, "_"))[3]),
                                                                                                price_Change("5D", unlist(strsplit(x, "_"))[4]))
      )
      
      
      
  reactable(class = "rt", rownames = F, columns = list(name = colDef(html = TRUE,width = 1000), 
        marketCap = colDef(show =F), 
        pe = colDef(show =F), 
        `description` = colDef(show =F),
        industry = colDef(show =F), 
        sector = colDef(show =F),
        isin = colDef(show =F),
        prevClose = colDef(show =F),
        low = colDef(show =F),
        open = colDef(show =F),
        pc = colDef(show =F),
        X1D = colDef(show =F),
        X5D = colDef(show =F)
        ),
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
                  tags$th('Sector')
                 ,tags$th('Industry')
                 ,tags$th('Company Info')
                 ,tags$th('PE')
                 ,tags$th('Market Cap')
                 ,tags$th('prevClose')
                 ,tags$th('low')
                 ,tags$th('open')
                 )
                 ,tags$tr(
                  tags$td(main_data[index,][['sector']])
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


  output$getDiff <- renderUI({div(class = "label-left",sliderInput("getDiff_id", "Difference", min = min(main_data['yearHigh - dayHigh']), max = max(main_data['yearHigh - dayHigh']), value = c(min(main_data['yearHigh - dayHigh']),max(main_data['yearHigh - dayHigh']))))})
  output$getDHP <- renderUI({div(class = "label-left",sliderInput("getDHP_id", "Day High Price", min = round_of_two(min(main_data['dayHigh'])), max = round_of_two(max(main_data['dayHigh'])), value = c(min(main_data['dayHigh']),max(main_data['dayHigh']))))})
  output$getEPS <- renderUI({div(class = "label-left",sliderInput("getEPS_id", "EPS", min = round_of_two(min(main_data['eps'],na.rm = TRUE)), max = round_of_two(max(main_data['eps'],na.rm = TRUE)), value = min(main_data['eps'],na.rm = TRUE)))})
  output$getPrice <- renderUI({div(class = "label-left",sliderInput("getPrice_id", "Price", min = round_of_two(min(main_data['price'],na.rm = TRUE)), max = round_of_two(max(main_data['price'],na.rm = TRUE)), value = min(main_data['price'],na.rm = TRUE)))})
  output$getID <- renderUI({div(class = "label-left",sliderInput("getID_id", "1D Change", min = min(main_data['X1D']), max = max(main_data['X1D']), value = c(min(main_data['X1D']),max(main_data['X1D']))))})
  output$get5D <- renderUI({div(class = "label-left",sliderInput("get5D_id", "5D Change", min = min(main_data['X5D']), max = max(main_data['X5D']), value = c(min(main_data['X5D']),max(main_data['X5D']))))})

}


shinyApp(ui, server)