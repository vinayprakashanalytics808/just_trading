# library(shiny)
# library(jsonlite)
# library(rjson)
# library(stringr)
# require(reshape2)
# library(dplyr)
# library(tidyverse)
# require(reshape2)
# library(readxl)
# library(lubridate)
# library(zoo)
# library(shinyalert)
# library(DT)
# library(plotly)
# library(rhandsontable)
# library(kableExtra)
source("DButils.R")
source("functions.R")

ui <- fluidPage(includeCSS("www/design.css"),includeScript("www/design.js"),
  br(),
  uiOutput("getCom"),
  fluidRow(
            column(3,uiOutput("getDiff")),
            column(3,uiOutput("getDHP")),
            column(3,uiOutput("getEPS")),
            column(3,uiOutput("getPrice"))),
  dataTableOutput("details")
)

server <- function(input, output, session) {
  
  storage <- reactiveValues()
  storage$data <- data.frame()
  storage$data = do.call(rbind.data.frame, lapply(rjson::fromJSON(file = get_Links('To get latest day prices', TRUE)), nullToNA))
  

  
  observe({
    

    storage$data['yearHigh - dayHigh'] <- round(storage$data['yearHigh'],0) - round(storage$data['dayHigh'], 0)
    storage$data['eps'] <- round(storage$data['eps'],2)
    
    req(input$getCom_id, input$getDiff_id, input$getDHP_id)
    
    required_Columns <- c('symbol', 'name', 'price', 'yearHigh - dayHigh', 'dayHigh', 'eps', 'marketCap')
    
    if(input$getCom_id == "ALL" || is.null(input$getCom_id)){
      filter_data <<- storage$data[required_Columns][order(storage$data['yearHigh - dayHigh'], decreasing = FALSE),] %>% filter(between(`yearHigh - dayHigh`, input$getDiff_id[1], input$getDiff_id[2])
                                                                                                                              ,between(dayHigh, input$getDHP_id[1], input$getDHP_id[2]))
    } else {
      filter_data <<- storage$data[required_Columns][order(storage$data['yearHigh - dayHigh'], decreasing = FALSE),] %>% filter(name %in% input$getCom_id 
                                                                                                                              ,between(`yearHigh - dayHigh`, input$getDiff_id[1], input$getDiff_id[2])
                                                                                                                              ,between(dayHigh, input$getDHP_id[1], input$getDHP_id[2]))
    }
    
    
    output$details <- renderDataTable({
      
      # if(dim(filter_data)[1] == 0){
      #   "No data for the selected values. Please change values" 
      # } else {
      #   kbl(
      #     col.names = c("Company", "Price", "Difference", "Day High Price", "EPS", "Market Cap"),
      #     filter_data,row.names = FALSE) %>%
      #     kable_styling(bootstrap_options = c("striped", "hover", "condensed"), font_size = 12,  position = "left") %>%
      #     scroll_box(height = "400px") %>%
      #     add_header_above(data.frame("Companies that has year high price equal to day High price", 6),monospace = TRUE) %>%
      #     column_spec(3, width = "10em") 
      #   
      # }
      
      datatable(filter_data,rownames = F,escape = F, selection = 'single',
                options=list(columnDefs = list(list(visible=FALSE, targets=c(0)))))
      

    })
      
  })
  
  observeEvent(input$details_rows_selected, {
    print(do.call(rbind.data.frame, lapply(rjson::fromJSON(file = gsub("companies_to_be_passed",filter_data[input$details_rows_selected, "symbol"],get_Links('To get ratios', FALSE)$Link)), nullToNA)))
  })
  

  
  output$getCom <- renderUI({selectizeInput('getCom_id', label = NULL, choices = c('ALL', com$company_Name),multiple = FALSE,options = NULL, width = "100%")})
  output$getDiff <- renderUI({div(class = "label-left",sliderInput("getDiff_id", "Difference", min = min(storage$data['yearHigh - dayHigh']), max = max(storage$data['yearHigh - dayHigh']), value = c(min(storage$data['yearHigh - dayHigh']),max(storage$data['yearHigh - dayHigh']))))})
  output$getDHP <- renderUI({div(class = "label-left",sliderInput("getDHP_id", "Day High Price", min = round_of_two(min(storage$data['dayHigh'])), max = round_of_two(max(storage$data['dayHigh'])), value = c(min(storage$data['dayHigh']),max(storage$data['dayHigh']))))})
  output$getEPS <- renderUI({div(class = "label-left",sliderInput("getEPS_id", "EPS", min = round_of_two(min(storage$data['eps'],na.rm = TRUE)), max = round_of_two(max(storage$data['eps'],na.rm = TRUE)), value = min(storage$data['eps'],na.rm = TRUE)))})
  output$getPrice <- renderUI({div(class = "label-left",sliderInput("getPrice_id", "Price", min = round_of_two(min(storage$data['price'],na.rm = TRUE)), max = round_of_two(max(storage$data['price'],na.rm = TRUE)), value = min(storage$data['price'],na.rm = TRUE)))})
}

shinyApp(ui, server)