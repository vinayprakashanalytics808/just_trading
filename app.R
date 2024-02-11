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
source("DButils.R")

ui <- fluidPage(
  dataTableOutput("details")
)

server <- function(input, output, session) {
  
  storage <- reactiveValues()
  storage$data <- data.frame()
  
  storage$data = do.call(rbind.data.frame, rjson::fromJSON(file = get_Latest_day_Details))
  
  output$details <- renderDataTable({
    
    storage$data['yearHigh - dayHigh'] <- round(storage$data['yearHigh'],0) - round(storage$data['dayHigh'], 0)
    storage$data[c('name', 'yearHigh - dayHigh')]
  
    
    })
}

shinyApp(ui, server)