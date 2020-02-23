#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
# library(plotly)
# library(PKNCA)

source("global.R")

# input --------------------------------------------------------------------
# tmp <- read.csv("Demographics-REFMAL628_20.csv") %>% 
#     mutate(group = as.character(Subject))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # output$distPlot <- renderPlot({
    # 
    # 
    # })
    
    output$ae_tab <- DT::renderDT(
        {Ae},
        options = list(
            lengthMenu = list(c(5, 10, 15, -1), 
                              c('5', '10', '15', 'All')),
            pageLength = 5,
            pagingType = "simple_numbers",
            # autoWidth = TRUE,
            scrollX = TRUE,
            rowCallback = DT::JS(
                'function(row, data) {
          if(parseFloat(data[3]) >= 5.0 | parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("font-weight", "bold");
          if (parseFloat(data[3]) >= 5.0)
          $("td:eq(3)", row).css("color", "red");
          if (parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("color", "#0080ff")}')
        )
    )
    output$dm_tab <- DT::renderDT(
        {Dm},
        options = list(
            lengthMenu = list(c(5, 10, 15, -1), 
                              c('5', '10', '15', 'All')),
            pageLength = 5,
            pagingType = "simple_numbers",
            # autoWidth = TRUE,
            scrollX = TRUE,
            rowCallback = DT::JS(
                'function(row, data) {
          if(parseFloat(data[3]) >= 5.0 | parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("font-weight", "bold");
          if (parseFloat(data[3]) >= 5.0)
          $("td:eq(3)", row).css("color", "red");
          if (parseFloat(data[3]) <= 1.0)
          $("td:eq(3)", row).css("color", "#0080ff")}')
        )
    )

})
