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
# library(PKNCA)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    

    output$ae_tab <- renderTable ({
        Ae %>% filter(Subject %in% input$ID)
    })
    
    output$id_plot <- renderPlot({
        ggplot(filter(He, Subject %in% input$ID), aes(x = as.numeric(ANC), y = as.numeric(PLAT))) +
            geom_point() +
            theme_bw(base_size = 15)
    })

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
    
    # output$dm_tab <- renderTable ({
    #     Dm %>% filter(Subject %in% input$ID)
    # })


    output$he_tab <- renderTable ({
        He %>% filter(Subject %in% input$ID)
    })


    output$ch_tab <- renderTable ({
        Ch %>% filter(Subject %in% input$ID)
    })


    output$tl_tab <- renderTable ({
        Tl %>% filter(Subject %in% input$ID)
    })
    
})
