#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(tibble)
library(stringr)
library(PKNCA)

#This script is necessary in the app folder
#but the user should not have to edit it

#Output Graph Function
graphOutput <- function(inputId, width="100%", height="550px") {
  tagList(
    singleton(tags$head(
      tags$script(src="plotlyGraphWidget.js")
    )),
    tags$iframe(id=inputId, src="https://plot.ly/~playground/7.embed",
                class="graphs", style="border:none;", seamless=TRUE, width=width, height=height)
  )
}

renderGraph <- function(expr, env=parent.frame(), quoted=FALSE) {
  ## This gets called when inputs change --
  ## Place data wrangling code in here
  ## and pass the result to the client
  ## to be graphed.
  
  
  installExprFunction(expr, "func", env, quoted)
  
  function(){
    data = func();
    ## data is the state of the widgets: see server.R
    ## this function returns a list of named lists that descripe
    ## valid postMessage commands to be sent to the embedded iframe.
    ## see binding.renderValue for the receiving JS side of this function
    ## and https://github.com/plotly/Embed-API for more about the postMessage
    ## graph messages
    return(data)
    
  }
}


# input --------------------------------------------------------------------
tmp <- read.csv("Demographics-REFMAL628_20.csv") %>% 
    mutate(group = as.character(Subject))



final <- readRDS("PlotTrend.rds") %>% filter(drug %in% c("生物制品", "化学药物"))

Year <- summary(final$dt) %>% names()

PH1 <- filter(final, phase == "I期")
PH1num <- summary(PH1$dt)

PH2 <- filter(final, phase == "II期")
PH2num <- summary(PH2$dt)

PH3 <- filter(final, phase == "III期")
PH3num <- summary(PH3$dt)

data <- tibble(Year, PH1num, PH2num, PH3num)


cPH1num <- '#246a99'
cPH2num <- '#2e88c5'
cPH3num <- '#85c1e9'
# cPH1num <- '4285F4'
# cPH2num <- 'FBBC05'
# cPH3num <- 'EA4335'



# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    
    output$demography <- graphOutput({
        plot_ly(data, x = ~Year) %>%
            add_trace(y = ~PH1num, name = 'I',type = "bar",
                      text = ~PH1num, textposition = 'inside',
                      marker = list(color = cPH1num, opacity=0.9)) %>% 
            add_trace(y = ~PH2num, name = 'II',type = "bar",
                      text = ~PH2num, textposition = 'inside',
                      marker = list(color = cPH2num, opacity=0.9)) %>%
            add_trace(y = ~PH3num, name = 'III',type = "bar",
                      text = ~PH3num, textposition = 'inside',
                      marker = list(color = cPH3num, opacity=0.9)) %>%
            # add_text(textfont = t, textposition = "top center") %>%
            layout(yaxis = list(title = 'Count'), barmode = 'stack',
                   xaxis = list(title = "", tickangle = -45))
    })

})
