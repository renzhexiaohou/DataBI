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


source("plotlyGraphWidget.R")

# input --------------------------------------------------------------------
tmp <- read.csv("Demographics-REFMAL628_20.csv") %>% 
    mutate(group = as.character(Subject))





# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {


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
   
    
    output$demography <- renderGraph({
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
