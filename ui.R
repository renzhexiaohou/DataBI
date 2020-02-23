#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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
library(DT)
library(data.table)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    # Sidebar with a slider input for number of bins
    h3("adverse events"),
    div(DT::DTOutput("ae_tab")),
    h3("demographics"),
    div(DT::DTOutput("dm_tab"))
))
