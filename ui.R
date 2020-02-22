#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

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

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        # mainPanel(
        #     plotOutput("distPlot")
        # ),
        mainPanel(graphOutput("demography"))
    )
))
