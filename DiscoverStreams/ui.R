#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Shiny demo code
dataset <- streamflow_Prairie

fluidPage(
    
    titlePanel("DiscoverStreams"),
    
    sidebarPanel(
        
        sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
                    value=min(1000, nrow(dataset)), step=500, round=0),
        
        selectInput('x', 'X', names(dataset)),
        selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
        # selectInput('color', 'Color', c('None', names(dataset))),
        
        checkboxInput('log', 'Log')
        
    ),
    
    mainPanel(
        plotOutput('plot')
    )
)
