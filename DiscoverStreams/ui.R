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
dataset2 <- waterUSE_Pr_select

fluidPage(
    
    titlePanel("DiscoverStreams"),
    
    sidebarPanel(
        
        sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
                    value=min(1000, nrow(dataset)), step=500, round=0),
        
        selectInput('x1', 'X - Streamflow', names(dataset)),
        selectInput('y1', 'Y - Streamflow', names(dataset), names(dataset)[[2]]),
        
        checkboxInput('log', 'Log y-axis'),
        
        selectInput('x2', 'X - Water Use', names(dataset2)),
        selectInput('y2', 'Y - Water Use', names(dataset2), names(dataset2)[[2]]),
        # selectInput('color', 'Color', c('None', names(dataset))),
        
        
        
    ),
    
    mainPanel(
        plotOutput('plot'),
        plotOutput('plot_wu')
    )
)
