#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rsconnect)
library(ggplot2)

# Shiny demo code
function(input, output) {
    
    dataset <- reactive({
        streamflow_Prairie[sample(nrow(streamflow_Prairie), input$sampleSize),]
    })
    
    output$plot <- renderPlot({
        
        p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_line(color = color_mi[1])
        
        # if (input$color != 'None')
        #     p <- p + aes_string(color=input$color)
        

        if (input$log)
            p <- p + scale_y_continuous(trans='log10')
        
        print(p)
        
    }, height=700)
    
}
