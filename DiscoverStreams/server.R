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
    
    dataset <- reactive(streamflow)
    dataset2 <- reactive(irrigation)
    
    output$plot1 <- renderPlot({
        
        p1 <- ggplot(dataset(), aes_string(x="Date", y=input$y1)) + geom_line(color = color_mi[1])
        
        # if (input$color != 'None')
        #     p1 <- p1 + aes_string(color=input$color)
        min <- as.Date("1950-1-1")
        max <- NA
        
        p1 + scale_x_date(limits = c(min,max))

        if (input$log)
            p1 <- p1 + scale_y_continuous(trans='log10')
        
        print(p1)
        
    })
    
    output$plot2 <- renderPlot({
        
        p2 <- ggplot(dataset2(), aes_string(x="Date", y=input$y2)) + geom_point(color = color_mi[3])
        
        # if (input$color != 'None')
        #     p2 <- p2 + aes_string(color=input$color)
        
        
        # if (input$log)
        #     p2 <- p2 + scale_y_continuous(trans='log10')
        
        min <- as.Date("1950-1-1")
        max <- NA
        
        p2 + scale_x_date(limits = c(min,max))
        
        print(p2)
        
    })
    
   
}
