#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
## Only run this example in interactive R sessions
if (interactive()) {
    ui <- fluidPage(
        sidebarPanel(
            actionButton("OK", "OK"),
            conditionalPanel(
                condition = "input.OK == 1",
                textInput("caption", "Caption", "Data Summary"),
                verbatimTextOutput("value")
            ),
            selectInput("plotType", "Plot Type",
                        c(Scatter = "scatter", Histogram = "hist")
            ),
            # Only show this panel if the plot type is a histogram
            conditionalPanel(
                condition = "input.plotType == 'hist'",
                selectInput(
                    "breaks", "Breaks",
                    c("Sturges", "Scott", "Freedman-Diaconis", "[Custom]" = "custom")
                ),
                # Only show this panel if Custom is selected
                conditionalPanel(
                    condition = "input.breaks == 'custom'",
                    sliderInput("breakCount", "Break Count", min = 1, max = 50, value = 10)
                )
            )
        ),
        mainPanel(
            plotOutput("plot")
        )
    )
    
    server <- function(input, output) {
        x <- rnorm(100)
        y <- rnorm(100)
        
        output$value <- renderText({ input$caption })
        observeEvent(input$OK, {
            if (input$OK == 2){
                print(input$caption)
            }
        })
        
        output$plot <- renderPlot({
            if (input$plotType == "scatter") {
                plot(x, y)
            } else {
                breaks <- input$breaks
                if (breaks == "custom") {
                    breaks <- input$breakCount
                }
                
                hist(x, breaks = breaks)
            }
        })
    }
    
    shinyApp(ui, server)
}
