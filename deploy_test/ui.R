library(shiny)
library(idefix)


fluidPage(
  # Put setnr on screen
  column(8, align = 'center', textOutput("set.nr")),
  # Put design on screen
  column(8, align = 'center', tableOutput("choice.set")),
  # Put answer options on screen
  column(8, align = 'center', uiOutput('buttons')), 
  # put introtext on screen
  column(8, align = 'center', textOutput('intro')),
  # Put action button on screen
  column(8, align = "center", actionButton("OK", "OK")),
  # put end text on screen
  column(8, align = 'center', textOutput('end'))
  #column(8, align = 'center', numericInput("age", "Életkor:", 30, min = 1, max = 100), verbatimTextOutput("value")

)
