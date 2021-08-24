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
  column(8, align = 'center', textOutput('end')),
  # asking all the socio-demographic questions from the participants
  column(8, align = 'center', conditionalPanel(
    condition = "input.OK == 0",
    textInput("city", "Lakóhely (Város)", "Város"),
    numericInput("age", "Életkor", 20),
    selectInput("schooling", "Legmagasabb iskolai végzettség", c("8 általános", "Szakiskola", "Érettségi", "Főiskola", "Egyetem, vagy annál magasabb"))
  ))
  #column(8, align = 'center', numericInput("age", "Életkor:", 30, min = 1, max = 100), verbatimTextOutput("value")

)
