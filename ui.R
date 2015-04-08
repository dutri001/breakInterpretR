

library(shiny)

shinyUI(fluidPage(
  fluidRow(
    column(width = 3,
           uiOutput("inSelect")),
    column(width = 8, offset = 1,
           tabsetPanel(
             tabPanel('plot', plotOutput("bfastPlot")),
             tabPanel('table', tableOutput('table1'))
           ))
    ),
  fluidRow(
    column(width = 4,
           actionButton("update", "Register / Next time-series")),
    column(width = 4,
           selectInput('formula',
                       label = 'Formula',
                       choices = list('trend',
                                      'trend + harmon',
                                      'harmon'),
                       selected = 'trend + harmon'),
           numericInput("order", label = 'Harmonic order', value = 1)),
           
    column(width = 4,
           numericInput("breaks", label = 'Number of breaks allowed', value = -1),           
           sliderInput('h', label = 'minimal segment size', min = 0, max = 1, value = 0.15, step = 0.01),
           fileInput(inputId = 'zooTs', label = 'rds file', multiple = FALSE, accept = '.rds'))
    )
  ))



