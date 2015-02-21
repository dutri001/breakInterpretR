

library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Visual interpretation of break classes"),
                  sidebarPanel(numericInput(inputId = 'nBreaks', label = 'Number of breaks', value = 1),
                               uiOutput("inSelect"), # Dynamic input
                               actionButton("update", "Next break")),
                  mainPanel(tableOutput("table1")))
)

