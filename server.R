library(shiny)

shinyServer(function(input, output, session) {
  
  # dynamic select (link to uiOutput("inSelect") in UI)
  output$inSelect <- renderUI({
    nBreaks <- as.integer(input$nBreaks)
    lapply(1:nBreaks, function(i) {
      selectInput(paste0("Class", i), label = paste("Break_", i),  choices = c('a','b', 'c'), selected = 'a')
    })
  })
  
  # Reactive that returns a one column dataframe with all classes
  breakClass <- reactive({
    nBreaks <- as.numeric(input$nBreaks)
    
    breakClasses <- sapply(1:nBreaks, function(i) {
      input[[paste0("Class", i)]]
    })
    
    classes <- as.data.frame(breakClasses)
  })

    values <- reactiveValues()
    values$df <- data.frame(Class = factor(levels = c('a', 'b', 'c' )))
    newEntry <- observe({
      if(input$update > 0) {
        # isolate(values$df[nrow(values$df) + 1,] <- c(input$Class)) 
        isolate(values$df <- rbind(values$df, breakClass()))
      }
    })
    output$table1 <- renderTable({values$df})
  }
)


