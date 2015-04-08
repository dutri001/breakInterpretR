library(shiny)
library(zoo)
library(bfast)
library(strucchange)
library(ggplot2)
library(lubridate)
source('R/bpPhenoShiny.R')
source('R/ggplot.bfastIR.R')

shinyServer(function(input, output) {
  
  
  # Reactive that runs breakpoints()
  breakpts <- reactive({
    zooTs <- input$zooTs
    if (is.null(zooTs))
      return(NULL)
    
    zooTs <- readRDS(zooTs$datapath) 
    
    
    id <- tsNum$i
    
    formula <- switch(input$formula,
                      'trend' = response ~ trend,
                      'trend + harmon' = response ~ trend + harmon,
                      'harmon' = response ~ harmon)
    
    order <- input$order
    
    breaks <- input$breaks
    if(breaks == -1) {
      breaks <- NULL
    }
    
    h <- input$h
    
    # Subset time-series
    x <- zooTs[,id]
    
    # Run bfastIR function
    out <- bpPhenoShiny(x = x, order = order, formula = formula, breaks = breaks, h = h)
    return(out)
  })
  
  # First output (plot)  
  output$bfastPlot <- renderPlot({    
    # plot results
    formula <- switch(input$formula,
                      'trend' = response ~ trend,
                      'trend + harmon' = response ~ trend + harmon,
                      'harmon' = response ~ harmon)
    
    order <- input$order
    ggplot(breakpts(), formula = formula, order = order)      
  })


  
  # dynamic select (link to uiOutput("inSelect") in UI)
  output$inSelect <- renderUI({
    nBreaks <- as.integer(length(breakpts()$breaks$breakpoints))
    lapply(1:nBreaks, function(i) {
      selectInput(paste0("Class", i), label = paste("Break_", i),  choices = c('Burn','Reg2stable', 'Other'), selected = 'Burn')
    })
  })

  
  # Reactive that returns a one column dataframe with all classes
  breakClass <- reactive({
    nBreaks <- as.numeric(length(breakpts()$breaks$breakpoints))
    
    breakClasses <- sapply(1:nBreaks, function(i) {
      input[[paste0("Class", i)]]
    })
    
    classes <- as.data.frame(breakClasses)
    return(cbind(classes, breakpts()$dfOut))
  })


    values <- reactiveValues()
    values$df <- data.frame(Class = factor(levels = c('Burn','Reg2stable', 'Other')), 'Date' = numeric(0), 'SegDuration_pre' = numeric(0), 'SegSlope_pre'= numeric(0), 'SegYBegin_pre'= numeric(0), 'SegYEnd_pre'= numeric(0), 'SegMean_pre'= numeric(0), 'SegDuration_post'= numeric(0), 'SegSlope_post'= numeric(0), 'SegYBegin_post'= numeric(0), 'SegYEnd_post'= numeric(0), 'SegMean_post'= numeric(0))
    newEntry <- observe({
      if(input$update > 0) {
        # Add selected classes to dataframe
        isolate(values$df <- rbind(values$df, breakClass()))
      }
    })
    output$table1 <- renderTable({values$df})
  
  # Manage the time-series number using a reactiveValue and an observer
  tsNum <- reactiveValues(i = 1)
  observe({
    input$update
    tsNum$i <- isolate(tsNum$i) + 1
  })
})

  # Saving output
#   observe({
#     if (input$save == 0)
#       return()
#     
#     isolate({
#       # Do your saving in here
#     })
#   })



