library(dplyr)
library(ggplot2)
library(shiny)
library(plyr)

# Define server function required to create the scatterplot
shinyServer(function(session, input, output) {

  
  pid_selection <- NULL
  
  #UpdateVisualizations: In this function all visualizations are defined.
  UpdateVisualizations <- function() {
    if (!is.null(pid_selection)) {
      D <- D %>% filter(PID %in% pid_selection)
    }
    
    output$explorationPlot <- 
      renderPlotly({
        plot_ly(x = D[,input$Xaxis], y = D[,input$Yaxis]) %>% 
          #add_trace(data = D, type = 'scatter') %>%
          layout(xaxis = list(title = ~input$Xaxis), yaxis = list(title = ~input$Yaxis))
      })
    
    output$timeExplorationPlot <- 
      renderPlotly({
        plot_ly(x = D[,"DateTime"], y = D[,input$timeYaxis]) %>% 
          add_trace(data = D, type = 'scatter') %>%
          layout(xaxis = list(title = "DateTime"), yaxis = list(title = ~input$timeYaxis))
      }) 
  }  
  
  
  # Update PID Choosers to show PID numbers based on the data
  participants <- unique(D %>% distinct(PID))
  participants$PID[is.na(participants$PID)] <- "NA"
  choices = setNames(c(1:nrow(participants)),participants$PID)
  updateCheckboxGroupInput(session, label = "Filter by Participant:", "pidChooser", choices = choices, selected = NULL, inline = TRUE)
  
  
  # Create scatterplot object the plotOutput function is expecting
  #output$scatterplot <- renderPlot({
  #  df %>%
  #    filter(BaguetteSize == input$baguetteSize, EyePatch == input$eyePatch, Email == input$email) %>%
  #    ggplot(aes_string(x = input$x, y = input$y, color = input$z)) + 
  #    geom_smooth(method = "lm", se=FALSE) + geom_point(size = 2) +
  #    theme_set(theme_gray(base_size = 18)) +
  #    #geom_line() + geom_point(size = 7) +
  #    labs(title = "La Baguette Data", x = "", y = "", color = "") +
  #    expand_limits(y = 0)
  #})
  
  observeEvent(ignoreNULL=FALSE, {input$pidChooser}, {
    # prevent infinite loop - only update pid_selection to null, if the value is not already null.
    if (is.null(pid_selection) & is.null(input$pidChooser)) {
      return()
    }
    # CheckboxInputGroup sends an initial NULL value which overrides any query values.
    # Make sure we check whether a specific PID was specified as URL param before.
    else if (!is.null(input$pidChooser)) {
      pid_selection <<- unlist(participants[input$pidChooser,"PID"])
    } else {
      pid_selection <<- NULL
    }
    print(paste("pid_selection",pid_selection))
    UpdateVisualizations()
  })
  
  UpdateVisualizations()
  # Create data table
  #output$hovertable <- DT::renderDataTable({
  #  nearPoints(df, input$plot_hover) %>% 
  #    select(Email, input$x, input$y)
  #})
})