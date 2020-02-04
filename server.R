library(dplyr)
library(DT)
library(ggplot2)
library(shiny)
library(plyr)

# Define server function required to create the scatterplot
shinyServer(function(input, output) {
    
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
        layout(xaxis = list(title = "DateTime"), yaxis = list(title = ~input$Yaxis))
    }) 
  
  # Create data table
  #output$hovertable <- DT::renderDataTable({
  #  nearPoints(df, input$plot_hover) %>% 
  #    select(Email, input$x, input$y)
  #})
})