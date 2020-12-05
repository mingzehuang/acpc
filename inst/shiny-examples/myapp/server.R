function(input, output){
  title <- "random normal values"
  output$hist <- renderPlot({
    hist(rnorm(input$num))
  })
}