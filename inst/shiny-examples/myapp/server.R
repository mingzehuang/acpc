function(input, output, session) {
  if (!(exists("data_for_acpc"))) {
    data_for_acpc = readRDS(url("https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/census-app/data/counties.rds","rb"))
  }
  results <- reactive({acpc(data_for_acpc, input$clusters)})
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    par(mar = c(5.1, 4.1, 0, 1))
    plot(results()$U, col = results()$Y, pch = 20, cex = 3, xlab = "Principle Component 1", ylab = "Principle Component 2")
    points(results()$center, pch = 4, cex = 4, lwd = 4)
  })
  output$plot2 <- renderPlot({
    # Render a barplot
    barplot(results()$V[ ,input$pc], main = paste("Principle Component ", input$pc), ylab="Loading coefficients", xlab="Original Features")
  })
  session$onSessionEnded(function() {
    stopApp()
  })
}