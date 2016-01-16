library(shiny)
shinyApp(
  ui = fluidPage(
    sidebarPanel(selectInput("n", "n", c(1, 5, 10))),
    mainPanel(plotOutput("plot"))
  ),
  server = function(input, output) {
    output$plot <- renderPlot({
      plot(head(cars, as.numeric(input$n)))
    })
  }
)
