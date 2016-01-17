library(shiny)


ui <- fluidPage(h1("Text Predictor"),
      #Inputs
      textInput(inputId = "target", 
                label = "What would you like to say?", 
                value =""),
      # addPopover(id =  "target", title = "", content = paste0(" a test"), trigger = "hover"),
      
      #Outputs
      textOutput(outputId = "prediction")
      )

server <- function(input, output, session) {
  
  output$prediction<-renderText(input$target)
  
}

shinyApp(ui = ui, server = server)
