library(shiny)
library(shinyBS)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("target", "What do you want to say?"),
      bsTooltip("target", "Start typing.  Press <Enter> to accept a prediction or start typing next word to reject it",
                "right", options = list(container = "body"))
    ),
    mainPanel(textOutput(outputId = "flag")
    )
   )
)

server <- function(input, output, session) {
  
  
#     out <- renderText({  input$target  }  )
#     output$flag <- renderText( {if (stri_endswith(input$target, fixed = " ")) paste(input$target, "predict ") else ""} )
 
  observe({
    
     x<-if (stri_endswith(input$target, fixed = " ")) paste(input$target, "predict") else input$target
    
     updateTextInput(session, "target", value = x)
    
#      addPopover(session, "target", 
#                 title = "", 
#                 content = "prediction", 
#                 trigger = (stri_endswith(input$target, 
#                                          fixed = " ")))

  })}

shinyApp(ui = ui, server = server)
