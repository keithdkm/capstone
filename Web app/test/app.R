library(shiny)
library(shinyBS)
library(data.table)
library(stringi)

source("predictor.R")

restore.ngrams<-function (path) {  
  unigrams<<- readRDS(file =  paste0(path,"unigrams.RDS"))
  bigrams<<-  readRDS(file =  paste0(path,"bigrams.RDS"))
  trigrams<<- readRDS(file =  paste0(path,"trigrams.RDS"))
  quadrigrams<<- readRDS(file =  paste0(path,"quadrigrams.RDS"))}

# restore.ngrams("data/")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxInput("enabled","Enable text prediction", T),
      actionButton(inputId = "reset", label = "Clear Text")
      
      
    ),
    mainPanel(     
      fluidRow(column(8,offset =0, textInput("target", "What do you want to say?" )),
               actionButton(inputId = "reject", "Reject"),
                            
#                     bsTooltip("target", "Start typing.  Press Enter to accept a prediction or start typing next word to reject it",
#                               "right", options = list(container = "body"))
                        bsPopover(id = "target",title ="",
                                  content = "To get a prediction, type a word and then type space and pause.  Hit Reject if the word is wrong", 
                                  trigger = "hover")
)
    )
   )
)

server <- function(input, output, session) {
  
  reac<-reactiveValues(predict = F, reset = F, enabled = isolate(input$enabled),  target = isolate(input$target))
  
  predicted.last.word<-FALSE
  
  # observeEvent(input$reset, updateTextInput(session, "target", value = ""))
 
 
  observe( label = "Reject last word",
           x = { input$reject 
                  updateTextInput(session, 
                                                          "target",
                                                          value = isolate(stri_trim_right(stri_sub(input$target,
                                                                                   1,
                                                                                   stri_locate_last_words(input$target)[1]-2))))
                  
                  })
  
 #Watch Clear Text button
   observe( label = "Clear text box",
            x = {input$reset
                 updateTextInput(session, "target", value = "") })
   observe(
     label = "Watch target",
     x      =  {
       cat ("reset reac predict")
       input$target
       reac$predict<-FALSE} ) 
 # if user types and predict flag is set then copy to target phrase to reac$target to invalidate 
   observe(   x = {  invalidateLater(1050,session)
                     input$target
                     input$enabled
                     
#                      isolate(cat("Set timer: ", 
#                                  "input target:",input$target, 
#                                  "reac enabled:",reac$enabled, 
#                                  "reac target:",reac$target, 
#                                  "Predict:",reac$predict,"\n", sep =","))
      
                     if (isolate(reac$predict))
                       {reac$target<-input$target
                       reac$enabled <-input$enabled}
                     else isolate(reac$predict  <- TRUE)
                     
                     predicted.last.word<-FALSE
                     
                     },
          label = "Set Timer")
      
   
   
  observe(  label = "Make Prediction",  
    
                x = { 
                  # isolate(cat("Make Prediction", input$target, input$enabled, reac$target, reac$predict, "\n", sep =","))
                  # make sure reac has the latest version of input before predicting
                     x <- if ( stri_endswith(reac$target, fixed = " ")) 
                            paste0(reac$target, if (reac$enabled) phrase(reac$target
                                                                          ,1,
                                                                          "Interpolate") 
                                                else "") 
                    
                         
                    updateTextInput(session, "target", value = x)
                    predicted.last.word<-TRUE
              
                
          })
  
}


shinyApp(ui = ui, server = server)



#      if (stri_endswith(input$target, fixed = " ")) addPopover(session, "target", 
#                 title = "", 
#                 content = "prediction")

  