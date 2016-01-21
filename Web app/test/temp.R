library(shiny)
library(shinyBS)
library(shinythemes)
library(data.table)
library(stringi)

source("predictor.R")

restore.ngrams<-function (path) {  
  unigrams<<- readRDS(file =  paste0(path,"unigrams.RDS"))
  bigrams<<-  readRDS(file =  paste0(path,"bigrams.RDS"))
  trigrams<<- readRDS(file =  paste0(path,"trigrams.RDS"))
  quadrigrams<<- readRDS(file =  paste0(path,"quadrigrams.RDS"))}

 # restore.ngrams("data/")

ui <- fluidPage(theme = shinytheme ("flatly"),
                h1("Text Prediction Tool"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("speed", "Prediction Speed (seconds)", value = 1.5, min = 0.1,max = 5,step = 0.1),
      checkboxInput("enabled","Enable text prediction", T),
      actionButton(inputId = "reset", label = "Clear Text"),
      tags$style(type = "text/css", "#reset {margin:3px; width :175px; height:50px; color:red}")
                ),  #end of side bar panel 

#       actionButton(inputId = "copy" , label = "Copy Text to Clipboard"),
#       tags$style(type = "text/css", "#copy {margin:3px; width :175px; height:50px}")
   
    mainPanel(     
      fluidRow(column(5,offset = 0, 
                      
                                   tags$textarea(id="target", rows=20, cols=30  )
                      ), #end of first column,
                                    
               column(1,
                      actionButton(inputId = "prediction1", label = "First Alternative"),
                      tags$style(type = "text/css", "#prediction1 {margin:3px; width :175px; height:50px}"),
                      
                      actionButton(inputId = "prediction2", label = "Second Alternative"),
                      tags$style(type = "text/css", "#prediction2 {margin:3px; width :175px; height:50px}"),
                      
                      actionButton(inputId = "prediction3", label = "Third Alternative"),
                      tags$style(type = "text/css", "#prediction3 {margin:3px; width :175px; height:50px}"),
                      
                      actionButton(inputId = "reject",              "   Reject   "),
                      tags$style(type = "text/css", "#reject {margin:3px; width :175px; height:50px}"),
               # ,        
                       bsTooltip("speed", "Set delay in seconds for a prediction to be generated",
                                 "bottom", options = list(container = "body")),
                       bsTooltip("rest", "Clear text entry box",
                                 "bottom", options = list(container = "body")),   
                        
                        bsTooltip("target", "To get a prediction, type a word and hit space bar",
                                  "left", options = list(container = "body")),
                        bsTooltip("prediction1", "First alternate word",
                                  "right", options = list(container = "body")),
                        bsTooltip("prediction2", "Second alternate word",
                                  "right", options = list(container = "body")), 
                        bsTooltip("prediction3", "Third alternate word",
                                  "right", options = list(container = "body")),
                        bsTooltip("reject", "Reject prediction",
                                  "right", options = list(container = "body"))

                    ) #end of column
           ) #end of fluid row
         ) #end of main panel
        ) # end of SifeBarLayout
        )  #End of FluidPage              
                
               
# ,

#                         bsPopover(id = "target",title ="",
#                                   content = "To get a prediction, type a word and then type space and pause.  Hit Reject if the word is wrong", 
#                                   trigger = "hover"),
# 
#                         bsPopover(id = "prediction1",title ="",
#                                   content = "First Alternative", 
#                                   trigger = "hover" ,placement = "right"),
#                         bsPopover(id = "prediction2",title ="",
#                                   content = "Second Alternative", 
#                                   trigger = "hover" ,placement = "right"),
#                         bsPopover(id = "prediction3",title ="",
#                                   content = "Third Alternative", 
#                                   trigger = "hover" ,placement = "right")


server <- function(input, output, session) {
  
  reac<-reactiveValues(predict = F, reset = F, enabled = isolate(input$enabled),  target = isolate(input$target))
  
  predicted.last.word<-FALSE
  prediction<-rep("",4)
  # observeEvent(input$reset, updateTextInput(session, "target", value = ""))
 
 
  observe( label = "Reject last word",
           x = { input$reject 
                  updateTextInput(session,
                                  "target", 
                                  value = isolate(stri_trim_right(stri_sub(input$target,
                                                                            1,
                                                                            stri_locate_last_words(input$target)[1]-2))))
                  })
 
  
  observeEvent(input$prediction1,handlerExpr = 
                 
                 {
                   
                   x<-stri_trim_right(stri_sub(input$target,
                                           1,
                                           stri_locate_last_words(input$target)[1]-2))
                   
                  x<- paste(x, prediction[2])
                  
                  updateTextInput(session, "target", value = x)}) 
  
  observeEvent(input$prediction2,handlerExpr = 
                 
  {
    
    x<-stri_trim_right(stri_sub(input$target,
                                1,
                                stri_locate_last_words(input$target)[1]-2))
    
    x<- paste(x, prediction[3])
    
    updateTextInput(session, "target", value = x)}) 
  
  observeEvent(input$prediction3,handlerExpr = 
                 
  {
    
    x<-stri_trim_right(stri_sub(input$target,
                                1,
                                stri_locate_last_words(input$target)[1]-2))
    
    x<- paste(x, prediction[4])
    
    updateTextInput(session, "target", value = x)}) 
  
 #Watch Clear Text button
   observe( label = "Clear text box",
            x = {input$reset
                 updateTextInput(session, "target", value = "") })
   observe(
     label = "Watch target",
     x      =  {
       # cat ("reset reac predict")
       input$target
       reac$predict<-FALSE} ) 
 # if user types and predict flag is set then copy to target phrase to reac$target to invalidate 
   observe(   x = {  invalidateLater(isolate(input$speed*1000),session)
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
                     if ( stri_endswith(reac$target, fixed = " ")) {
                       prediction<<-if (reac$enabled) phrase(reac$target
                                                            ,4,
                                                            "Interpolate", params = list(l1 = 0.1,l2= 0.3, l3 = 0.4, l4 = 0.2))      
                                  else prediction<-rep("",4)    
                     
                                              
                    x<- paste0(reac$target, prediction[1])
                    updateButton(session,"prediction1",label = prediction[2])
                    updateButton(session,"prediction2",label = prediction[3])   
                    updateButton(session,"prediction3",label = prediction[4])   
                    updateTextInput(session, "target", value = x)
                    # output$x<-x
                    predicted.last.word<-TRUE}
              
                
          })
  
}


shinyApp(ui = ui, server = server)



#      if (stri_endswith(input$target, fixed = " ")) addPopover(session, "target", 
#                 title = "", 
#                 content = "prediction")

  