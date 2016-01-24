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

restore.ngrams("data/")

ui <- fluidPage(theme = shinytheme ("flatly"),
                h1("Text Prediction Tool"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("speed", "Prediction Speed (seconds)", value = 1.5, min = 0.5,max = 5,step = 0.1),
      checkboxInput("enabled","Enable text prediction", T),
      actionButton(inputId = "reset", label = "Clear Text"),
      shiny::tags$style(type = "text/css", "#reset {margin:3px; width :175px; height:50px; color:red}")
                ),  #end of side bar panel 



    mainPanel(     
      fluidRow(column(5,offset = 0, shiny::tags$textarea(id="target", rows=20, cols=30  )
                      ), #end of first column,
                                    
               column(1,
                      actionButton(inputId = "prediction1", label = "First Alternative"),
                      shiny::tags$style(type = "text/css", "#prediction1 {margin:3px; width :175px; height:50px}"),
                      
                      actionButton(inputId = "prediction2", label = "Second Alternative"),
                      shiny::tags$style(type = "text/css", "#prediction2 {margin:3px; width :175px; height:50px}"),
                      
                      actionButton(inputId = "prediction3", label = "Third Alternative"),
                      shiny::tags$style(type = "text/css", "#prediction3 {margin:3px; width :175px; height:50px}"),
                      
                      actionButton(inputId = "reject",              "   Reject   "),
                      shiny::tags$style(type = "text/css", "#reject {margin:3px; width :175px; height:50px}"),
                      
                       bsTooltip("speed", "Set how quickly you want a word predicted",
                                 "bottom", options = list(container = "body")),
                       bsTooltip("rest", "Clear entire text entry box",
                                 "bottom", options = list(container = "body")),   
                        
                        bsTooltip("target", "To get a prediction, type a word or words and hit space bar",
                                  "left", options = list(container = "body")),
                        bsTooltip("prediction1", "First alternate word",
                                  "right", options = list(container = "body")),
                        bsTooltip("prediction2", "Second alternate word",
                                  "right", options = list(container = "body")), 
                        bsTooltip("prediction3", "Third alternate word",
                                  "right", options = list(container = "body")),
                        bsTooltip("reject", "Reject all predictions",
                                  "right", options = list(container = "body"))

                    ) #end of column
           ) #end of fluid row
         ) #end of main panel
        ) # end of SifeBarLayout
        )  #End of FluidPage              
                
               

server <- function(input, output, session) {
  
  reac<-reactiveValues(predict = F, 
                       reset = F, 
                       enabled = isolate(input$enabled),  
                       target = isolate(input$target))
  
  # predicted.last.word<-FALSE
  prediction<-rep("",4)
  # observeEvent(input$reset, updateTextInput(session, "target", value = ""))
 
 
  
 #Watch Clear Text button
   observeEvent( input$reset, label = "Clear text box",
            handlerExpr =  updateTextInput(session, "target", value = "") )
   
   observe(
     label = "Watch target",
     x     =  {
       input$target
       reac$predict<-FALSE} ) 
   
   
 # if user types and predict flag is set then copy to target phrase to reac$target to invalidate 
   observe( label = "Set Timer",
            x     = {  
                     invalidateLater(isolate(input$speed*1000),session)
                     input$target
                     input$enabled
                    
#                      isolate(cat("Set timer: ", 
#                                  "input target:",input$target,
#                                  "reac target:",reac$target, 
#                                  "Predict:",reac$predict,"\n", sep =","))
                     
                     
                     if (isolate(reac$predict & (stri_endswith(input$target, fixed = " ")))){ isolate(reac$target  <- input$target)
                                                 }
                     
                     else isolate(reac$predict              <- TRUE)
                     
                     # predicted.last.word<-FALSE
                     }
          )
      
     
  observe(  label = "Make Prediction",  
    
            x = {
              reac$target
              # isolate(cat("Make Prediction", input$target, input$enabled, reac$target, reac$predict, "\n", sep =","))
              # isolate(reac$predict              <- FALSE)
              if ( stri_endswith(isolate(input$target), fixed = " ") & !stri_endswith(isolate(input$target), fixed = ". "))
                {old.target<<-stri_trim_right(reac$target)  #save current text for reject button press
              if (input$enabled & isolate(reac$predict)){ prediction<<- phrase(t.text = reac$target,
                                        n =  4,
                                        model = "Interpolate", 
                                        params = list(l1 = 0.1,l2= 0.3, l3 = 0.4, l4 = 0.2))
   
              
                #Make sure user hasn't typed anthing while prediction was being made.  If not, make prediction
              if (isolate(input$target == reac$target ))
              {x<- paste0  (reac$target,                      prediction[1])
                # old.target<<-reac$target  #save current text for reject button press
                updateButton(session,    "prediction1",label = ifelse(is.na(prediction[2]),"First Alternative",prediction[2]))
                updateButton(session,    "prediction2",label = ifelse(is.na(prediction[3]),"Second Alternative",prediction[3]))   
                updateButton(session,    "prediction3",label = ifelse(is.na(prediction[4]),"Third Alternative",prediction[4]))   
                updateTextInput(session, "target", value = x)}
                
                }
                } 
                })
  
  observeEvent(  input$reject,
                 { reac$predict<-F
                   updateTextInput(session = session,
                                         "target", value = old.target)
                   updateButton(session,    "prediction1",label = "First Alternative")
                   updateButton(session,    "prediction2",label = "Second Alternative") 
                   updateButton(session,    "prediction3",label = "Third Alternative")
                   prediction<<-NULL
#                                    value = isolate(stri_trim_right(stri_sub(input$target,
#                                                                             1,
#                                                                             stri_locate_last_words(input$target)[1]-2))))
                 })
  
 
### Button Handlers 
   
  observeEvent((input$prediction1 ),handlerExpr = 
                 
  {
    if (!is.null(prediction))
    {x<-stri_trim_right(stri_sub(input$target,
                                1,
                                stri_locate_last_words(input$target)[1]-2))
    
    x<- paste(x, prediction[2])
    
    updateTextInput(session, "target", value = x)}}) 
  
  observeEvent(input$prediction2,handlerExpr = 
                 
  {
    if (!is.null(prediction))
    {
    x<-stri_trim_right(stri_sub(input$target,
                                1,
                                stri_locate_last_words(input$target)[1]-2))
    
    x<- paste(x, prediction[3])
    
    updateTextInput(session, "target", value = x)}}) 
  
  observeEvent(input$prediction3,handlerExpr = 
                 
  {if (!is.null(prediction))
  {
    
    x<-stri_trim_right(stri_sub(input$target,
                                1,
                                stri_locate_last_words(input$target)[1]-2))
    
    x<- paste(x, prediction[4])
    
    updateTextInput(session, "target", value = x)}}) 
  
  }


shinyApp(ui = ui, server = server)




  