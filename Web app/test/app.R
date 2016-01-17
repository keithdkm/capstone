library(shiny)
library(shinyBS)
library(data.table)
library(stringi)

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
      fluidRow(column(12,offset =0, textInput("target", "What do you want to say?" )),
                            
#                     bsTooltip("target", "Start typing.  Press Enter to accept a prediction or start typing next word to reject it",
#                               "right", options = list(container = "body"))
                      bsPopover(id = "target",title ="", content = "Prediction", trigger = "manual")
)
    )
   )
)

server <- function(input, output, session) {
  
  reac<-reactiveValues(reset = F, predict = F,  target = isolate(input$target))
  
  # observeEvent(input$reset, updateTextInput(session, "target", value = ""))
 
  #if user types then set predict flag to false
  observe( label = "Watch target",
          x      =  {input$target
                     reac$predict<-FALSE} )
  
  observe( {input$reset
           updateTextInput(session, "target", value = "") })
  
 # if user types and predict flag is set then copy to target phrase to reac$target to invalidate 
   observe(   x = {
                     invalidateLater(1500,session)
                     isolate(reac$target<-input$target)
                     # input$target
#                     # input$reset
#                      print(paste(system.time(), 
#                           "reac$predict =",  isolate(reac$predict), 
#                           "reac$target=",    isolate(reac$target),"\n"))
      
#                      # if (isolate(reac$predict)) reac$target<-input$target
#                      # else isolate(reac$predict  <- TRUE) 
 
                     },
          label = "Set Timer")
      
     
  observe(  label = "Make Prediction",  
    
                x = {x<-if (stri_endswith(reac$target, fixed = " ")) paste0(reac$target, 
                                                            if (input$enabled) phrase(reac$target,1, 
                                                                                      "Interpolate")) else reac$target
                    updateTextInput(session, "target", value = x)
    
  })
  
}
phrase <-  
  function(target,n = 1,model = "Interpolate", l1 = 0.2, l2 = 0.35, l3 = 0.45, l4= 0)
    {
  
  y <- ""
 
  tags<-c("<p>","<n>", "<s>","<e>", "<UNK>")
  
  #########!!!!!!!!!!!!!!   This needs to clean target rather than just lower casing it
  target<-tolower(target)
  
  
  phrase.length <- stri_count_words(target) 
  
  if (phrase.length==1) target<-data.table(V1 = "", V2 = "", V3 = target) else 
    if (phrase.length==2) target<-data.table(V1 = "", V2 = stri_extract_all_words(target)[[1]][1], V3 = stri_extract_all_words(target)[[1]][2]) else
      target<-data.table(t(stri_extract_all_words(target)[[1]][(phrase.length-2):phrase.length]))
  
  #If phrase is longer than 3 words, discard all but the last 3
  
  
  
  setnames(target, c("V1","V2", "V3"),c("u","v","w"))
  
  if (phrase.length>3) {phrase.length<-3}
  
  
  # replace out of vocab words in target with <UNK> tag
  target[,lapply(.SD , function(y) ifelse ((y %in% unigrams$x), y , "<UNK>"))]
  
  if (model %in% c("Backoff")) {
    
    
    # target<-data.table(u = target[1],v = target[2],w = target[3])
    
    if ( phrase.length  == 3 ) { y<-quadrigrams[target , .(x,probability), nomatch = 0 ]
    
    if (y[,.N]==0) phrase.length<-phrase.length-1 else y<-y[order(probability)[1:n],x]}#print("Backing off to trigrams")
    
    
    if ( phrase.length  == 2 ) {y<-    trigrams[target[,.(v,w)] , .(x,probability)]
    
    
    
    if (y[,.N]==0) phrase.length<-phrase.length-1 else y<-y[order(probability)[1:n], w]} #print("Backing off to bigrams")
    
    
    
    
    if ( phrase.length  == 1 ) {y<-     bigrams[target[,w], .(x,probability)]
    
    
    if (y[,.N]==0) phrase.length<-phrase.length-1 else y<-y[order(probability)[1:n],v]} #print("Backing off to unigrams")
    
    
    
    if ( phrase.length  == 0 )  y<-    unigrams[order(probability),ngram ][1:n]
    
  }
  
  else if (model=="Interpolate"){
    
    #create a table of probabilites of words based on for quadrigrams -> unigrams
    table.predict<-  data.table(rbind(
      
      
      quadrigrams[target[,.(u,v,w)], ][!(x %in% tags)][,weighted.prob := l4* 2^-probability][order(-weighted.prob), .(x, weighted.prob)][1:min(.N,10)],   
      trigrams   [target[,.(  v,w)], ][!(x %in% tags)][,weighted.prob := l3* 2^-probability][order(-weighted.prob), .(x, weighted.prob)][1:min(.N,10)], 
      bigrams    [target[,.(    w)], ][!(x %in% tags)][,weighted.prob := l2* 2^-probability][order(-weighted.prob), .(x, weighted.prob)][1:min(.N,10)], 
      unigrams                        [!(x %in% tags)][,weighted.prob := l1* 2^-probability][order(-weighted.prob), .(x, weighted.prob)]))
    
    
    
    #sum the probabilities      
    y<-table.predict[, sum(weighted.prob, rm.na = T), by = x][order(-V1), x][1:n]
    
    
    
  }
  
  else print("Not a valid model")
  y}


shinyApp(ui = ui, server = server)



#      if (stri_endswith(input$target, fixed = " ")) addPopover(session, "target", 
#                 title = "", 
#                 content = "prediction")

  