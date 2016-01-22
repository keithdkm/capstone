

path<- getwd()

conf<-file(paste0(path,"/data/dirty.txt"),'r')
profanity<-paste0("\\b(",paste0(readLines(conf),collapse = "|"),")\\b")    #profanity<-readLines(conf)
close (conf)
 
contr<-file(paste0(path,"/data/contractions.txt"),'r')  #load list of English contractions
contractions <- data.table(read.csv(contr,F))
close(contr)

                                    
phrase <-function(t.text,n = 1,model = "Interpolate", params = list(l1 = 0.15, l2 = 0.2, l3 = 0.4, l4 = 0.25)) {


  y <- ""
  tags<-c("<p>","<n>", "<s>","<e>", "<UNK>")
  
  t.text<-stri_trim_right(t.text)   #remove whitespace
  t.text<-gsub   ('[<>]+'           ," ",t.text) #remove tagging chars
  t.text<-tolower(t.text) #set everything to lowercase
  t.text<-gsub   (profanity         ,"<p>", t.text) #tag profanity
  t.text<-gsub   ('((([0-9]{1,3})(,[0-9]{3})*)|([0-9]+))(.[0-9]+)?',   "<n>",t.text) #tag numbers
  t.text<-gsub   ('[\'’]'           , "" ,t.text)  #remove apostrophes
  t.text<-gsub   ('[()\"“”:;,_-]'   , " ",t.text) #remove other characters
  t.text<-gsub   ('[^a-zA-Z \n<>\']', " ",t.text) #remove anything that's not a letter
  t.text<-gsub   ('[ ][ ]+'         , " ",t.text) #remove whitespace
  
  
  
  phrase.length <- stri_count_boundaries(t.text) 
  
 
  
  if (phrase.length>0){
    
  t.text<-as.list(stri_extract_all_boundaries(t.text)[[1]])
  
  
  
  if (phrase.length==1)   
    
    target<-data.table(u = "", 
                       v = "", 
                       w = stri_trim_right(t.text[1])) else 
                                       if (phrase.length==2) target<-data.table( u = "", 
                                                                                 v = stri_trim_right(t.text[1]), 
                                                                                 w = stri_trim_right(t.text[2])) else
                                                                                 target<-data.table( u = stri_trim_right(t.text[[phrase.length-2]]), 
                                                                                                     v = stri_trim_right(t.text[[phrase.length-1]]), 
                                                                                                     w = stri_trim_right(t.text[[phrase.length]]))
                                                                                
    phrase.length<- ifelse (phrase.length>3, 3, phrase.length)

#   
#   t.text<-as.list(stri_extract_all_boundaries(t.text)[[1]][3:1])  
#    
#   #load  copy target text into datatable   
#   target<-data.table(u = t.text[[1]], v=t.text[[2]], w =t.text[[3]])
#    
# 
#    
#   # setnames(target, c("V1","V2", "V3"),c("u","v","w"))
  
 
  
  
  # replace out of vocab words in target with <UNK> tag
  target[,c("u","v","w"):=lapply(.SD , function(y) {
    # print(y %in% c(unigrams$x,""))
    ifelse ((y %in% c(unigrams$x,"")), y , "<UNK>")  #replace OOV words with <UNK> 
    
    })]
  
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
    table.predict<<-  data.table(rbind(
      
      
      quadrigrams[target[,.(u,v,w)], nomatch = 0 ][!(x %in% tags)][,weighted.prob := params$l4* (probability/2^20)][order(-weighted.prob), .(u,v,w,x, weighted.prob,table = 4, lambda = params$l4)][1:min(.N,10)],   
      trigrams   [target[,.(  v,w)],  ][!(x %in% tags)][,weighted.prob := params$l3* (probability/2^20)][order(-weighted.prob), .(v,w,x, weighted.prob,table = 3, lambda = params$l3)][1:min(.N,10)], 
      bigrams    [target[,.(    w)],  ][!(x %in% tags)][,weighted.prob := params$l2* (probability/2^20)][order(-weighted.prob), .(w,x, weighted.prob,table = 2, lambda = params$l2)][1:min(.N,10)], 
      unigrams   [order(-probability),][!(x %in% tags)][,weighted.prob := params$l1* (probability/2^20)][order(-weighted.prob), .(x, weighted.prob,table = 1, lambda = params$l1)],fill = T))
    
    ngram.probs<<- data.table()
    
    
    #sum the probabilities      

    y<-table.predict[,.( prob = sum(weighted.prob)), by = x][order(-prob),ifelse (x=="i","I", x) ][1:n]

    
    
  }
  
  else print("Not a valid model")}
  else y<-""
  
  y}