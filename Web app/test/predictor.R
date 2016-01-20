
getwd()

conf<-file("data/dirty.txt",'r')
profanity<-paste0("\\b(",paste0(readLines(conf),collapse = "|"),")\\b")    #profanity<-readLines(conf)
close (conf)
 
contr<-file("data/contractions",'r')  #load list of English contractions
contractions <- data.table(read.csv(contr,F))
close(contr)
                           
                                    
phrase <-function(target,n = 1,model = "Interpolate", params = list(l1 = 0.15, l2 = 0.2, l3 = 0.4, l4 = 0.25)) {


  y <- ""
  tags<-c("<p>","<n>", "<s>","<e>", "<UNK>")
  
  target<-stri_trim_right(target)   #remove whitespace
  target<-gsub   ('[<>]+'           ," ",target) #remove tagging chars
  target<-tolower(target) #set everything to lowercase
  target<-gsub   (profanity         ,"<p>", target) #tag profanity
  target<-gsub   ('((([0-9]{1,3})(,[0-9]{3})*)|([0-9]+))(.[0-9]+)?',   "<n>",target) #tag numbers
  target<-gsub   ('[\'’]'           , "" ,target)  #remove apostrophes
  target<-gsub   ('[()\"“”:;,_-]'   , " ",target) #remove other characters
  target<-gsub   ('[^a-zA-Z \n<>\']', " ",target) #remove anything that's not a letter
  target<-gsub   ('[ ][ ]+'         , " ",target) #remove whitespace
  
  
  
  phrase.length <- stri_count_words(target) 
  
  if (phrase.length==1) target<-data.table(V1 = "", 
                                           V2 = "", 
                                           V3 = target) else 
                                             if (phrase.length==2) target<-data.table(V1 = "", 
                                                                                      V2 = stri_extract_all_words(target)[[1]][1], 
                                                                                      V3 = stri_extract_all_words(target)[[1]][2]) else
                                                                                        target<-data.table(t(stri_extract_all_words(target)[[1]][(phrase.length-2):phrase.length]))
  
  #If phrase is longer than 3 words, discard all but the last 3
  
  
  
  setnames(target, c("V1","V2", "V3"),c("u","v","w"))
  
  if (phrase.length>3) {phrase.length<-3}
  
  
  # replace out of vocab words in target with <UNK> tag
  target[,c("u","v","w"):=lapply(.SD , function(y) ifelse ((y %in% c(unigrams$x,"")), y , "<UNK>"))]
  
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
      
      
      quadrigrams[target[,.(u,v,w)], nomatch = 0 ][!(x %in% tags)][,weighted.prob := params$l4* (probability/2^20)][order(-weighted.prob), .(x, weighted.prob,4)][1:min(.N,10)],   
      trigrams   [target[,.(  v,w)], ][!(x %in% tags)][,weighted.prob := params$l3* 2^-probability][order(-weighted.prob), .(x, weighted.prob,3)][1:min(.N,10)], 
      bigrams    [target[,.(    w)], ][!(x %in% tags)][,weighted.prob := params$l2* 2^-probability][order(-weighted.prob), .(x, weighted.prob,2)][1:min(.N,10)], 
      unigrams   [1:100,]             [!(x %in% tags)][,weighted.prob := params$l1* 2^-probability][order(-weighted.prob), .(x, weighted.prob,1)]))
    
    ngram.probs<<- data.table()
    
    
    #sum the probabilities      

    y<-table.predict[,.( prob = sum(weighted.prob)), by = x][order(-prob),ifelse (x=="i","I", x) ][1:n]

    
    
  }
  
  else print("Not a valid model")
  y}