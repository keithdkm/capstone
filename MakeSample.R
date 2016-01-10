##Required libraries
library("caret", lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("e1071", lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("tm",    lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
options( java.parameters = "-Xmx4g" )
library("RWeka", lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("data.table", lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("stringi", lib.loc = "~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("plyr", lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("git2r",      lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("hash", lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")


setwd("~/R/Capstone")

# Checks to see if original source files are loaded and loads them if required
load.data<-function(){
  ### FILE INFO CALCUALTIONS
  datasize <- function(file) {
    
    size <- object.size(file)
    
    nlines <- length(file)
    
    nwords <- sum(stri_count_words(file))
    
    data.frame ( File.Size = as.numeric(size/2^10), Lines = nlines, Words = nwords )
    
    
  }
  setwd("~/R/Capstone")
  if (!exists("allblogs")) {
    allblogs<<- iconv(readLines("Initial Dataset/final/en_US/en_US.blogs.txt",
                                n       = -1,
                                skipNul = TRUE, 
                                warn    = FALSE,
                                encoding= "UTF-8"))
    print("Blog data loaded")
    
  }
   ## print(unlist(datasize(allblogs))) 
  
  
  
  if (!exists("allnews")) { 
    allnews<<- iconv(readLines("Initial Dataset/final/en_US/en_US.news.txt", 
                               n       = -1,
                               skipNul = TRUE, 
                               warn    = FALSE ,
                               encoding= "UTF-8"))
    print("News data loaded")
    # print(unlist(datasize(allnews))) 
  }
  
  
  if (!exists("alltwitter")) {
    alltwitter<<- iconv(readLines("Initial Dataset/final/en_US/en_US.twitter.txt",
                                  n       = -1,
                                  skipNul = TRUE, 
                                  warn    = FALSE,
                                  encoding= "UTF-8"))
    
    print("Twitter data loaded")
    # print(unlist(datasize(alltwitter)))
  }
  
  return(cbind(File.Name = c("en_US.blogs.txt","en_US.news.txt","en_US.twitter.txt"), 
               rbind( datasize(allblogs),datasize(allnews),datasize(alltwitter))))
  
  
  
}


## Generate n samples of size% of the entire dataset
corpSample<-function(n,size)  {

  # Cleans the data stream to clean the data streams.  Intitially we wil remove 
  # Remove any whitespace beyond a single space between words 
  clean<-function(x,stopw){
    
  #General tagging function that accepts a corpus to search, a pattern to replace with the replacement  
    tagwords<-content_transformer(function(x,pattern, replacement) {gsub(pattern,
                                                                         replacement,
                                                                         x, 
                                                                         ignore.case = TRUE) })
    
    if(!dir.exists("Results")) dir.create("Results")
    
    
    con<-file("~/R/Capstone/Results/clean_text.txt","wt")
    writeLines("\nRAW TEXT", con)
    writeLines(substring(x[[1]]$content,1,12000),con)
 
    replacechars<-content_transformer(function(x,pattern,new) gsub(pattern, 
                                                                   new, 
                                                                   x))

    #replace <> so that I can use the <> characters for tagging
    x<-tm_map(x, replacechars, '[<>]+', " ") 
    writeLines("\nRemove other <> characters\n", con)
    writeLines(substring(x[[1]]$content,1,12000),con)
    
    
    
    #convert to Lower
    x<-tm_map(x,content_transformer(tolower))
    
    
    # Tag any words appearing on Google's list of profane words  
    conf<-file("~/R/Capstone/Required Data/dirty.txt",'r')
    profanity<-paste0("\\b(",paste0(readLines(conf),collapse = "|"),")\\b")    #profanity<-readLines(conf)
    close(conf)
    x<-tm_map(x,tagwords, profanity,replacement = " <P> ")
    writeLines("\nReplace profanity with a <P> tag\n", con)
    writeLines(substring(x[[1]]$content,1,12000),con)
    
   
    #remove stopwords
    if (stopw) {
      x<-tm_map(x, removeWords, stopwords("en"))
      writeLines("\nRemove stop words\n", con)
      writeLines(substring(x[[1]]$content,1,12000),con)}
    
    #replace numbers with <N> tags
    x<-tm_map(x, replacechars, '((([0-9]{1,3})(,[0-9]{3})*)|([0-9]+))(.[0-9]+)?',   " <N> " )  
    writeLines("\nTag Number with <N>", con)
    writeLines(substring(x[[1]]$content,1,12000),con)
    
    #replace all apostrophes with space '
    ## TEMPORARY replacing apostrohes with NULLS to treat contractions/possesives as unigrams
    # x<-tm_map(x, replacechars, '\'',              " \'") 
    x<-tm_map(x, replacechars, '[\'’]',              "") 
    # writeLines("\n Replace apostrophes with space apostrophes so that tokenizer treats contractions as two words", con)
    writeLines("\n Replace apostrophes with NULL  so that tokenizer treats contractions as one words", con)
    
    writeLines(substring(x[[1]]$content,1,12000),con)
    
        
    #replace all sentence ending chars with sentence end tag ,newline and sentence start tag
    
#     x<-tm_map(x, replacechars, '[.?!]+ ',              " <e> \n <s> ") 
#     writeLines("\nReplace sentence start and end\n", con)
#     writeLines(substring(x[[1]]$content,1,12000),con)
#     
    #remove apostrohes from contractions 
#     x<-tm_map(x, replacechars, '[\'\`]',      "" )  
#     writeLines("\nRemove apostrophes", con)
#     writeLines(substring(x[[1]]$content,1,12000),con)
    
    #   x<-tm_map(x, replacechars, '[@][a-zA-Z]+',"\n")  #remove twitter names
    #   x<-tm_map(x, replacechars, '[#][a-zA-Z]+',"\n")  #remove twitter hashtags
    
    #Replace other non-alphanumerics with a blank  
    x<-tm_map(x, replacechars, '[()\"“”:;,_-]', " ") 
    writeLines("\nRemove other stop characters\n", con)
    writeLines(substring(x[[1]]$content,1,12000),con)
    
    #Replace other umknown characters with a blank
    x<-tm_map(x, replacechars, '[^a-zA-Z \n<>\']',         " ")  
    writeLines("\nRemove other unknown characters\n", con)
    writeLines(substring(x[[1]]$content,1,12000),con)
    
    #Remove single letters that are not valid single letters 
    x<-tm_map(x, replacechars, "[ ][^ai\n][ ]",       " ") 
    writeLines("\nRemove invalid single characters\n", con)
    writeLines(substring(x[[1]]$content,1,12000),con)
    
    #remove extra whitespace 
    x<-tm_map(x, replacechars, '[ ][ ]+', " ") 
    writeLines("\nRemove additional whitespace characters\n", con)
    writeLines(substring(x[[1]]$content,1,12000),con)
    close(con)
    return(x)
    
  }
  

  print("SAMPLING AND CLEANING DATA")

  started.at = proc.time()
  
  print(paste0("Loading Raw Data starting at ", Sys.time()))
  
  load.data()
  
  load.time<<-timetaken(started.at)
  
  cat("Loaded entire corpus in ",load.time,"\n") 
  
  started.at = proc.time()
  
  setwd("~/R/Capstone")

  if(!dir.exists("Sample Data")) dir.create("Sample Data")
  
  if(!dir.exists("Test Data")) dir.create("Test Data")
  
  size      <- size/100  # what proportion of the file should the sample represent

  blogsize    = round(size * length(allblogs))
  newssize    = round(size * length(allnews))
  twittersize = round(size * length(alltwitter))

  set.seed(12051105)

  
  
  blogs.inds      <-   matrix(sample(x  = length(allblogs), 
                                     size    = 2 * n * blogsize, 
                                     replace = FALSE), nrow = 2 * n , ncol = blogsize)
  
  news.inds       <-   matrix(sample(x  = length(allnews), 
                                     size    = 2 * n * newssize, 
                                     replace = FALSE), nrow = 2 * n , ncol = newssize)

  twitter.inds    <-   matrix(sample(x  = length(alltwitter), 
                                     size    = 2 * n * twittersize, 
                                     replace = FALSE), nrow = 2 * n , ncol = twittersize)
  
  test.Corpus <<- VCorpus(VectorSource(NULL))
  
  ##Create a directory to save the sample  
  
  run_time <- gsub("[ :-]","_",as.character(Sys.time()))
  tr.path  <- paste0("Sample Data/",n,"_",size*100,"_",run_time)
  test.path<- paste0("Test Data/"  ,n,"_",size*100,"_",run_time)
 
  dir.create(tr.path)
  dir.create(test.path)
  

  for (i in 1:n){

    tr.samp   = paste(paste(allblogs[blogs.inds[i,]],collapse = " "),
                      paste(allnews [news.inds[i,]],collapse = " "),
                      paste(alltwitter [twitter.inds[i,]],collapse = " "), collapse = " ")
    
    test.samp = paste(paste(allblogs[blogs.inds[n+i,]],collapse = " "),
                      paste(allnews [news.inds[n+i,]],collapse = " "),
                      paste(alltwitter [twitter.inds[n+i,]],collapse = " "), collapse = " ")
    
    tr.samp   <- clean(VCorpus(VectorSource(tr.samp)),FALSE)
    
    test.samp <- clean(VCorpus(VectorSource(test.samp)),FALSE)
    
    #test.Corpus <<- c(test.Corpus,test.samp)   ########experimental
    
    file.name.tr<-paste0("Sample Data/",n,"_",size*100,"_",run_time,"/trsamp_",i,".RDS")
    
    file.name.test<-paste0("Test Data/",n,"_",size*100,"_",run_time,"/testsamp_",i,".RDS")
    
    saveRDS(tr.samp, file = file.name.tr)
    
    saveRDS(test.samp, file = file.name.test)
    
    print(paste0("Sample ", i, " saved"))
    
    }

  samp.time <<- timetaken(started.at)
  
  print(paste("Generated, Cleaned and Saved ",
              n,
              " Samples, each representing ", 
              as.character(size*100),
              " percent of the full text and a correpsonding set of Test Data",
              samp.time))

  return(list(tr.path = tr.path,test.path = test.path))
}






##Divides table of ngrams into separate tables and calculates the frequency
make.ngrams<-function(path,min.ng,max.ng,n,size,coverage){

  print( "GENERATING NGRAMS")
  
   #Extracts count for each ngram from Term Document matrix
  extr_ngram_counts<-function(tdm) {
    
    tdm<-as.matrix(tdm)
    
    word.freq <- data.table(ngram = row.names(tdm), count = tdm[,1]) 

    word.freq
  }
  
  # Tokenizer control generates ngrams of length min to max 
  Tokenizer <- function(x) {
                            NGramTokenizer(x, 
                                            Weka_control(min        = min.ng, 
                                                         max        = max.ng,
                                                         delimiters = " .\n)"))}
  
  started.at = proc.time()
  
  ngramfreq<-data.table(NULL)
  
  
  
  for (i in 1:n) {
    
    file.name<-paste0(path,"/trsamp_",i,".RDS")
    
    print(paste0("Reading ", file.name," in ",path))
    
    tr.samp<-readRDS(file.name)  # Read in hte corpus of prepared samples
    
    print(paste("Processing Sample ", i, " at", Sys.time())) 
    
    tdm <- TermDocumentMatrix(tr.samp, control = list(wordLengths = c(1,Inf),
                                                      tokenize    = Tokenizer)) ; rm(tr.samp)
    
    ngramfreq<-rbind(ngramfreq,extr_ngram_counts(tdm),fill=TRUE) ; rm(tdm)
    
  }

  
  setkey(ngramfreq,ngram)
  ## Rolls up ngram counts into totals by ngram

  
  ngramfreq<-unique(ngramfreq[, count := sum(count),by = ngram][, wordcount := stri_count_words(ngram) ])

  setkey(ngramfreq,wordcount,ngram)
  
  # UNIGRAMS
  
  tags<-c("<p>","<n>", "<s>","<e>", "<UNK>")
  
  if (max.ng>0) {
  unigrams<<- ngramfreq[.(1), 
                        .(count = sum(count)) , 
                        by = .(ngram)]
  
  setnames(unigrams, "ngram", "x")
  
  old_count<- unigrams[,sum(count)] 
  #unigrams<<-unigrams[!(ngram %in% tags),]  #remove tagged counts - we don't need to know probabilities of tags for  the moment
  #????????????????????? should <n> and <p. have probability mass?  remove i below to give them mass
  unigrams<<-rbind(unigrams[!(x %in% tags),Mean.Probability := count/sum(count)][(x %in% tags),Mean.Probability := 0][order(-count),
                                                                   ':='(Cum.Probability  = cumsum(Mean.Probability),
                                                                        probability      = -log2(Mean.Probability))][Cum.Probability<=(coverage/100),][order(x),],
                   
                   data.table(x       = "<UNK>", 
                              count       = old_count - unigrams[Cum.Probability<=(coverage/100),sum(count)],
                              Mean.Probability = (100-coverage)/100,
                              Cum.Probability  = 1,
                              probability = -log2((100-coverage)/100)),fill = T )
  

                   
                   }
                 
        
                setkey(unigrams,x)

  #BIGRAMS
  
  if (max.ng>1) {
  # Take bigrams from ngram and sum their count by bigram. Split bigram into two separate columns   
  
  bigrams <<- ngramfreq[.(2), .(count = sum(count)) , by = .(ngram)][, c("w", "x") := tstrsplit(ngram, " ", fixed = TRUE)]
  
  bigrams[,c("w","x") :=  lapply(.(w,x), function(ngram) ifelse ((ngram %in% unigrams[,x]), ngram, "<UNK>"))]
  
  bigrams[w=="<UNK>" | x=="<UNK>" ,count:=sum(count), by = .(w, x)]
  
  setkey(bigrams,w,x)
  
  bigrams<<-unique (bigrams)
  
  bigrams[, probability := -log2(count/ unigrams[bigrams$w, count])]

  setkey(bigrams,w,x)
  }
                
                  
    
  #TRIGRAMS
  
if (max.ng > 2) {
   trigrams <<- ngramfreq[.(3), .(count = sum(count)) , by = .(ngram)][, c("v","w","x") := tstrsplit(ngram, " ", fixed = TRUE)]
  
   #tag u,  v and w not in the vocabualry with <UNK> 
 
   trigrams[,c("v","w","x") :=  lapply(.(v,w,x), function(ngram) ifelse (!(ngram %in% unigrams[,x]), "<UNK>", ngram))][,vw:=paste0(v," ",w)]
   
   trigrams[ v=="<UNK>" | w == "<UNK>" | x=="<UNK>" ,count:=sum(count), by = .(v, w, x)]

   setkey(trigrams,v,w,x)
   
   trigrams<<-unique (trigrams)
  
  
  trigrams[, probability := -log2(count/ bigrams[.(trigrams$v,trigrams$w), count])]
  }
                
   setkey (trigrams,v,w,x)                 

  #QUADRIGRAMS
                
  if (max.ng > 3) {
    
    quadrigrams <<- ngramfreq[.(4), .(count = sum(count)) , by = .(ngram)][, c("u", "v","w","x") := tstrsplit(ngram, " ", fixed = TRUE)]
  
    quadrigrams[,c("u","v","w", "x") :=  lapply(.(u,v,w,x), function(ngram) ifelse (!(ngram %in% unigrams[,x]), "<UNK>", ngram))][,uvw:=paste0(u," ",v," ",w)]
                    
    quadrigrams[u=="<UNK>" | v=="<UNK>" | w == "<UNK> "| x == "<UNK>",count:=sum(count), by = .(u, v, w, x)]
    
    setkey(quadrigrams,u,v,w,x)
    
    quadrigrams<<-unique (quadrigrams)
    
    setkey(quadrigrams,ngram)
                    
    quadrigrams[, probability := -log2(count/ trigrams[.(quadrigrams$u,quadrigrams$v,quadrigrams$w), count])]}
                  
                    # quadrigrams[,p4_wvu := log10(count)]
                    
  if (max.ng > 1)   setkey(bigrams,w)
  if (max.ng > 2)   setkey(trigrams,v,w)
  if (max.ng > 3)   setkey(quadrigrams,u,v,w)
                
  ngram.time<<-timetaken(started.at)
  
  cat("Finished ",n,"samples in ",timetaken(started.at),"\n") 
  
 #save the entire ngramfrq table for use in the Interim report 
  saveRDS(ngramfreq,"Results/Interim.RDS")
  
  
  return (ngramfreq)
 
  
  
    }
 


## examines unigram table and returns the lsit of words required to get the specified coverage

vocabulary<-function(coverage){
  
  setorder(unigrams,-count)
  old_count<-unigrams[,sum(count)]
  
  ##remove all unigrams that are in the least probable (100-coverage)% of unigrams and add an <UNK> unigram to replace unseen unigrams (OOV words)  
  unigrams <<- rbind(unigrams[,Mean.Probability := count/sum(count)][,Cum.Probability:=cumsum(Mean.Probability)][Cum.Probability<=(coverage/100),][order(ngram),], 
                     data.table(ngram = "<UNK>", count = total_count - unigrams[,sum(count)], probability = -log2(0.05)),fill = T )
  
  new_count<-unigrams[,sum(count)]
  unigrams[ngram=="<UNK>", count:=old_count-new_count]
  
  
  # Remove additional probability columns
 ## unigrams[,c("Mean.Probability","Cum.Probability") := NULL]
}


prune<-function(n){

# remove predicted words not in the vocabulary    
if (n==4) quadrigrams<<- quadrigrams[x %in% unigrams[,ngram]]
if (n==3) trigrams   <<-    trigrams[w %in% unigrams[,ngram]]
if (n==2) bigrams    <<-    bigrams [v %in% unigrams[,ngram]]
  
######################   PRUNING IDEAS   #################
 #Remove ngrams that have <UNK> unigrams and a low (1?) count 
}


# Predicts n possible next words from a phrase x 
phrase <-  function(target,n = 1,model = "Interpolate", params) {
  2
  y <- ""
  tags<-c("<p>","<n>", "<s>","<e>", "<UNK>")
  
  #########!!!!!!!!!!!!!!   This needs to clean target rather than just lower casing it
  target<-tolower(target)
  
  
  phrase.length <- stri_count_words(target) 
 
  
  #If phrase is longer than 3 words, discard all but the last 3
  
  target<-data.table(t(stri_extract_all_words(target)[[1]][(phrase.length-2):phrase.length]));
  
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


                                     quadrigrams[target[,.(u,v,w)], ][!(x %in% tags)][,weighted.prob := params$l4* 2^-probability][order(-weighted.prob), .(x, weighted.prob)][1:min(.N,10)],   
                                     trigrams   [target[,.(  v,w)], ][!(x %in% tags)][,weighted.prob := params$l3* 2^-probability][order(-weighted.prob), .(x, weighted.prob)][1:min(.N,10)], 
                                     bigrams    [target[,.(    w)], ][!(x %in% tags)][,weighted.prob := params$l2* 2^-probability][order(-weighted.prob), .(x, weighted.prob)][1:min(.N,10)], 
                                     unigrams                        [!(x %in% tags)][,weighted.prob := params$l1* 2^-probability][order(-weighted.prob), .(x, weighted.prob)]))


  
  #sum the probabilities      
  y<-table.predict[, sum(weighted.prob, rm.na = T), by = x][order(-V1), x][1:n]
  
  
    
  }
  
  else print("Not a valid model")
   y}

#measures model accuracy against n test strings
accuracy<-function(n = 100, model = "Interpolate", params){
  
   path<-paths$test.path
  
   phrases<-""
   actual_words<-""
   pred_words<-""
   
   
   
  for (i in 1:1) {
    
    file.name<-paste0(path,"/testsamp_",i,".RDS")
    
    print(paste0("Reading ", file.name," in ",path))
    
    test.samp<-readRDS(file.name)  # Read in hte corpus of prepared samples
    
    print(paste("Measuring accuracy with Sample ", i, " at", Sys.time())) 
  
    testlist<-stri_extract_all_words(test.samp[[1]]$content)[[1]]
    testlength<-round((length(testlist)-3))
    test.table<<- data.table(u = testlist[1:(testlength-3)], 
                             v = testlist[2:(testlength-2)], 
                             w = testlist[3:(testlength-1)],
                             x = testlist[4:(testlength)])
    
    correct<-0
    
    started.at = proc.time()
    
    
    test.table[1:n,prediction := lapply ( paste(u,v,w),phrase,1,model, params)]
    
    }
  
    
    
    acc_time<-timetaken(started.at)
    
    time.per.sample<-round(as.numeric(as.difftime(acc_time,  format="%H:%M:%S",units = "secs"))/n,2)
    
    acc<-paste0(round(test.table[,sum(x == prediction,rm.na =T)/n]*100,1),"%")
    
    print(paste("Parameters of " , paste(params), "yield",acc," accuracy in ",n," word sample in", time.per.sample,
                " seconds per prediction" )
                
                )
    
  

#    Correct <- (actual_words==pred_words)
#    acc_test_results<<-cbind(phrases,actual_words,pred_words, Correct)
   
   list( words = n, accuracy = acc, time.per.prediction = time.per.sample )
}

main<-function(resamp = F,path = "Sample Data/",num.sample = 200, sz.sample = 0.1, gengram = F,ng.size = 4, coverage = 95, model = "Interpolate", params) {
  
  ## Summary Results are stored in the masterlsit file
  # rm(GlobalEnv::unigrams);rm(trigrams);rm(bigrams);rm(results)
              
  Exec.time<-Sys.time()
#If reusing a sample take passed in path
  paths$tr.path <<- path
#otherwise make a new sample    
  if (resamp) paths<<-corpSample(num.sample,sz.sample)
  
  if (gengram) make.ngrams(path = paths$tr.path, min = 1, max = ng.size, num.sample, sz.sample,coverage)
  
  
  repo<-repository("~/R/Capstone/")
  
  
  ##read in current results table
  ifelse (file.exists("~/R/Capstone/Results/masterlist.RDS"),
          x<-readRDS("~/R/Capstone/Results/masterlist.RDS"),
          x<-data.table(NULL))
  
  acc <-accuracy(500, params = params)
  
  new_results<-list(Time           = strftime(Exec.time, "%c"),
                    Commit         = substr(branch_target(head(repo)),1,8),
                    Notes          = commits(repo)[[1]]@message,
                    N              = paste(num.sample,"samples"), 
                    Size           = paste0(sz.sample,"%"),
                    DataPath       = paths$tr.path,
                    Model.Size     = ng.size,
                    Load.Time      = load.time,
                    Sample.Time    = samp.time,
                    Ngram.Time     = ngram.time,
                    N.unigrams     = unigrams[,.N],
                    U.size         = paste0(round(object.size(unigrams)/10^6,2),"Mb"),
                    N.bigrams      = bigrams[,.N],
                    bi.size        = paste0(round(object.size(bigrams)/10^6,2),"Mb"),
                    N.trigrams     = ifelse(exists("trigrams"),trigrams[,.N],0),
                    tri.size       = ifelse(exists("trigrams"),paste0(round(object.size(trigrams)/10^6,2),"Mb"), "0Mb"),
                    N.quadrigrams  = ifelse(exists("quadrigrams"),quadrigrams[,.N],0),
                    quad.size      = ifelse(exists("quadrigrams"),paste0(round(object.size(quadrigrams)/10^6,2),"Mb"), "0Mb"),
                    Coverage       =  coverage,
                    Test.size       = acc$words,
                    Accuracy       =  acc$accuracy,
                    Performance    =  paste(acc$time.per.prediction,"secs per word"),
                    Parameters     =  params
                    )
  
  
  

  results<<- rbind(x,  data.table(t(new_results)),fill = TRUE)
  
  saveRDS(results,"~/R/Capstone/Results/masterlist.RDS")
  
  
  
  
  }


save.ngrams<-function () {  path = paste0( "~/R/Capstone/ngrams/", gsub("[ :]", "_", x = Sys.time()))
                            if (!dir.exists(path)) dir.create(path)
                            saveRDS(object = unigrams,
                                    file =  paste0(path,"/unigrams.RDS"))
                            saveRDS(object = bigrams,
                                    file =  paste0(path,"/bigrams.RDS"))
                            saveRDS(object = trigrams,
                                    file =  paste0(path,"/trigrams.RDS"))
                            saveRDS(object = quadrigrams,
                                    file =  paste0(path,"/quadrigrams.RDS"))}
  
restore.ngrams<-function (path) {  
                            unigrams<<- readRDS(file =  paste0(path,"unigrams.RDS"))
                            bigrams<<-  readRDS(file =  paste0(path,"bigrams.RDS"))
                            trigrams<<- readRDS(file =  paste0(path,"trigrams.RDS"))
                            quadrigrams<<- readRDS(file =  paste0(path,"quadrigrams.RDS"))
                            
                            }


##############################################################################
################################################################################

#Calculates the perplexity of model x,against the cleaned corpus y.  X shoudl indicate the maximum length of trigrams to use
# perplexity <-function(y) {
#   
#   
#   a<-unlist(stri_split_regex(y[[1]]$content,"<e>\n<s>"))
#   
#   c<-data.frame(t(sapply(a[1:200], function(x){ word.count <- stri_count_words(x);
#                             
#                             b<-stri_extract_all_words(x)[[1]][1:2]
#                             
#                             phr.prob <- sum(trigrams[paste0("<e> <s> ",b[1]),pw_uv],
#                                        trigrams[paste0("<s> ",b[1]," ",b[2]),pw_uv],na.rm = TRUE)
#                             
#                             if(word.count>2) {b <- NGramTokenizer(x,     
#                                                Weka_control(min        = 3, 
#                                                             max        = 3,
#                                                             delimiters = ". "));
#                             
#                             
#                             phr.prob <- sum(phr.prob, sum(sapply(b, 
#                                                    function(z) {
#                                                      trigrams[z,pw_uv]}),na.rm=TRUE),na.rm=TRUE)
#                             
#                             }
#                                             
#                             
#                             data.frame(sentence = x,
#                                         M = word.count,
#                                         sent.prob = phr.prob)})))
# 
#    
#   sum(unlist(c$sent.prob))/sum(unlist(c$M))
#   
#   }
# 


# 
#   a_spl <- function(dt) {
#     ll <- unlist(strsplit(dt$PREFIX, "_", fixed=TRUE))
#     idx <- seq(1, length(ll), by = 2)
#     dt[, `:=`(px = ll[idx], py = ll[idx+1])]
#   }
#   
#   a_sub <- function(dt, u) {
#     dt[, `:=`(px = substring(PREFIX, 1, u), 
#               py = substring(PREFIX, u+2, nchar(PREFIX)))]
#     
#   }
#   
#   op <- function(dt) {
#     dt[, px := sapply(strsplit(dt$PREFIX, split="_"), "[", 1)]
#     dt[, py := sapply(strsplit(dt$PREFIX, split="_"), "[", 2)]
#   }
#   
#   dt = data.table( PREFIX=rep(c("A_B","A_C","A_D","B_A","B_C","B_D"), 
#                               1000000), VALUE=rep(c(1,2,3,4,5,6), 1000000) )
#   
#   require(microbenchmark)
#   microbenchmark(a1 <- a_spl(copy(dt)), 
#                  a2 <- a_sub(copy(dt), 1), 
#                  a3 <- op(copy(dt)), times = 5)
#   
#   Unit: seconds
#   expr       min        lq     median         uq        max neval
#   a1 <- a_spl(copy(dt))  4.496515  4.577718   7.620985   7.898372  11.081218     5
#   a2 <- a_sub(copy(dt), 1)  2.545999  2.603123   2.671415   2.785419   3.193427     5
#   a3 <- op(copy(dt)) 95.287777 96.092578 114.013580 122.539977 130.565100     5
#   
#   identical(a1, a2) # TRUE
#   identical(a1, a3) # TRUE

optimum<-function(){
  
  #create a datatable with all possible values of lambda
  
  possibles<-CJ( l1= seq(0,1,0.1), l2 = seq(0,1,0.1),l3 = seq(0,1,0.1), l4 = seq(0,1,0.1))

  #filter rows so that only valid combinations are used
  
  possibles<-possibles[l1+l2+l3+l4 <=1,]

  lapply(split(possibles,seq(possibles[,.N])), main, resamp = F,
                              path =   "Sample Data/200_0.1_2016_01_09_14_07_15/",
                              num.sample = 200,
                              sz.sample = 0.1,
                              gengram = F,
                              ng.size = 4,
                              coverage = 95,
                              model = "Interpolate"
                              )


  }
