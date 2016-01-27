
library("data.table", lib.loc="~/R/win-library/3.2")
library("stringi", lib.loc = "~/R/win-library/3.2")

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
                                warn    = FALSE
                                ,encoding= "UTF-8"
    ))
    
    
    print("Blog data loaded")
    
  }
  ## print(unlist(datasize(allblogs))) 
  
  
  
  if (!exists("allnews")) { 
    allnews<<- iconv(readLines("Initial Dataset/final/en_US/en_US.news.txt", 
                               n       = -1,
                               skipNul = TRUE, 
                               warn    = FALSE,
                               encoding= "UTF-8"
    ))
    
    
    print("News data loaded")
    # print(unlist(datasize(allnews))) 
  }
  
  
  if (!exists("alltwitter")) {
    alltwitter<<- iconv(readLines("Initial Dataset/final/en_US/en_US.twitter.txt",
                                  n       = -1,
                                  skipNul = TRUE, 
                                  warn    = FALSE,
                                  encoding= "UTF-8"
    ))
    
    
    print("Twitter data loaded")
    # print(unlist(datasize(alltwitter)))
  }
  
  return(cbind(File.Name = c("en_US.blogs.txt","en_US.news.txt","en_US.twitter.txt"), 
               rbind( datasize(allblogs),datasize(allnews),datasize(alltwitter))))
  
  
  
}

## Generate n samples of size% of the entire dataset
corpSample<-function(n,sample,holdout,test)  {
  
  # Cleans the data stream to clean the data streams.  Intitially we wil remove 
  # Remove any whitespace beyond a single space between words 
  clean<-function(x,stopw){
    
    #General tagging function that accepts a corpus to search, a pattern to replace with the replacement  
    tagwords<-content_transformer(function(x,pattern, replacement) {gsub(pattern,
                                                                         replacement,
                                                                         x, 
                                                                         ignore.case = TRUE) })
    ######cleaning algorithm
    ###Initialize
    
    
    
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
    conf<-file("~/R/Capstone/Web app/test/data/dirty.txt",'r')
    profanity<-paste0("\\b(",paste0(readLines(conf),collapse = "|"),")\\b")    #profanity<-readLines(conf)
    close(conf)
    x<-tm_map(x,tagwords, profanity,replacement = "<pr>")
    writeLines("\nReplace profanity with a <pr> tag\n", con)
    writeLines(substring(x[[1]]$content,1,12000),con)
    
    
    #remove stopwords
    if (stopw) {
      x<-tm_map(x, removeWords, stopwords("en"))
      writeLines("\nRemove stop words\n", con)
      writeLines(substring(x[[1]]$content,1,12000),con)}
    
    #replace numbers with <num> tags
    x<-tm_map(x, replacechars, '((([0-9]{1,3})(,[0-9]{3})*)|([0-9]+))(.[0-9]+)?',   "<num>" )  
    writeLines("\nTag Number with <num>", con)
    writeLines(substring(x[[1]]$content,1,12000),con)
    
    #replace all apostrophes with space '
    ## TEMPORARY replacing apostrohes with NULLS to treat contractions/possesives as unigrams
    # x<-tm_map(x, replacechars, '\'',              " \'") 
    x<-tm_map(x, replacechars, "['’]", "") 
    # writeLines("\n Replace apostrophes with space apostrophes so that tokenizer treats contractions as two words", con)
    writeLines("\n Replace apostrophes with NULL  so that tokenizer treats contractions as one words", con)
    
    writeLines(substring(x[[1]]$content,1,12000),con)
    
    
    #replace all sentence boundaries with sentence  tag 
    
    x<-tm_map(x, replacechars, '[.?!]+ ',              " <se> ") 
    writeLines("\nReplace sentence start and end\n", con)
    writeLines(substring(x[[1]]$content,1,12000),con)
    
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
    
    #remove extra whitespace 
    x<-tm_map(x, replacechars, '[ ][ ]+', " ") 
    writeLines("\nRemove additional whitespace characters\n", con)
    writeLines(substring(x[[1]]$content,1,12000),con)
    
    
    
    #Attach floating contractions to their preceding words for  apostophes not picked up by apostrophes scan
    pp<-c("s","d","t","ve", "m" ,"re", "ll")
    
    for (i in pp) {
      x<-tm_map(x, replacechars, paste0(" ",i," "), paste0(i," ")) }
    writeLines("\nAttach floating contractions\n", con)
    writeLines(substring(x[[1]]$content,1,12000),con)
    
    #Remove single letters that are not valid single letters 
    x<-tm_map(x, replacechars, "\\b[^ai\n]\\b",       " ") 
    writeLines("\nRemove invalid single characters\n", con)
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


tags<-c("<pr>","<num>", "<se>", "<UNK>")

cleantext<- function(t.text){
  # t.text<-stri_trim_right(t.text)   #remove whitespace
  # t.text<-paste( unlist(stri_extract_all_words(allnews)), sep = "<n>")
  
  t.text<-gsub   ('[<>]+'           ," "   ,t.text) #remove tagging chars
  # t.text<-gsub   ('\n',              "<n>" ,t.text)  # replace end of lines with <n> tag
  t.text<-tolower(t.text) #set everything to lowercase
  t.text<-gsub   (profanity         ,"<pr>", t.text) #tag profanity
  t.text<-gsub   ('[.?!]+'," <se> "        ,t.text) #Replace end of sentence characters with <se> tag
  t.text<-gsub   ('((([0-9]{1,3})(,[0-9]{3})*)|([0-9]+))(.[0-9]+)?',   "<num>",t.text) #tag numbers
  t.text<-gsub   ('[\'’]'           , "" ,t.text)  #remove apostrophes
  t.text<-gsub   ('[()\"“”:;,_-]'   , " ",t.text) #remove other characters
  t.text<-gsub   ('[^a-zA-Z \n<>\']', " ",t.text) #remove anything that's not a letter
  t.text<-gsub   ('[ ][ ]+'         , " ",t.text) #remove whitespace
  pp    <-c      ("s","d","t","ve", "m" ,"re", "ll")
    for (i in pp) {
  t.text<-gsub   ( paste0(" ",i," "), paste0(i," "), t.text) }
  t.text<-gsub   ("\\b[^ai\n]\\b"," ",t.text)
  t.text<-paste  (t.text,"<n>")
}

make.ngrams2 <- function(t.text){

  print("SAMPLING AND CLEANING DATA")
  
  started.at = proc.time()
  
  print(paste0("Loading Raw Data starting at ", Sys.time()))
 

  t.text.allnews<<-lapply(FUN = cleantext,X= t.text)
  
  load.time<<-timetaken(started.at)

  print("End Clean data") 


cat("Cleaned entire corpus in ",load.time,"\n") 

print("Building Ngrams")

started.at = proc.time()

print(paste0("Loading Raw Data starting at ", Sys.time()))


t.text.allnews <- unlist(stri_extract_all_boundaries(t.text.allnews))

# all.grams<- data.table(u = t.text.allnews, 
#                        v = c(t.text.allnews[2:last(t.text.allnews)],NA), 
#                        w = c(t.text.allnews[3:last(t.text.allnews)],NA,NA),
#                        x = c(t.text.allnews[4:last(t.text.allnews)],NA,NA,NA))   

unigrams<-     data.table(V1 = t.text.allnews, count = 1)

bigrams <-     data.table(cbind(unigrams,
                 V2 =  c(t.text.allnews[2:length(t.text.allnews)],NA)))

trigrams <-    data.table(cbind(bigrams,
                   V3 =  c(t.text.allnews[3:length(t.text.allnews)],NA,NA)))

quadrigrams <- data.table(cbind(trigrams,
                    V4 =   c(t.text.allnews[4:length(t.text.allnews)],NA,NA,NA)))

setnames(unigrams, "V1", "x")
setnames(bigrams, c("V1","V2"), c("w","x"))
setnames(trigrams, c("V1","V2","V3"), c("v","w","x"))
setnames(quadrigrams, c("V1","V2","V3", "V4"), c("u","v","w","x"))

setkey(unigrams, x)
setkey(bigrams,w,x)
setkey(trigrams,v,w,x)
setkey(quadrigrams,u,v,w, x)

setcolorder(bigrams,c("w","x","count"))
setcolorder(trigrams,c("v","w","x","count"))
setcolorder(quadrigrams,c("u","v","w","x","count"))


unigrams[,count:= sum( count), by = x]
bigrams[,count:= sum( count), by = .(w,x)]
trigrams[,count:= sum( count), by = .(v,w,x)]
quadrigrams[,count:= sum( count), by = .(u,v,w,x)]

unigrams<<-unique(unigrams)
bigrams<<-unique(bigrams)
trigrams<<-unique(trigrams)
quadrigrams<<-unique(quadrigrams)



ngramtime<<-timetaken(started.at)

print("End Ngram build") 

}

# unigrams<-data.table(x = pp, count = 1)




make.ngrams<-function(path,min.ng,max.ng,n,size,coverage, unk = TRUE){
  
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
  
  tags<-c("<pr>","<num>", "<se>","<e>", "<UNK>")
  
  if (max.ng>0) {
    
    unigrams<<- ngramfreq[.(1), 
                          .(count = sum(count)) , 
                          by = .(ngram)]
    #save the entire ngramfrq table for use in the Interim report 
    saveRDS(ngramfreq,"Results/Interim.RDS")
    
    setnames(unigrams, "ngram", "x")
    
    old_count<- unigrams[,sum(count)] 
    #unigrams<<-unigrams[!(ngram %in% tags),]  #remove tagged counts - we don't need to know probabilities of tags for  the moment
    #????????????????????? should <num> and <p. have probability mass?  remove i below to give them mass   -- DONE 1/22/16
    unigrams<<-rbind(unigrams[,Mean.Probability := count/sum(count)][order(-count),
                                                                     ':='(Cum.Probability  = cumsum(Mean.Probability),
                                                                          probability      = as.integer(round(2^25*(Mean.Probability))))][Cum.Probability<=(coverage/100),][order(x),],
                     
                     data.table(x       = "<UNK>", 
                                count       = old_count - unigrams[Cum.Probability<=(coverage/100),sum(count)],
                                Mean.Probability = (100-coverage)/100,
                                Cum.Probability  = 1,
                                probability = as.integer(round(2^20*((100-coverage)/100)),fill = T )))
    
  }
  #remove Mean and Cumulative probability columns as they are no longer required    
  unigrams[,c("Cum.Probability","Mean.Probability"):=NULL]            
  
  setkey(unigrams,x)
  
  #BIGRAMS
  
  if (max.ng>1) {
    # Take bigrams from ngram and sum their count by bigram. Split bigram into two separate columns   
    
    bigrams <<- ngramfreq[.(2), .(count = sum(count)) , by = .(ngram)][, c("w", "x") := tstrsplit(ngram, " ", fixed = TRUE)][, ngram:=NULL]
    
    if (unk) {bigrams[,c("w","x") :=  lapply(.(w,x), function(ngram) ifelse ((ngram %in% unigrams[,x]), ngram, "<UNK>"))]
      
      bigrams[w=="<UNK>" | x=="<UNK>" ,count:=sum(count), by = .(w, x)]}
    
    setkey(bigrams,w,x)
    
    bigrams<<-unique (bigrams)
    
    bigrams[, probability := as.integer(round(2^25*(count/ unigrams[bigrams$w, count])))]
    
    setkey(bigrams,w,x)
  }
  
  
  
  #TRIGRAMS
  
  if (max.ng > 2) {
    trigrams <<- ngramfreq[.(3), .(count = sum(count)) , by = .(ngram)][, c("v","w","x") := tstrsplit(ngram, " ", fixed = TRUE)][, ngram:=NULL]
    
    #tag u,  v and w not in the vocabualry with <UNK> 
    if (unk) {trigrams[,c("v","w","x") :=  lapply(.(v,w,x), function(ngram) ifelse (!(ngram %in% unigrams[,x]), "<UNK>", ngram))]
      
      trigrams[ v=="<UNK>" | w == "<UNK>" | x=="<UNK>" ,count:=sum(count), by = .(v, w, x)]}
    
    setkey(trigrams,v,w,x)
    
    trigrams<<-unique (trigrams)
    
    
    trigrams[, probability := as.integer(round(2^25*(count/ bigrams[.(trigrams$v,trigrams$w), count])))]
  }
  
  setkey (trigrams,v,w,x)                 
  
  #QUADRIGRAMS
  
  if (max.ng > 3) {
    
    quadrigrams <<- ngramfreq[.(4), .(count = sum(count)) , by = .(ngram)][, c("u", "v","w","x") := tstrsplit(ngram, " ", fixed = TRUE)][, ngram:=NULL]
    
    
    if (unk) {quadrigrams[,c("u","v","w", "x") :=  lapply(.(u,v,w,x), function(ngram) ifelse (!(ngram %in% unigrams[,x]), "<UNK>", ngram))]
      
      quadrigrams[u=="<UNK>" | v=="<UNK>" | w == "<UNK> "| x == "<UNK>",count:=sum(count), by = .(u, v, w, x)]}
    
    setkey(quadrigrams,u,v,w,x)
    
    quadrigrams<<-unique (quadrigrams[count>1])
    
    quadrigrams[, probability := as.integer(round(2^25*(count/ (trigrams[.(quadrigrams$u,quadrigrams$v,quadrigrams$w),count])),0))]
  }
  
  # quadrigrams[,p4_wvu := log10(count)]
  if (max.ng > 0)   setkey(unigrams,probability) ;
  if (max.ng > 1)   setkey(bigrams,w)
  if (max.ng > 2)   setkey(trigrams,v,w)
  if (max.ng > 3)   setkey(quadrigrams,u,v,w)
  
  ngram.time<<-timetaken(started.at)
  
  cat("Finished ",n,"samples in ",timetaken(started.at),"\n") 
  
  
  save.ngrams()
  
  
  
  
  
}

accuracy<-function(tests = 20, num.words = 500,num.sample, model = "Interpolate", params){
  
  path<-paths$test.path
  
  phrases<-""
  actual_words<-""
  pred_words<-""
  all.samp.accs<-NULL
  all.samp.times<-NULL
  set.seed(8992066)
  
  print(paste("Parameters of lambda1 =", params$l1,"lambda2 = ",params$l2,"lambda3 =",params$l3,"lambda4 = ",params$l4 ))
  
  for (i in round(sample(1:num.sample,size = tests,replace = F))) {
    
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
    
    
    test.table[1:num.words,prediction := lapply ( paste(u,v,w),function(x) { x<-tolower(x); x<-gsub("'","",x); phrase(x,1,model, params)})]
    
    
    
    acc_time<-timetaken(started.at)
    
    time.per.sample<-round(as.numeric(as.difftime(acc_time,  format="%H:%M:%S",units = "secs"))/num.words,2)
    
    if(is.na(time.per.sample)) time.per.sample<-round(as.numeric(as.difftime(acc_time,  format="%S",units = "secs"))/num.words,2)
    
    acc<-round(test.table[,sum(x == prediction,rm.na =T)/num.words]*100,1)
    
    print (paste(acc,"% accuracy in",num.words,"word sample in", time.per.sample,"seconds per prediction" ))
    all.samp.accs<-c(all.samp.accs,acc)
    all.samp.times <- c(all.samp.times,time.per.sample)
  }
  
  #    Correct <- (actual_words==pred_words)
  #    acc_test_results<<-cbind(phrases,actual_words,pred_words, Correct)
  
  list( samples = tests ,words = num.words, accuracy = mean(all.samp.accs),accuracysd = round(sd(all.samp.accs),2), time.per.prediction = mean(all.samp.times) )
}
