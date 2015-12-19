##Required libraries


library("tm",         lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
options( java.parameters = "-Xmx4g" )
library("RWeka",      lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("data.table", lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("stringi",    lib.loc = "~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("plyr",       lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("git2r",       lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")

setwd("~/R/Capstone")

load.data<-function(){
  ### FILE INFO CALCUALTIONS
  datasize <- function(file) {
    
    size <- object.size(file)
    
    nlines <- length(file)
    
    nwords <- sum(stri_count_words(file))
    
    data.frame ( as.numeric(size/2^10), nlines, nwords )
    
    
  }
  setwd("~/R/Capstone")
  if (!exists("allblogs")) {
    allblogs<<- iconv(readLines("Initial Dataset/final/en_US/en_US.blogs.txt",
                                n       = -1,
                                skipNul = TRUE, 
                                warn    = FALSE,
                                encoding= "UTF-8"))
    print("Blog data loaded")
    # print(unlist(datasize(allblogs))) 
  }
  
  
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
  
  #raw.file.info<<-cbind(c("en_US.blogs.txt","en_US.news.txt","en_US.twitter.txt"), rbind( datasize(allblogs),datasize(allnews),datasize(alltwitter)))
  
  
  
}
## Generate n samples of size% of the entire dataset
corpSample<-function(n,size)  {

  
  # Checks to see if original source files are loaded and loads them if required
  
 
   
  # Cleans the data stream to clean the data streams.  Intitially we wil remove 
  # Remove any whitespace beyond a single space between words 
  clean<-function(x,stopw){
    
 
    tagwords<-content_transformer(function(x,pattern, replacement) {gsub(pattern,
                                                                         replacement,
                                                                         x, 
                                                                         ignore.case = TRUE) })
    
    if(!dir.exists("Results")) dir.create("Results")
    
    
    con<-file("~/R/Capstone/Results/clean_text.txt","wt")
    writeLines("\nRAW TEXT", con)
    writeLines(substring(x[[1]]$content,1,4000),con)
 
    replacechars<-content_transformer(function(x,pattern,new) gsub(pattern, 
                                                                   new, 
                                                                   x))

    #replace <> so that I can use the <> characters for tagging
    x<-tm_map(x, replacechars, '[<>]+', ".") 
    writeLines("\nRemove other <> characters\n", con)
    writeLines(substring(x[[1]]$content,1,4000),con)
    
    
    
    #convert to Lower
    x<-tm_map(x,content_transformer(tolower))
    
    
    # Tag any words appearing on Google's list of profane words  
    conf<-file("~/R/Capstone/Required Data/dirty.txt",'r')
    profanity<-paste0("\\b",paste0(readLines(conf),collapse = "\\b|\\b"),"\\b")
    #profanity<-readLines(conf)
    close(conf)
    x<-tm_map(x,tagwords, pattern = profanity,replacement = " <P> ", ignore.case = TRUE)
    # x<-tm_map(x,removeWords,profanity)
    writeLines("\nReplace profanity with a <p> tag\n", con)
    writeLines(substring(x[[1]]$content,1,4000),con)
    

# 
    
   
    #remove stopwords
    if (stopw) {
      x<-tm_map(x, removeWords, stopwords("en"))
      writeLines("\nRemove stop words\n", con)
      writeLines(substring(x[[1]]$content,1,4000),con)}
    
    #replace all sentence ending chars with newline
    x<-tm_map(x, replacechars, '[.?!]+ ',              " <e>\n<s> ") 
    writeLines("\nReplace sentence endings with newlines\n", con)
    writeLines(substring(x[[1]]$content,1,4000),con)
    
    #remove apostrohes from contractions 
    x<-tm_map(x, replacechars, '[\'\`]',      "" )  
    writeLines("\nRemove apostrohes", con)
    writeLines(substring(x[[1]]$content,1,4000),con)
    
    #   x<-tm_map(x, replacechars, '[@][a-zA-Z]+',"\n")  #remove twitter names
    #   x<-tm_map(x, replacechars, '[#][a-zA-Z]+',"\n")  #remove twitter hashtags
    
    #All other stop characters and numerics replace with a period to force tokenizer 
    x<-tm_map(x, replacechars, '[0-9()\"“”\':;,]', ".") 
    writeLines("\nRemove other stop characters\n", con)
    writeLines(substring(x[[1]]$content,1,4000),con)
    
    #remove all other unknown chars 
    x<-tm_map(x, replacechars, '[^a-zA-Z. \n<>-]',         "")  
    writeLines("\nRemove other unknown characters\n", con)
    writeLines(substring(x[[1]]$content,1,4000),con)
    
    #Remove single letters that are not valid single letters 
    x<-tm_map(x, replacechars, '[ ][^AaIi\n][ ]',       ".") 
    writeLines("\nRemove invalid single characters\n", con)
    writeLines(substring(x[[1]]$content,1,4000),con)
    
    #remove extra whitespace 
    x<-tm_map(x, replacechars, '[ ][ ]+', " ") 
    writeLines("\nRemove additional whitespace characters\n", con)
    writeLines(substring(x[[1]]$content,1,4000),con)
    close(con)
    return(x)
    
  }
  
  

  
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
  
  for (i in 1:n){

    tr.samp   = paste(paste(allblogs[blogs.inds[i,]],collapse = " "),
                      paste(allnews [news.inds[i,]],collapse = " "),
                      paste(alltwitter [twitter.inds[i,]],collapse = " "), collapse = " ")
    
    test.samp = paste(paste(allblogs[blogs.inds[n+i,]],collapse = " "),
                      paste(allnews [news.inds[n+i,]],collapse = " "),
                      paste(alltwitter [twitter.inds[n+i,]],collapse = " "), collapse = " ")
    
    tr.samp   <- clean(VCorpus(VectorSource(tr.samp)),FALSE)
    
    test.samp <- clean(VCorpus(VectorSource(test.samp)),FALSE)
    
    test.Corpus <<- c(test.Corpus,test.samp)   ########experimental
    
    file.name.tr<-paste0("Sample Data/trsamp_",i,"_",size*100,".RDS")
    
    file.name.test<-paste0("Test Data/testsamp_",i,"_",size*100,".RDS")
    
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

  
}

##Divides table of ngrams into separate tables and calculates the frequency
make.ngrams<-function(min.ng,max.ng,n,size){

   #Returns ngram count for each ngram
  extr_ngram_counts<-function(tdm) {
    
    tdm<-as.matrix(tdm)
    
    word.freq <- data.table(wordi = row.names(tdm), count = tdm[,1], sample = i) 

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
    
    file.name<-paste0("~/R/Capstone/Sample Data/trsamp_",i,"_",as.character(size),".RDS")
    
    print(paste0("Reading ", file.name))
    
    tr.samp<-readRDS(file.name)  # Read in hte corpus of prepared samples
    
    print(paste("Processing Sample", i, " of ",size,"percent at", Sys.time())) 
    
    tdm <- TermDocumentMatrix(tr.samp, control = list(wordLengths = c(1,Inf),
                                                      tokenize    = Tokenizer)) ; rm(tr.samp)
    
    ngramfreq<-rbind(ngramfreq,extr_ngram_counts(tdm),fill=TRUE) ; rm(tdm)
    
  }

  ngramfreq[, numwords := stri_count_words(wordi) ]

  setkey(ngramfreq,numwords,wordi)
  
  # UNIGRAMS
  
  if (max.ng>0) unigrams<<- ngramfreq[.(1), .(count = sum(count)) , by = .(wordi)][,probability := -log2(count/sum(count))]
  
                setkey(unigrams,wordi)
  
  #BIGRAMS
  
  if (max.ng>1) {bigrams <<- ngramfreq[.(2), .(count = sum(count)) , by = .(wordi)][, c("W1", "W2") := tstrsplit(wordi, " ", fixed = TRUE)]
  
                  setkey(bigrams,W1)
                  
                  bigrams[,pw2_w1 := -log2(count/unigrams[bigrams$W1,count])]}
    
  #TRIGRAMS
  
  if (max.ng > 2) {trigrams <<- ngramfreq[.(3), .(count = sum(count)) , by = .(wordi)][, c("W1", "W2","W3") := tstrsplit(wordi, " ", fixed = TRUE)][,W1W2:=paste0(W1," ",W2)]
  
                    setkey(trigrams,W1W2)
                    
                    trigrams[, pw3_w2w1 := -log2(count/ bigrams[trigrams$W1W2, count])]}
                

  #QUADRIGRAMS
                
  if (max.ng > 3) {quadrigrams <<- ngramfreq[.(4), .(count = sum(count)) , by = .(wordi)][, c("W1", "W2","W3","W4") := tstrsplit(wordi, " ", fixed = TRUE)][,W1W2W3:=paste0(W1," ",W2," ",W3)]
  
                    setkey(quadrigrams,W1W2W3)
                    
                    quadrigrams[,pw4_w3w2w1 := -log2(count/ trigrams[quadrigrams$W1W2W3, count])]}
                  
                    # quadrigrams[,p4_w3w2w1 := log10(count)]
                    
                    ngram.time<<-timetaken(started.at)
  
  cat("Finished ",n,"samples in ",timetaken(started.at),"\n") 
  
    }
 

# Predicts wnext word from a phrase x 
phrase <-  function(x) {
  
  
        x<-tolower(x)
         
        # phrase <- ""
        phrase.length <- stri_count_words(x) 
        
        if (phrase.length == 3) 
          phrase<-quadrigrams[x, .(wordi,pw4_w3w2w1)][order(pw4_w3w2w1)[1:3], wordi]
        
        if (is.na(phrase[1])) {phrase <- paste(stri_extract_all_words(x)[[1]][2:3], collapse = " ") ; phrase.length<-phrase.length-1; print("Backing off to trigrams")}
        
        if ( phrase.length  == 2) 
          phrase<-trigrams[x , .(wordi,pw3_w2w1)][order(pw3_w2w1)[1:3], wordi]

        if (is.na(phrase[1])) {phrase <- paste(stri_extract_all_words(x)[[1]][2], collapse = " ") ; phrase.length<-phrase.length-1; print("Backing off to bigrams")}
        
        if ( phrase.length  == 1) 
         phrase<-bigrams[x, .(wordi,pw2_w1)][order(pw2_w1)[1:3],wordi]
       
        if (is.na(phrase[1])) { phrase.length<-phrase.length-1; print("Backing off to unigrams")}
        
        if (phrase.length == 0)
         phrase<-unigrams[order(probability),wordi ][1:3]

   
   phrase}


#Calculates the perplexity of model x,against the cleaned corpus y.  X shoudl indicate the maximum length of trigrams to use
perplexity <-function(y) {
  
  
  a<-unlist(stri_split_regex(y[[1]]$content,"<e>\n<s>"))
  
  c<-data.frame(t(sapply(a[1:200], function(x){ word.count <- stri_count_words(x);
                            
                            b<-stri_extract_all_words(x)[[1]][1:2]
                            
                            phr.prob <- sum(trigrams[paste0("<e> <s> ",b[1]),pw3_w2w1],
                                       trigrams[paste0("<s> ",b[1]," ",b[2]),pw3_w2w1],na.rm = TRUE)
                            
                            if(word.count>2) {b <- NGramTokenizer(x,     
                                               Weka_control(min        = 3, 
                                                            max        = 3,
                                                            delimiters = ". "));
                            
                            
                            phr.prob <- sum(phr.prob, sum(sapply(b, 
                                                   function(z) {
                                                     trigrams[z,pw3_w2w1]}),na.rm=TRUE),na.rm=TRUE)
                            
                            }
                                            
                            
                            data.frame(sentence = x,
                                        M = word.count,
                                        sent.prob = phr.prob)})))

   
  sum(unlist(c$sent.prob))/sum(unlist(c$M))
  
  }




main<-function(resamp,num.sample, sz.sample, ng.size) {
  
  ## Summary Results are stored in the masterlsit file
  # rm(GlobalEnv::unigrams);rm(trigrams);rm(bigrams);rm(results)
              
  Exec.time<-Sys.time()

  if (resamp) corpSample(num.sample,sz.sample)
  
  make.ngrams(min = 1, max = ng.size, num.sample, sz.sample)
  
  repo<-repository("~/R/Capstone/")
  
  new_results<-list(Time        = strftime(Exec.time, "%c"),
                    Commit      = substr(branch_target(head(repo)),1,8),
                    N           = paste(num.sample,"samples"), 
                    Size        = paste0(sz.sample,"%"),
                    Model.Size  = ng.size,
                    Load.Time   = load.time,
                    Sample.Time = samp.time,
                    Ngram.Time  = ngram.time,
                    N.unigrams  = unigrams[,.N],
                    U.size      = paste0(round(object.size(unigrams)/10^6,2),"Mb"),
                    N.bigrams   = bigrams[,.N],
                    bi.size     = paste0(round(object.size(bigrams)/10^6,2),"Mb"),
                    N.trigrams  = ifelse(exists("trigrams"),trigrams[,.N],0),
                    tri.size    = ifelse(exists("trigrams"),paste0(round(object.size(trigrams)/10^6,2),"Mb"), "0Mb"),
                    N.quadrigrams  = ifelse(exists("quadrigrams"),quadrigrams[,.N],0),
                    quad.size    = ifelse(exists("quadrigrams"),paste0(round(object.size(quadrigrams)/10^6,2),"Mb"), "0Mb"), 
                    Perplexity  =  perplexity(test.Corpus))
  
  
  ifelse (file.exists("~/R/Capstone/Results/masterlist.RDS"),
          x<-readRDS("~/R/Capstone/Results/masterlist.RDS"),
          x<-data.table(NULL))

  results<<- rbind(x,  data.table(t(new_results)))
  
  saveRDS(results,"~/R/Capstone/Results/masterlist.RDS")
  
  
  
  
  }




# 
# test.samp<-VCorpus(VectorSource(""))
# for(a in 1:100){
#   test.samp<-c(test.samp,readRDS(paste0("Test Data/testsamp_",a,"_",0.1,".RDS" )))}
# 
# 
# 
# 
# 
# 







  #
  # bigrams<-ngramfreq[, totalcount := sum(count) , by = .(wordi)][, numwords := stri_count_words(wordi), ][,pwiw1 := log10(totalcount)-log10(unigrams[stri_extract_first_words(ngramfreq$wordi),count])]
  
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


#   ngramfreq[, numwords := stri_count_words(wordi) ]
# 
#   setkey(ngramfreq,numwords,wordi)
#   
#   unigrams<- ngramfreq[numwords == 1, .(count = sum(count)) , by = .(wordi)][,probability := log10(count/sum(count))]
#   
#   setkey(unigrams,wordi)
#   
#   bigrams <- ngramfreq[numwords == 2, .(count = sum(count)) , by = .(wordi)][, c("W1", "W2") := tstrsplit(wordi, " ", fixed = TRUE)]
#   
#   setkey(bigrams,wordi)
#   
#   # bigrams[,pw2_w1 := log10(count/unigrams[stri_extract_first_words(bigrams$wordi),count])]
#   
#   bigrams[,pw2_w1 := log10(count/unigrams[bigrams$W1,count])]
#   
#   trigrams <- ngramfreq[numwords == 3, .(count = sum(count)) , by = .(wordi)][, c("W1", "W2","W3") := tstrsplit(wordi, " ", fixed = TRUE)][,W1W2:=paste0(W1," ",W2)]
#   
#   setkey(trigrams,W1W2)
#   
#   trigrams[, pw3_w2w1 := log10(count/ bigrams[trigrams$W1W2, count])]
#   
# quadgrams <- ngramfreq[numwords == 4, .(count = sum(count)) , by = .(wordi)][, c("W1", "W2","W3","W4") := tstrsplit(wordi, " ", fixed = TRUE)][,W1W2W3:=paste0(W1," ",W2," ",W3)]

