##REquired libraries
library("tm",         lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
options( java.parameters = "-Xmx4g" )
library("RWeka",      lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("data.table", lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("stringi",    lib.loc = "~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("plyr",       lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")

setwd("~/R/Capstone")


### FILE INFO CALCUALTIONS
datasize <- function(file) {
  
  size <- object.size(file)
  
  nlines <- length(file)
  
  nwords <- sum(stri_count_words(file))
  
  data.frame ( as.numeric(size/2^10), nlines, nwords )
  
  
}





# function checkes to see if original source files are loaded and loads them if required

load.data<-function(){
 
setwd("~/R/Capstone")
  if (!exists("allblogs",where = .GlobalEnv
              )) {
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
  
  a<-cbind(c("en_US.blogs.txt","en_US.news.txt","en_US.twitter.txt"), rbind( datasize(allblogs),datasize(allnews),datasize(alltwitter)))
  


  }

  
  ##function to generate n samples of size% of the entire dataset
  
corpSample<-function(n,size)  {

  #if needed, load the data into the environment
  
  started.at = proc.time()
  
  load.data()
  
  setwd("~/R/Capstone")

  if(!dir.exists("Sample Data")) dir.create("Sample Data")
  
  
  size      <- size/100  # what proportion of the file should the sample represent

  blogsize    = round(size * length(allblogs))
  newssize    = round(size * length(allnews))
  twittersize = round(size * length(alltwitter))


  set.seed(12051105)
  
  
    
    blogs.inds      <-   matrix(sample(x  = length(allblogs), 
                                 size    = n * blogsize, 
                                replace = FALSE), nrow = n , ncol = blogsize)
    
    news.inds       <-   matrix(sample(x  = length(allnews), 
                                       size    = n * newssize, 
                                       replace = FALSE), nrow = n , ncol = newssize)
    twitter.inds    <-   matrix(sample(x  = length(alltwitter), 
                                       size    = n * twittersize, 
                                       replace = FALSE), nrow = n , ncol = twittersize)
    for (i in 1:n){
    tr.samp  = 
            
            paste(paste(allblogs[blogs.inds[i,]],collapse = " "),
                        paste(allnews [news.inds[i,]],collapse = " "),
                        paste(alltwitter [twitter.inds[i,]],collapse = " "), collapse = " ")
    
    
    tr.samp<-clean(VCorpus(VectorSource(tr.samp)),FALSE)
    
    file.name<-paste0("Sample Data/trsamp_",i,"_",size*100,".RDS")
    
    saveRDS(tr.samp, file = file.name)
    
    print(paste0("Sample", i, "saved"))
    
    
    }

    

# tr.samp <- list(n = n, size = size, sample = tr.samp)
# 
# 
# file.name<-paste0("Sample Data/trsamp_",n,"_",size,".RDS")
# 
# saveRDS(tr.samp, file = file.name)
  print(paste("Generated, Cleaned and Saved ",n," Sample Corpuses, each representing ", as.character(size*100)," percent of the full text in ",timetaken(started.at)))
  
}



# This function removes cleans the data stream to clean the data streams.  Intitially we wil remove 
# Remove any whitespace beyond a single space between words  

clean<-function(x,stopw){
  
#   con<-file("~/R/Capstone/Results/clean_text.txt","wt")
#   writeLines("\nRAW TEXT", con)
#   writeLines(substring(x[[1]]$content,1,600),con)
  
  
  # Remove any words appearing on Google's list of profane words  
  conf<-file("~/R/Capstone/Required Data/dirty.txt",'r')
  profanity<-readLines(conf)
  close(conf)
  x<-tm_map(x,removeWords,profanity)
  
    #open file to send text samples to
  replacechars<-content_transformer(function(x,pattern,new) gsub(pattern, new, x))
  
  #convert to Lower
  x<-tm_map(x,content_transformer(tolower))
  
  #remove stopwords
  if (stopw) {
    x<-tm_map(x, removeWords, stopwords("en"))
    writeLines("\nRemove stop words", con)
    writeLines(substring(x[[1]]$content,1,600),con)}
  
  #replace all sentence ending chars with newline
  x<-tm_map(x, replacechars, '[.?!]+[ ]',              "\n") 
  writeLines("Replace sentence endings with newlines", con)
  writeLines(substring(x[[1]]$content,1,600),con)
  
  #remove apostrohes from contractions 
  x<-tm_map(x, replacechars, '[\'\`]',      "" )  
  writeLines("\nRemove apostrohes", con)
  writeLines(substring(x[[1]]$content,1,600),con)
  
  #   x<-tm_map(x, replacechars, '[@][a-zA-Z]+',"\n")  #remove twitter names
  #   x<-tm_map(x, replacechars, '[#][a-zA-Z]+',"\n")  #remove twitter hashtags
  
  #All other stop characters and numerics replace with a period
  x<-tm_map(x, replacechars, '[0-9()\"“”:;,]', ".") 
  writeLines("\nRemove other stop chararcters", con)
  writeLines(substring(x[[1]]$content,1,600),con)
  
  #remove all other unknown chars 
  x<-tm_map(x, replacechars, '[^a-z. \n]',         "")  
  writeLines("\nRemove other unknown chararcters", con)
  writeLines(substring(x[[1]]$content,1,600),con)
  
  #Remove single letters that are not valid single letters 
  x<-tm_map(x, replacechars, '[ ][^AaIi\n][ ]',       ".") 
  writeLines("\nRemove invalid single chars", con)
  writeLines(substring(x[[1]]$content,1,600),con)
  
  #remove extra whitespace 
  x<-tm_map(x, replacechars, '[ ][ ]+', " ") 
  writeLines("\nRemove additional whitespace", con)
  writeLines(substring(x[[1]]$content,1,600),con)
  close(con)
  return(x)
  
}


# Tokenizer control single word tokens
Tokenizer_1 <- function(x) NGramTokenizer(x, 
                                          Weka_control(min = 1, 
                                                       max = 1,
                                                       delimiters =" .\n"))


# Tokenizer control single word tokens
Tokenizer_2 <- function(x) NGramTokenizer(x, 
                                          Weka_control(min = 1, 
                                                       max = 2,
                                                       delimiters =" .\n"))

#Tokenizer control function for 2grams, 3grams
Tokenizer_3 <- function(x) NGramTokenizer(x, 
                                          Weka_control(min = 1, 
                                                       max = 3,
                                                       delimiters =" .\n"))

#Tokenizer control function for 2grams, 3grams
Tokenizer_4 <- function(x) NGramTokenizer(x, 
                                          Weka_control(min = 4, 
                                                       max = 4,
                                                       delimiters =" .\n"))

#Returns ngram count for each ngram
ngramcoverage<-function(tdm) {
  
  tdm<-as.matrix(tdm)
  
  word.freq <- data.table(wordi = row.names(tdm), count = tdm[,1], sample = i) 
    
  #word.freq [, wordcount := stri_count_words(ngram)]
  
  word.freq
}

###################################################  MAIN PROCESSING STARTS HERE
#################################################################################


started.at = proc.time()
  
  
  ngramfreq<-data.table(list(list()))
  
  for (i in 1:n) {
    
    file.name<-paste0("~/R/Capstone/Sample Data/trsamp_",i,"_",as.character(size),".RDS")
    
    print(paste0("Reading ", file.name))
    
    tr.samp<-readRDS(file.name)  # Read in hte corpus of prepared samples
    
    print(paste("Processing Sample", i, " of ",size,"percent at", Sys.time())) 
    
    tdm <- TermDocumentMatrix(tr.samp, control = list(wordLengths = c(1,Inf),tokenize = Tokenizer_4 )) ; rm(tr.samp)
    
    ngramfreq<-rbind(ngramfreq,ngramcoverage(tdm),fill=TRUE) 
    
    rm(tdm)
    
  }
  
  
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
  
  quadgrams <- ngramfreq[, .(count = sum(count)) , by = .(wordi)][, c("W1", "W2","W3","W4") := tstrsplit(wordi, " ", fixed = TRUE)][,W1W2W3:=paste0(W1," ",W2," ",W3)]
  
  setkey(quadgrams,W1W2W3)
  
  # quadgrams[,p4_w3w2w1 := log10(count/ trigrams[quadgrams$W1W2W3, count])]
  
  
  
 cat("Finished ",n,"samples in ",timetaken(started.at),"\n") 
  
 
 phrase <-  function(x) quadgrams [as.character(x), ][order(-count),wordi]
 
 
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
