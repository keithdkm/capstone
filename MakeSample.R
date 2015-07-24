
### FILE INFO CALCUALTIONS
datasize <- function(file) {
  
  
  size <- object.size(file)
  nlines <- length(file)
  nwords <- sum(stri_count_words(file))
  
  a<- list ( size = size,linecount = nlines, wordcount = nwords)
  
  
}

load.data<-function(){
  # function checkes to see if original source files are loaded and loads them if required
setwd("~/R/Capstone")
  if (!exists("allblogs",where = .GlobalEnv
              )) {
            allblogs<<-readLines("Initial Dataset/final/en_US/en_US.blogs.txt",
                      n       = -1,
                      skipNul = TRUE, 
                      warn    = FALSE,
                      encoding= "UTF-8"); 
  print("Blog data loaded")
  print(unlist(datasize(allblogs))) }
  
  
  if (!exists("allnews")) { 
  allnews<<-readLines("Initial Dataset/final/en_US/en_US.news.txt", 
                     n       = -1,
                     skipNul = TRUE, 
                     warn    = FALSE ,
                     encoding= "UTF-8")
  print("News data loaded")
  print(datasize(allnews)) }
  
  
  if (!exists("alltwitter")) {
  alltwitter<<-readLines("Initial Dataset/final/en_US/en_US.twitter.txt",
                        n       = -1,
                        skipNul = TRUE, 
                        warn    = FALSE,
                        encoding= "UTF-8")

  print("Twitter data loaded")
  print(datasize(alltwitter))}
}

  
  
  
  ##function to generate n samples of size% of the entire dataset
  
corpSample<-function(n,size)  {

set.seed(11051205)
print(paste("Producing ",n," Sample(s), each representing ", as.character(size)," percent of the full text"))
size      <- size/100  # what proportion of the file should the sample represent

blogsize = round(size* length(allblogs))
newssize = round(size* length(allnews))
twittersize = round(size* length(alltwitter))


tr.samp = ""

for (samp in 1:n) {
  
  blogs.inds      <-   sample(x      = length(allblogs), 
                              size    = blogsize, 
                              replace = FALSE)
  news.inds       <-   sample(x      = length(allnews), 
                              size    = newssize,
                              replace = FALSE)
  twitter.inds    <-   sample(x      = length(alltwitter), 
                              size    = twittersize,
                              replace = FALSE)

  tr.samp[samp]   = iconv(paste(paste(allblogs[blogs.inds],collapse = " "),
                          paste(allnews [news.inds],collapse = " "),
                          paste(alltwitter [twitter.inds],collapse = " "), collapse = " "))

}


#tr.samp <-VCorpus(VectorSource(tr.samp))

file.name<-paste0("Sample Data/trsamp_",n,"_",size*1000,".RDS")

saveRDS(tr.samp, file = file.name)



}

clean<-function(x,stopw){
  # This function removes cleans the data stream to clean the data streams.  Intitially we wil remove 
  # Remove any whitespace beyond a single space between words  
  
  # Remove any words appearing on Google's list of profane words  
  con<-file("Required Data/dirty.txt",'r')
  profanity<-readLines(con)
  close(con)
  x<-tm_map(x,removeWords,profanity)
  
  con<-file("clean_text.txt","wt")  #open file to send text samples to
  replacechars<-content_transformer(function(x,pattern,new) gsub(pattern, new, x))
  
  #convert to Lower
  x<-tm_map(x,content_transformer(tolower))
  
  #remove stopwords
  x<-tm_map(x, removeWords, stopwords("en"))
  writeLines("\nRemove stop words", con)
  writeLines(c(x[[1]]$content,x[[2]]$content),con)
  
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
                                          Weka_control(min = 3, 
                                                       max = 3,
                                                       delimiters =" .\n"))