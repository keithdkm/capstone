library("tm",    lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("RWeka", lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("stringi", lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")


### FILE INFO CALCUALTIONS
datasize <- function(file) {
  
  if (!exists(file)) {
  size <- object.size(file)
  nlines <- length(file)
  nwords <- sum(stri_count_words(file))}
  
  
  a<- list (size = size,linecount = nlines, wordcount = nwords)
  
  
}

corpSample<-function(n,size)
  #function  n samples of size% sample of the target files
{setwd("~/R/Capstone")
  # if (!exists(allblogs) ) 
            allblogs<-readLines("Initial Dataset/final/en_US/en_US.blogs.txt",
                      n       = -1,
                      skipNul = TRUE, 
                      warn    = FALSE,
                      encoding= "UTF-8")
  # if (!exists(allnews) ) 
  allnews<-readLines("Initial Dataset/final/en_US/en_US.news.txt", 
                     n       = -1,
                     skipNul = TRUE, 
                     warn    = FALSE ,
                     encoding= "UTF-8")
  # if (!exists(alltwitter) ) 
  alltwitter<-readLines("Initial Dataset/final/en_US/en_US.twitter.txt",
                        n       = -1,
                        skipNul = TRUE, 
                        warn    = FALSE,
                        encoding= "UTF-8")

sourcesize<- lapply(list(allblogs,allnews,alltwitter), datasize)

  
  

set.seed(11051205)
print(paste("Producing ",n," Sample(s), each representing ", as.character(size)," percent of the full text"))
size      <- size/100  # what proportion of the file should the sample represent

blogsize = round(size* length(allblogs))
newssize = round(size* length(allnews))
twittersize = round(size* length(alltwitter))


tr.samp = ""

for (samp in 1:n) {
  
  blogs.inds      <-   sample(x      = sourcesize[[1]]$linecount, 
                              size    = blogsize, 
                              replace = FALSE)
  news.inds       <-   sample(x      = sourcesize[[2]]$linecount, 
                              size    = newssize,
                              replace = FALSE)
  twitter.inds    <-   sample(x      = sourcesize[[3]]$linecount, 
                              size    = twittersize,
                              replace = FALSE)

  tr.samp[samp]   = paste(paste(allblogs[blogs.inds],collapse = " "),
                          paste(allnews [news.inds],collapse = " "),
                          paste(alltwitter [twitter.inds],collapse = " "), collapse = " ")

}




tr.samp <-VCorpus(VectorSource(tr.samp))



saveRDS(tr.samp, file = "Sample Data/trsamp.RDS")



}