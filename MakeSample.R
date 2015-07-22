corpSample<-function(n,size)
  #function  n samples of size% sample of the target files
{setwd("~/R/Capstone")
  allblogs<-readLines("Initial Dataset/final/en_US/en_US.blogs.txt",
                      n       = -1,
                      skipNul = TRUE, 
                      warn    = FALSE,
                      encoding= "UTF-8")
  allnews<-readLines("Initial Dataset/final/en_US/en_US.news.txt", 
                     n       = -1,
                     skipNul = TRUE, 
                     warn    = FALSE ,
                     encoding= "UTF-8")
  alltwitter<-readLines("Initial Dataset/final/en_US/en_US.twitter.txt",
                        n       = -1,
                        skipNul = TRUE, 
                        warn    = FALSE,
                        encoding= "UTF-8")


set.seed(11051205)
print(paste("Producing ",n," Sample(s), each representing ", as.character(size)," percent of the full text"))
size      <- size/100  # what proportion of the file should the sample represent


blogs.inds      <-   sample(x      = length(allblogs), 
                            size    = round(n* size* length(allblogs)), 
                            replace = FALSE)
news.inds       <-   sample(x      = length(allnews), 
                            size    = round(n* size* length(allnews)),
                            replace = FALSE)
twitter.inds    <-   sample(x      = length(alltwitter), 
                            size    = round(n* size* length(alltwitter)),
                            replace = FALSE)
for (samp in 0:n-1) {
  
start = samp*size+1;
end   = start + size - 
tr.blogs[samp]   <- paste(allblogs[blogs.inds[start:end]],collapse = " ")
tr.news[samp]    <- paste(allnews [news.inds[start:end]],collapse = " ")
tr.twitter[samp] <- paste(alltwitter [twitter.inds[start:end]],collapse = " ")}

corp.blogs <- VCorpus(VectorSource(tr.blogs))
corp.news <- VCorpus(VectorSource(tr.news))
corp.twitter <- VCorpus(VectorSource(tr.twitter))

tr.samp <-c(corp.blogs,corp.news,corp.twitter)


saveRDS(tr.samp, file = "Sample Data/trsamp.RDS")
# saveRDS(tr.news, file = "Sample Data/newssamp.RDS")
# saveRDS(tr.twitter, file = "Sample Data/twittersamp.RDS")



}