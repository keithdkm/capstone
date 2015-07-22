library("tm",    lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")
library("RWeka", lib.loc="~/R/Capstone/packrat/lib/x86_64-w64-mingw32/3.2.1")


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
blogsize = n* round(size* length(allblogs))
newssize = n* round(size* length(allnews))
twittersize = n* round(size* length(alltwitter))

blogs.inds      <-   sample(x      = length(allblogs), 
                            size    = blogsize, 
                            replace = FALSE)
news.inds       <-   sample(x      = length(allnews), 
                            size    = newssize,
                            replace = FALSE)
twitter.inds    <-   sample(x      = length(alltwitter), 
                            size    = twittersize,
                            replace = FALSE)
tr.blogs = ""
tr.news = ""
tr.twitter = ""

for (samp in 1:n) {
  
#     tr.blogs[samp]<-""
#    


    tr.blogs[samp]   = paste(allblogs[blogs.inds[((samp-1)*blogsize/n + 1) :        ((samp-1)*blogsize/n + blogsize/n)]],collapse = " ")
    tr.news[samp]    = paste(allnews [news.inds [((samp-1)*newssize/n + 1) :        ((samp-1)*newssize/n + newssize/n)]],collapse = " ")
    tr.twitter[samp] = paste(alltwitter [twitter.inds[((samp-1)*twittersize/n + 1) :   ((samp-1)*blogsize/n + twittersize/n)]],collapse = " ")
}


corp.blogs <- VCorpus(VectorSource(tr.blogs))
corp.news <- VCorpus(VectorSource(tr.news))
corp.twitter <- VCorpus(VectorSource(tr.twitter))

meta(corp.blogs, tag = "origin") <- "blog"
meta(corp.news, tag = "origin") <- "news"
meta(corp.twitter, tag = "origin") <- "twitter"

tr.samp <-c(corp.blogs,corp.news,corp.twitter)



saveRDS(tr.samp, file = "Sample Data/trsamp.RDS")
# saveRDS(tr.news, file = "Sample Data/newssamp.RDS")
# saveRDS(tr.twitter, file = "Sample Data/twittersamp.RDS")



}