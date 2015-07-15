corpsample<-function(n,size)
  #function  n samples of size% sample of the target files
{setwd("C:/Users/Keith_2/Documents/R/Capstone")
  allblogs<-readLines("Initial Dataset/final/en_US/en_US.blogs.txt",
                      n       = -1,
                      skipNul = TRUE, 
                      warn    = FALSE,
                      encoding= "UTF-8")
#   allnews<-readLines("Initial Dataset/final/en_US/en_US.news.txt", 
#                      n       = -1,
#                      skipNul = TRUE, 
#                      warn    = FALSE ,
#                      encoding= "UTF-8")
#   alltwitter<-readLines("Initial Dataset/final/en_US/en_US.twitter.txt",
#                         n       = -1,
#                         skipNul = TRUE, 
#                         warn    = FALSE,
#                         encoding= "UTF-8")


set.seed(11051205)
print(paste("Producing ",n," Sample(s), each representing ", as.character(size)," percent of the full text"))
size      <- size/100  # what proportion of the file should the sample represent
# mn.char.blogs <- round(mean(nchar(allblogs)))  #calculate the mean number of characters in each line
# mn.char.news    <- round(mean(nchar(allnews)))
# mn.char.twitter <- round(mean(nchar(alltwitter)))


blogs.inds      <-   sample(x      = length(allblogs), 
                            size    = round(n* size* length(allblogs)), 
                            replace = FALSE)
# news.inds       <-   sample(x      = length(allnews), 
#                             size    = round(n* size* length(allnews)),
#                             replace = FALSE)
# twitter.inds    <-   sample(x      = length(alltwitter), 
#                             size    = round(n* size* length(alltwitter)),
#                             replace = FALSE)

tr.blog   <- paste(allblogs[blogs.inds[c(1,2)]],collapse = ".")
print(allblogs[blogs.inds[1]]);print(allblogs[blogs.inds[2]])
# tr.news    <- paste(allnews [news.inds],collapse = ".")
# tr.twitter <- paste(alltwitter [twitter.inds],collapse = ".")

saveRDS(tr.blog, file = "Sample Data/blogsamp.RDS")
# saveRDS(tr.news, file = "Sample Data/newssamp.RDS")
# saveRDS(tr.twitter, file = "Sample Data/twittersamp.RDS")



}