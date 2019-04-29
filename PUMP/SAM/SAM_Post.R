# Post
library("plyr")
setwd("~/Desktop/NRL/PUMP/PUMP")

# PUMP05 Error Code / PUMP06 no Post / PUMP11 no post
filelist <- c("PUMP01/SAM/Post/PUMP01Post7Day",
              "PUMP02/SAM/Post/PUMP02Post7Day",
              "PUMP03/SAM/Post/PUMP03Post7Day",
              "PUMP04/SAM/Post/PUMP04Post7Day",
              "PUMP07/SAM/Post/PUMP07Post7Day",
              "PUMP08/SAM/Post/PUMP08Post7Day",
              "PUMP09/SAM/Post/PUMP09Post7Day",
              "PUMP10/SAM/Post/PUMP10Post7Day")

idlist <- c()
d <- data.frame()
for (file in filelist){
  id <- substr(file, 1, 6)
  idlist <- c(idlist, id)
  
  df <- read.csv(file, stringsAsFactors = F, skip = 27)
  date <- unlist(strsplit(df[2,], "\t"))[-1]
  hour <- unlist(strsplit(df[1,], "\t"))[-1]
  stepcount <- unlist(strsplit(df[3,], "\t"))[-1]
  
  p <- c()
  for (i in 1:length(date)){
    hour[i] <- unlist(strsplit(hour[i], "[\\(\\)]"))[2]
    p <- cbind(p, date[i],hour[i],stepcount[i])
  }
  d <- rbind.fill(d, data.frame(p))
}
rownames(d) <- idlist

write.csv(d, "SAM/SAM_Post.csv")