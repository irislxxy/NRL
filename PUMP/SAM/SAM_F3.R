# FollowUp3
library("plyr")
setwd("~/Desktop/NRL/PUMP/PUMP")

# PUMP06 no F3 / PUMP07 no F3/ PUMP09 no F3 / PUMP10 no F3/ PUMP11 no F3
filelist <- c("PUMP01/SAM/FollowUp3/PUMP01FollowUp3",
              "PUMP02/SAM/FollowUp3/PUMP02FollowUp3",
              "PUMP03/SAM/FollowUp3/PUMP03FollowUp3",
              "PUMP04/SAM/FollowUp3/PUMP04FollowUp3",
              "PUMP05/SAM/FollowUp3/PUMP05FollowUp3",
              "PUMP08/SAM/FollowUp3/PUMP08FollowUp3")

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

write.csv(d, "SAM/SAM_F3.csv")