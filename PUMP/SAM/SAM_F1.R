# FollowUp1
library("plyr")
setwd("~/Desktop/NRL/PUMP/PUMP")

# PUMP06 no F1 / PUMP07 no F1
filelist <- c("PUMP01/SAM/FollowUp1/PUMP01FollowUp1",
              "PUMP02/SAM/FollowUp1/PUMP02FollowUp1",
              "PUMP03/SAM/FollowUp1/PUMP03FollowUp1",
              "PUMP04/SAM/FollowUp1/PUMP04FollowUp1",
              "PUMP05/SAM/FollowUp1/PUMP05FollowUp1",
              "PUMP08/SAM/FollowUp1/PUMP08FollowUp1",
              "PUMP09/SAM/FollowUp1/PUMP09FollowUp1",
              "PUMP10/SAM/FollowUp1/PUMP10FollowUp1",
              "PUMP11/SAM/FollowUp1/PUMP11FollowUp1")

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

write.csv(d, "SAM/SAM_F1.csv")