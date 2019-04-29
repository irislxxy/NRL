# FollowUp2
library("plyr")
setwd("~/Desktop/NRL/PUMP/PUMP")

# PUMP06 no F2 / PUMP07 no F2/ PUMP11 no F2
filelist <- c("PUMP01/SAM/FollowUp2/PUMP01FollowUp2",
              "PUMP02/SAM/FollowUp2/PUMP02FollowUp2",
              "PUMP03/SAM/FollowUp2/PUMP03FollowUp2",
              "PUMP04/SAM/FollowUp2/PUMP04FollowUp2",
              "PUMP05/SAM/FollowUp2/PUMP05FollowUp2",
              "PUMP08/SAM/FollowUp2/PUMP08FollowUp2",
              "PUMP09/SAM/FollowUp2/PUMP09FollowUp2",
              "PUMP10/SAM/FollowUp2/PUMP10FollowUp2")

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

write.csv(d, "SAM/SAM_F2.csv")