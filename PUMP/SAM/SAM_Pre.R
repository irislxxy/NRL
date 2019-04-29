# Pre
library("plyr")
setwd("~/Desktop/NRL/PUMP/PUMP")

# PUMP11 no Pre
filelist <- c("PUMP01/SAM/Pre/PUMP01Pre7Day",
              "PUMP02/SAM/Pre/PUMP02Pre7Day",
              "PUMP03/SAM/Pre/PUMP03Pre7Day",
              "PUMP04/SAM/Pre/PUMP04Pre7Day",
              "PUMP05/SAM/Pre/PUMP05Pre7Day",
              "PUMP06/SAM/Pre/PUMP06Pre7Day",
              "PUMP07/SAM/Pre/PUMP07Pre7Day",
              "PUMP08/SAM/Pre/PUMP08Pre7Day",
              "PUMP09/SAM/Pre/PUMP09Pre7Day",
              "PUMP10/SAM/Pre/PUMP10Pre7Day")

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

write.csv(d, "SAM/SAM_Pre.csv")