# Therapy
library("plyr")
setwd("~/Desktop/NRL/PUMP/PUMP")

# PUMP03 2 Therapy / PUMP08 2 Therapy
filelist <- c("PUMP01/SAM/Therapy/PUMP01Therapy",
              "PUMP02/SAM/Therapy/PUMP02Therapy",
              "PUMP03/SAM/Therapy/PUMP03Therapy1",
              "PUMP04/SAM/Therapy/PUMP04Therapy",
              "PUMP05/SAM/Therapy/PUMP05Therapy",
              "PUMP06/SAM/Therapy/PUMP06Therapy",
              "PUMP07/SAM/Therapy/PUMP07Therapy",
              "PUMP08/SAM/Therapy/PUMP08Therapy1",
              "PUMP09/SAM/Therapy/PUMP09Therapy",
              "PUMP10/SAM/Therapy/PUMP10Therapy",
              "PUMP11/SAM/Therapy/PUMP11Therapy")

idlist <- c()
d <- data.frame()
for (file in filelist){
  id <- substr(file, 1, 6)
  idlist <- c(idlist, id)
  
  df <- read.csv(file, stringsAsFactors = F, skip = 27)
  date <- unlist(strsplit(df[2,], "\t"))[-1]
  hour <- unlist(strsplit(df[1,], "\t"))[-1]
  stepcount <- unlist(strsplit(df[3,], "\t"))[-1]
  
  if (id == "PUMP03"){
    df2 <- read.csv("PUMP03/SAM/Therapy/PUMP03Therapy2", stringsAsFactors = F, skip = 27)
    date2 <- unlist(strsplit(df2[2,], "\t"))[-1]
    hour2 <- unlist(strsplit(df2[1,], "\t"))[-1]
    stepcount2 <- unlist(strsplit(df2[3,], "\t"))[-1]
  }
  
  else if (id == "PUMP08"){
    df2 <- read.csv("PUMP08/SAM/Therapy/PUMP08Therapy2", stringsAsFactors = F, skip = 27)
    date2 <- unlist(strsplit(df2[2,], "\t"))[-1]
    hour2 <- unlist(strsplit(df2[1,], "\t"))[-1]
    stepcount2 <- unlist(strsplit(df2[3,], "\t"))[-1]
  }
  
  p <- c()
  for (i in 1:length(date)){
    hour[i] <- unlist(strsplit(hour[i], "[\\(\\)]"))[2]
    p <- cbind(p, date[i],hour[i],stepcount[i])
  }
    
  if (id == "PUMP03" | id == "PUMP08"){
    for (i in 1:length(date2)){
      hour2[i] <- unlist(strsplit(hour2[i], "[\\(\\)]"))[2]
      p <- cbind(p, date2[i],hour2[i],stepcount2[i])
    }
  }
  
  d <- rbind.fill(d, data.frame(p))
}
rownames(d) <- idlist

write.csv(d, "SAM/SAM_Therapy.csv")