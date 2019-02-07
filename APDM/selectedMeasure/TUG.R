#TUG Duration
setwd("~/Desktop/NRL/iWear/APDM")

Condition <-  c("TUG Duration",
                "TUG with Cognitive Duration")
df <- data.frame(Condition)

#GHI
GHIpath <- "GHI"
GHIfiles <- list.files(GHIpath)
##fix GH1/GH18 filename manually
for (x in GHIfiles){
  id <- unlist(strsplit(x, "_"))[4]
  df[,id] <- NA
  path <- paste0(GHIpath,"/",x)
  files <- list.files(path)
  for (filename in files){
    ##TUG
    if (grepl("TUG_trial.csv", filename)){
      filepath <- paste0(GHIpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[1,id] <- f$Mean[1]
    }
    ##TUG with Cognitive
    if (grepl("TUGw", filename)){
      filepath <- paste0(GHIpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[2,id] <- f$Mean[1]
    }
  }
}

#TC
TCpath <- "TC"
TCfiles <- list.files(TCpath)
for (x in TCfiles){
  id <- unlist(strsplit(x, "_"))[4]
  df[,id] <- NA
  path <- paste0(TCpath,"/",x)
  files <- list.files(path)
  for (filename in files){
    ##TUG
    if (grepl("TUG_trial.csv", filename)){
      filepath <- paste0(TCpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[1,id] <- f$Mean[1]
    }
    ##TUG with Cognitive
    if (grepl("TUGw", filename)){
      filepath <- paste0(TCpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[2,id] <- f$Mean[1]
    }
  }
}


#WS
WSpath <- "WS"
WSfiles <- list.files(WSpath)
for (x in WSfiles){
  id <- x
  df[,id] <- NA
  filepath <- paste0(WSpath,"/",x,"/TUG_trials.csv")
  f <- read.csv(filepath, stringsAsFactors = F)
  ##TUG
  df[1,id] <- f$Duration..s.[which(f$Condition == "TUG (I-wear)")]
  ##TUG with Cognitive
  df[2,id] <- f$Duration..s.[which(f$Condition == "TUG-COG (i-Wear)")]
}

df <- t(df)
write.table(df, "TUG.csv", sep=",",  col.names=FALSE)