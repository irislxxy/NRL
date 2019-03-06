# 2 Minute Walk
setwd("~/Desktop/NRL/APDM")

# GHI
GHIpath <- "GHI"
GHIfiles <- list.files(GHIpath)
for (x in GHIfiles){
  id <- unlist(strsplit(x, "_"))[4]
  path <- paste0(GHIpath,"/",x)
  files <- list.files(path)
  filepath <- paste0(GHIpath,"/",x,"/",files[1])
  f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
  # check condition
  condition <- read.csv(filepath, stringsAsFactors = F)
  print(condition[3,2])
  
  if (x == "MobilityLab_Subject_Export_GH1_20180728-164439"){
    df <- data.frame(matrix(ncol = 49, nrow = 0))
    colnames(df) <- f$Measure[6:54]
    df[1,] <- f$Mean[6:54]
    rownames(df) <- id
  }
  
  else{
    df <- rbind(df, f$Mean[6:54])
    rownames(df)[nrow(df)] <- id
  }
}

# TC
TCpath <- "TC"
TCfiles <- list.files(TCpath)
for (x in TCfiles){
  id <- unlist(strsplit(x, "_"))[4]
  path <- paste0(TCpath,"/",x)
  files <- list.files(path)
  if (id == "IW10TC"){
    filepath <- paste0(TCpath,"/",x,"/",files[17])
  }
  else if (id == "IW1TC"){
    filepath <- paste0(TCpath,"/",x,"/",files[1])
  }
  else{
    filepath <- paste0(TCpath,"/",x,"/",files[2])
  }
  f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
  # check condition
  condition <- read.csv(filepath, stringsAsFactors = F)
  print(condition[3,2])
  
  df <- rbind(df, f$Mean[6:54])
  rownames(df)[nrow(df)] <- id
}

# WS
WSpath <- "WS"
WSfiles <- list.files(WSpath)
for (x in WSfiles){
  id <- x
  path <- paste0(WSpath,"/",x)
  files <- list.files(path)
  filepath <- paste0(WSpath,"/",x,"/",files[3])
  f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
  # check condition
  condition <- read.csv(filepath, stringsAsFactors = F)
  print(condition[3,2])
  
  df <- rbind(df, f$Mean[6:54])
  rownames(df)[nrow(df)] <- id
}

write.csv(df, "2 Minute Walk.csv")