# Feet Together, Eyes Open, Firm Surface with Stroop
setwd("~/Desktop/NRL/APDM")

# GHI
GHIpath <- "GHI"
GHIfiles <- list.files(GHIpath)
for (x in GHIfiles){
  id <- unlist(strsplit(x, "_"))[4]
  path <- paste0(GHIpath,"/",x)
  files <- list.files(path)
  if (id == "GH1" | id == "GH3") {
    filepath <- paste0(GHIpath,"/",x,"/",files[5])
  }
  else if (id == "GH19" | id == "GH20"){
    fileidx <- grep("FTEOFirmwstroop",files) 
    filepath <- paste0(GHIpath,"/",x,"/",files[fileidx])
  }
  else{
    filepath <- paste0(GHIpath,"/",x,"/",files[6])
  }
  f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
  # check condition
  condition <- read.csv(filepath, stringsAsFactors = F)
  print(condition[3,2])
  
  if (x == "MobilityLab_Subject_Export_GH1_20180728-164439"){
    df <- data.frame(matrix(ncol = 33, nrow = 0))
    colnames(df) <- f$Measure
    df[1,] <- f$Mean
    rownames(df) <- id
  }
  
  else{
    df <- rbind(df, f$Mean)
    rownames(df)[nrow(df)] <- id
  }
}

# TC
## fix IW11TC/IW14TCCO/IW15TC/IW1TC/IW1TCCO/IW5TCCO/IW6TC filename manually
TCpath <- "TC"
TCfiles <- list.files(TCpath)
for (x in TCfiles){
  id <- unlist(strsplit(x, "_"))[4]
  path <- paste0(TCpath,"/",x)
  files <- list.files(path)
  fileidx <- grep("FTEOFirmwstroop",files) 
  filepath <- paste0(TCpath,"/",x,"/",files[fileidx])
  f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
  # check condition
  condition <- read.csv(filepath, stringsAsFactors = F)
  print(condition[3,2])
  
  df <- rbind(df, f$Mean)
  rownames(df)[nrow(df)] <- id
}

# WS
WSpath <- "WS"
WSfiles <- list.files(WSpath)
for (x in WSfiles){
  id <- x
  path <- paste0(WSpath,"/",x)
  files <- list.files(path)
  if (x == "IW8WS") {
    filepath <- paste0(WSpath,"/",x,"/",files[16])
  }
  else{
    filepath <- paste0(WSpath,"/",x,"/",files[15])
  }
  f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
  # check condition
  condition <- read.csv(filepath, stringsAsFactors = F)
  print(condition[3,2])
  
  df <- rbind(df, f$Mean)
  rownames(df)[nrow(df)] <- id
}

write.csv(df, "FTEOFirmStroop.csv")