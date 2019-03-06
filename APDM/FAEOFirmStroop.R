# Feet Apart, Eyes Open, Firm Surface with Stroop
setwd("~/Desktop/NRL/APDM")

# GHI
## fix GH19 condition manually - Feet Apart, Eyes Open, Firm Surface
## fix GH20 condition manually - Feet Apart, Eyes Closed, Firm Surface
GHIpath <- "GHI"
GHIfiles <- list.files(GHIpath)
for (x in GHIfiles){
  id <- unlist(strsplit(x, "_"))[4]
  path <- paste0(GHIpath,"/",x)
  files <- list.files(path)
  filepath <- paste0(GHIpath,"/",x,"/",files[4])
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
  fileidx <- grep("FAEOFirmwstroop",files) 
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
    filepath <- paste0(WSpath,"/",x,"/",files[14])
  }
  else{
    filepath <- paste0(WSpath,"/",x,"/",files[13])
  }
  f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
  # check condition
  condition <- read.csv(filepath, stringsAsFactors = F)
  print(condition[3,2])
  
  df <- rbind(df, f$Mean)
  rownames(df)[nrow(df)] <- id
}

write.csv(df, "FAEOFirmStroop.csv")