# PUMP Walk - Post
# PUMP06 - No Post
setwd("/Users/iris/Desktop/NRL/PUMP/PUMP/PUMPAPDM/Walk")

PUMPpath <- "/Users/iris/Desktop/NRL/PUMP/PUMP"
PUMPfiles <- list.files(PUMPpath)[-12]
PUMPfiles <- PUMPfiles[-6]
for (x in PUMPfiles){
  id <- x
  path <- paste0(PUMPpath,"/",x,"/APDMPost")
  files <- list.files(path)
  # filepath
  fileidx <- grep("Walk",files) 
  filepath <- paste0(path,"/",files[fileidx])
  f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
  # check condition
  condition <- read.csv(filepath, stringsAsFactors = F)
  print(condition[3,2])
  
  if (x == "PUMP01"){
    df <- data.frame(matrix(ncol = 59, nrow = 0))
    colnames(df) <- f$Measure[6:64]
    df[1,] <- f$Mean[6:64]
    rownames(df) <- id
  }
  
  else{
    df <- rbind(df, f$Mean[6:64])
    rownames(df)[nrow(df)] <- id
  }
}

write.csv(df, "PUMP_Walk_Post.csv")