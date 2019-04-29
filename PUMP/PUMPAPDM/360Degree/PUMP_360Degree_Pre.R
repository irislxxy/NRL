# PUMP 360 Degree Turn - Pre
# PUMP11 - No Pre
setwd("/Users/iris/Desktop/NRL/PUMP/PUMP/PUMPAPDM/360Degree")

PUMPpath <- "/Users/iris/Desktop/NRL/PUMP/PUMP"
PUMPfiles <- list.files(PUMPpath)[-12]
PUMPfiles <- PUMPfiles[-11]
for (x in PUMPfiles){
  id <- x
  path <- paste0(PUMPpath,"/",x,"/APDMPre")
  files <- list.files(path)
  # filepath
  fileidx <- grep("360",files) 
  filepath <- paste0(path,"/",files[fileidx])
  f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
  # check condition
  condition <- read.csv(filepath, stringsAsFactors = F)
  print(condition[4,2])
  
  if (x == "PUMP01"){
    df <- data.frame(matrix(ncol = 6, nrow = 0))
    colnames(df) <- f$Measure
    df[1,] <- f$Mean
    rownames(df) <- id
  }
  
  else{
    df <- rbind(df, f$Mean)
    rownames(df)[nrow(df)] <- id
  }
}

write.csv(df, "PUMP_360Degree_Pre.csv")