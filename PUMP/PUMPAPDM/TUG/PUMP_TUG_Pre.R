# PUMP TUG - Pre
# PUMP03/PUMP09/PUMP11 - No TUG
setwd("/Users/iris/Desktop/NRL/PUMP/PUMP/PUMPAPDM/TUG")

PUMPpath <- "/Users/iris/Desktop/NRL/PUMP/PUMP"
PUMPfiles <- list.files(PUMPpath)[-12]
PUMPfiles <- PUMPfiles[-c(3,9,11)]
for (x in PUMPfiles){
  id <- x
  path <- paste0(PUMPpath,"/",x,"/APDMPre")
  files <- list.files(path)
  # filepath
  fileidx <- grep("TUG",files) 
  filepath <- paste0(path,"/",files[fileidx])
  f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
  # check condition
  condition <- read.csv(filepath, stringsAsFactors = F)
  print(condition[4,2])
  
  if (x == "PUMP01"){
    df <- data.frame(matrix(ncol = 15, nrow = 0))
    colnames(df) <- f$Measure
    df[1,] <- f$Mean
    rownames(df) <- id
  }
  
  else{
    df <- rbind(df, f$Mean)
    rownames(df)[nrow(df)] <- id
  }
}

write.csv(df, "PUMP_TUG_Pre.csv")