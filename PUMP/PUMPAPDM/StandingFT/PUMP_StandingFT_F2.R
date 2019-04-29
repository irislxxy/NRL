# PUMP Standing Feet Together - F2
# PUMP06/PUMP07/PUMP11 - No F2
setwd("/Users/iris/Desktop/NRL/PUMP/PUMP/PUMPAPDM/StandingFT")

PUMPpath <- "/Users/iris/Desktop/NRL/PUMP/PUMP"
PUMPfiles <- list.files(PUMPpath)[-12]
PUMPfiles <- PUMPfiles[-c(6,7,11)]
for (x in PUMPfiles){
  id <- x
  path <- paste0(PUMPpath,"/",x,"/APDMFolUp2")
  files <- list.files(path)
  # filepath
  if (x == "PUMP09"){
    filepath <- paste0(path,"/",files[3])
    f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
  }
  else if (x == "PUMP10"){
    filepath <- paste0(path,"/",files[5])
    f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
  }
  else {
    filepath <- paste0(path,"/",files[4])
    f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
  }
  # check condition
  condition <- read.csv(filepath, stringsAsFactors = F)
  print(condition[3,2])
  
  if (x == "PUMP01"){
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

write.csv(df, "PUMP_StandingFT_F2.csv")