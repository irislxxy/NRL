# PUMP Standing Unsupported - Post
# PUMP06 - No Post
setwd("/Users/iris/Desktop/NRL/PUMP/PUMP/PUMPAPDM/StandingUnsupported")

PUMPpath <- "/Users/iris/Desktop/NRL/PUMP/PUMP"
PUMPfiles <- list.files(PUMPpath)[-12]
PUMPfiles <- PUMPfiles[-6]
for (x in PUMPfiles){
  id <- x
  path <- paste0(PUMPpath,"/",x,"/APDMPost")
  files <- list.files(path)
  # filepath
  if (x == "PUMP09"){
    filepath <- paste0(path,"/",files[1])
    f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
  }
  else {
    filepath <- paste0(path,"/",files[2])
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

write.csv(df, "PUMP_StandingUnsupported_Post.csv")