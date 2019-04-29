# PUMP Sit to Stand - F2
# PUMP06 - No F2 / PUMP07 - No F2/ PUMP09 - No Sit to Stand / PUMP11 - No F2
setwd("/Users/iris/Desktop/NRL/PUMP/PUMP/PUMPAPDM/SitToStand")

PUMPpath <- "/Users/iris/Desktop/NRL/PUMP/PUMP"
PUMPfiles <- list.files(PUMPpath)[-12]
PUMPfiles <- PUMPfiles[-c(6,7,9,11)]
for (x in PUMPfiles){
  id <- x
  path <- paste0(PUMPpath,"/",x,"/APDMFolUp2")
  files <- list.files(path)
  fileidx <- grep("Sit to Stand",files) 
  filepath <- paste0(path,"/",files[fileidx])
  f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
  # check condition
  condition <- read.csv(filepath, stringsAsFactors = F)
  print(condition[3,2])
  
  if (x == "PUMP01"){
    df <- data.frame(matrix(ncol = 10, nrow = 0))
    colnames(df) <- f$Measure
    df[1,] <- f$Mean
    rownames(df) <- id
  }
  
  else{
    df <- rbind(df, f$Mean)
    rownames(df)[nrow(df)] <- id
  }
}

write.csv(df, "PUMP_SitToStand_F2.csv")