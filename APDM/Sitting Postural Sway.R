# Sitting Postural Sway
setwd("~/Desktop/NRL/APDM")

path <- "Sitting Postural Sway"
files <- list.files(path)
for (x in files){
  id <- unlist(strsplit(x, "_"))[5]
  filepath <- paste0(path,"/",x,"/Sway_trials.csv")
  f <- read.csv(filepath, stringsAsFactors = F)

  if (id == "GH1"){
    df <- f[,12:44]
    rownames(df) <- id
  }
  
  else{
    df <- rbind(df, f[1,12:44])
    rownames(df)[nrow(df)] <- id
  }
}

write.csv(df, "Sitting Postural Sway.csv")