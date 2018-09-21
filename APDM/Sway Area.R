#Sway Area
setwd("~/Desktop/NRL/iWear/APDM")

Condition <-  c("Feet Apart, Eyes Open, Firm Surface",
                "Feet Apart, Eyes Open, Firm Surface with Stroop",
                "Feet Together, Eyes Open, Firm Surface",
                "Feet Together, Eyes Open, Firm Surface with Stroop",
                "Feet Apart, Eyes Open, Foam Surface",
                "Feet Apart, Eyes Open, Foam Surface with Stroop")
df <- data.frame(Condition)

#GHI
GHIpath <- "GHI"
GHIfiles <- list.files(GHIpath)
##GH1-GH18
##fix GH1 condition manually
for (x in GHIfiles[-c(11,13)]){
  id <- unlist(strsplit(x, "_"))[4]
  path <- paste0(GHIpath,"/",x)
  filepath <- paste0(GHIpath,"/",x,"/Sway_trials.csv")
  f <- read.csv(filepath, stringsAsFactors = F)
  f <- f[,c("Condition", "Postural.Sway...Angles...Sway.Area..degrees.2.")]
  for (i in 1:nrow(f)){
    #remove iWear
    if (substr(f$Condition[i], 1, 5) == "iWear"){
      f$Condition[i] <- substr(f$Condition[i], 9, nchar(f$Condition[i]))
    }
    #remove copy
    if (substr(f$Condition[i], nchar(f$Condition[i])-3, nchar(f$Condition[i])) == "copy"){
      f$Condition[i] <- substr(f$Condition[i], 1, nchar(f$Condition[i])-5)
    }
    #remove stroop name
    if (substr(f$Condition[i], nchar(f$Condition[i])-9, nchar(f$Condition[i])-4) == "Stroop"){
      f$Condition[i] <- substr(f$Condition[i], 1, nchar(f$Condition[i])-4)
    }
    print(f$Condition[i])
  }
  df <- merge(df, f, all.x = T, sort = F)
  colnames(df)[ncol(df)] <- id
}

##GH19/GH20
for (x in GHIfiles[c(11,13)]){
  id <- unlist(strsplit(x, "_"))[4]
  df[,id] <- NA
  path <- paste0(GHIpath,"/",x)
  files <- list.files(path)
  for (filename in files){
    ##FAEOFirm
    if (grepl("FAEOFirm_", filename)){
      filepath <- paste0(GHIpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[1,id] <- f$Mean[29]
    }
    ##FAEOFirmwstroop
    if (grepl("FAEOFirmwstroop", filename)){
      filepath <- paste0(GHIpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[2,id] <- f$Mean[29]
    }
    ##FTEOFirm
    if (grepl("FTEOFirm_", filename)){
      filepath <- paste0(GHIpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[3,id] <- f$Mean[29]
    }
    ##FTEOFirmwstroop
    if (grepl("FTEOFirmwstroop", filename)){
      filepath <- paste0(GHIpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[4,id] <- f$Mean[29]
    }
    ##FAEOFoam
    if (grepl("FAEOFoam_", filename)){
      filepath <- paste0(GHIpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[5,id] <- f$Mean[29]
    }
    ##FAEOFoamwstroop
    if (grepl("FAEOFoamwstroop", filename)){
      filepath <- paste0(GHIpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[6,id] <- f$Mean[29]
    }
  }
}

#TC
TCpath <- "TC"
TCfiles <- list.files(TCpath)
##fix IW11TC/IW14TCCO/IW15TC/IW1TC/IW1TCCO/IW5TCCO/IW6TC filename manually
for (x in TCfiles){
  id <- unlist(strsplit(x, "_"))[4]
  df[,id] <- NA
  path <- paste0(TCpath,"/",x)
  files <- list.files(path)
  for (filename in files){
    ##FAEOFirm
    if (grepl("FAEOFirm_", filename)){
      filepath <- paste0(TCpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[1,id] <- f$Mean[29]
    }
    ##FAEOFirmwstroop
    if (grepl("FAEOFirmwstroop", filename)){
      filepath <- paste0(TCpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[2,id] <- f$Mean[29]
    }
    ##FTEOFirm
    if (grepl("FTEOFirm_", filename)){
      filepath <- paste0(TCpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[3,id] <- f$Mean[29]
    }
    ##FTEOFirmwstroop
    if (grepl("FTEOFirmwstroop", filename)){
      filepath <- paste0(TCpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[4,id] <- f$Mean[29]
    }
    ##FAEOFoam
    if (grepl("FAEOFoam_", filename)){
      filepath <- paste0(TCpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[5,id] <- f$Mean[29]
    }
    ##FAEOFoamwstroop
    if (grepl("FAEOFoamwstroop", filename)){
      filepath <- paste0(TCpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[6,id] <- f$Mean[29]
    }
  }
}

#WS
WSpath <- "WS"
WSfiles <- list.files(WSpath)
for (x in WSfiles){
  id <- x
  filepath <- paste0(WSpath,"/",x,"/Sway_trials.csv")
  f <- read.csv(filepath, stringsAsFactors = F)
  f <- f[,c("Condition", "Postural.Sway...Angles...Sway.Area..degrees.2.")]
  for (i in 1:nrow(f)){
    #remove iWear/copy manually
    #remove stroop name
    if (substr(f$Condition[i], 1, 40) == "Feet Apart, Eyes Open, Firm Surface plus"){
      f$Condition[i] <- "Feet Apart, Eyes Open, Firm Surface with Stroop"
    }
    else if (substr(f$Condition[i], 1, 43) == "Feet Together, Eyes Open, Firm Surface plus"){
      f$Condition[i] <- "Feet Together, Eyes Open, Firm Surface with Stroop"
    }
    else if (substr(f$Condition[i], 1, 40) == "Feet Apart, Eyes Open, Foam Surface plus"){
      f$Condition[i] <- "Feet Apart, Eyes Open, Foam Surface with Stroop"
    }
    print(f$Condition[i])
  }
  df <- merge(df, f, all.x = T, sort = F)
  colnames(df)[ncol(df)] <- id
}

df$Condition <-  c("FAEOFirm Sway Area",
                   "FAEOFirmStroop Sway Area",
                   "FTEOFirm Sway Area",
                   "FTEOFirmStroop Sway Area",
                   "FAEOFoam Sway Area",
                   "FAEOFoamStroop Sway Area")

df <- t(df)
write.table(df, "Sway Area.csv", sep=",",  col.names=FALSE)