#Walk
setwd("~/Desktop/NRL/iWear/APDM")

Condition <-  c("Walk Double Support L",
                "Walk Double Support R",
                "Alphabet Double Support L",
                "Alphabet Double Support R",
                "EOL Double Support L",
                "EOL Double Support R")
df <- data.frame(Condition)

#GHI
GHIpath <- "GHI"
GHIfiles <- list.files(GHIpath)
##GH12/GH17/GH9 no data
for (x in GHIfiles){
  id <- unlist(strsplit(x, "_"))[4]
  df[,id] <- NA
  path <- paste0(GHIpath,"/",x)
  files <- list.files(path)
  ##remove 2-minute walk
  for (filename in files[-1]){
    ##Walk
    if (grepl("Walk_trial.csv", filename)){
      filepath <- paste0(GHIpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[1,id] <- f$Mean[9]  #Double Support L
      df[2,id] <- f$Mean[10] #Double Support R
    }
    ##Alphabet
    if (grepl("WalkAlphabet", filename)){
      filepath <- paste0(GHIpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[3,id] <- f$Mean[9]  #Double Support L
      df[4,id] <- f$Mean[10] #Double Support R
    }
    ##EOLetter
    if (grepl("WalkEOLetter", filename)){
      filepath <- paste0(GHIpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[5,id] <- f$Mean[9]  #Double Support L
      df[6,id] <- f$Mean[10] #Double Support R
    }
  }
}

#TC
TCpath <- "TC"
TCfiles <- list.files(TCpath)
for (x in TCfiles){
  id <- unlist(strsplit(x, "_"))[4]
  df[,id] <- NA
  path <- paste0(TCpath,"/",x)
  files <- list.files(path)
  ##remove 2-minute walk
  for (filename in files[-1]){
    ##Walk
    if (grepl("Walk_trial.csv", filename)){
      filepath <- paste0(TCpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[1,id] <- f$Mean[9]  #Double Support L
      df[2,id] <- f$Mean[10] #Double Support R
    }
    ##Alphabet
    if (grepl("WalkAlphabet", filename)){
      filepath <- paste0(TCpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[3,id] <- f$Mean[9]  #Double Support L
      df[4,id] <- f$Mean[10] #Double Support R
    }
    ##EOLetter
    if (grepl("WalkEOLetter", filename)){
      filepath <- paste0(TCpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[5,id] <- f$Mean[9]  #Double Support L
      df[6,id] <- f$Mean[10] #Double Support R
    }
  }
}

#WS
WSpath <- "WS"
WSfiles <- list.files(WSpath)
for (x in WSfiles){
  id <- x
  df[,id] <- NA
  filepath <- paste0(WSpath,"/",x,"/walk_trials.csv")
  f <- read.csv(filepath, stringsAsFactors = F)
  ##Walk
  df[1,id] <- f$Gait...Lower.Limb...Double.Support.L...GCT...mean.[which(f$Condition == "WWTT (walk) ")]
  df[2,id] <- f$Gait...Lower.Limb...Double.Support.R...GCT...mean.[which(f$Condition == "WWTT (walk) ")]
  ##Alphabet
  df[3,id] <- f$Gait...Lower.Limb...Double.Support.L...GCT...mean.[which(f$Condition == "WWTT + Alphabet")]
  df[4,id] <- f$Gait...Lower.Limb...Double.Support.R...GCT...mean.[which(f$Condition == "WWTT + Alphabet")]
  ##EOLetter
  df[5,id] <- f$Gait...Lower.Limb...Double.Support.L...GCT...mean.[which(f$Condition == "WWTT + Every Other Letter Alphabet")]
  df[6,id] <- f$Gait...Lower.Limb...Double.Support.R...GCT...mean.[which(f$Condition == "WWTT + Every Other Letter Alphabet")]
}

colnames(df)[1] <- "as_correct"
df <- t(df)
write.table(df, "Walk_DoubleSupport.csv", sep=",",  col.names=FALSE)