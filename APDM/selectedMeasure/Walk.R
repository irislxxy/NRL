#Walk
setwd("~/Desktop/NRL/iWear/APDM")

Condition <-  c("Walk Duration",
                "Walk Gait Speed L",
                "Walk Gait Speed R",
                "Alphabet Duration",
                "Alphabet Gait Speed L",
                "Alphabet Gait Speed R",
                "EOL Duration",
                "EOL Gait Speed L",
                "EOL Gait Speed R")
df <- data.frame(Condition)

#GHI
GHIpath <- "GHI"
GHIfiles <- list.files(GHIpath)
##GH12/GH17/GH9 no gait speed
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
      df[1,id] <- f$Mean[6]  #Duration
      df[2,id] <- f$Mean[15] #Gait Speed L
      df[3,id] <- f$Mean[16] #Gait Speed R
    }
    ##Alphabet
    if (grepl("WalkAlphabet", filename)){
      filepath <- paste0(GHIpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[4,id] <- f$Mean[6]  #Duration
      df[5,id] <- f$Mean[15] #Gait Speed L
      df[6,id] <- f$Mean[16] #Gait Speed R
    }
    ##EOLetter
    if (grepl("WalkEOLetter", filename)){
      filepath <- paste0(GHIpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[7,id] <- f$Mean[6]  #Duration
      df[8,id] <- f$Mean[15] #Gait Speed L
      df[9,id] <- f$Mean[16] #Gait Speed R
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
      df[1,id] <- f$Mean[6]  #Duration
      df[2,id] <- f$Mean[15] #Gait Speed L
      df[3,id] <- f$Mean[16] #Gait Speed R
    }
    ##Alphabet
    if (grepl("WalkAlphabet", filename)){
      filepath <- paste0(TCpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[4,id] <- f$Mean[6]  #Duration
      df[5,id] <- f$Mean[15] #Gait Speed L
      df[6,id] <- f$Mean[16] #Gait Speed R
    }
    ##EOLetter
    if (grepl("WalkEOLetter", filename)){
      filepath <- paste0(TCpath,"/",x,"/",filename)
      f <- read.csv(filepath, stringsAsFactors = F, skip = 9)
      df[7,id] <- f$Mean[6]  #Duration
      df[8,id] <- f$Mean[15] #Gait Speed L
      df[9,id] <- f$Mean[16] #Gait Speed R
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
  df[1,id] <- f$Duration..s.[which(f$Condition == "WWTT (walk) ")]
  df[2,id] <- f$Gait...Lower.Limb...Gait.Speed.L..m.s...mean.[which(f$Condition == "WWTT (walk) ")]
  df[3,id] <- f$Gait...Lower.Limb...Gait.Speed.R..m.s...mean.[which(f$Condition == "WWTT (walk) ")]
  ##Alphabet
  df[4,id] <- f$Duration..s.[which(f$Condition == "WWTT + Alphabet")]
  df[5,id] <- f$Gait...Lower.Limb...Gait.Speed.L..m.s...mean.[which(f$Condition == "WWTT + Alphabet")]
  df[6,id] <- f$Gait...Lower.Limb...Gait.Speed.R..m.s...mean.[which(f$Condition == "WWTT + Alphabet")]
  ##EOLetter
  df[7,id] <- f$Duration..s.[which(f$Condition == "WWTT + Every Other Letter Alphabet")]
  df[8,id] <- f$Gait...Lower.Limb...Gait.Speed.L..m.s...mean.[which(f$Condition == "WWTT + Every Other Letter Alphabet")]
  df[9,id] <- f$Gait...Lower.Limb...Gait.Speed.R..m.s...mean.[which(f$Condition == "WWTT + Every Other Letter Alphabet")]
}

df <- t(df)
write.table(df, "Walk.csv", sep=",",  col.names=FALSE)