library(tibble)
setwd("/Users/iris/Desktop/NRL/iWear")

#APDM
df_sway <- read.csv("APDM/Sway Area.csv", header=T, stringsAsFactors=FALSE)
df_gait <- read.csv("APDM/Walk.csv", header=T, stringsAsFactors=FALSE)
df_tug <- read.csv("APDM/TUG.csv", header=T, stringsAsFactors=FALSE)

df_apdm <- merge(df_sway, df_gait)
df_apdm <- merge(df_apdm, df_tug)
colnames(df_apdm)[1] <- c("as_correct")
df_apdm$as_correct[1:20] <- paste0("IW",substr(df_apdm$as_correct[1:20],3,4),"GHI")

##Gait Speed
Walk.Gait.Speed <- (df_apdm$Walk.Gait.Speed.L + df_apdm$Walk.Gait.Speed.R)/2 
df_apdm <- add_column(df_apdm, Walk.Gait.Speed, .after = "Walk.Gait.Speed.R")

Alphabet.Gait.Speed <- (df_apdm$Alphabet.Gait.Speed.L + df_apdm$Alphabet.Gait.Speed.R)/2 
df_apdm <- add_column(df_apdm, Alphabet.Gait.Speed, .after = "Alphabet.Gait.Speed.R")

EOL.Gait.Speed <- (df_apdm$EOL.Gait.Speed.L + df_apdm$EOL.Gait.Speed.R)/2 
df_apdm <- add_column(df_apdm, EOL.Gait.Speed, .after = "EOL.Gait.Speed.R")

#REDCap
df <- read.csv("HDIWear_DATA_2018-07-30.csv", stringsAsFactors=FALSE)
fields <- colnames(df)

#participants: TC, Wayne State and Germany
df <- df[which(df$redcap_data_access_group %in% c("","wayne_state","germany")),]

#cognitive-standing
#single task
##interference
dte <- df[,c("as_correct","stroop_it_correct","stroop_it_errors")]
dte$it_crr <- dte$stroop_it_correct/45

#dual task
##faeofs
dte$faeofs_correct <- df$stroop_correct
dte$faeofs_errors <- df$stroop_errors
dte$faeofs_crr <- dte$faeofs_correct/45
dte$faeofs_dte <- 100 * (dte$faeofs_crr - dte$it_crr)/dte$it_crr

##fteofs
dte$fteofs_correct <- df$stroop_correct2
dte$fteofs_errors <- df$stroop_errors2
dte$fteofs_crr <- dte$fteofs_correct/45
dte$fteofs_dte <- 100 * (dte$fteofs_crr - dte$it_crr)/dte$it_crr

##faeofoam
dte$faeofoam_correct <- df$stroop_correct3
dte$faeofoam_errors <- df$stroop_errors3
dte$faeofoam_crr <- dte$faeofoam_correct/45
dte$faeofoam_dte <- 100 * (dte$faeofoam_crr - dte$it_crr)/dte$it_crr

#cognitive-walking
#single task: alphabet sitting
as <- grep("as", fields)[c(1,24:29)]
df_as <- df[,as]
dte <- merge(dte, df_as)
as_crr <- (dte$as_correct1 - dte$as_errors)/dte$as_time
dte <- add_column(dte, as_crr, .after="as_errors")
dte$as_eol_crr <- (as.numeric(dte$as_eol_correct) - dte$as_eol_errors)/dte$as_eol_time

#dual task: walking while talking
wwt <- grep("wwt", fields)
df_wwt <- df[,c(1,wwt)]
dte <- merge(dte, df_wwt)

##Walking While Talking (Alphabet)
wwt_time <- df_apdm$Alphabet.Duration[match(dte$as_correct,df_apdm$as_correct)]
dte <- add_column(dte, wwt_time, .after="wwt_errors")
wwt_crr <- (dte$wwt_correct - dte$wwt_errors)/dte$wwt_time
dte <- add_column(dte, wwt_crr, .after="wwt_time")
wwt_dte <- 100 * (dte$wwt_crr - dte$as_crr)/dte$as_crr
dte <- add_column(dte, wwt_dte, .after="wwt_crr")

##Walking While Talking (Every Other Letter)
dte$wwt_eol_time <- df_apdm$EOL.Duration[match(dte$as_correct,df_apdm$as_correct)]
dte$wwt_eol_crr <- (dte$wwt_eol_correct - dte$wwt_eol_errors)/dte$wwt_eol_time
dte$wwt_eol_dte <- 100 * (dte$wwt_eol_crr - dte$as_eol_crr)/dte$as_eol_crr

#motor-standing
dte <- merge(dte, df_apdm[,c(1:7)], all.x = TRUE)

##faeofs
motor_faeofs_dte <- -100 * (dte$FAEOFirmStroop.Sway.Area - dte$FAEOFirm.Sway.Area)/dte$FAEOFirm.Sway.Area
dte <- add_column(dte, motor_faeofs_dte, .after="FAEOFirmStroop.Sway.Area")

##fteofs
motor_fteofs_dte <- -100 * (dte$FTEOFirmStroop.Sway.Area - dte$FTEOFirm.Sway.Area)/dte$FTEOFirm.Sway.Area
dte <- add_column(dte, motor_fteofs_dte, .after="FTEOFirmStroop.Sway.Area")

##faeofoam
dte$motor_faeofoam_dte <- -100 * (dte$FAEOFoamStroop.Sway.Area - dte$FAEOFoam.Sway.Area)/dte$FAEOFoam.Sway.Area

#motor-walking
dte <- merge(dte, df_apdm[,c(1,11,15,19)], all.x = TRUE)

##Walking While Talking (Alphabet)
dte$motor_alphabet_dte <- 100 * (dte$Alphabet.Gait.Speed - dte$Walk.Gait.Speed)/dte$Walk.Gait.Speed

##Walking While Talking (Every Other Letter)
dte$motor_eol_dte <- 100 * (dte$EOL.Gait.Speed - dte$Walk.Gait.Speed)/dte$Walk.Gait.Speed

#motor-TUG
dte <- merge(dte, df_apdm[,c(1,20,21)], all.x = T)
dte$motor_tug_dte <- -100 * (dte$TUG.with.Cognitive.Duration - dte$TUG.Duration)/dte$TUG.Duration 

df_hd <- df[,c("as_correct","hd_or_healthy")]
dte <- merge(df_hd, dte, all.x = T)
dte <- na.omit(dte)
write.csv(dte, "Dual Task/DTE.csv", row.names = F)

df_anova <- merge(df_hd, df_apdm[,c(1:7,11,15,19,20,21)], all.x = T)
df_anova <- merge(df_anova, dte[,c(1,grep("crr",colnames(dte)))], all.x = T)
which(is.na(df_anova), arr.ind=TRUE) #GH9/GH12/GH17/IW4TC/IW13TC
#df_anova <- na.omit(df_anova)
write.csv(df_anova, "Dual Task/ANOVA.csv", row.names = F)