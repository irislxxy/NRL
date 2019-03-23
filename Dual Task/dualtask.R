library(tibble)
setwd("/Users/iris/Desktop/NRL")

# APDM
df_sway <- read.csv("APDM/Measure/Sway Area.csv", header=T, stringsAsFactors=FALSE)
df_gait <- read.csv("APDM/Measure/Walk.csv", header=T, stringsAsFactors=FALSE)
df_tug <- read.csv("APDM/Measure/TUG.csv", header=T, stringsAsFactors=FALSE)

df_apdm <- merge(df_sway, df_gait)
df_apdm <- merge(df_apdm, df_tug)
colnames(df_apdm)[1] <- c("as_correct")
df_apdm$as_correct[1:20] <- paste0("IW",substr(df_apdm$as_correct[1:20],3,4),"GHI")

## Gait Speed
Walk.Gait.Speed <- (df_apdm$Walk.Gait.Speed.L + df_apdm$Walk.Gait.Speed.R)/2 
df_apdm <- add_column(df_apdm, Walk.Gait.Speed, .after = "Walk.Gait.Speed.R")

Alphabet.Gait.Speed <- (df_apdm$Alphabet.Gait.Speed.L + df_apdm$Alphabet.Gait.Speed.R)/2 
df_apdm <- add_column(df_apdm, Alphabet.Gait.Speed, .after = "Alphabet.Gait.Speed.R")

EOL.Gait.Speed <- (df_apdm$EOL.Gait.Speed.L + df_apdm$EOL.Gait.Speed.R)/2 
df_apdm <- add_column(df_apdm, EOL.Gait.Speed, .after = "EOL.Gait.Speed.R")

# REDCap
df <- read.csv("iWear/HDIWear_DATA_2019-03-22.csv", stringsAsFactors=FALSE)
fields <- colnames(df)

# participants: TC, Wayne State and Germany
df <- df[which(df$redcap_data_access_group %in% c("","wayne_state","germany")),]



# Cognitive - Standing
## interference - single task
dte <- df[,c("as_correct","stroop_it_correct")]
dte$it_crr <- dte$stroop_it_correct/45

## faeofs
dte$faeofs_correct <- df$stroop_correct
dte$faeofs_crr <- dte$faeofs_correct/45
dte$faeofs_dte <- 100 * (dte$faeofs_crr - dte$it_crr)/dte$it_crr

## fteofs
dte$fteofs_correct <- df$stroop_correct2
dte$fteofs_crr <- dte$fteofs_correct/45
dte$fteofs_dte <- 100 * (dte$fteofs_crr - dte$it_crr)/dte$it_crr

## faeofoam
dte$faeofoam_correct <- df$stroop_correct3
dte$faeofoam_crr <- dte$faeofoam_correct/45
dte$faeofoam_dte <- 100 * (dte$faeofoam_crr - dte$it_crr)/dte$it_crr

# Motor - Standing
dte <- merge(dte, df_apdm[,c(1:7)], all.x = TRUE)

## faeofs
motor_faeofs_dte <- -100 * (dte$FAEOFirmStroop.Sway.Area - dte$FAEOFirm.Sway.Area)/dte$FAEOFirm.Sway.Area
dte <- add_column(dte, motor_faeofs_dte, .after="FAEOFirmStroop.Sway.Area")

## fteofs
motor_fteofs_dte <- -100 * (dte$FTEOFirmStroop.Sway.Area - dte$FTEOFirm.Sway.Area)/dte$FTEOFirm.Sway.Area
dte <- add_column(dte, motor_fteofs_dte, .after="FTEOFirmStroop.Sway.Area")

## faeofoam
dte$motor_faeofoam_dte <- -100 * (dte$FAEOFoamStroop.Sway.Area - dte$FAEOFoam.Sway.Area)/dte$FAEOFoam.Sway.Area



# Cognitive - Walking
## Alphabet 
dte$as_correct1 <- df$as_correct1
dte$as_time <- df$as_time
dte$as_crr <- dte$as_correct1/dte$as_time

dte$wwt_correct <- df$wwt_correct
dte$wwt_time <- df_apdm$Alphabet.Duration[match(dte$as_correct,df_apdm$as_correct)]
dte$wwt_crr <- dte$wwt_correct/dte$wwt_time
dte$wwt_dte <- 100 * (dte$wwt_crr - dte$as_crr)/dte$as_crr

## Every Other Letter
dte$as_eol_correct <- df$as_eol_correct
dte$as_eol_time <- df$as_eol_time
dte$as_eol_crr <- as.numeric(dte$as_eol_correct)/dte$as_eol_time

dte$wwt_eol_correct <- df$wwt_eol_correct
dte$wwt_eol_time <- df_apdm$EOL.Duration[match(dte$as_correct,df_apdm$as_correct)]
dte$wwt_eol_crr <- dte$wwt_eol_correct/dte$wwt_eol_time
dte$wwt_eol_dte <- 100 * (dte$wwt_eol_crr - dte$as_eol_crr)/dte$as_eol_crr

## DKEFS
dte$d_kefs_totalcorrect1 <- df$d_kefs_totalcorrect1
dte$as_dkefs_crr <- df$d_kefs_totalcorrect1/60

dte$d_kefs_totalcorrect <- df$d_kefs_totalcorrect
dte$wwt_dkefs_time <- df_apdm$DKEFS.Duration[match(dte$as_correct,df_apdm$as_correct)]
dte$wwt_dkefs_crr <- dte$d_kefs_totalcorrect/dte$wwt_dkefs_time
dte$wwt_dkefs_dte <- 100 * (dte$wwt_dkefs_crr - dte$as_dkefs_crr)/dte$as_dkefs_crr

# Motor - Walking
dte <- merge(dte, df_apdm[,c(1,8,12,16,20)], all.x = TRUE)

## Walking While Talking (Alphabet)
# dte$motor_alphabet_dte <- 100 * (dte$Alphabet.Gait.Speed - dte$Walk.Gait.Speed)/dte$Walk.Gait.Speed
dte$motor_alphabet_dte <- -100 * (dte$Alphabet.Duration - dte$Walk.Duration)/dte$Walk.Duration

## Walking While Talking (Every Other Letter)
# dte$motor_eol_dte <- 100 * (dte$EOL.Gait.Speed - dte$Walk.Gait.Speed)/dte$Walk.Gait.Speed
dte$motor_eol_dte <- -100 * (dte$EOL.Duration - dte$Walk.Duration)/dte$Walk.Duration

## Walking While Talking (DKEFS) 
dte$motor_dkefs_dte <- -100 * (dte$DKEFS.Duration - dte$Walk.Duration)/dte$Walk.Duration 



# Motor - TUG
dte <- merge(dte, df_apdm[,c(1,23,24)], all.x = T)
dte$motor_tug_dte <- -100 * (dte$TUG.with.Cognitive.Duration - dte$TUG.Duration)/dte$TUG.Duration 



df_hd <- df[,c("as_correct","hd_or_healthy")]
dte <- merge(df_hd, dte, all.x = T)
which(is.na(dte), arr.ind=TRUE) # IW13TC/IW4TC as_eol_time & IW2WS DKEFS duration
write.csv(dte, "Dual Task/DTE_0322.csv", row.names = F)