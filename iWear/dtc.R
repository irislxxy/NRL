library(tibble)
setwd("/Users/iris/Desktop/NRL/iWear")

#DTC[%] = 100 * (single-task score - dual-task score)/single-task score

#stroop
##The single task is the stroop interference. 
##The dual task is faeofs, fteofs, faeofoam compared to the seated stroop interference. 
df_stroop <- read.csv("stroop_crr.csv", stringsAsFactors=FALSE)
df_stroop <-df_stroop[,c("as_correct","it_crr","faeofs_crr","fteofs_crr","faeofoam_crr")]

##faeofs
faeofs_dtc <- 100 * (df_stroop$it_crr - df_stroop$faeofs_crr)/df_stroop$it_crr
df_stroop <- add_column(df_stroop, faeofs_dtc, .after="faeofs_crr")
##fteofs
fteofs_dtc <- 100 * (df_stroop$it_crr - df_stroop$fteofs_crr)/df_stroop$it_crr
df_stroop <- add_column(df_stroop, fteofs_dtc, .after="fteofs_crr")
##faeofoam
faeofoam_dtc <- 100 * (df_stroop$it_crr - df_stroop$faeofoam_crr)/df_stroop$it_crr
df_stroop <- add_column(df_stroop, faeofoam_dtc, .after="faeofoam_crr")

write.csv(df_stroop, "stroop_dtc.csv", row.names = F)

#walking
##The single task is alphabet sitting.
df <- read.csv("HDIWear_DATA_2018-06-22.csv", stringsAsFactors=FALSE)
fields <- colnames(df)
as <- grep("as", fields)[c(1,24:29)]
df_as <- df[,as]
as_crr <- (df_as$as_correct1 - df_as$as_errors)/df_as$as_time
as_eol_crr <- (as.numeric(df_as$as_eol_correct) - df_as$as_eol_errors)/df_as$as_eol_time

##The dual task is walking while talking.
df_alphabet <- read.csv("wwt_crr.csv", stringsAsFactors=FALSE)
df_alphabet <-df_alphabet[,c("as_correct","alphabet_crr","eol_crr")]
colnames(df_alphabet)[2:3] <- c("wwt_crr","wwt_eol_crr")

##alphabet
df_alphabet <- add_column(df_alphabet, as_crr, .before="wwt_crr")
alphabet_dtc <- 100 * (df_alphabet$as_crr - df_alphabet$wwt_crr)/df_alphabet$as_crr
df_alphabet <- add_column(df_alphabet, alphabet_dtc, .after="wwt_crr")

##eol
df_alphabet <- add_column(df_alphabet, as_eol_crr, .before="wwt_eol_crr")
eol_dtc <- 100 * (df_alphabet$as_eol_crr - df_alphabet$wwt_eol_crr)/df_alphabet$as_eol_crr
df_alphabet <- add_column(df_alphabet, eol_dtc, .after="wwt_eol_crr")

write.csv(df_alphabet, "wwt_dtc.csv", row.names = F)
