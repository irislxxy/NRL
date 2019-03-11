library(tibble)

setwd("/Users/iris/Desktop/NRL/iWear")
df <- read.csv("HDIWear_DATA_2018-06-22.csv", stringsAsFactors=FALSE, na.strings=c("",NA))
fields <- colnames(df)

#stroop
##Feet Apart, Eyes Open, Firm Surface with Stroop
stroop <- grep("stroop", fields)
df_stroop <- df[,c(1,stroop)]
faeofs_crr <- df_stroop$stroop_correct/45
df_stroop <- add_column(df_stroop, faeofs_crr, .after=5)

##Feet Together, Eyes Open, Firm Surface with Stroop
fteofs_crr <- df_stroop$stroop_correct2/45
df_stroop <- add_column(df_stroop, fteofs_crr, .after=10)

##Feet Apart, Eyes Open, Foam Surface with Stroop
faeofoam_crr <- df_stroop$stroop_correct3/45
df_stroop <- add_column(df_stroop, faeofoam_crr, .after=15)

##Stroop Word Reading Test
wrt_crr <- df_stroop$stroop_wrt_correct/45
df_stroop <- add_column(df_stroop, wrt_crr, .after=19)

##Stroop Color Naming Test
cnt_crr <- df_stroop$stroop_cnt_correct/45
df_stroop <- add_column(df_stroop, cnt_crr, .after=23)

##Stroop Interference Test
it_crr <- df_stroop$stroop_it_correct/45
df_stroop <- add_column(df_stroop, it_crr, .after=27)

write.csv(df_stroop, "stroop_crr.csv", row.names = F)

#Walking
duration <- read.csv("APDM/Walking Duration.csv", stringsAsFactors=FALSE)
ghi_name <- paste0("IW",substr(duration$id[1:20],3,4),"GHI")
duration$id[1:20] <- ghi_name

wwt <- grep("wwt", fields)
df_wwt <- df[,c(1,wwt)]

##Walking While Talking (Alphabet)
alphabet_duration <- duration$WWTT..Alphabet.[match(df_wwt$as_correct,duration$id)]
df_wwt <- add_column(df_wwt, alphabet_duration, .after=3)
alphabet_crr <- (df_wwt$wwt_correct - df_wwt$wwt_errors)/df_wwt$alphabet_duration
df_wwt <- add_column(df_wwt, alphabet_crr, .after=4)

##Walking While Talking (Every Other Letter)
df_wwt$eol_duration <- duration$WWTT..Every.other.letter..[match(df_wwt$as_correct,duration$id)]
df_wwt$eol_crr <- (df_wwt$wwt_eol_correct - df_wwt$wwt_eol_errors)/df_wwt$eol_duration

write.csv(df_wwt, "wwt_crr.csv", row.names = F)