setwd("/Users/iris/Desktop/NRL/iWear")
df <- read.csv("iWear_complete.csv")

# Exclude IW1TACO and IW01CFCO
which(df$as_correct == "IW1TACO")  # 11
which(df$as_correct == "IW01CFCO") # 1
df <- df[-c(1,11),]

df_HD <- df[which(df$hd_or_healthy==1),]
df_CO <- df[which(df$hd_or_healthy==2),]

bi <- function(df){
  # Age
  cat("Age Mean:", round(mean(df$age)), "\n")
  cat("Age SD:", round(sd(df$age)), "\n")
  cat("Age Range:", range(df$age), "\n")
  
  # Sex
  table(df$bi_sex)
  cat("Number of Male: ", table(df$bi_sex)[1], "\n")
  cat("Number of Female: ", table(df$bi_sex)[2], "\n")
  
  # Site
  table(df$redcap_data_access_group)
}

cal <- function(df){
  cat("TMS Mean:", round(mean(df$TMS,na.rm=T)), "\n")
  cat("TMS SD:", round(sd(df$TMS,na.rm=T)), "\n")
  cat("TMS Range:", range(df$TMS,na.rm=T), "\n")
  
  cat("TFC Mean:", round(mean(df$TFC,na.rm=T)), "\n")
  cat("TFC SD:", round(sd(df$TFC,na.rm=T)), "\n")
  cat("TFC Range:", range(df$TFC,na.rm=T), "\n")
  
  cat("FA Mean:", round(mean(df$FA,na.rm=T)), "\n")
  cat("FA SD:", round(sd(df$FA,na.rm=T)), "\n")
  cat("FA Range:", range(df$FA,na.rm=T), "\n")
}

bi(df_HD)
bi(df_CO)
cal(df_HD)

# DT
table(df$redcap_data_access_group)
df_DT <- df[which((is.na(df$redcap_data_access_group))
                  |(df$redcap_data_access_group=="germany")
                  |(df$redcap_data_access_group=="wayne_state")),]
df_DT_HD <- df_DT[which(df_DT$hd_or_healthy==1),]
df_DT_CO <- df_DT[which(df_DT$hd_or_healthy==2),]

bi(df_DT_HD)
bi(df_DT_CO)
cal(df_DT_HD)