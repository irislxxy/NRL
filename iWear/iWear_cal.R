setwd("/Users/iris/Desktop/NRL/iWear")

# bi
df <- read.csv("HDIWear_DATA_2018-10-22.csv")

# Exclude IW1TACO and IW01CFCO
which(df$as_correct == "IW1TACO")  # 11
which(df$as_correct == "IW01CFCO") # 1
df <- df[-c(1,11),]

df_HD <- df[which(df$hd_or_healthy==1),]
df_CO <- df[which(df$hd_or_healthy==2),]

bi <- function(df){
  # Age
  age <- 2018 - as.numeric(substr(df$bi_birthdate, 1, 4))
  cat("Age Mean:", round(mean(age)), "\n")
  cat("Age SD:", round(sd(age)), "\n")
  cat("Age Range:", range(age), "\n")
  
  # Sex
  table(df$bi_sex)
  cat("Number of Male: ", table(df$bi_sex)[1], "\n")
  cat("Number of Female: ", table(df$bi_sex)[2], "\n")
  
  # Site
  table(df$redcap_data_access_group)
}

bi(df_HD)
bi(df_CO)

# TMS/TFC/FA
df2 <- read.csv("iWear_complete.csv")

# Exclude IW1TACO and IW01CFCO
which(df2$as_correct == "IW1TACO")  # 11
which(df2$as_correct == "IW01CFCO") # 1
df2 <- df2[-c(1,11),]

df2_HD <- df2[which(df2$hd_or_healthy==1),]
df2_CO <- df2[which(df2$hd_or_healthy==2),]

cal <- function(df2){
  cat("TMS Mean:", round(mean(df2$TMS,na.rm=T)), "\n")
  cat("TMS SD:", round(sd(df2$TMS,na.rm=T)), "\n")
  cat("TMS Range:", range(df2$TMS,na.rm=T), "\n")
  
  cat("TFC Mean:", round(mean(df2$TFC,na.rm=T)), "\n")
  cat("TFC SD:", round(sd(df2$TFC,na.rm=T)), "\n")
  cat("TFC Range:", range(df2$TFC,na.rm=T), "\n")
  
  cat("FA Mean:", round(mean(df2$FA,na.rm=T)), "\n")
  cat("FA SD:", round(sd(df2$FA,na.rm=T)), "\n")
  cat("FA Range:", range(df2$FA,na.rm=T), "\n")
}

cal(df2_HD)