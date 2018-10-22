setwd("/Users/iris/Desktop/NRL/iWear")
df <- read.csv("HDIWear_DATA_2018-09-21.csv")

# Exclude IW1TACO and IW01CFCO
which(df$as_correct == "IW1TACO")  # 11
which(df$as_correct == "IW01CFCO") # 1
df <- df[-c(1,11),]

# HD/Control
table(df$hd_or_healthy)
cat("Number of HD: ", table(df$hd_or_healthy)[1], "\n")
cat("Number of Control: ", table(df$hd_or_healthy)[2], "\n")

# Age
age <- 2018 - as.numeric(substr(df$bi_birthdate, 1, 4))
cat("Age Mean:", mean(age), "\n")
cat("Age SD:", sd(age), "\n")

# Sex
table(df$bi_sex)
cat("Number of Male: ", table(df$bi_sex)[1], "\n")
cat("Number of Female: ", table(df$bi_sex)[2], "\n")

# Site
table(df$redcap_data_access_group)

# TMS/TFC/FA
df2 <- read.csv("IWear_0921.csv")

which(df2$as_correct == "IW1TACO")  # 37
which(df2$as_correct == "IW01CFCO") # 1
df2 <- df2[-c(1,37),]

cat("TMS Mean:", mean(df2$TMS), "\n")
cat("TMS SD:", sd(df2$TMS), "\n")
cat("TFC Mean:", mean(df2$TFC), "\n")
cat("TFC SD:", sd(df2$TFC), "\n")
cat("FA Mean:", mean(df2$FA), "\n")
cat("FA SD:", sd(df2$FA), "\n")

# bi function
df_HD <- df[which(df$hd_or_healthy==1),]
df_CO <- df[which(df$hd_or_healthy==2),]
bi <- function(df) {
  # Age
  age <- 2018 - as.numeric(substr(df$bi_birthdate, 1, 4))
  cat("Age Mean:", mean(age), "\n")
  cat("Age SD:", sd(age), "\n")
  
  # Sex
  table(df$bi_sex)
  cat("Number of Male: ", table(df$bi_sex)[1], "\n")
  cat("Number of Female: ", table(df$bi_sex)[2], "\n")
  
  # Site
  table(df$redcap_data_access_group)
}

bi(df)
bi(df_HD)
bi(df_CO)
