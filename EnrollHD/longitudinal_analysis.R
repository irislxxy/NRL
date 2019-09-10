library(tibble)
setwd("/Users/iris/Desktop/NRL/EnrollHD")
df <- read.csv("tug_4seq.csv", stringsAsFactors=FALSE)

# exclude participants under 18 years old
which(is.na(as.numeric(df$age_0)))
df[2845,c("subjid","age_0")] 
## 433-436   R105309514 <18
## 1969-1972 R422281881 <18
## 2309-2312 R482648410 <18
## 2845-2858 R589934995 <18

df <- df[-which(is.na(as.numeric(df$age_0))),]
age <- as.numeric(df$age_0) + as.numeric(df$seq) - 1
df <- add_column(df, age, .after = "age_0")

# CAP score
cap <- df$age * (as.numeric(df$caghigh) - 30)/6.27 # When L = 30 and K = 6.27, CAP will be equal to 100 at the subject’s expected age of onset of motor symptoms.
df <- add_column(df, cap, .after = "caghigh")

# UHDRS composite score
df$cUHDRS <- (df$tfcscore - mean(df$tfcscore, na.rm = T))/sd(df$tfcscore, na.rm = T) - # Note the negative sign for TMS
  (df$motscore - mean(df$motscore, na.rm = T))/sd(df$motscore, na.rm = T) + 
  (df$sdmt1 - mean(df$sdmt1, na.rm = T))/sd(df$sdmt1, na.rm = T) + 
  (df$swrt1 - mean(df$swrt1, na.rm = T))/sd(df$swrt1, na.rm = T) + 
  10 # allow the composite measure to take on positive values

# We want to follow their linear regression models (One for TUG and one for STS) were used to 
## estimate differences in annualized measures of change between groups (controls, preHD, and HD) and adjust for age and sex. 
# We definitely want this annualized measure of change for both. 


