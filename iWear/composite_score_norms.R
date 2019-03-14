# z = (HD - normal mean) / normal sd
setwd("/Users/iris/Desktop/NRL/iWear")
data <- read.csv("iWear_complete_0219.csv")
data <- data[-1,] # Exclude IW01CFCO - No age 

df <- data[,c("as_correct","hd_or_healthy","age")] # Age Range 30-79

# Symbol Digit Modalities
df$sdm_correct <- data$sdm_correct
df$sdm_zscore <- NA
## Edu - 13 yrs or more
for (i in which(!is.na(df$sdm_correct))){
  if (df$age[i] < 35){ # 25-35
    df$sdm_zscore[i] <- (df$sdm_correct[i] - 57.72)/9.08
  }
  else if (df$age[i] >= 35 & df$age[i] <= 44){
    df$sdm_zscore[i] <- (df$sdm_correct[i] - 54.2)/11.17
  }
  else if (df$age[i] >= 45 & df$age[i] <= 54){
    df$sdm_zscore[i] <- (df$sdm_correct[i] - 52.27)/8.48
  }
  else if (df$age[i] >= 55 & df$age[i] <= 64){
    df$sdm_zscore[i] <- (df$sdm_correct[i] - 47.6)/8.31
  }
  else if (df$age[i] >= 65 & df$age[i] <= 75){ # 65-76 
    df$sdm_zscore[i] <- (df$sdm_correct[i] - 43.55)/11.27
  }
  else if (df$age[i] >= 76 & df$age[i] <= 80){
    df$sdm_zscore[i] <- (df$sdm_correct[i] - 32.75)/10.16
  }
  else{ # 81-91
    df$sdm_zscore[i] <- (df$sdm_correct[i] - 28.84)/8.93
  }
}
  
# Stroop
## Stroop Word Reading Test
df$wrt_correct <- data$stroop_wrt_correct 
df$wrt_zscore <- NA
for (i in which(!is.na(df$wrt_correct))){
  if (df$age[i] <= 39){ # 18-39
    df$wrt_zscore[i] <- (df$wrt_correct[i] - 13)/2.9
  }
  else if (df$age[i] >= 40 & df$age[i] <= 59){
    df$wrt_zscore[i] <- (df$wrt_correct[i] - 15.2)/2.9
  }
  else if (df$age[i] >= 60 & df$age[i] <= 69){
    df$wrt_zscore[i] <- (df$wrt_correct[i] - 15.9)/5.1
  }
  else if (df$age[i] >= 70 & df$age[i] <= 79){
    df$wrt_zscore[i] <- (df$wrt_correct[i] - 18.6)/5.4
  }
  else{ # 80-94
    df$wrt_zscore[i] <- (df$wrt_correct[i] - 22.1)/6
  }
}

## Stroop Color Naming Test
df$cnt_correct <- data$stroop_cnt_correct 
df$cnt_zscore <- NA
for (i in which(!is.na(df$cnt_correct))){
  if (df$age[i] <= 39){ # 18-39
    df$cnt_zscore[i] <- (df$cnt_correct[i] - 22.1)/7.2
  }
  else if (df$age[i] >= 40 & df$age[i] <= 59){
    df$cnt_zscore[i] <- (df$cnt_correct[i] - 27.8)/8.2
  }
  else if (df$age[i] >= 60 & df$age[i] <= 69){
    df$cnt_zscore[i] <- (df$cnt_correct[i] - 29.4)/9
  }
  else if (df$age[i] >= 70 & df$age[i] <= 79){
    df$cnt_zscore[i] <- (df$cnt_correct[i] - 37.1)/11.9
  }
  else{ # 80-94
    df$cnt_zscore[i] <- (df$cnt_correct[i] - 50.4)/23.9
  }
}

## Stroop Interference Test
df$it_correct <- data$stroop_it_correct
df$it_zscore <- NA
for (i in which(!is.na(df$it_correct))){
  if (df$age[i] <= 39){ # 18-39
    df$it_zscore[i] <- (df$it_correct[i] - 2)/0.6
  }
  else if (df$age[i] >= 40 & df$age[i] <= 59){
    df$it_zscore[i] <- (df$it_correct[i] - 2.3)/0.7
  }
  else if (df$age[i] >= 60 & df$age[i] <= 69){
    df$it_zscore[i] <- (df$it_correct[i] - 2.5)/0.8
  }
  else if (df$age[i] >= 70 & df$age[i] <= 79){
    df$it_zscore[i] <- (df$it_correct[i] - 2.7)/1
  }
  else{ # 80-94
    df$it_zscore[i] <- (df$it_correct[i] - 3.2)/1.6
  }
}

# Verbal Fluency
## Letter F
df$vf_f_correct <- data$vf_st
df$vf_f_zscore <- NA
for (i in which(!is.na(df$vf_f_correct))){
  if (df$age[i] <= 19){ # 16-19
    df$vf_f_zscore[i] <- (df$vf_f_correct[i] - 39.9)/12
  }
  else if (df$age[i] >= 20 & df$age[i] <= 29){
    df$vf_f_zscore[i] <- (df$vf_f_correct[i] - 41.2)/9.2
  }
  else if (df$age[i] >= 30 & df$age[i] <= 39){
    df$vf_f_zscore[i] <- (df$vf_f_correct[i] - 43.1)/11.4
  }
  else if (df$age[i] >= 40 & df$age[i] <= 49){ # ???
    df$vf_f_zscore[i] <- (df$vf_f_correct[i] - ?)/?
  }
  else if (df$age[i] >= 50 & df$age[i] <= 59){
    df$vf_f_zscore[i] <- (df$vf_f_correct[i] - 42.1)/11.1
  }
  else if (df$age[i] >= 60 & df$age[i] <= 69){
    df$vf_f_zscore[i] <- (df$vf_f_correct[i] - 38.5)/13.7
  }
  else if (df$age[i] >= 70 & df$age[i] <= 79){
    df$vf_f_zscore[i] <- (df$vf_f_correct[i] - 34.8)/12.8
  }
  else{ # 80-94
    df$vf_f_zscore[i] <- (df$vf_f_correct[i] - 28.9)/11.7
  }
}

## Letter A
df$vf_a_correct <- data$vf_st2_9c7
df$vf_a_zscore <- NA

## Letter S
df$vf_s_correct <- data$vf_st2_9c72_089
df$vf_s_zscore <- NA

# Average z-scores 
fields <- colnames(df)
zscore_idx <- grep("zscore",fields2)
df$composite_score <- mean(as.numeric(df[,zscore_idx]))

# write.csv(df, "composite_score_norms.csv", row.names = F)