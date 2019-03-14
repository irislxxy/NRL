# Aim: Estimate how far the HD group deviate from normal
setwd("/Users/iris/Desktop/NRL/iWear")
data <- read.csv("HDIWear_DATA_2018-10-22.csv", stringsAsFactors=FALSE)
fields <- colnames(data)

# 1. Add SDMT, Stroop interference and 3 verbal fluency scores
df <- data[,c("as_correct","hd_or_healthy")]
## Symbol Digit Modalities
df$sdm_crr <- data$sdm_correct/90
## Stroop
df$it_crr <- data$stroop_it_correct/45
df$faeofs_crr <- data$stroop_correct/45
df$fteofs_crr <- data$stroop_correct2/45
df$faeofoam_crr <- data$stroop_correct3/45
## Verbal Fluency
df$vf_f_crr <- data$vf_st/60
df$vf_a_crr <- data$vf_st2_9c7/60
df$vf_s_crr <- data$vf_st2_9c72_089/60

# 2. Determine if controls are normally distributed.
normallist <- c()
for (x in colnames(df)[-c(1,2)]){
  t <- shapiro.test(df[df$hd_or_healthy==2,x])
  cat(x, ":\n")
  print(t)
  if (t$p.value > 0.05){
    normallist <- c(normallist, x)
    cat(x,"controls are normally distributed.\n\n\n")
  }
  else{
    cat(x,"controls are not normally distributed.\n\n\n")
  }
}
cat(paste( unlist(normallist), collapse=", "), "controls are normally distributed.")

# 3. If yes, then use that to calculate z scores for each participant.
## z = (HD - normal mean) / normal sd
for (x in normallist){
  mu <- mean(df[df$hd_or_healthy==2,x], na.rm = T)
  sigma <- sd(df[df$hd_or_healthy==2,x], na.rm = T)
  varname <- paste0(x, "_zscore")
  df[,varname] <- NA
  df[df$hd_or_healthy==1,varname] <- (df[df$hd_or_healthy==1,x] - mu) / sigma
  df[df$hd_or_healthy==2,varname] <- "CO"
}

# 4. Average z-scores 
fields2 <- colnames(df)
zscore_idx <- grep("zscore",fields2)

df$composite_score <- NA
df$composite_score[df$hd_or_healthy==2] <- "CO"
for (i in which(df$hd_or_healthy==1)){
  df$composite_score[i] <- mean(as.numeric(df[i,zscore_idx]))
}

write.csv(df, "composite_score.csv", row.names = F)