setwd("/Users/iris/Desktop/NRL/iWear")
df <- read.csv("iWear_complete_0322.csv")

# Exclude IW1TACO and IW01CFCO
which(df$as_correct == "IW1TACO")  # 11
which(df$as_correct == "IW01CFCO") # 1
df <- df[-c(1,11),]

# Functions
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

cal <- function(variable){
  cat("Mean:", round(mean(variable,na.rm=T)), "\n")
  cat("SD:", round(sd(variable,na.rm=T)), "\n")
  cat("Range:", range(variable,na.rm=T), "\n")
  cat("Confidence Interval:", t.test(variable)$conf.int, "\n")
  cat("N:", sum(!is.na(variable)), "\n\n")
}

cal_df <- function(df){
  cat("Step Count:\n")
  cal(df$step_count)
  
  cat("Composite Score:\n")
  cal(df$composite_score)
  
  cat("TMS:\n")
  cal(df$TMS)
  
  cat("TFC:\n")
  cal(df$TFC)
  
  cat("FA:\n")
  cal(df$FA)
  
  cat("WHO DAS:\n")
  cal(df$whodas_score)
  
  cat("IPAQ PRE:\n")
  cal(df$ipaq_score_pre)
  
  cat("LifeSpace:\n")
  cal(df$ls_score)
  
  cat("Functional Assessment Independence:\n")
  cal(df$func_assess_independ)
  
  cat("Functional Ambulation Category:\n")
  table(df$func_ambul_cat)
  
}

# Full Dataset
df_HD <- df[which(df$hd_or_healthy==1),]
df_CO <- df[which(df$hd_or_healthy==2),]

bi(df_HD)
bi(df_CO)
cal_df(df_HD)
cal(df_CO$step_count) # IW10CFCO 
cal(df_CO$composite_score)
cal(df_CO$ipaq_score_pre)


# DT
table(df$redcap_data_access_group)
df_DT <- df[which((is.na(df$redcap_data_access_group))
                  |(df$redcap_data_access_group=="germany")
                  |(df$redcap_data_access_group=="wayne_state")),]
df_DT_HD <- df_DT[which(df_DT$hd_or_healthy==1),]
df_DT_CO <- df_DT[which(df_DT$hd_or_healthy==2),]

bi(df_DT_HD)
bi(df_DT_CO)
cal_df(df_DT_HD)
cal(df_DT_CO$step_count) 
cal(df_DT_CO$composite_score) 
cal(df_DT_CO$ipaq_score_pre)


# Correlation
cor_list <- c("composite_score","TMS","TFC","FA","whodas_score","ipaq_score_pre","ls_score","func_assess_independ")
for (i in cor_list){
  sub <- c("step_count",i)
  df_sub <- df[,sub]
  df_sub <- na.omit(df_sub)
  cat(i,":",cor(df_sub[,1],df_sub[,2]),"\n")
}