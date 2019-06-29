library(rJava)
library(xlsxjars)
library(xlsx)
library(tibble)
setwd("/Users/iris/Desktop/NRL/iWear")

# replace blank/N/ N/A /none with NA
df <- read.csv("HDIWear_DATA_2019-03-22.csv", stringsAsFactors=FALSE, na.strings=c("",NA))
df[df == "N" | df == "N/A" | df == "none"] <- NA
fields <- colnames(df)

# repalce NA with blank for notes
notes <- grep("notes", fields)
df[,notes][is.na(df[,notes])] <- ""

# Basic Information for everyone
for (i in 1:nrow(df)){
  if (df[i,"bi_medications"]==0 | is.na(df[i,"bi_medications"])){
    df[i,c(23:59)] <- ""
  }
  else if (df[i,"bi_medications"]==1){
    df[i,c(32:59)] <- ""
  }
  else if (df[i,"bi_medications"]==2){
    df[i,c(41:59)] <- ""
  }
  else if (df[i,"bi_medications"]==3){
    df[i,c(50:59)] <- ""
  } 
  else if (df[i,"bi_medications"]==4){
    df[i,59] <- ""
  } 
}

# WHO DAS for HD only
whodas_temp <- grep("^who_das", fields)
whodas_idx <- seq(whodas_temp[1]+1, whodas_temp[2]-1)
whodas_col <- fields[whodas_idx]

## whodas score
whodas_s12 <- whodas_idx[1:12]
whodas_score <- rowSums(df[whodas_s12])

# History of Falls Questionnaire PRE for HD only
hfq_temp <- grep("^history_of_falls_questionnaire_pre", fields)
hfq_idx <- seq(hfq_temp[1]+1, hfq_temp[2]-1)
df[df$hd_or_healthy==2,hfq_idx] <- ""

## 1. Do you use an assistive device to walk?
hfq_1_specify_idx <- which(df$hfq_1_yes___6==0)
df$hfq_1_specify[hfq_1_specify_idx] <- ""

## 2. Have you fallen in the past six months?
## If hfq_2=0, ingore the following fields.
hfq_2_no_idx <- which(df$hfq_2==0)
hfq_2_field_idx <- seq(hfq_temp[1]+10, hfq_temp[2]-2)
hfq_2_field <- fields[hfq_2_field_idx]
df[hfq_2_no_idx,hfq_2_field] <- "" 

## 3. What were you doing at the time of your most serious fall?
hfq_3_specify_idx <- which(df$hfq_3!=12)
df$hfq_3_specify[hfq_3_specify_idx] <- "" 

## 4. What happened or caused you to fall on that occasion?
hfq_4_specify_idx <- which(df$hfq_4___21==0)
df$hfq_4_specify[hfq_4_specify_idx] <- ""

## 6. Did objects or conditions in the environment contribute to the fall
## If hfq_6=0, ingore the following fields.
hfq_6_no_idx <- which(df$hfq_6==0)
hfq_6_field_idx <- grep("hfq_6",fields)
hfq_6_field <- fields[hfq_6_field_idx][-1]
df[hfq_6_no_idx,hfq_6_field] <- ""
hfq_6_specify_idx <- which(df$hfq_6_specify___14==0)
df$hfq_6_otherspecify[hfq_6_specify_idx] <- ""

## 7. Have you changed anything to prevent reoccurrence?
hfq_7_specify_idx <- which(df$hfq_7==0)
df$hfq_7_specify[hfq_7_specify_idx] <- ""

## 13. Have you completely recovered to the point that you feel like yourself again?
hfq_13_no_idx <- which(df$hfq_13==0)
df$hfq_13_yes[hfq_13_no_idx] <- ""

## 15A. For your most serious injury, what type of injury did you suffer? 
hfq_15a_specify_idx <- which(df$hfq_15a!=6)
df$hfq_15a_specify[hfq_15a_specify_idx] <- ""

## 15B. What was the location of your most serious injury? 
hfq_15b_specify_idx <- which(df$hfq_15b!=23)
df$hfq_15b_specify[hfq_15b_specify_idx] <- ""

## 17A. For your second most serious injury, what type of injury did you suffer?
hfq_17a_specify_idx <- which(df$hfq_17a!=6)
df$hfb_17a_specify[hfq_17a_specify_idx] <- ""

## 17B. What was the location of your second most serious injury?
hfq_17b_specify_idx <- which(df$hfq_17a!=23)
df$hfq_17b_specify[hfq_17b_specify_idx] <- ""

# Functional Ambulation Category for HD only
func_temp <- grep("^functional_ambulation_category", fields)
func_idx <- seq(func_temp[1]+1, func_temp[2]-1)
df[df$hd_or_healthy==2,func_idx] <- ""

# IPAQ Short Last 7 Days PRE for everyone
ipaq_pre_temp <- grep("^ipaq_short_last_7_days_telephone_pre", fields)
ipaq_pre_idx <- seq(ipaq_pre_temp[1]+1, ipaq_pre_temp[2])
ipaq_pre <- c("as_correct", fields[ipaq_pre_idx])
df_ipaq_pre <- df[,ipaq_pre]

## At least one value is required in the following 4 fields for question 1/3/5/7. 
## If all the values are missing, report NA in the first field (Hours per day).

## 1. During the last 7 days, on how many days did you do vigorous physical activities?
df_ipaq_pre[,c(3:6)][is.na(df_ipaq_pre[,c(3:6)])] <- ""
for (i in which(!(df_ipaq_pre$ipaq_ts_vday %in% c(0,8,9,NA)))){
  if (is.na(df[i,"ipaq_ts_vdhrs"])&is.na(df[i,"ipaq_ts_vdmin"])&is.na(df[i,"ipaq_ts_vwhrs"])&is.na(df[i,"ipaq_ts_vwmin"])){
    df_ipaq_pre[i,"ipaq_ts_vdhrs"] <- NA
  }
}

## 3. During the last 7 days, on how many days did you do moderate physical activities?
df_ipaq_pre[,c(8:11)][is.na(df_ipaq_pre[,c(8:11)])] <- ""
for (i in which(!(df_ipaq_pre$ipaq_ts_mday %in% c(0,8,9,NA)))){
  if (is.na(df[i,"ipaq_ts_mdhrs"])&is.na(df[i,"ipaq_ts_mdmin"])&is.na(df[i,"ipaq_ts_mwhrs"])&is.na(df[i,"ipaq_ts_mwmin"])){
    df_ipaq_pre[i,"ipaq_ts_mdhrs"] <- NA
  }
}

## 5. During the last 7 days, on how many days did you walk for at least 10 minutes at a time?
df_ipaq_pre[,c(13:16)][is.na(df_ipaq_pre[,c(13:16)])] <- ""
for (i in which(!(df_ipaq_pre$ipaq_ts_wday %in% c(0,8,9,NA)))){
  if (is.na(df[i,"ipaq_ts_wdhrs"])&is.na(df[i,"ipaq_ts_wdmin"])&is.na(df[i,"ipaq_ts_wwhrs"])&is.na(df[i,"ipaq_ts_wwmin"])){
    df_ipaq_pre[i,"ipaq_ts_wdhrs"] <- NA
  }
}

## 7. During the last 7 days, how much time did you usually spend sitting on a week day?
df_ipaq_pre[,c(17:20)][is.na(df_ipaq_pre[,c(17:20)])] <- ""
for (i in 1:nrow(df_ipaq_pre)){
  if (is.na(df[i,"ipaq_ts_sdhrs"])&is.na(df[i,"ipaq_ts_sdmin"])&is.na(df[i,"ipaq_ts_swhrs"])&is.na(df[i,"ipaq_ts_swmin"])){
    df_ipaq_pre[i,"ipaq_ts_sdhrs"] <- NA
  }
}

## replace df with df_ipaq_pre
df[,ipaq_pre_idx] <- df_ipaq_pre[-1]

# LifeSpace for HD only (except germany) 
ls_temp <- grep("^lifespace", fields)
ls_idx <- seq(ls_temp[1]+1, ls_temp[2]-1)
df[df$hd_or_healthy==2,ls_idx] <- ""
df[which(df$redcap_data_access_group=="germany"),ls_idx] <- ""
ls <- c("as_correct", fields[ls_idx])
df_ls <- df[,ls]

## If ls1=0, ingore the following fields.
ls1_idx <- which(df_ls$ls1==0)
df_ls[ls1_idx,c(3:21)][is.na(df_ls[ls1_idx,c(3:21)])] <- ""

## If ls2=0, ingore the following fields.
ls2_idx <- which(df_ls$ls2==0)
df_ls[ls2_idx,c(7:21)][is.na(df_ls[ls2_idx,c(7:21)])] <- ""

## If ls3=0, ingore the following fields.
ls3_idx <- which(df_ls$ls3==0)
df_ls[ls3_idx,c(11:21)][is.na(df_ls[ls3_idx,c(11:21)])] <- ""

## If ls4=0, ingore the following fields.
ls4_idx <- which(df_ls$ls4==0)
df_ls[ls4_idx,c(15:21)][is.na(df_ls[ls4_idx,c(15:21)])] <- ""

## If ls5=0, ingore the following fields.
ls5_idx <- which(df_ls$ls5==0)
df_ls[ls5_idx,c(19:21)][is.na(df_ls[ls5_idx,c(19:21)])] <- ""

## replace df with df_ls
df[,ls_idx] <- df_ls[-1]

# Non-UHDRS Sitting Tasks + 2 Minute Walk Distance + Balance and Walking Cognitive Records + UHDRS
## motor
## IW11TC - motor_diagnostic / IW13GHI - motor_dystonia_trunk.
motor <- fields[251:282]
df_motor <- df[motor]
df_motor[df_motor >= 98] <- NA
TMS <- rowSums(df_motor)

## uhdrs
df$uhdrs_occupation1 <- df$uhdrs_occupation1 - 1 # fix functional capacity scale
uhdrs <- fields[283:287]
df_uhdrs <- df[uhdrs]
df_uhdrs[df_uhdrs >= 98] <- NA
TFC <- rowSums(df_uhdrs)

## func
func <- fields[289:313]
df_func <- df[func]
df_func[df_func >= 98] <- NA
FA <- rowSums(df_func)

# IPAQ Short Last 7 Days Telephone POST for everyone
ipaq_post_temp <- grep("^ipaq_short_last_7_days_telephone_post", fields)
ipaq_post_idx <- seq(ipaq_post_temp[1]+1, ipaq_post_temp[2]-1)
ipaq_post <- c("as_correct", fields[ipaq_post_idx])
df_ipaq_post <- df[,ipaq_post]

## At least one value is required in the following 4 fields for question 1/3/5/7. 
## If all the values are missing, report NA in the first field (Hours per day).

## 1. During the last 7 days, on how many days did you do vigorous physical activities?
df_ipaq_post[,c(3:6)][is.na(df_ipaq_post[,c(3:6)])] <- ""
for (i in which(!(df_ipaq_post$ipaq_ts_vday_post %in% c(0,8,9,NA)))){
  if (is.na(df[i,"ipaq_ts_vdhrs_post"])&is.na(df[i,"ipaq_ts_vdmin_post"])&is.na(df[i,"ipaq_ts_vwhrs_post"])&is.na(df[i,"ipaq_ts_vwmin_post"])){
    df_ipaq_post[i,"ipaq_ts_vdhrs_post"] <- NA
  }
}

## 3. During the last 7 days, on how many days did you do moderate physical activities?
df_ipaq_post[,c(8:11)][is.na(df_ipaq_post[,c(8:11)])] <- ""
for (i in which(!(df_ipaq_post$ipaq_ts_mday_post %in% c(0,8,9,NA)))){
  if (is.na(df[i,"ipaq_ts_mdhrs_post"])&is.na(df[i,"ipaq_ts_mdmin_post"])&is.na(df[i,"ipaq_ts_mwhrs_post"])&is.na(df[i,"ipaq_ts_mwmin_post"])){
    df_ipaq_post[i,"ipaq_ts_mdhrs_post"] <- NA
  }
}

## 5. During the last 7 days, on how many days did you walk for at least 10 minutes at a time?
df_ipaq_post[,c(13:16)][is.na(df_ipaq_post[,c(13:16)])] <- ""
for (i in which(!(df_ipaq_post$ipaq_ts_wday_post %in% c(0,8,9,NA)))){
  if (is.na(df[i,"ipaq_ts_wdhrs_post"])&is.na(df[i,"ipaq_ts_wdmin_post"])&is.na(df[i,"ipaq_ts_wwhrs_post"])&is.na(df[i,"ipaq_ts_wwmin_post"])){
    df_ipaq_post[i,"ipaq_ts_wdhrs_post"] <- NA
  }
}

## 7. During the last 7 days, how much time did you usually spend sitting on a week day?
df_ipaq_post[,c(17:20)][is.na(df_ipaq_post[,c(17:20)])] <- ""
for (i in 1:nrow(df_ipaq_post)){
  if (is.na(df[i,"ipaq_ts_sdhrs_post"])&is.na(df[i,"ipaq_ts_sdmin_post"])&is.na(df[i,"ipaq_ts_swhrs_post"])&is.na(df[i,"ipaq_ts_swmin_post"])){
    df_ipaq_post[i,"ipaq_ts_sdhrs_post"] <- NA
  }
}

## replace df with df_ipaq_post
df[,ipaq_post_idx] <- df_ipaq_post[-1]

# Telephone Interview POST for everyone
## 5
phone_4_idx <- which(df$phone_4==0)
df$phone_5[phone_4_idx] <- ""

## 8
phone_7_idx <- which(df$phone_7___10!=1)
df$phone_8[phone_7_idx] <- ""

## 10&11
phone_9_idx <- which(df$phone_9==0)
df$phone_10[phone_9_idx] <- ""
df$phone_11[phone_9_idx] <- ""

## 13
df$phone_13[is.na(df$phone_13)] <- ""

# History of Falls Questionnaire POST for HD only
hfq_post_temp <- grep("^history_of_falls_questionnaire_post", fields)
hfq_post_idx <- seq(hfq_post_temp[1]+1, hfq_post_temp[2]-1)
df[df$hd_or_healthy==2,hfq_post_idx] <- ""
hfq_post <- c("as_correct", fields[hfq_post_idx])
df_hfq_post <- df[,hfq_post]

## 2
## If hfq_2_post=0, ingore the following fields.
hfq_2_post_idx <- which(df_hfq_post$hfq_2_post==0 | is.na(df_hfq_post$hfq_2_post))
df_hfq_post[hfq_2_post_idx,c(3:63)] <- "" 

## hfq_7_changed_post
hfq_7_post_idx <- which(df_hfq_post$hfq_7_post==0)
df_hfq_post$hfq_7_changed_post[hfq_7_post_idx] <- "" 

## 15&17
hfq_9_post_idx <- which(df_hfq_post$hfq_9_post==0)
df_hfq_post[hfq_9_post_idx,c(55:63)] <- "" 

## specify
specify <- grep("specify", colnames(df_hfq_post))
df_hfq_post[,specify][is.na(df_hfq_post[,specify])] <- ""

## replace df with df_hfq_post
df[,hfq_post_idx] <- df_hfq_post[-1]

# Falls and Trips Questionnaire for HD only
ftq_temp <- grep("^falls_and_trips_questionnaire", fields)
ftq_idx <- seq(ftq_temp[1]+1, ftq_temp[2]-1)
df[df$hd_or_healthy==2,ftq_idx] <- ""

# Adverse Event (AE) and Serious Adverse Event (SAE) Reporting Form for HD only
ae_temp <- grep("^adverse_event_ae_and_serious_adverse_event_sae_rep", fields)
ae_idx <- seq(ae_temp[1]+1, ae_temp[2])
df[df$hd_or_healthy==2,ae_idx] <- ""

# add age column
age <- 2018 - as.numeric(substr(df$bi_birthdate, 1, 4))
df <- add_column(df, age, .after="bi_birthdate")

# add WHO DAS score column
df <- add_column(df, whodas_score, .after="whodas_15b")
whodas_col <- c(whodas_col, "whodas_score")
df[df$hd_or_healthy==2,whodas_col] <- ""

# add IPAQ PRE score column
met_pre <- read.xlsx("IPAQ-SFScoring(pre).xlsx", 2, startRow=6)
met_pre <- met_pre[1:73,c(1,24)]
colnames(met_pre) <- c("id","Total")
ipaq_score_pre <- met_pre$Total[match(df$as_correct,met_pre$id)]
df <- add_column(df, ipaq_score_pre, .after="ipaq_short_last_7_days_telephone_pre_complete")

# add LifeSpace score column
ls <- read.csv("assessment_ls_1022.csv")
ls_score <- ls$total_score[match(df$as_correct,ls$as_correct)]
df <- add_column(df, ls_score, .after="lifespace_complete")
df$ls_score[is.na(df$ls_score)] <- ""

# add TMS/TFC/FA column
df <- add_column(df, TMS, .after="motor_diagnostic")
df <- add_column(df, TFC, .after="uhdrs_carelevel")
df <- add_column(df, FA, .after="func_assess_25")

# add Balance_Subscore column
# Balance_Subscore = Sum of Tandem Walk and Retropulsion Pull Tests from UHDRS Motor 
Balance_Subscore = df$motor_tandem_walking + df$motor_retropulsion
df <- add_column(df, Balance_Subscore, .after="TMS")

# add Dystonia_Subscore column
# Dystonia_Subscore = Sum of Maximal Dystonia and Rigidity
Dystonia_Subscore = df$motor_dystonia_trunk + df$motor_dystonia_rue + df$motor_dystonia_lue +
  df$motor_dystonia_rle + df$motor_dystonia_lle + df$motor_rigid_arm_right + df$motor_rigid_arm_left
df <- add_column(df, Dystonia_Subscore, .after="Balance_Subscore")

# add Chorea_Subscore column
# Chorea_Subscore = Sum of Maximal Chorea Measures (i.e, Face, BOL, Trunk, RUE, LUE, RLE, LLE)  
Chorea_Subscore = df$motor_chorea_face + df$motor_chorea_bol + df$motor_chorea_trunk +
  df$motor_chorea_rue + df$motor_chorea_lue + df$motor_chorea_rle + df$motor_chorea_lle
df <- add_column(df, Chorea_Subscore, .after="Dystonia_Subscore")

# add Chorea_Subscore_Trunk_UE column
# Chorea_Subscore_Trunk_UE = Sum of Maximal Chorea Measures (i.e, Trunk, RUE, LUE)  
Chorea_Subscore_Trunk_UE = df$motor_chorea_trunk + df$motor_chorea_rue + df$motor_chorea_lue
df <- add_column(df, Chorea_Subscore_Trunk_UE, .after="Chorea_Subscore")

# add Chorea_Eye_Subscore column
Chorea_Eye_Subscore = df$motor_ocular_horiz + df$motor_ocular_vert + 
  df$motor_sacc_int_horiz + df$saccade_int_vert  + df$motor_sacc_veloc_horiz + df$motor_sacc_veloc_vert
df <- add_column(df, Chorea_Eye_Subscore, .after="Chorea_Subscore_Trunk_UE")

# add Chorea_Total_Subscore column
Chorea_Total_Subscore = df$Chorea_Subscore + df$Chorea_Eye_Subscore
df <- add_column(df, Chorea_Total_Subscore, .after="Chorea_Eye_Subscore")

# add IPAQ POST score column
met_post <- read.xlsx("IPAQ-SFScoring(post).xlsx", 2, startRow=6)
met_post <- met_post[1:66,c(1,24)]
colnames(met_post) <- c("id","Total")
ipaq_score_post <- met_post$Total[match(df$as_correct,met_post$id)]
df <- add_column(df, ipaq_score_post, .after="ipaq_short_last_7_days_telephone_post_complete")

# add composite_score column
iwear_composite <- read.csv("composite_score_norms.csv")
df$composite_score <- iwear_composite$composite_score[match(df$as_correct,iwear_composite$as_correct)]

# merge with iwear_step_summary_WithAvg dataset
iwear_step <- read.xlsx("iwear_step_summary_WithAvg.xlsx", 1)
iwear_step$sed_perc <- iwear_step$sed/(iwear_step$sed + iwear_step$light + iwear_step$mod + iwear_step$vig)
colnames(iwear_step)[1] <- "as_correct"
df <- merge(df, iwear_step, all.x = TRUE)

# delete timestamp columns
timestamp <- grep("timestamp", colnames(df))
df <- df[,-timestamp]

# delete complete columns
complete <- grep("complete", colnames(df))
df <- df[,-complete]

# delete redcap_survey_identifier column
df <- df[,-3]

write.csv(df, "iWear_complete_0625.csv", row.names=F)