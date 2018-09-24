library(tibble)
setwd("/Users/iris/Desktop/NRL/iWear")

##replace blank/N/ N/A /none with NA
df <- read.csv("HDIWear_DATA_2018-06-22.csv", stringsAsFactors=FALSE, na.strings=c("",NA))
df[df == "N" | df == "N/A" | df == "none"] <- NA
fields <- colnames(df)
##notes
notes <- grep("notes", fields)
df[,notes][is.na(df[,notes])] <- ""

##HD
df_HD <- df[which(df$hd_or_healthy==1),]

#function: return the record and field of each missing value
f_na <- function(df){
  fields <- colnames(df)
  df_idx <- as.data.frame(which(is.na(df), arr.ind=T))
  df_idx <- df_idx[order(df_idx$row),]
  df_na <- data.frame(record=character(),
                      field=character(),
                      stringsAsFactors=F)
  for (i in 1:nrow(df_idx)){
    idx <- df_idx[i,]
    df_na[i,"record"] <- as.character(df[idx$row,"as_correct"])
    df_na[i,"field"] <- fields[idx$col]
  }
  return(df_na)
}

#basic information for everyone
##part 1: without medications
bi_temp <- grep("^basic_information", fields)
bi_med <- which(colnames(df)=="bi_medications")
bi_idx_1 <- seq(bi_temp[1]+1, bi_med-1)
bi_1 <- c("as_correct", fields[bi_idx_1], "basic_information_31e1af_complete")
df_bi_1 <- df[,bi_1]

##If bi_carepartner=2 or bi_carepartner is NA, ignore the NA in bi_payedcare.
table(df[,"bi_carepartner"], exclude=NULL)
nopartner_idx <- which(df_bi_1[,"bi_carepartner"]==2 | is.na(df_bi_1[,"bi_carepartner"]))
payedcare_na_idx <- which(is.na(df_bi_1[,"bi_payedcare"]))
care_idx <- intersect(nopartner_idx, payedcare_na_idx)
df_bi_1$bi_payedcare[care_idx] <- "" ##replace NA with blank

res_bi_1 <- f_na(df_bi_1)

##part 2: medications
#bi_idx_2 <- seq(bi_med, bi_temp[2]-1)
#bi_2 <- c("as_correct",fields[bi_idx_2])
#df_bi_2 <- df[,bi_2]

##For there are so many details in medications, 
##only determine whether the number of drugnames equal to the bi_medications. 
bi_idx_2 <- grep("^bi_drugname", fields)
bi_2 <- c("as_correct", "bi_medications", fields[bi_idx_2])
df_bi_2 <- df[,bi_2]

##replace NA with blank
for (i in 1:nrow(df_bi_2)){
  if (df_bi_2[i,"bi_medications"]==0 | is.na(df_bi_2[i,"bi_medications"])){
    df_bi_2[i,-c(1:2)] <- ""
  }
  else if (df_bi_2[i,"bi_medications"]==1){
    df_bi_2[i,-c(1:3)] <- ""
  }
  else if (df_bi_2[i,"bi_medications"]==2){
    df_bi_2[i,-c(1:4)] <- ""
  }
  else if (df_bi_2[i,"bi_medications"]==3){
    df_bi_2[i,-c(1:5)] <- ""
  } 
}

res_bi_2 <- f_na(df_bi_2)

#WHO DAS for HD only
whodas_temp <- grep("^who_das", fields)
whodas_idx <- seq(whodas_temp[1]+1, whodas_temp[2])
whodas <- c("as_correct", fields[whodas_idx])
df_HD_whodas <- df_HD[,whodas]
res_whodas <- f_na(df_HD_whodas)

#History of Falls Questionnaire PRE for HD only
hfq_temp <- grep("^history_of_falls_questionnaire_pre", fields)
hfq_idx <- seq(hfq_temp[1]+1, hfq_temp[2])
hfq <- c("as_correct", fields[hfq_idx])
df_HD_hfq <- df_HD[,hfq]

##1. Do you use an assistive device to walk?
hfq_1_specify_idx <- which(df_HD_hfq$hfq_1_yes___6==0)
df_HD_hfq$hfq_1_specify[hfq_1_specify_idx] <- ""

##2. Have you fallen in the past six months?
##If hfq_2=0, ingore the following fields.
hfq_2_no_idx <- which(df_HD_hfq$hfq_2==0)
hfq_2_field_idx <- seq(hfq_temp[1]+10, hfq_temp[2]-2)
hfq_2_field <- fields[hfq_2_field_idx]
df_HD_hfq[hfq_2_no_idx,hfq_2_field] <- "" 

##3. What were you doing at the time of your most serious fall?
hfq_3_specify_idx <- which(df_HD_hfq$hfq_3!=12)
df_HD_hfq$hfq_3_specify[hfq_3_specify_idx] <- "" 

##4. What happened or caused you to fall on that occasion?
hfq_4_specify_idx <- which(df_HD_hfq$hfq_4___21==0)
df_HD_hfq$hfq_4_specify[hfq_4_specify_idx] <- ""

##6. Did objects or conditions in the environment contribute to the fall
##If hfq_6=0, ingore the following fields.
hfq_6_no_idx <- which(df_HD_hfq$hfq_6==0)
fields_hfq <- colnames(df_HD_hfq) 
hfq_6_field_idx <- grep("hfq_6",fields_hfq)
hfq_6_field <- fields_hfq[hfq_6_field_idx][-1]
df_HD_hfq[hfq_6_no_idx,hfq_6_field] <- ""
hfq_6_specify_idx <- which(df_HD_hfq$hfq_6_specify___14==0)
df_HD_hfq$hfq_6_otherspecify[hfq_6_specify_idx] <- ""

##7. Have you changed anything to prevent reoccurrence?
hfq_7_specify_idx <- which(df_HD_hfq$hfq_7==0)
df_HD_hfq$hfq_7_specify[hfq_7_specify_idx] <- ""

##13. Have you completely recovered to the point that you feel like yourself again?
hfq_13_no_idx <- which(df_HD_hfq$hfq_13==0)
df_HD_hfq$hfq_13_yes[hfq_13_no_idx] <- ""

##15A. For your most serious injury, what type of injury did you suffer? 
hfq_15a_specify_idx <- which(df_HD_hfq$hfq_15a!=6)
df_HD_hfq$hfq_15a_specify[hfq_15a_specify_idx] <- ""

##15B. What was the location of your most serious injury? 
hfq_15b_specify_idx <- which(df_HD_hfq$hfq_15b!=23)
df_HD_hfq$hfq_15b_specify[hfq_15b_specify_idx] <- ""

##17A. For your second most serious injury, what type of injury did you suffer?
hfq_17a_specify_idx <- which(df_HD_hfq$hfq_17a!=6)
df_HD_hfq$hfq_17a_specify[hfq_17a_specify_idx] <- ""

##17B. What was the location of your second most serious injury?
hfq_17b_specify_idx <- which(df_HD_hfq$hfq_17a!=23)
df_HD_hfq$hfq_17b_specify[hfq_17b_specify_idx] <- ""

res_hfq <- f_na(df_HD_hfq)

#Functional Ambulation Category for HD only
func_temp <- grep("^functional_ambulation_category", fields)
func_idx <- seq(func_temp[1]+1, func_temp[2])
func <- c("as_correct", fields[func_idx])
df_HD_func <- df_HD[,func]
res_func <- f_na(df_HD_func)

#IPAQ Short Last 7 Days PRE for everyone
ipaq_pre_temp <- grep("^ipaq_short_last_7_days_telephone_pre", fields)
ipaq_pre_idx <- seq(ipaq_pre_temp[1]+1, ipaq_pre_temp[2])
ipaq_pre <- c("as_correct", fields[ipaq_pre_idx])
df_ipaq_pre <- df[,ipaq_pre]

##At least one value is required in the following 4 fields for question 1/3/5/7. 
##If all the values are missing, report NA in the first field (Hours per day).

##1. During the last 7 days, on how many days did you do vigorous physical activities?
df_ipaq_pre[,c(3:6)][is.na(df_ipaq_pre[,c(3:6)])] <- ""
for (i in which(!(df_ipaq_pre$ipaq_ts_vday %in% c(0,8,9,NA)))){
  if (is.na(df[i,"ipaq_ts_vdhrs"])&is.na(df[i,"ipaq_ts_vdmin"])&is.na(df[i,"ipaq_ts_vwhrs"])&is.na(df[i,"ipaq_ts_vwmin"])){
    df_ipaq_pre[i,"ipaq_ts_vdhrs"] <- NA
  }
}

##3. During the last 7 days, on how many days did you do moderate physical activities?
df_ipaq_pre[,c(8:11)][is.na(df_ipaq_pre[,c(8:11)])] <- ""
for (i in which(!(df_ipaq_pre$ipaq_ts_mday %in% c(0,8,9,NA)))){
  if (is.na(df[i,"ipaq_ts_mdhrs"])&is.na(df[i,"ipaq_ts_mdmin"])&is.na(df[i,"ipaq_ts_mwhrs"])&is.na(df[i,"ipaq_ts_mwmin"])){
    df_ipaq_pre[i,"ipaq_ts_mdhrs"] <- NA
  }
}

##5. During the last 7 days, on how many days did you walk for at least 10 minutes at a time?
df_ipaq_pre[,c(13:16)][is.na(df_ipaq_pre[,c(13:16)])] <- ""
for (i in which(!(df_ipaq_pre$ipaq_ts_wday %in% c(0,8,9,NA)))){
  if (is.na(df[i,"ipaq_ts_wdhrs"])&is.na(df[i,"ipaq_ts_wdmin"])&is.na(df[i,"ipaq_ts_wwhrs"])&is.na(df[i,"ipaq_ts_wwmin"])){
    df_ipaq_pre[i,"ipaq_ts_wdhrs"] <- NA
  }
}

##7. During the last 7 days, how much time did you usually spend sitting on a week day?
df_ipaq_pre[,c(17:20)][is.na(df_ipaq_pre[,c(17:20)])] <- ""
for (i in 1:nrow(df_ipaq_pre)){
  if (is.na(df[i,"ipaq_ts_sdhrs"])&is.na(df[i,"ipaq_ts_sdmin"])&is.na(df[i,"ipaq_ts_swhrs"])&is.na(df[i,"ipaq_ts_swmin"])){
    df_ipaq_pre[i,"ipaq_ts_sdhrs"] <- NA
  }
}

res_ipaq_pre <- f_na(df_ipaq_pre)

# res_ipaq_pre <- add_column(res_ipaq_pre, rep(NA,nrow(res_ipaq_pre)), .after=1)
# colnames(res_ipaq_pre)[2] <- "hd_or_healthy"
# for (i in 1:nrow(res_ipaq_pre)){
#   res_ipaq_pre$hd_or_healthy[i]  <- df$hd_or_healthy[which(df$as_correct==res_ipaq_pre$record[i])]
# }
# write.csv(res_ipaq_pre, "missingvalue_ipaq_pre.csv", row.names=F)

#LifeSpace for HD only (except germany) 
ls_temp <- grep("^lifespace", fields)
ls_idx <- seq(ls_temp[1]+1, ls_temp[2])
ls <- c("as_correct", fields[ls_idx])
germany_idx <- which(df_HD$redcap_data_access_group=="germany")
df_HD_ls <- df_HD[-germany_idx,ls]

##If ls1=0, ingore the following fields.
ls1_idx <- which(df_HD_ls$ls1==0)
df_HD_ls[ls1_idx,c(3:21)][is.na(df_HD_ls[ls1_idx,c(3:21)])] <- ""

##If ls2=0, ingore the following fields.
ls2_idx <- which(df_HD_ls$ls2==0)
df_HD_ls[ls2_idx,c(7:21)][is.na(df_HD_ls[ls2_idx,c(7:21)])] <- ""

##If ls3=0, ingore the following fields.
ls3_idx <- which(df_HD_ls$ls3==0)
df_HD_ls[ls3_idx,c(11:21)][is.na(df_HD_ls[ls3_idx,c(11:21)])] <- ""

##If ls4=0, ingore the following fields.
ls4_idx <- which(df_HD_ls$ls4==0)
df_HD_ls[ls4_idx,c(15:21)][is.na(df_HD_ls[ls4_idx,c(15:21)])] <- ""

##If ls5=0, ingore the following fields.
ls5_idx <- which(df_HD_ls$ls5==0)
df_HD_ls[ls5_idx,c(19:21)][is.na(df_HD_ls[ls5_idx,c(19:21)])] <- ""

res_ls <- f_na(df_HD_ls)
write.csv(res_ls, "missingvalue_ls.csv", row.names=F)

#Sitting and Walking for everyone
sw_start <- grep("sitting_alphabet_every_other_letter_timestamp", fields)
sw_end <- grep("uhdrs_complete", fields)
sw_temp <- fields[sw_start:sw_end]
sw_timestamp <- grep("timestamp",sw_temp)
sw <- c("as_correct", sw_temp[-sw_timestamp])
df_sw <- df[,sw]
res_sw <- f_na(df_sw)

#IPAQ Short Last 7 Days Telephone POST for everyone
ipaq_post_temp <- grep("^ipaq_short_last_7_days_telephone_post", fields)
ipaq_post_idx <- seq(ipaq_post_temp[1]+1, ipaq_post_temp[2])
ipaq_post <- c("as_correct", fields[ipaq_post_idx])
df_ipaq_post <- df[,ipaq_post]

##At least one value is required in the following 4 fields for question 1/3/5/7. 
##If all the values are missing, report NA in the first field (Hours per day).

##1. During the last 7 days, on how many days did you do vigorous physical activities?
df_ipaq_post[,c(3:6)][is.na(df_ipaq_post[,c(3:6)])] <- ""
for (i in which(!(df_ipaq_post$ipaq_ts_vday_post %in% c(0,8,9,NA)))){
  if (is.na(df[i,"ipaq_ts_vdhrs_post"])&is.na(df[i,"ipaq_ts_vdmin_post"])&is.na(df[i,"ipaq_ts_vwhrs_post"])&is.na(df[i,"ipaq_ts_vwmin_post"])){
    df_ipaq_post[i,"ipaq_ts_vdhrs_post"] <- NA
  }
}

##3. During the last 7 days, on how many days did you do moderate physical activities?
df_ipaq_post[,c(8:11)][is.na(df_ipaq_post[,c(8:11)])] <- ""
for (i in which(!(df_ipaq_post$ipaq_ts_mday_post %in% c(0,8,9,NA)))){
  if (is.na(df[i,"ipaq_ts_mdhrs_post"])&is.na(df[i,"ipaq_ts_mdmin_post"])&is.na(df[i,"ipaq_ts_mwhrs_post"])&is.na(df[i,"ipaq_ts_mwmin_post"])){
    df_ipaq_post[i,"ipaq_ts_mdhrs_post"] <- NA
  }
}

##5. During the last 7 days, on how many days did you walk for at least 10 minutes at a time?
df_ipaq_post[,c(13:16)][is.na(df_ipaq_post[,c(13:16)])] <- ""
for (i in which(!(df_ipaq_post$ipaq_ts_wday_post %in% c(0,8,9,NA)))){
  if (is.na(df[i,"ipaq_ts_wdhrs_post"])&is.na(df[i,"ipaq_ts_wdmin_post"])&is.na(df[i,"ipaq_ts_wwhrs_post"])&is.na(df[i,"ipaq_ts_wwmin_post"])){
    df_ipaq_post[i,"ipaq_ts_wdhrs_post"] <- NA
  }
}

##7. During the last 7 days, how much time did you usually spend sitting on a week day?
df_ipaq_post[,c(17:20)][is.na(df_ipaq_post[,c(17:20)])] <- ""
for (i in 1:nrow(df_ipaq_post)){
  if (is.na(df[i,"ipaq_ts_sdhrs_post"])&is.na(df[i,"ipaq_ts_sdmin_post"])&is.na(df[i,"ipaq_ts_swhrs_post"])&is.na(df[i,"ipaq_ts_swmin_post"])){
    df_ipaq_post[i,"ipaq_ts_sdhrs_post"] <- NA
  }
}

res_ipaq_post <- f_na(df_ipaq_post)

# res_ipaq_post <- add_column(res_ipaq_post, rep(NA,nrow(res_ipaq_post)), .after=1)
# colnames(res_ipaq_post)[2] <- "hd_or_healthy"
# for (i in 1:nrow(res_ipaq_post)){
#   res_ipaq_post$hd_or_healthy[i]  <- df$hd_or_healthy[which(df$as_correct==res_ipaq_post$record[i])]
# }
# write.csv(res_ipaq_post, "missingvalue_ipaq_post.csv", row.names=F)

#Telephone Interview POST for everyone
phone_temp <- grep("^telephone_interview", fields)
phone_idx <- seq(phone_temp[1]+1, phone_temp[2])
phone <- c("as_correct", fields[phone_idx])
df_phone <- df[,phone]

##5
phone_4_idx <- which(df_phone$phone_4==0)
df_phone$phone_5[phone_4_idx] <- ""

##8
phone_7_idx <- which(df_phone$phone_7___10!=1)
df_phone$phone_8[phone_7_idx] <- ""

##10&11
phone_9_idx <- which(df_phone$phone_9==0)
df_phone$phone_10[phone_9_idx] <- ""
df_phone$phone_11[phone_9_idx] <- ""

##13
df_phone$phone_13[is.na(df_phone$phone_13)] <- ""

res_phone <- f_na(df_phone)

#History of Falls Questionnaire POST for HD only
hfq_post_temp <- grep("^history_of_falls_questionnaire_post", fields)
hfq_post_idx <- seq(hfq_post_temp[1]+1, hfq_post_temp[2])
hfq_post <- c("as_correct", fields[hfq_post_idx])
df_HD_hfq_post <- df_HD[,hfq_post]

##2
##If hfq_2_post=0, ingore the following fields.
hfq_2_post_idx <- which(df_HD_hfq_post$hfq_2_post==0 | is.na(df_HD_hfq_post$hfq_2_post))
df_HD_hfq_post[hfq_2_post_idx,c(3:63)] <- "" 

##hfq_7_changed_post
hfq_7_post_idx <- which(df_HD_hfq_post$hfq_7_post==0)
df_HD_hfq_post$hfq_7_changed_post[hfq_7_post_idx] <- "" 

##15&17
hfq_9_post_idx <- which(df_HD_hfq_post$hfq_9_post==0)
df_HD_hfq_post[hfq_9_post_idx,c(55:63)] <- "" 

##specify
specify <- grep("specify", colnames(df_HD_hfq_post))
df_HD_hfq_post[,specify][is.na(df_HD_hfq_post[,specify])] <- ""

res_hfq_post <- f_na(df_HD_hfq_post)

#Falls and Trips Questionnaire for HD only
ftq_temp <- grep("^falls_and_trips_questionnaire", fields)
ftq_idx <- seq(ftq_temp[1]+1, ftq_temp[2])
ftq <- c("as_correct", fields[ftq_idx])
df_HD_ftq <- df_HD[,ftq]

ft_idx <- which(df_HD_ftq$ftq_f1_trip==2 & df_HD_ftq$ftq_f1_fall==2)
df_HD_ftq[ft_idx,-c(3,4)] <- ""

#Adverse Event (AE) and Serious Adverse Event (SAE) Reporting Form for HD only
ae_temp <- grep("^adverse_event_ae_and_serious_adverse_event_sae_rep", fields)
ae_idx <- seq(ae_temp[1]+1, ae_temp[2])
ae <- c("as_correct", fields[ae_idx])
df_HD_ae <- df_HD[,ae]

res <- rbind(res_bi_1,res_bi_2,res_hfq,res_func,
             res_ipaq_pre,res_ipaq_post,res_ls,
             res_sw,
             res_hfq_post)
res <- res[order(res$record),]
write.csv(res,"missingvalue.csv", row.names=F)