library(tibble)
setwd("/Users/iris/Desktop/NRL/iWear")

#
##replace blank/N/ N/A /none with NA
df <- read.csv("HDIWear_DATA_2018-09-21.csv", stringsAsFactors=FALSE, na.strings=c("",NA))
df[df == "N" | df == "N/A" | df == "none"] <- NA
fields <- colnames(df)
##repalce NA with blank in notes
notes <- grep("notes", fields)
df[,notes][is.na(df[,notes])] <- ""

#calculate TMS/TFC/FA
##motor
motor <- fields[251:282]
df_motor <- df[motor]
df_motor[df_motor >= 98] <- NA
TMS <- rowSums(df_motor, na.rm=TRUE)

##uhdrs
uhdrs <- fields[283:287]
df_uhdrs <- df[uhdrs]
df_uhdrs[df_uhdrs >= 98] <- NA
TFC <- rowSums(df_uhdrs, na.rm=TRUE)

##func
func <- fields[289:313]
df_func <- df[func]
df_func[df_func >= 98] <- NA
FA <- rowSums(df_func, na.rm=TRUE)

#Functional Ambulation Category for HD only
func_temp <- grep("^functional_ambulation_category", fields)
func_idx <- seq(func_temp[1]+1, func_temp[2])
##replace NA with blank for control
df[which(df$hd_or_healthy==2),fields[func_idx]][is.na(df[which(df$hd_or_healthy==2),fields[func_idx]])] <- ""

#Sitting and Walking for everyone
##Non-UHDRS Sitting Tasks + 2 Minute Walk Distance + Balance and Walking Cognitive Records + UHDRS
sw_start <- grep("sitting_alphabet_every_other_letter_timestamp", fields)
sw_end <- grep("uhdrs_complete", fields)
sw_temp <- fields[sw_start:sw_end]
sw_timestamp <- grep("timestamp",sw_temp)

#LifeSpace for HD only (except germany) 
ls_temp <- grep("^lifespace", fields)
ls_idx <- seq(ls_temp[1]+1, ls_temp[2])
##replace NA with blank for control
df[which(df$hd_or_healthy==2),fields[ls_idx]][is.na(df[which(df$hd_or_healthy==2),fields[ls_idx]])] <- ""
##replace NA with blank for germany
df[which(df$redcap_data_access_group=="germany"),fields[ls_idx]][is.na(df[which(df$redcap_data_access_group=="germany"),fields[ls_idx]])] <- ""

ls <- c("as_correct", fields[ls_idx])
df_ls <- df[,ls]

##If ls1=0, ingore the following fields.
ls1_idx <- which(df_ls$ls1==0)
df_ls[ls1_idx,c(3:21)][is.na(df_ls[ls1_idx,c(3:21)])] <- ""

##If ls2=0, ingore the following fields.
ls2_idx <- which(df_ls$ls2==0)
df_ls[ls2_idx,c(7:21)][is.na(df_ls[ls2_idx,c(7:21)])] <- ""

##If ls3=0, ingore the following fields.
ls3_idx <- which(df_ls$ls3==0)
df_ls[ls3_idx,c(11:21)][is.na(df_ls[ls3_idx,c(11:21)])] <- ""

##If ls4=0, ingore the following fields.
ls4_idx <- which(df_ls$ls4==0)
df_ls[ls4_idx,c(15:21)][is.na(df_ls[ls4_idx,c(15:21)])] <- ""

##If ls5=0, ingore the following fields.
ls5_idx <- which(df_ls$ls5==0)
df_ls[ls5_idx,c(19:21)][is.na(df_ls[ls5_idx,c(19:21)])] <- ""

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

#combine
idx <- c("as_correct", "hd_or_healthy", "bi_sex", "bi_birthdate",
         sw_temp[-sw_timestamp], fields[func_idx])
df2 <- df[,idx]
df2 <- add_column(df2, TMS, .after = 4)
df2 <- add_column(df2, TFC, .after = 5)
df2 <- add_column(df2, FA, .after = 6)

df3 <- merge(df2, df_ipaq_pre)
df3 <- merge(df3, df_ipaq_post)
df3 <- merge(df3, df_ls)

write.csv(df3, "iWear_0921.csv", row.names=F)

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
res <- f_na(df3)
write.csv(res, "missingvalue_0921.csv", row.names=F)