library(rJava)
library(xlsxjars)
library(xlsx)
library(tibble)

setwd("/Users/iris/Desktop/NRL/iWear")
df <- read.csv("HDIWear_DATA_2018-06-22.csv", stringsAsFactors=FALSE, na.strings=c("",NA))
fields <- colnames(df)

#IPAQ Short Last 7 Days PRE for everyone
ipaq_pre_temp <- grep("^ipaq_short_last_7_days_telephone_pre", fields)
ipaq_pre_idx <- seq(ipaq_pre_temp[1]+1, ipaq_pre_temp[2])
ipaq_pre <- c("as_correct", fields[ipaq_pre_idx])
df_ipaq_pre <- df[,ipaq_pre]

#IPAQ-SF Response
##Vigorous Activity
df_ipaq_pre$ipaq_ts_vdhrs[!is.na(df_ipaq_pre$ipaq_ts_vdmin) & is.na(df_ipaq_pre$ipaq_ts_vdhrs)] <- 0
df_ipaq_pre$ipaq_ts_vdmin[!is.na(df_ipaq_pre$ipaq_ts_vdhrs) & is.na(df_ipaq_pre$ipaq_ts_vdmin)] <- 0
df_ipaq_pre$ipaq_ts_vdmin <- df_ipaq_pre$ipaq_ts_vdhrs*60 + df_ipaq_pre$ipaq_ts_vdmin
df_ipaq_pre$ipaq_ts_vdmin[which(df_ipaq_pre$ipaq_ts_vdmin==998)] <- 0 #998 = Don't Know/Not Sure
df_ipaq_pre <- df_ipaq_pre[,-c(3,5,6)]

##Moderate Activity
df_ipaq_pre$ipaq_ts_mdhrs[!is.na(df_ipaq_pre$ipaq_ts_mdmin) & is.na(df_ipaq_pre$ipaq_ts_mdhrs)] <- 0
df_ipaq_pre$ipaq_ts_mdmin[!is.na(df_ipaq_pre$ipaq_ts_mdhrs) & is.na(df_ipaq_pre$ipaq_ts_mdmin)] <- 0
df_ipaq_pre$ipaq_ts_mdmin <- df_ipaq_pre$ipaq_ts_mdhrs*60 + df_ipaq_pre$ipaq_ts_mdmin
df_ipaq_pre <- df_ipaq_pre[,-c(5,7,8)]

##Walking Activity
df_ipaq_pre$ipaq_ts_wdhrs[!is.na(df_ipaq_pre$ipaq_ts_wdmin) & is.na(df_ipaq_pre$ipaq_ts_wdhrs)] <- 0
df_ipaq_pre$ipaq_ts_wdmin[!is.na(df_ipaq_pre$ipaq_ts_wdhrs) & is.na(df_ipaq_pre$ipaq_ts_wdmin)] <- 0
df_ipaq_pre$ipaq_ts_wdmin <- df_ipaq_pre$ipaq_ts_wdhrs*60 + df_ipaq_pre$ipaq_ts_wdmin
df_ipaq_pre <- df_ipaq_pre[,-c(7,9,10)]

##Sitting(hours pey day)
df_ipaq_pre$ipaq_ts_sdhrs[!is.na(df_ipaq_pre$ipaq_ts_sdmin) & is.na(df_ipaq_pre$ipaq_ts_sdhrs)] <- 0
df_ipaq_pre$ipaq_ts_sdmin[!is.na(df_ipaq_pre$ipaq_ts_sdhrs) & is.na(df_ipaq_pre$ipaq_ts_sdmin)] <- 0
#998 = Don't Know/Not Sure
#IW1TA same information in sdhrs(4) and sdhrs(240)
for (i in which(!(df_ipaq_pre$ipaq_ts_sdmin %in% c(NA,0,998,240)))){ 
  df_ipaq_pre$ipaq_ts_sdhrs[i] <- df_ipaq_pre$ipaq_ts_sdhrs[i] + df_ipaq_pre$ipaq_ts_sdmin[i]/60 
}
df_ipaq_pre <- df_ipaq_pre[,-c(9:13)]

#Missing Value
for (i in which(df_ipaq_pre$ipaq_ts_vday %in% c(0,8,9))){
  if (is.na(df[i,"ipaq_ts_vdmin"])){
    df_ipaq_pre[i,"ipaq_ts_vdmin"] <- 0
  }
}

for (i in which(df_ipaq_pre$ipaq_ts_mday %in% c(0,8,9))){
  if (is.na(df[i,"ipaq_ts_mdmin"])){
    df_ipaq_pre[i,"ipaq_ts_mdmin"] <- 0
  }
}

for (i in which(df_ipaq_pre$ipaq_ts_wday %in% c(0,8,9))){
  if (is.na(df[i,"ipaq_ts_wdmin"])){
    df_ipaq_pre[i,"ipaq_ts_wdmin"] <- 0
  }
}

df_ipaq_pre <- na.omit(df_ipaq_pre)
write.csv(df_ipaq_pre, "assessment_ipaq_pre.csv", row.names=F)

#MET-Minutes
met <- read.xlsx("IPAQ-SFScoring(pre).xlsx", 2, startRow=6)
met <- met[1:72,c(1,24)]
met <- add_column(met, rep(NA,nrow(met)), .after=1)
colnames(met) <- c("id","hd_or_healthy","Total")
for (i in 1:nrow(met)){
  met$hd_or_healthy[i]  <- df$hd_or_healthy[which(df$as_correct==met$id[i])]
}

sum(met$hd_or_healthy==1)
sum(met$hd_or_healthy==2)
aggregate(Total~hd_or_healthy, met, mean)
aggregate(Total~hd_or_healthy, met, sd)
