library(rJava)
library(xlsxjars)
library(xlsx)
library(tibble)

setwd("/Users/iris/Desktop/NRL/iWear")
df <- read.csv("HDIWear_DATA_2018-06-22.csv", stringsAsFactors=FALSE, na.strings=c("",NA))
fields <- colnames(df)

#IPAQ Short Last 7 Days Telephone POST for everyone
ipaq_post_temp <- grep("^ipaq_short_last_7_days_telephone_post", fields)
ipaq_post_idx <- seq(ipaq_post_temp[1]+1, ipaq_post_temp[2])
ipaq_post <- c("as_correct", fields[ipaq_post_idx])
df_ipaq_post <- df[,ipaq_post]

#IPAQ-SF Response
##Vigorous Activity
df_ipaq_post$ipaq_ts_vdhrs_post[!is.na(df_ipaq_post$ipaq_ts_vdmin_post) & is.na(df_ipaq_post$ipaq_ts_vdhrs_post)] <- 0
df_ipaq_post$ipaq_ts_vdmin_post[!is.na(df_ipaq_post$ipaq_ts_vdhrs_post) & is.na(df_ipaq_post$ipaq_ts_vdmin_post)] <- 0
df_ipaq_post$ipaq_ts_vdmin_post <- df_ipaq_post$ipaq_ts_vdhrs_post*60 + df_ipaq_post$ipaq_ts_vdmin_post
df_ipaq_post <- df_ipaq_post[,-c(3,5,6)]

##Moderate Activity
df_ipaq_post$ipaq_ts_mdhrs_post[!is.na(df_ipaq_post$ipaq_ts_mdmin_post) & is.na(df_ipaq_post$ipaq_ts_mdhrs_post)] <- 0
df_ipaq_post$ipaq_ts_mdmin_post[!is.na(df_ipaq_post$ipaq_ts_mdhrs_post) & is.na(df_ipaq_post$ipaq_ts_mdmin_post)] <- 0
df_ipaq_post$ipaq_ts_mdmin_post <- df_ipaq_post$ipaq_ts_mdhrs_post*60 + df_ipaq_post$ipaq_ts_mdmin_post
df_ipaq_post <- df_ipaq_post[,-c(5,7,8)]

##Walking Activity
df_ipaq_post$ipaq_ts_wdhrs_post[!is.na(df_ipaq_post$ipaq_ts_wdmin_post) & is.na(df_ipaq_post$ipaq_ts_wdhrs_post)] <- 0
df_ipaq_post$ipaq_ts_wdmin_post[!is.na(df_ipaq_post$ipaq_ts_wdhrs_post) & is.na(df_ipaq_post$ipaq_ts_wdmin_post)] <- 0
df_ipaq_post$ipaq_ts_wdmin_post <- df_ipaq_post$ipaq_ts_wdhrs_post*60 + df_ipaq_post$ipaq_ts_wdmin_post
df_ipaq_post <- df_ipaq_post[,-c(7,9,10)]

##Sitting(hours pey day)
df_ipaq_post$ipaq_ts_sdhrs_post[!is.na(df_ipaq_post$ipaq_ts_sdmin_post) & is.na(df_ipaq_post$ipaq_ts_sdhrs_post)] <- 0
df_ipaq_post$ipaq_ts_sdmin_post[!is.na(df_ipaq_post$ipaq_ts_sdhrs_post) & is.na(df_ipaq_post$ipaq_ts_sdmin_post)] <- 0
#19-13 same information in sdhrs(16) and sdhrs(960)
for (i in which(!(df_ipaq_post$ipaq_ts_sdmin_post %in% c(NA,0,960)))){ 
  df_ipaq_post$ipaq_ts_sdhrs_post[i] <- df_ipaq_post$ipaq_ts_sdhrs_post[i] + df_ipaq_post$ipaq_ts_sdmin_post[i]/60 
}
df_ipaq_post <- df_ipaq_post[,-c(9:13)]

#Missing Value
for (i in which(df_ipaq_post$ipaq_ts_vday_post %in% c(0,8,9))){
  if (is.na(df[i,"ipaq_ts_vdmin_post"])){
    df_ipaq_post[i,"ipaq_ts_vdmin_post"] <- 0
  }
}

for (i in which(df_ipaq_post$ipaq_ts_mday_post %in% c(0,8,9))){
  if (is.na(df[i,"ipaq_ts_mdmin_post"])){
    df_ipaq_post[i,"ipaq_ts_mdmin_post"] <- 0
  }
}

for (i in which(df_ipaq_post$ipaq_ts_wday_post %in% c(0,8,9))){
  if (is.na(df[i,"ipaq_ts_wdmin_post"])){
    df_ipaq_post[i,"ipaq_ts_wdmin_post"] <- 0
  }
}

df_ipaq_post <- na.omit(df_ipaq_post)
write.csv(df_ipaq_post, "assessment_ipaq_post.csv", row.names=F)

#MET-Minutes
met <- read.xlsx("IPAQ-SFScoring(post).xlsx", 2, startRow=6)
met <- met[1:65,c(1,24)]
met <- add_column(met, rep(NA,nrow(met)), .after=1)
colnames(met) <- c("id","hd_or_healthy","Total")
for (i in 1:nrow(met)){
  met$hd_or_healthy[i]  <- df$hd_or_healthy[which(df$as_correct==met$id[i])]
}

sum(met$hd_or_healthy==1)
sum(met$hd_or_healthy==2)
aggregate(Total~hd_or_healthy, met, mean)
aggregate(Total~hd_or_healthy, met, sd)
