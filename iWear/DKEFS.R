library(tibble)
setwd("/Users/iris/Desktop/NRL/iWear")

df_iWear <- read.csv("HDIWear_DATA_2019-03-22.csv", stringsAsFactors=FALSE, na.strings=c("",NA))
fields_iWear <- colnames(df_iWear)

df_APDM <- read.csv("../APDM/Walk DKEFS.csv", stringsAsFactors=FALSE)
df_APDM <- df_APDM[,c(1:2)]
colnames(df_APDM)[1] <- "as_correct"
df_APDM$as_correct[1:20] <- paste0("IW",substr(df_APDM$as_correct[1:20],3,4),"GHI")

# DKEFS d_kefs
dkefs <- grep("d_kefs", fields_iWear)
df_dkefs <- df_iWear[,c(1,dkefs)]

# Check Equality
interval4 <- df_dkefs$d_kefs_1_15+df_dkefs$d_kefs_16_30+df_dkefs$d_kefs_31_45+df_dkefs$d_kefs_46_60
category2 <- df_dkefs$d_kefs_fruits+df_dkefs$d_kefs_furniture
df_dkefs <- add_column(df_dkefs, interval4==category2, .after="d_kefs_furniture")
df_dkefs <- add_column(df_dkefs, category2==df_dkefs$d_kefs_totalcorrect1, .after="d_kefs_totalcorrect1")

interval4_2 <- df_dkefs$d_kefs_1_15_2+df_dkefs$d_kefs_16_30_2+df_dkefs$d_kefs_31_45_2+df_dkefs$d_kefs_46_60_2
category2_2 <- df_dkefs$d_kefs_fruits2+df_dkefs$d_kefs_furniture2
df_dkefs <- add_column(df_dkefs, interval4_2==category2_2, .after="d_kefs_furniture2")
df_dkefs <- add_column(df_dkefs, category2_2==df_dkefs$d_kefs_totalcorrect, .after="d_kefs_totalcorrect")

write.csv(df_dkefs, "dkefs_equality_0322.csv", row.names = F)

# 
df <- merge(df_APDM, df_dkefs, by = "as_correct", sort = F)
fields <- colnames(df)
grep("d_kefs_1_15", fields)
grep("totalcorrect", fields)
df <- df[,c(1,3:6,10,14:17,22,2)] 

df$baseline <- df$d_kefs_totalcorrect1/60
df$dual_total <- df$d_kefs_totalcorrect/df$Duration..s.
df$dual_first_interval <- df$d_kefs_1_15_2/15

write.csv(df, "dkefs_crr_0322.csv", row.names = F)