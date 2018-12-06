setwd("~/Desktop/NRL/IPD")
ipd <- read.csv("IPD.csv")

v.list <- c("PID","Study","Site","Group",
            "Age","Gender",
            "modmotscpre","modmotscpost",
            "EQ5Dpre",
            "TFC",
            "UHDRStV1",
            "SDMTpre")
df <- ipd[,v.list]

# count missing data
na.count <- sapply(df, function(x) sum((is.na(x))))
na.count/158

# exclude participants with missing outcome data
df <- df[!is.na(df$modmotscpost),]

# simple random imputation
random.imp <- function(a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample(a.obs, n.missing, replace = T)
  return(imputed)
}

#  create 20 imputed datasets
for (i in c(1:20)){
  set.seed(i)
  df_new <- df
  df_new$modmotscpre <- random.imp(df$modmotscpre)
  df_new$EQ5Dpre <- random.imp(df$EQ5Dpre)
  df_new$TFC <- random.imp(df$TFC)
  df_new$UHDRStV1 <- random.imp(df$UHDRStV1)
  df_new$SDMTpre <- random.imp(df$SDMTpre)
  df_new$responder <- ifelse((df_new$modmotscpost - df_new$modmotscpre) >= 3,
                             "responder",
                             "non-responder")
  df_new$responder[df_new$Group == "control"] <- ""
  filename <- paste0("datasets/imp_", i, ".csv")
  write.csv(df_new, filename, row.names = F)
}