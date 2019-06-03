library(mice)
setwd("~/Desktop/NRL/IPD")

load("imp100.Rdata")
df <- complete(imp100,1)

df$responder <- ifelse((df$modmotscpost - df$modmotscpre) >= 3,
                       "responder",
                       "non-responder")

table(df$responder)

table(df$Study)

# MtoE
table(df[which(df$Study=="MtoE"),"responder"])
# COMMET
table(df[which(df$Study=="COMMET"),"responder"])
# TRAIN
table(df[which(df$Study=="TRAIN"),"responder"])
# EXERT
table(df[which(df$Study=="EXERT"),"responder"])
# ENGAGE
table(df[which(df$Study=="ENGAGE"),"responder"])
