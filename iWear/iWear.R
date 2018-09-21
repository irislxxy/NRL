library(tibble)
setwd("/Users/iris/Desktop/NRL/iWear")
df <- read.csv("HDIWear_DATA_2018-06-22.csv", stringsAsFactors=FALSE, na.strings=c("",NA))
fields <- colnames(df)

motor <- fields[251:282]
uhdrs <- fields[283:287]
func <- fields[289:313]

TMS <- rowSums(df[motor], na.rm=TRUE)
df <- add_column(df, TMS, .after = "motor_diagnostic")

TFC <- rowSums(df[uhdrs], na.rm=TRUE)
df <- add_column(df, TFC, .after = "uhdrs_info_source")

FA <- rowSums(df[func], na.rm=TRUE)
df <- add_column(df, FA, .after = "func_assess_25")

#df[,c("TMS","TFC","FA")][df[,c("TMS","TFC","FA")]==0 | df[,c("TMS","TFC","FA")]>=98] <- ""

maxList <- list()
minList <- list()
meanList <- list()
sdList <- list()
for (i in c(1:540)){
  maxList <- c(maxList, max(df[,i],na.rm=T))
  minList <- c(minList, min(df[,i],na.rm=T))
  meanList <- c(meanList, mean(df[,i],na.rm=T))
  sdList <- c(sdList, sd(df[,i],na.rm=T))
}
df<-rbind(df, maxList)
df<-rbind(df, minList)
df<-rbind(df, meanList)
df<-rbind(df, sdList)
df[78:81,1] <- c("max","min","mean","sd")

write.csv(df, "iWear.csv", row.names=F)