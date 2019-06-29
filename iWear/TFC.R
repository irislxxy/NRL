setwd("/Users/iris/Desktop/NRL/iWear")
df <- read.csv("iWear_complete_0610.csv")
fields <- colnames(df)

grep("TFC", fields)
fields[274:279]

df_TFC <- df[,c(1,8,274:279)]

write.csv(df_TFC, "TFC.csv", row.names=F)