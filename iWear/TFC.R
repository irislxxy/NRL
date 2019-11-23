setwd("/Users/iris/Desktop/NRL/iWear")
df <- read.csv("iWear_complete_20191030.csv")
fields <- colnames(df)

grep("TFC", fields)
fields[275:280]

df_TFC <- df[,c(1,8,275:280)]

write.csv(df_TFC, "TFC.csv", row.names=F)