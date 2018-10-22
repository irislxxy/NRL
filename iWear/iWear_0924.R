setwd("/Users/iris/Desktop/NRL/iWear")
df <- read.csv("iWear_1022.csv")
fields <- colnames(df)

#delete identifiable information
df <- df[,-4]

#replace NA with -9
df[is.na(df)] <- -9

write.csv(df, "iWear_1022_9.csv", row.names=F)

df_HD <- df[which(df$hd_or_healthy==1),]
write.csv(df_HD, "iWear_1022_9_HD.csv", row.names=F)

df_CO <- df[which(df$hd_or_healthy==2),]
write.csv(df_CO, "iWear_1022_9_CO.csv", row.names=F)