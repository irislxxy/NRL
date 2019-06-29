setwd("/Users/iris/Desktop/NRL/Enroll-HD")
df <- read.csv("enroll.csv", stringsAsFactors=FALSE)

# max seq for each participant
n <- nrow(df)
idList <- c()
seqList <- c()
for (i in 1:(n-1)){
  print(i)
  if (df$subjid[i] != df$subjid[i+1]){
    idList <- c(idList, df$subjid[i])
    seqList <- c(seqList, df$seq[i])
  }
}

df_seq <- as.data.frame(cbind(idList, seqList))
colnames(df_seq) <- c("subjid","seq")
write.csv(df_seq, "seq_max.csv", row.names = F)

# N for each seq that have 4 variables
df2<- df[,c("subjid","seq","tug","tug1","scst","scst1")]
table(df2$seq)

df3 <- na.omit(df2)
table(df3$seq)

# subset
seq1 <- df3$subjid[which(df3$seq == 1)]
seq2 <- df3$subjid[which(df3$seq == 2)]
seq3 <- df3$subjid[which(df3$seq == 3)]
seq4 <- df3$subjid[which(df3$seq == 4)]
id <- intersect(seq1, seq2)
id <- intersect(id, seq3)
id <- intersect(id, seq4)
df4 <- subset(df, (df$subjid %in% id))
df4 <- subset(df4, (df4$seq %in% c(1:4)))
write.csv(df4, "enroll_4seq.csv", row.names = F)