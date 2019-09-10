library(tibble)
setwd("/Users/iris/Desktop/NRL/PreActiveHD/PD")

# HD
HD <- read.csv("PreActiveHD_DATA.csv")
fieldsHD <-  colnames(HD)
fieldsHD[grep("breq", fieldsHD)]
grep("breq", fieldsHD) # 87:105
grep("stroop_interf_correct", fieldsHD) # 282
grep("smwt_total_distance", fieldsHD) # 199
HD_sub <- HD[,c(1, 87:105, 282, 199)]

# PD
PD <- read.csv("PreActivePD_DATA.csv")
fieldsPD <-  colnames(PD)
fieldsPD[grep("breq", fieldsPD)]
grep("breq", fieldsPD) # 36:54
grep("stroop_total", fieldsPD) # 79
grep("smwt_total", fieldsPD) # 107
PD_sub <- PD[,c(1, 36:54, 79, 107)]

# merge
colnames(HD_sub)[2:20] <- colnames(PD_sub)[2:20]
colnames(PD_sub) <- colnames(HD_sub)
df <- rbind(HD_sub, PD_sub)

# BREQ2
amotivation <- (df$breq5 + df$breq9 + df$breq12 + df$breq19)/4
external <- (df$breq1 + df$breq6 + df$breq11 + df$breq16)/4
introjected <- (df$breq2 + df$breq7 + df$breq13)/3
identified <- (df$breq3 + df$breq8 + df$breq14 + df$breq17)/4
intrinsic <- (df$breq4 + df$breq10 + df$breq15 + df$breq18)/4
df <- add_column(df, amotivation, .after = "breq19")
df <- add_column(df, external, .after = "amotivation")
df <- add_column(df, introjected, .after = "external")
df <- add_column(df, identified, .after = "introjected")
df <- add_column(df, intrinsic, .after = "identified")
write.csv(df, "HDPD.csv", row.names = F)

# difference
## PREHD 5/8/12 & MPREPD 7 do not have follow up
df_cal <- df[-c(9,14,21,38),]
variableList <- c("amotivation", "external", "introjected", "identified", "intrinsic",
                  "stroop_interf_correct", "smwt_total_distance")
participantList <- paste0("HD",c(1:4,6:7,9:11,13:14))
participantList <- c(participantList, paste0("PD", c(1:6,8:13)))
diff <- data.frame(row.names = participantList)
for (i in 1:23){
  for (j in variableList){
    # post - pre
    diff[i,j] <- df_cal[i*2,j] - df_cal[i*2-1,j]
  }
}
write.csv(diff, "HDPD_Diff.csv")

cal <- function(df){
  for (i in variableList){
    cat(i,"\n")
    cat("Mean:", round(mean(df[,i],na.rm=T),2), "\n")
    cat("SD:", round(sd(df[,i],na.rm=T),2), "\n")
    cat("Confidence Interval:", t.test(df[,i])$conf.int, "\n\n")
    wilcox.test(df[,i], alternative="two.sided", conf.int = TRUE)
  }
}
cal(diff)
