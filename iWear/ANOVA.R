#anova
#aov(response ~ factor, data)
df <- read.csv("/Users/iris/Desktop/NRL/iWear/Dual Task/ANOVA.csv", stringsAsFactors=FALSE)

df_1 <- df[,c(1,2,3)]
df_1$single_or_dual <- rep(1,57)
df_2 <- df[,c(1,2,4)]
df_2$single_or_dual <- rep(2,57)
colnames(df_2)[3] <- "FAEOFirm.Sway.Area"
df_12 <- rbind(df_1,df_2)

mod <- aov(FAEOFirm.Sway.Area ~ factor(hd_or_healthy) * factor(single_or_dual), data = df_12)
summary(mod)
