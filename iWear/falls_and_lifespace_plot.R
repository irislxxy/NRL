library(ggplot2)
setwd("~/Desktop/NRL")

# iWear
iwear <- read.csv("iWear/iWear_complete_20191030.csv")
df <- iwear[c("as_correct","hd_or_healthy","hfq_2_yes","ls_score","FA")]

# APDM
sit <- read.csv("APDM/Sitting Postural Sway.csv", stringsAsFactors=FALSE)
sit$X[1:20] <- paste0("IW", substr(sit$X[1:20],3,4), "GHI")
df_sit <- sit[c(1,30)]
colnames(df_sit) <- c("as_correct","total_sway_area")

# merge and exclude controls
df <- merge(df, df_sit, sort = F) 
df <- df[which(df$hd_or_healthy == 1),] # 33 participants = 18GHI + 15TC

# falls vs sitting postural sway 
df$as_correct[is.na(df$hfq_2_yes)]
temp <- df[,c("hfq_2_yes", "total_sway_area")]
temp <- na.omit(temp) # 18 participants

ggplot(temp) +
  geom_point(aes(x = hfq_2_yes, y = total_sway_area)) +
  labs(x = "falls", y = "sitting postural sway") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.subtitle = element_text(hjust=0.5))
ggsave("iWear/falls_postural_sway.png")

# lifespace vs sitting postural sway 
df$as_correct[is.na(df$ls_score)]
temp <- df[,c("ls_score", "total_sway_area")]
temp <- na.omit(temp) # 15 participants

ggplot(temp) +
  geom_point(aes(x = ls_score, y = total_sway_area)) +
  labs(x = "ls_score", y = "sitting postural sway") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.subtitle = element_text(hjust=0.5))
ggsave("iWear/lifespace_postural_sway.png")

# lifespace vs FA
df$as_correct[is.na(df$FA)]
temp <- df[,c("ls_score", "FA")]
temp <- na.omit(temp) # 13 participants

ggplot(temp) +
  geom_point(aes(x = ls_score, y = FA)) +
  labs(x = "ls_score", y = "functional assessment") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.subtitle = element_text(hjust=0.5))
ggsave("iWear/lifespace_FA.png")
