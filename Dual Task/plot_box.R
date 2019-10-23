library(ggplot2)
library(reshape)
library(dplyr)
setwd("/Users/iris/Desktop/NRL/Dual Task")

df <- read.csv("DTE_0322.csv", stringsAsFactors=FALSE)
df$hd_or_healthy <- factor(df$hd_or_healthy)

# duration
df_walk <- df[,c("as_correct", "hd_or_healthy",
                 "Walk.Duration","Alphabet.Duration","EOL.Duration","DKEFS.Duration")]
colnames(df_walk)[3:6] <- c("Walk","Alphabet","EOL","DKEFS")
df_walk <- melt(df_walk, id=c("as_correct","hd_or_healthy"))

df_walk_HD <- df_walk[which(df_walk$hd_or_healthy==1),]
df_walk_CO <- df_walk[which(df_walk$hd_or_healthy==2),]

## box plot
ggplot(df_walk_HD) +
  geom_boxplot(aes(x = variable, y = value), fill = "#1E8449") +
  ylim(0, 40) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Duration (s)")
ggsave("walk_box_HD.png")

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

ggplot(df_walk_CO) +
  geom_boxplot(aes(x = variable, y = value), fill = "#2E86C1") +
  ylim(0, 40) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Duration (s)")
ggsave("walk_box_CO.png")

## outlier
df_walk_HD <- na.omit(df_walk_HD)
df_walk_HD %>%
  group_by(variable) %>%
  mutate(outlier = ifelse(is_outlier(value), as_correct, as.numeric(NA))) %>%
  ggplot(., aes(x = factor(variable), y = value)) +
  geom_boxplot() +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  ylim(0, 40) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Duration (s)")
ggsave("walk_box_outlier_HD.png")

df_walk_CO <- na.omit(df_walk_CO)
df_walk_CO %>%
  group_by(variable) %>%
  mutate(outlier = ifelse(is_outlier(value), as_correct, as.numeric(NA))) %>%
  ggplot(., aes(x = factor(variable), y = value)) +
  geom_boxplot() +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  ylim(0, 40) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Duration (s)")
ggsave("walk_box_outlier_CO.png")



# sway area
df_sway <- df[,c("as_correct", "hd_or_healthy",
                 "FAEOFirm.Sway.Area", "FAEOFirmStroop.Sway.Area", 
                 "FTEOFirm.Sway.Area", "FTEOFirmStroop.Sway.Area",
                 "FAEOFoam.Sway.Area", "FAEOFoamStroop.Sway.Area")]    
colnames(df_sway)[3:8] <- c("FA(ST)", "FA(DT)",
                            "FT(ST)", "FT(DT)",
                            "FO(ST)", "FO(DT)")
df_sway <- melt(df_sway, id=c("as_correct","hd_or_healthy"))

df_sway_HD <- df_sway[which(df_sway$hd_or_healthy==1),]
df_sway_CO <- df_sway[which(df_sway$hd_or_healthy==2),]

## box plot
ggplot(df_sway_HD) +
  geom_boxplot(aes(x = variable, y = value), fill = "#1E8449") +
  ylim(0, 650) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Sway Area (degrees^2)")
ggsave("swayarea_box_HD.png")

ggplot(df_sway_CO) +
  geom_boxplot(aes(x = variable, y = value), fill = "#2E86C1") +
  ylim(0, 650) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Sway Area (degrees^2)")
ggsave("swayarea_box_CO.png")

## grouped box plot
df_sway_HD$task <- substr(df_sway_HD$variable,1,2)
df_sway_HD$task <- factor(df_sway_HD$task, levels = c("FA","FT","FO"), ordered = TRUE)
df_sway_HD$condition <- substr(df_sway_HD$variable,4,5)
df_sway_HD$condition <- factor(df_sway_HD$condition, levels = c("ST","DT"), ordered = TRUE)

ggplot(df_sway_HD, aes(x = task, y = value, fill = condition)) +
  geom_boxplot() +
  ylim(0, 650) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Sway Area (degrees^2)") +
  scale_fill_manual(values=c("#A9DFBF","#1E8449"))
ggsave("swayarea_box_group_HD.png")

df_sway_CO$task <- substr(df_sway_CO$variable,1,2)
df_sway_CO$task <- factor(df_sway_CO$task, levels = c("FA","FT","FO"), ordered = TRUE)
df_sway_CO$condition <- substr(df_sway_CO$variable,4,5)
df_sway_CO$condition <- factor(df_sway_CO$condition, levels = c("ST","DT"), ordered = TRUE)

ggplot(df_sway_CO, aes(x = task, y = value, fill = condition)) +
  geom_boxplot() +
  ylim(0, 650) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Sway Area (degrees^2)") +
  scale_fill_manual(values=c("#AED6F1","#2E86C1"))
ggsave("swayarea_box_group_CO.png")

## box plot with scatter
ggplot(df_sway) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value, color= hd_or_healthy)) +
  theme_bw() +
  theme(line = element_blank()) +
  labs(x = "Conditions", y = "Sway Area") +
  scale_color_manual(values=c("#2E86C1", "#AED6F1"),
                     name="Group",
                     breaks=c("1", "2"),
                     labels=c("HD", "Control"))
# ggsave("swayarea_box_scatter.png")

# cognitive
df_cog <- melt(df[,c("as_correct", "hd_or_healthy", colnames(df)[grep("crr", colnames(df))])],
               id=c("as_correct","hd_or_healthy"))

ggplot(df_cog) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value, color= hd_or_healthy)) +
  theme_bw() +
  theme(line = element_blank()) +
  labs(x = "Conditions", y = "Correct Response Rate") +
  scale_color_manual(values=c("#2E86C1", "#AED6F1"),
                     name="Group",
                     breaks=c("1", "2"),
                     labels=c("HD", "Control"))
# ggsave("cog_box_scatter.png")

# TUG
# sway area
df_tug <- df[,c("as_correct", "hd_or_healthy",
                 "TUG.Duration", "TUG.with.Cognitive.Duration")]  
colnames(df_tug)[3:4] <- c("TUG", "TUG.with.Cognitive")
df_tug <- melt(df_tug, id=c("as_correct","hd_or_healthy"))

df_tug_HD <- df_tug[which(df_tug$hd_or_healthy==1),]
df_tug_CO <- df_tug[which(df_tug$hd_or_healthy==2),]

## box plot
ggplot(df_tug_HD) +
  geom_boxplot(aes(x = variable, y = value), fill = "#1E8449") +
  ylim(0, 40) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Duration (s)")
ggsave("tug_box_HD.png")

ggplot(df_tug_CO) +
  geom_boxplot(aes(x = variable, y = value), fill = "#2E86C1") +
  ylim(0, 40) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Duration (s)")
ggsave("tug_box_CO.png")

## box plot with scatter
ggplot(df_tug_HD) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value), color = "#2E86C1") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Duration (s)")
# ggsave("tug_box_scatter_HD.png")

ggplot(df_tug_CO) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value), color = "#AED6F1") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Duration (s)")
# ggsave("tug_box_scatter_CO.png")
