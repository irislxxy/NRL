library(ggplot2)
library(reshape)
setwd("/Users/iris/Desktop/NRL/iWear/Dual Task")

df <- read.csv("DTE_1028.csv", stringsAsFactors=FALSE)
df$hd_or_healthy <- factor(df$hd_or_healthy)

#duration
colnames(df)[45:47] <- c("Walk", "Alphabet", "EOL")
df_walk <- melt(df[,c("as_correct", "hd_or_healthy", colnames(df)[45:47])],
                id=c("as_correct","hd_or_healthy"))

df_walk_HD <- df_walk[which(df_walk$hd_or_healthy==1),]
df_walk_CO <- df_walk[which(df_walk$hd_or_healthy==2),]

##box plot
ggplot(df_walk_HD) +
  geom_boxplot(aes(x = variable, y = value), fill = "#1B4F72") +
  ylim(5, 40) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Duration (s)")
ggsave("walk_box_HD.png")

ggplot(df_walk_CO) +
  geom_boxplot(aes(x = variable, y = value), fill = "#AED6F1") +
  ylim(5, 40) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Duration (s)")
ggsave("walk_box_CO.png")

#sway area
colnames(df)[c(36,37,39,40,42,43)] <- c("FAEOFirm",
                                        "FAEOFirmStroop",
                                        "FTEOFirm",
                                        "FTEOFirmStroop", 
                                        "FAEOFoam",
                                        "FAEOFoamStroop")
df_sway <- melt(df[,c("as_correct", "hd_or_healthy", colnames(df)[c(36,37,39,40,42,43)])],
                id=c("as_correct","hd_or_healthy"))

df_sway_HD <- df_sway[which(df_sway$hd_or_healthy==1),]
df_sway_CO <- df_sway[which(df_sway$hd_or_healthy==2),]

##box plot
ggplot(df_sway_HD) +
  geom_boxplot(aes(x = variable, y = value), fill = "#1B4F72") +
  ylim(0, 650) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Sway Area (degrees^2)")
ggsave("swayarea_box_HD.png")

ggplot(df_sway_CO) +
  geom_boxplot(aes(x = variable, y = value), fill = "#AED6F1") +
  ylim(0, 650) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Sway Area (degrees^2)")
ggsave("swayarea_box_CO.png")

##box plot with scatter
ggplot(df_sway) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value, color= hd_or_healthy)) +
  theme_bw() +
  theme(line = element_blank()) +
  labs(x = "Conditions", y = "Sway Area") +
  scale_color_manual(values=c("#1B4F72", "#AED6F1"),
                     name="Group",
                     breaks=c("1", "2"),
                     labels=c("HD", "Control"))
ggsave("swayarea_box.png")

#cognitive
df_cog <- melt(df[,c("as_correct", "hd_or_healthy", colnames(df)[grep("crr", colnames(df))])],
               id=c("as_correct","hd_or_healthy"))

ggplot(df_cog) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value, color= hd_or_healthy)) +
  theme_bw() +
  theme(line = element_blank()) +
  labs(x = "Conditions", y = "Correct Response Rate") +
  scale_color_manual(values=c("#1B4F72", "#AED6F1"),
                     name="Group",
                     breaks=c("1", "2"),
                     labels=c("HD", "Control"))
ggsave("cog_box.png")

#TUG
colnames(df)[50:51] <- c("TUG", "TUG.with.Cognitive")
df_tug <- melt(df[,c("as_correct", "hd_or_healthy", colnames(df)[50:51])],
               id=c("as_correct","hd_or_healthy"))

df_tug_HD <- df_tug[which(df_tug$hd_or_healthy==1),]
df_tug_CO <- df_tug[which(df_tug$hd_or_healthy==2),]

##box plot
ggplot(df_tug_HD) +
  geom_boxplot(aes(x = variable, y = value), fill = "#1B4F72") +
  ylim(0, 40) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Duration (s)")
ggsave("tug_box_HD.png")

ggplot(df_tug_CO) +
  geom_boxplot(aes(x = variable, y = value), fill = "#AED6F1") +
  ylim(0, 40) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Duration (s)")
ggsave("tug_box_CO.png")

##box plot with scatter
ggplot(df_tug_HD) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value), color = "#1B4F72") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Duration (s)")
ggsave("tug_box_HD_scatter.png")

ggplot(df_tug_CO) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value), color = "#AED6F1") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Duration (s)")
ggsave("tug_box_CO_scatter.png")
