library(ggplot2)
library(reshape)
setwd("/Users/iris/Desktop/NRL/iWear/Dual Task")

df <- read.csv("DTE_1022.csv", stringsAsFactors=FALSE)
df$hd_or_healthy <- factor(df$hd_or_healthy)

#x-cognition y-motor
#standing
df_s <- df[which(df$stroop_it_correct!=0),] #GH4/GH8

##faeofs
ggplot(df_s, aes(x = faeofs_dte, y = motor_faeofs_dte, color = hd_or_healthy)) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  xlim(-100, 100) + ylim(-2050, 2050) +
  labs(x = "Cognition DTE", y = "Motor DTE", title = "Feet Apart, Eyes Open, Firm Surface") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("#1B4F72", "#AED6F1"),
                     name="Group",
                     breaks=c("1", "2"),
                     labels=c("HD", "Control")) +
  annotate("text", x = 50, y = 1000, label = "Mutual facilitation") +
  annotate("text", x = -50, y = 1000, label = "Motor-priority") +
  annotate("text", x = -50, y = -1000, label = "Mutual interference") +
  annotate("text", x = 50, y = -1000, label = "Cognitive-priority")
ggsave("faeofs_1022.png")

##fteofs
ggplot(df_s, aes(x = fteofs_dte, y = motor_fteofs_dte, color = hd_or_healthy))+
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  xlim(-100, 100) + ylim(-1050, 1050) +
  labs(x = "Cognition DTE", y = "Motor DTE", title = "Feet Together, Eyes Open, Firm Surface") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("#1B4F72", "#AED6F1"),
                     name="Group",
                     breaks=c("1", "2"),
                     labels=c("HD", "Control")) +
  annotate("text", x = 50, y = 500, label = "Mutual facilitation") +
  annotate("text", x = -50, y = 500, label = "Motor-priority") +
  annotate("text", x = -50, y = -500, label = "Mutual interference") +
  annotate("text", x = 50, y = -500, label = "Cognitive-priority")
ggsave("fteofs_1022.png")

##faeofoam
ggplot(df_s, aes(x = faeofoam_dte, y = motor_faeofoam_dte, color = hd_or_healthy))+
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  xlim(-100, 100) + ylim(-800, 800) +
  labs(x = "Cognition DTE", y = "Motor DTE", title = "Feet Apart, Eyes Open, Foam Surface") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("#1B4F72", "#AED6F1"),
                     name="Group",
                     breaks=c("1", "2"),
                     labels=c("HD", "Control")) +
  annotate("text", x = 50, y = 400, label = "Mutual facilitation") +
  annotate("text", x = -50, y = 400, label = "Motor-priority") +
  annotate("text", x = -50, y = -400, label = "Mutual interference") +
  annotate("text", x = 50, y = -400, label = "Cognitive-priority")
ggsave("faeofoam_1022.png")

#walking
df_w <- df[which(df$as_eol_crr!=0),] #GH8

##alphabet
ggplot(df_w, aes(x=wwt_dte, y=motor_alphabet_dte, color = hd_or_healthy))+
  geom_point()
ggsave("alphabet.png")

##eol
ggplot(df_w, aes(x=wwt_eol_dte, y=motor_eol_dte, color = hd_or_healthy))+
  geom_point()
ggsave("eol.png")

#bar plot & box plot
##gait speed
colnames(df)[45:47] <- c("Walk", "Alphabet", "EOL")
df_speed <- melt(df[,c("as_correct", "hd_or_healthy", "Walk", "Alphabet", "EOL")],
                 id=c("as_correct","hd_or_healthy"))
##separate
df_speed_HD <- df_speed[which(df_speed$hd_or_healthy==1),]
df_speed_CO <- df_speed[which(df_speed$hd_or_healthy==2),]

ggplot(df_speed_HD) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value), color = "#1B4F72") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Conditions", y = "Gait Speed", title = "Gait Speed HD")
ggsave("gaitspeed_box_HD.png")

ggplot(df_speed_CO) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value), color = "#1B4F72") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Conditions", y = "Gait Speed", title = "Gait Speed CO")
ggsave("gaitspeed_box_CO.png")

##combine
ggplot(df_speed) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value, color= hd_or_healthy)) +
  theme_bw() +
  theme(line = element_blank()) +
  labs(x = "Conditions", y = "Gait Speed") +
  scale_color_manual(values=c("#1B4F72", "#AED6F1"),
                     name="Group",
                     breaks=c("1", "2"),
                     labels=c("HD", "Control"))
ggsave("gaitspeed_box.png")

df_speed2 <- data_summary(df_speed, varname="value", groupnames=c("hd_or_healthy", "variable"))

ggplot(df_speed, aes(x = variable, y = value, color = hd_or_healthy)) +
  geom_bar(data = df_speed2, aes(x = variable, y = value, fill = hd_or_healthy), stat="identity", alpha = .3, position=position_dodge()) + 
  geom_point(position=position_dodge()) +
  theme_bw() +
  theme(line = element_blank())
ggsave("gaitspeed_bar.png")

# ggplot(df_speed2, aes(x=variable, y=value, fill=hd_or_healthy)) +
#    geom_bar(stat="identity", position=position_dodge()) +
#    geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2, position=position_dodge(.9))
# ggsave("gaitspeed.png")

##sway area
colnames(df)[c(36,37,39,40,42,43)] <- c("FAEOFirm",
                                        "FAEOFirmStroop",
                                        "FTEOFirm",
                                        "FTEOFirmStroop", 
                                        "FAEOFoam",
                                        "FAEOFoamStroop")
df_sway <- melt(df[,c("as_correct", "hd_or_healthy", colnames(df)[c(36,37,39,40,42,43)])],
                id=c("as_correct","hd_or_healthy"))

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


df_sway2 <- data_summary(df_sway, varname="value", groupnames=c("hd_or_healthy", "variable"))

# ggplot(df_sway2, aes(x=variable, y=value, fill=hd_or_healthy)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2, position=position_dodge(.9))
# ggsave("swayarea.png")

##cognitive
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

df_cog2 <- data_summary(df_cog, varname="value", groupnames=c("hd_or_healthy", "variable"))

# ggplot(df_cog2, aes(x=variable, y=value, fill=hd_or_healthy)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2, position=position_dodge(.9))
# ggsave("cog.png")

##TUG
df_tug <- melt(df[,c("as_correct", "hd_or_healthy", colnames(df)[50:51])],
               id=c("as_correct","hd_or_healthy"))

##separate
df_tug_HD <- df_tug[which(df_tug$hd_or_healthy==1),]
df_tug_CO <- df_tug[which(df_tug$hd_or_healthy==2),]

ggplot(df_tug_HD) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value), color = "#1B4F72") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Conditions", y = "Duration", title = "TUG Duration HD")
ggsave("tug_box_HD.png")

ggplot(df_tug_CO) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value), color = "#1B4F72") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Conditions", y = "Duration", title = "TUG Duration CO")
ggsave("tug_box_CO.png")

df_tug2 <- data_summary(df_tug, varname="value", groupnames=c("hd_or_healthy", "variable"))

# ggplot(df_tug2, aes(x=variable, y=value, fill=hd_or_healthy)) +
#   geom_bar(stat="identity", position=position_dodge()) +
#   geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2, position=position_dodge(.9))
# ggsave("tug.png")