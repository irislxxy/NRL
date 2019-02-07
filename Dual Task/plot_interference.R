library(ggplot2)
library(reshape)
setwd("/Users/iris/Desktop/NRL/iWear/Dual Task")

df <- read.csv("DTE_1028.csv", stringsAsFactors=FALSE)
df$hd_or_healthy <- factor(df$hd_or_healthy)

#x-cognition y-motor
#standing
df_s <- df[which(df$stroop_it_correct!=0),] #GH4/GH8
df_s <- df_s[-7,] #GH12

##faeofs
ggplot(df_s, aes(x = faeofs_dte, y = motor_faeofs_dte, color = hd_or_healthy)) + 
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  xlim(-100, 100) + ylim(-2050, 2050) +
  labs(x = "Cognition DTE", y = "Motor DTE", title = "Feet Apart, Eyes Open, Firm Surface") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("#2E86C1", "#AED6F1"),
                     name="Group",
                     breaks=c("1", "2"),
                     labels=c("HD", "Control")) +
  annotate("text", x = 50, y = 1000, label = "Mutual facilitation") +
  annotate("text", x = -50, y = 1000, label = "Motor-priority") +
  annotate("text", x = -50, y = -1000, label = "Mutual interference") +
  annotate("text", x = 50, y = -1000, label = "Cognitive-priority")
ggsave("faeofs_1101.png")

##fteofs
ggplot(df_s, aes(x = fteofs_dte, y = motor_fteofs_dte, color = hd_or_healthy))+
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
# xlim(-100, 100) + ylim(-1050, 1050) +
  xlim(-100, 100) + ylim(-2050, 2050) +
  labs(x = "Cognition DTE", y = "Motor DTE", title = "Feet Together, Eyes Open, Firm Surface") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("#2E86C1", "#AED6F1"),
                     name="Group",
                     breaks=c("1", "2"),
                     labels=c("HD", "Control")) +
  annotate("text", x = 50, y = 1000, label = "Mutual facilitation") +
  annotate("text", x = -50, y = 1000, label = "Motor-priority") +
  annotate("text", x = -50, y = -1000, label = "Mutual interference") +
  annotate("text", x = 50, y = -1000, label = "Cognitive-priority")
ggsave("fteofs_1101.png")

##faeofoam
ggplot(df_s, aes(x = faeofoam_dte, y = motor_faeofoam_dte, color = hd_or_healthy))+
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
# xlim(-100, 100) + ylim(-800, 800) +
  xlim(-100, 100) + ylim(-2050, 2050) +
  labs(x = "Cognition DTE", y = "Motor DTE", title = "Feet Apart, Eyes Open, Foam Surface") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("#2E86C1", "#AED6F1"),
                     name="Group",
                     breaks=c("1", "2"),
                     labels=c("HD", "Control")) +
  annotate("text", x = 50, y = 1000, label = "Mutual facilitation") +
  annotate("text", x = -50, y = 1000, label = "Motor-priority") +
  annotate("text", x = -50, y = -1000, label = "Mutual interference") +
  annotate("text", x = 50, y = -1000, label = "Cognitive-priority")
ggsave("faeofoam_1101.png")

#walking
df_w <- df[which(df$as_eol_crr!=0),] #GH8/GH17 #IW13TC/IW4TC NA

##alphabet
ggplot(df_w, aes(x=wwt_dte, y=motor_alphabet_dte, color = hd_or_healthy))+
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  xlim(-100, 100) + ylim(-400, 400) +
  labs(x = "Cognition DTE", y = "Motor DTE", title = "WWIT Alphabet") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("#2E86C1", "#AED6F1"),
                     name="Group",
                     breaks=c("1", "2"),
                     labels=c("HD", "Control")) +
  annotate("text", x = 50, y = 200, label = "Mutual facilitation") +
  annotate("text", x = -50, y = 200, label = "Motor-priority") +
  annotate("text", x = -50, y = -200, label = "Mutual interference") +
  annotate("text", x = 50, y = -200, label = "Cognitive-priority")
ggsave("alphabet_1101.png")

##eol
ggplot(df_w, aes(x=wwt_eol_dte, y=motor_eol_dte, color = hd_or_healthy))+
  geom_point() + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) +
  xlim(-100, 100) + ylim(-400, 400) +
  labs(x = "Cognition DTE", y = "Motor DTE", title = "WWIT Every Other Letter") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c("#2E86C1", "#AED6F1"),
                     name="Group",
                     breaks=c("1", "2"),
                     labels=c("HD", "Control")) +
  annotate("text", x = 50, y = 200, label = "Mutual facilitation") +
  annotate("text", x = -50, y = 200, label = "Motor-priority") +
  annotate("text", x = -50, y = -200, label = "Mutual interference") +
  annotate("text", x = 50, y = -200, label = "Cognitive-priority")
ggsave("eol_1101.png")