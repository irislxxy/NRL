library(ggplot2)
setwd("~/Desktop/NRL")

# iWear
iwear <- read.csv("iWear/iWear_complete_0625.csv")
df_score <- iwear[c("as_correct","hd_or_healthy","TMS","TFC", "Chorea_Subscore")]
colnames(df_score)[5] <- c("Chorea")

# DTE
dte <- read.csv("Dual Task/DTE_0322.csv") 
df_dte <- dte[,c("as_correct","motor_faeofs_dte","motor_fteofs_dte","motor_faeofoam_dte")]
colnames(df_dte) <- c("as_correct","FA","FT","FO")

# APDM
## siting
sit <- read.csv("APDM/Sitting Postural Sway.csv", stringsAsFactors=FALSE)
sit$X[1:20] <- paste0("IW", substr(sit$X[1:20],3,4), "GHI")
df_sit <- sit[c(1,12)]
colnames(df_sit) <- c("as_correct","Jerk_Sit")

## standing
stand <- read.csv("APDM/FAEOFirm.csv", stringsAsFactors=FALSE)
stand$X[1:20] <- paste0("IW", substr(stand$X[1:20],3,4), "GHI")
df_stand <- stand[c(1,12)]
colnames(df_stand) <- c("as_correct","Jerk_Stand")

# merge
df <- merge(df_score, df_dte, sort = F) # 49 participants - 20GHI + 29TC (no IW5TCCO)
df <- merge(df, df_sit, sort = F) 
df <- merge(df, df_stand, sort = F) 

# group
df$Group <- ifelse(df$TFC<=10, "TFC <= 10", "TFC > 10")

# Jerk of sitting postural sway with TMS
temp <- df[,c("as_correct","Group","Jerk_Sit","TMS")] # remove control group
temp <- na.omit(temp)                                 # 33 participants - IW11TC/IW13GHI no TMS
cor.test(temp$Jerk_Sit, temp$TMS)

ggplot(temp, aes(x = Jerk_Sit, y = TMS)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = "Jerk of sitting postural sway with TMS",
       subtitle = "r = 0.3914616",
       x = "Jerk (m^2/s^5)", y = "TMS") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.subtitle = element_text(hjust=0.5))
ggsave("iWear/Correlation/jerk_sit_TMS.png")

## group
table(temp$Group)
cor.test(temp[which(temp$Group == "TFC <= 10"),"Jerk_Sit"], temp[which(temp$Group == "TFC <= 10"),"TMS"])
cor.test(temp[which(temp$Group == "TFC > 10"),"Jerk_Sit"], temp[which(temp$Group == "TFC > 10"),"TMS"])

ggplot(temp, aes(x = Jerk_Sit, y = TMS)) +
  geom_point(aes(color = Group)) +
  geom_smooth(method = "lm") +
  geom_smooth(aes(color = Group), method = "lm") +
  annotate(geom = "text", 
           label = "Total r = 0.3914616 \n TFC <= 10 r = 0.1891387 \n TFC > 10 r = 0.6679459 ", 
           x = Inf, y = 0, hjust = 1, vjust = 0) + 
  labs(title = "Jerk of sitting postural sway with TMS", x = "Jerk (m^2/s^5)", y = "TMS") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.subtitle = element_text(hjust=0.5)) +
  scale_color_manual(values=c("#AED6F1","#2874A6"),
                     name="Group",
                     labels=c("TFC <= 10 n = 20", "TFC > 10   n = 13"))
ggsave("iWear/Correlation/jerk_sit_TMS_group.png")

# Jerk of sitting postural sway with TFC 
temp <- df[,c("as_correct","Group","Jerk_Sit","TFC")] # remove control group
temp <- na.omit(temp)                                 # 35 participants
cor.test(temp$Jerk_Sit, temp$TFC)

ggplot(temp, aes(x = Jerk_Sit, y = TFC)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = "Jerk of sitting postural sway with TFC",
       subtitle = "r = -0.1770946",
       x = "Jerk (m^2/s^5)", y = "TFC") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.subtitle = element_text(hjust=0.5))
ggsave("iWear/Correlation/jerk_sit_TFC.png")

## group
table(temp$Group)
cor.test(temp[which(temp$Group == "TFC <= 10"),"Jerk_Sit"], temp[which(temp$Group == "TFC <= 10"),"TFC"])
cor.test(temp[which(temp$Group == "TFC > 10"),"Jerk_Sit"], temp[which(temp$Group == "TFC > 10"),"TFC"])

ggplot(temp, aes(x = Jerk_Sit, y = TFC)) +
  geom_point(aes(color = Group)) +
  geom_smooth(method = "lm") +
  geom_smooth(aes(color = Group), method = "lm") +
  annotate(geom = "text", 
           label = "Total r = -0.1770946 \n TFC <= 10 r = 0.2184196 \n TFC > 10 r = -0.4094976 ", 
           x = Inf, y = 0, hjust = 1, vjust = 0) + 
  labs(title = "Jerk of sitting postural sway with TFC", x = "Jerk (m^2/s^5)", y = "TMS") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.subtitle = element_text(hjust=0.5)) +
  scale_color_manual(values=c("#AED6F1","#2874A6"),
                     name="Group",
                     labels=c("TFC <= 10 n = 22", "TFC > 10   n = 13"))
ggsave("iWear/Correlation/jerk_sit_TFC_group.png")

# Jerk of sitting postural sway with chorea subscore
temp <- df[,c("as_correct","Group","Jerk_Sit","Chorea")] # remove control group
temp <- na.omit(temp)                                    # 35 participants
cor.test(temp$Jerk_Sit, temp$Chorea)

ggplot(temp, aes(x = Jerk_Sit, y = Chorea)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = "Jerk of sitting postural sway with chorea subscore",
       subtitle = "r = 0.5414237",
       x = "Jerk (m^2/s^5)", y = "Chorea") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.subtitle = element_text(hjust=0.5))
ggsave("iWear/Correlation/jerk_sit_chorea.png")

## group
table(temp$Group)
cor.test(temp[which(temp$Group == "TFC <= 10"),"Jerk_Sit"], temp[which(temp$Group == "TFC <= 10"),"Chorea"])
cor.test(temp[which(temp$Group == "TFC > 10"),"Jerk_Sit"], temp[which(temp$Group == "TFC > 10"),"Chorea"])

ggplot(temp, aes(x = Jerk_Sit, y = Chorea)) +
  geom_point(aes(color = Group)) +
  geom_smooth(method = "lm") +
  geom_smooth(aes(color = Group), method = "lm") +
  annotate(geom = "text", 
           label = "Total r = 0.5414237 \n TFC <= 10 r = 0.4953655 \n TFC > 10 r = 0.7324569 ", 
           x = Inf, y = 0, hjust = 1, vjust = 0) + 
  labs(title = "Jerk of sitting postural sway with chorea subscore", x = "Jerk (m^2/s^5)", y = "Chorea") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.subtitle = element_text(hjust=0.5)) +
  scale_color_manual(values=c("#AED6F1","#2874A6"),
                     name="Group",
                     labels=c("TFC <= 10 n = 22", "TFC > 10   n = 13"))
ggsave("iWear/Correlation/jerk_sit_chorea_group.png")

# Jerk of standing feet apart with chorea subscore
temp <- df[,c("as_correct","Group","Jerk_Stand","Chorea")] # remove control group
temp <- na.omit(temp)                                      # 35 participants
cor.test(temp$Jerk_Stand, temp$Chorea)

ggplot(temp, aes(x = Jerk_Stand, y = Chorea)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(title = "Jerk of standing feet apart with chorea subscore",
       subtitle = "r = 0.2929618",
       x = "Jerk (m^2/s^5)", y = "Chorea") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.subtitle = element_text(hjust=0.5))
ggsave("iWear/Correlation/jerk_stand_chorea.png")

# ## exclude three participants with jerk above 250
# temp_below_250 <- temp[-which(temp$Jerk_Stand > 250),]
# cor.test(temp_below_250$Jerk_Stand, temp_below_250$Chorea)

## group
table(temp$Group)
cor.test(temp[which(temp$Group == "TFC <= 10"),"Jerk_Stand"], temp[which(temp$Group == "TFC <= 10"),"Chorea"])
cor.test(temp[which(temp$Group == "TFC > 10"),"Jerk_Stand"], temp[which(temp$Group == "TFC > 10"),"Chorea"])

ggplot(temp, aes(x = Jerk_Stand, y = Chorea)) +
  geom_point(aes(color = Group)) +
  geom_smooth(method = "lm") +
  geom_smooth(aes(color = Group), method = "lm") +
  annotate(geom = "text", 
           label = "Total r = 0.2929618 \n TFC <= 10 r = 0.1933411 \n TFC > 10 r = 0.2987416 ", 
           x = Inf, y = 0, hjust = 1, vjust = 0) + 
  labs(title = "Jerk of standing feet apart with chorea subscore", x = "Jerk (m^2/s^5)", y = "Chorea") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.subtitle = element_text(hjust=0.5)) +
  scale_color_manual(values=c("#AED6F1","#2874A6"),
                     name="Group",
                     labels=c("TFC <= 10 n = 22", "TFC > 10   n = 13"))
ggsave("iWear/Correlation/jerk_stand_chorea_group.png")

# polynomial regression
temp <- df[,c("as_correct","Group","Jerk_Sit","Jerk_Stand","Chorea")] # remove control group
temp <- na.omit(temp)                                                 # 35 participants
table(temp$Group)

## Jerk of sitting postural sway with chorea subscore
poly_model <- function(Jerk, Chorea){
  mod1 <- lm(Chorea ~ Jerk); print(summary(mod1)$r.squared)
  mod2 <- lm(Chorea ~ I(Jerk^2)); print(summary(mod2)$r.squared)
  mod3 <- lm(Chorea ~ I(Jerk^3)); print(summary(mod3)$r.squared)
  mod4 <- lm(Chorea ~ poly(Jerk,2)); print(summary(mod4)$r.squared)
  mod5 <- lm(Chorea ~ poly(Jerk,3)); print(summary(mod5)$r.squared)
}
poly_model(temp$Jerk_Sit, temp$Chorea)

### lm(Chorea ~ Jerk)
ggplot(temp, aes(x = Jerk_Sit, y = Chorea)) +
  geom_point(aes(color = Group)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(aes(color = Group), method = "lm", formula = y ~ x) +
  labs(title = "Jerk of sitting postural sway with chorea subscore",
       subtitle = "r = 0.5414237",
       x = "Jerk (m^2/s^5)", y = "Chorea") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.subtitle = element_text(hjust=0.5)) +
  scale_color_manual(values=c("#AED6F1","#2874A6"),
                     name="Group",
                     labels=c("TFC <= 10 n = 22", "TFC > 10   n = 13"))
ggsave("iWear/Correlation/poly_model_sit.png")

### lm(Chorea ~ poly(Jerk, 3))
ggplot(temp, aes(x = Jerk_Sit, y = Chorea)) +
  geom_point(aes(color = Group)) +
  geom_smooth(method = "lm", formula = y ~ poly(x,3)) +
  geom_smooth(aes(color = Group), method = "lm", formula = y ~ poly(x,3)) +
  labs(title = "Jerk of sitting postural sway with chorea subscore",
       subtitle = "r = 0.5414237",
       x = "Jerk (m^2/s^5)", y = "Chorea") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.subtitle = element_text(hjust=0.5)) +
  scale_color_manual(values=c("#AED6F1","#2874A6"),
                     name="Group",
                     labels=c("TFC <= 10 n = 22", "TFC > 10   n = 13"))
ggsave("iWear/Correlation/poly_model_sit3.png")

## Jerk of standing feet apart with chorea subscore
poly_model(temp$Jerk_Stand, temp$Chorea)

### lm(Chorea ~ Jerk)
ggplot(temp, aes(x = Jerk_Stand, y = Chorea)) +
  geom_point(aes(color = Group)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_smooth(aes(color = Group), method = "lm", formula = y ~ x) +
  labs(title = "Jerk of standing feet apart with chorea subscore",
       subtitle = "r = 0.3768734",
       x = "Jerk (m^2/s^5)", y = "Chorea") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.subtitle = element_text(hjust=0.5)) +
  scale_color_manual(values=c("#AED6F1","#2874A6"),
                     name="Group",
                     labels=c("TFC <= 10 n = 22", "TFC > 10   n = 13"))
ggsave("iWear/Correlation/poly_model_stand.png")

### lm(Chorea ~ poly(Jerk, 3))
ggplot(temp, aes(x = Jerk_Stand, y = Chorea)) +
  geom_point(aes(color = Group)) +
  geom_smooth(method = "lm", formula = y ~ poly(x,3)) +
  geom_smooth(aes(color = Group), method = "lm", formula = y ~ poly(x,3)) +
  labs(title = "Jerk of standing feet apart with chorea subscore",
       subtitle = "r = 0.3768734",
       x = "Jerk (m^2/s^5)", y = "Chorea") +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5)) +
  theme(plot.subtitle = element_text(hjust=0.5)) +
  scale_color_manual(values=c("#AED6F1","#2874A6"),
                     name="Group",
                     labels=c("TFC <= 10 n = 22", "TFC > 10   n = 13"))
ggsave("iWear/Correlation/poly_model_stand3.png")

# ggpairs
library(GGally)
my_fn <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method = "lm", ...)
}
ggpairs(df, columns = c(9,3:8), lower = list(continuous = my_fn))