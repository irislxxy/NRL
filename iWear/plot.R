library(ggplot2)
library(reshape)
setwd("/Users/iris/Desktop/NRL/iWear/Dual Task")

df <- read.csv("DTE.csv", stringsAsFactors=FALSE)
df$hd_or_healthy <- factor(df$hd_or_healthy)

#x-cognition y-motor
#standing
df_s <- df[which(df$stroop_it_correct!=0),] #GH4/GH8

##faeofs
ggplot(df_s, aes(x=faeofs_dte, y=motor_faeofs_dte, color = hd_or_healthy))+
  geom_point()
ggsave("faeofs.png")

##fteofs
ggplot(df_s, aes(x=fteofs_dte, y=motor_fteofs_dte, color = hd_or_healthy))+
  geom_point()
ggsave("fteofs.png")

##faeofoam
ggplot(df_s, aes(x=faeofoam_dte, y=motor_faeofoam_dte, color = hd_or_healthy))+
  geom_point()
ggsave("faeofoam.png")

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

#bar plot
##gait speed
df_speed <- melt(df[,c("as_correct", "hd_or_healthy", "Walk.Gait.Speed", "Alphabet.Gait.Speed", "EOL.Gait.Speed")],
                 id=c("as_correct","hd_or_healthy"))

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df_speed <- data_summary(df_speed, varname="value", groupnames=c("hd_or_healthy", "variable"))

ggplot(df_speed, aes(x=variable, y=value, fill=hd_or_healthy)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2, position=position_dodge(.9))
ggsave("gaitspeed.png")

##sway
df_sway <- melt(df[,c("as_correct", "hd_or_healthy", colnames(df)[c(36,37,39,40,42,43)])],
                id=c("as_correct","hd_or_healthy"))
df_sway <- data_summary(df_sway, varname="value", groupnames=c("hd_or_healthy", "variable"))

ggplot(df_sway, aes(x=variable, y=value, fill=hd_or_healthy)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2, position=position_dodge(.9))

##cognitive
df_cog <- melt(df[,c("as_correct", "hd_or_healthy", colnames(df)[grep("crr", colnames(df))])],
               id=c("as_correct","hd_or_healthy"))
df_cog <- data_summary(df_cog, varname="value", groupnames=c("hd_or_healthy", "variable"))

ggplot(df_cog, aes(x=variable, y=value, fill=hd_or_healthy)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2, position=position_dodge(.9))

##TUG
df_tug <- melt(df[,c("as_correct", "hd_or_healthy", colnames(df)[50:51])],
               id=c("as_correct","hd_or_healthy"))
df_tug <- data_summary(df_tug, varname="value", groupnames=c("hd_or_healthy", "variable"))

ggplot(df_tug, aes(x=variable, y=value, fill=hd_or_healthy)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2, position=position_dodge(.9))
ggsave("tug.png")