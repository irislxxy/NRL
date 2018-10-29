library(ggplot2)
library(reshape)
setwd("/Users/iris/Desktop/NRL/iWear/Dual Task")

df <- read.csv("DTE_1022.csv", stringsAsFactors=FALSE)
df$hd_or_healthy <- factor(df$hd_or_healthy)

#gait speed
colnames(df)[45:47] <- c("Walk", "Alphabet", "EOL")
df_speed <- melt(df[,c("as_correct", "hd_or_healthy", colnames(df)[45:47])],
                 id=c("as_correct","hd_or_healthy"))

df_speed_HD <- df_speed[which(df_speed$hd_or_healthy==1),]
df_speed_CO <- df_speed[which(df_speed$hd_or_healthy==2),]

##box plot with scatter
ggplot(df_speed_HD) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value), color = "#1B4F72") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Gait Speed (m/s)")
ggsave("gaitspeed_box_HD.png")

ggplot(df_speed_CO) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value), color = "#AED6F1") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=15)) +
  labs(y = "Gait Speed (m/s)")
ggsave("gaitspeed_box_CO.png")

ggplot(df_speed) +
  geom_boxplot(aes(x = variable, y = value)) +
  geom_point(aes(x = variable, y = value, color= hd_or_healthy)) +
  theme_bw() +
  theme(line = element_blank()) +
  labs(x = "Conditions", y = "Gait Speed (m/s)") +
  scale_color_manual(values=c("#1B4F72", "#AED6F1"),
                     name="Group",
                     breaks=c("1", "2"),
                     labels=c("HD", "Control"))
ggsave("gaitspeed_box.png")

##bar plot
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

df_speed2 <- data_summary(df_speed, varname="value", groupnames=c("hd_or_healthy", "variable"))

ggplot(df_speed2, aes(x=variable, y=value, fill=hd_or_healthy)) +
   geom_bar(stat="identity", position=position_dodge()) +
   geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.2, position=position_dodge(.9))
ggsave("gaitspeed_bar.png")