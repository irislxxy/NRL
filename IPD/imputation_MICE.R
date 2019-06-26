library(mice)
library(VIM)
setwd("~/Desktop/NRL/IPD")

ipd <- read.csv("IPD.csv")
v.list <- c("PID","Study","Site","Group",
            "Age","Gender",
            "modmotscpre","modmotscpost",
            "EQ5Dpre",
            "TFC",
            "UHDRStV1",
            "SDMTpre")
df <- ipd[,v.list]

# count the missing data
na.count <- sapply(df, function(x) sum((is.na(x))))
na.count

# use mice for looking at missing data pattern
df_mice <- df[,-c(1:6)]
md.pattern(df_mice)
aggr_plot <- aggr(df_mice, col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, 
                  labels=names(df_mice), cex.axis=0.7, cex.numbers=0.7, 
                  gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

# exclude participants with missing outcome data
df2 <- df[!is.na(df$modmotscpost),]

na.count2 <- sapply(df2, function(x) sum((is.na(x))))
na.count2

df_mice2 <- df2[,-c(1:6)]
md.pattern(df_mice2)
aggr_plot2 <- aggr(df_mice2, col=c('navyblue','red'),
                   numbers=TRUE, sortVars=TRUE, 
                   ylabs=c("Histogram of missing data","Pattern"),
                   labels=names(df_mice),
                   cex.axis=0.7, cex.numbers=0.7, gap=3)

# list of the available imputation methods
methods(mice)

# normality test
shapiro.test(df_mice2$modmotscpre) ## p-value = 0.3465 normally distributed
shapiro.test(df_mice2$EQ5Dpre) ## p-value = 6.928e-09 not normally distributed
shapiro.test(df_mice2$TFC) ## p-value = 0.009832 not normally distributed
shapiro.test(df_mice2$UHDRStV1) ## p-value = 0.5634 normally distributed
shapiro.test(df_mice2$SDMTpre) ## p-value = 0.0803 normally distributed

# impute the missing data
imp30 <- mice(df2,
              method='pmm',
              m=30, 
              maxit=10, 
              seed=2019) 
summary(imp30)
imp30$imp$modmotscpre ## check the imputed data
imp30$meth ## check the imputation method
# complete(imp30,1)

# pool
fit <- with(imp30, lm(modmotscpost ~ Age + Gender + modmotscpre + EQ5Dpre + TFC + UHDRStV1 +SDMTpre))
summary(pool(fit))

save(imp30, file = "imp30.RData")