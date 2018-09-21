library(tibble)
library(rJava)
library(xlsxjars)
library(xlsx)
setwd("/Users/iris/Desktop/NRL/iWear")

df <- read.csv("HDIWear_DATA_2018-07-30.csv", stringsAsFactors=FALSE)
fields <- colnames(df)

#
##Gait Speed/Total Sway Area/TUG Duration/Correct Response Rate
f1 <- read.csv("Dual Task/ANOVA.csv", stringsAsFactors=FALSE)

##Double Support
f_ds <- read.csv("APDM/Walk_DoubleSupport.csv", header=T, stringsAsFactors=FALSE)
f_ds$as_correct[1:20] <- paste0("IW",substr(f_ds$as_correct[1:20],3,4),"GHI")
f_ds <-  f_ds[order(f_ds$as_correct),] 
f1$Walk.Double.Support <- (f_ds$Walk.Double.Support.L + f_ds$Walk.Double.Support.R)/2 
f1$Alphabet.Double.Support <- (f_ds$Alphabet.Double.Support.L + f_ds$Alphabet.Double.Support.R)/2 
f1$EOL.Double.Support <- (f_ds$EOL.Double.Support.L + f_ds$EOL.Double.Support.R)/2 

##Stride Length
f_sl <- read.csv("APDM/Walk_StrideLength.csv", header=T, stringsAsFactors=FALSE)
f_sl$as_correct[1:20] <- paste0("IW",substr(f_sl$as_correct[1:20],3,4),"GHI")
f_sl <-  f_sl[order(f_sl$as_correct),] 
f1$Walk.Stride.Length <- (f_sl$Walk.Stride.Length.L + f_sl$Walk.Stride.Length.R)/2 
f1$Alphabet.Stride.Length <- (f_sl$Alphabet.Stride.Length.L + f_sl$Alphabet.Stride.Length.R)/2 
f1$EOL.Stride.Length <- (f_sl$EOL.Stride.Length.L + f_sl$EOL.Stride.Length.R)/2 

##TMS/TFC/FA
motor <- fields[251:282]
uhdrs <- fields[283:287]
func <- fields[289:313]

TMS <- rowSums(df[motor], na.rm=TRUE)
df <- add_column(df, TMS, .after = "motor_diagnostic")

TFC <- rowSums(df[uhdrs], na.rm=TRUE)
df <- add_column(df, TFC, .after = "uhdrs_info_source")

FA <- rowSums(df[func], na.rm=TRUE)
df <- add_column(df, FA, .after = "func_assess_25")

f2 <- df[,c("as_correct","TMS","TFC","FA")]

##IPAQ
f_ipaq_pre <- read.xlsx("IPAQ-SFScoring(pre).xlsx", 2, startRow=6)
f_ipaq_pre  <- f_ipaq_pre [1:72,c(1,24)]
colnames(f_ipaq_pre) <- c("as_correct","ipaq_pre")

f_ipaq_post <- read.xlsx("IPAQ-SFScoring(post).xlsx", 2, startRow=6)
f_ipaq_post  <- f_ipaq_post[1:65,c(1,24)]
colnames(f_ipaq_post) <- c("as_correct","ipaq_post")

##LifeSpace: HD only (except germany)
f_ls <- read.csv("assessment_ls.csv", stringsAsFactors=FALSE)
f_ls <- f_ls[,c(1,32)]
colnames(f_ls) <- c("as_correct","lifespace")

##Merge
f <- merge(f1, f2, all.x = T)
f <- merge(f, f_ipaq_pre, all.x = T)
f <- merge(f, f_ipaq_post, all.x = T)
f <- merge(f, f_ls, all.x = T)

write.csv(f, "normality.csv", row.names = F)

#Normality Test
##Shapiro-Wilk test
for (x in colnames(f)[-c(1,2)]){
  t1 <- shapiro.test(f[,x])
  if (t1$p.value > 0.05){
    cat(x, ":", "The p-value is", t1$p.value, ". For the p-value > 0.05, the distribution is not significantly different from normal distribution.\n\n")
  }
  else{
    cat(x, ":", "The p-value is", t1$p.value, ". For the p-value < 0.05, the distribution is significantly different from normal distribution.\n\n")
  }
}

#Homogeneity of Variances
##Levene's test
library(car)
for (x in colnames(f)[-c(1,2,33)]){
  t2 <- leveneTest(f[,x] ~ as.factor(f$hd_or_healthy))
  if (t2$`Pr(>F)`[1] > 0.05){
    cat(x, ":", "The p-value is", t2$`Pr(>F)`[1], ". For the p-value > 0.05, the variance is not significantly different for the HD and control groups.\n\n")
  }
  else{
    cat(x, ":", "The p-value is", t2$`Pr(>F)`[1], ". For the p-value < 0.05, the variance is significantly different for the HD and control groups.\n\n")
  }
}