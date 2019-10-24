library(mice)
setwd("~/Desktop/NRL/IPD")
source("PS_Functions.R")
load("imp30.Rdata")

# ATE/ATT - regression estimation
causal_effect <- function(imp){
  
  glm <- glm(Group ~ Age + Gender + I(modmotscpre^2) + EQ5Dpre + I(TFC^2) + UHDRStV1 +SDMTpre,
             data = imp, family = "binomial")
  ps <- predict(glm, type = "response")
  logit_ps <- log(ps/(1 - ps))
  
  ## eliminate non-overlapping cases
  group <- ifelse(imp$Group == "control", 0, 1)
  ovlp_ind1 <- ovlp_ind(trt = group, lps = logit_ps, caliper = .2) ## create overlap indicator
  sum(ovlp_ind1)
  table(ovlp_ind1, group)
  
  ## create a subset of only overlapping cases
  imp_ovlp <- imp[ovlp_ind1,]
  
  ## create a data subset for group intervention
  dat1 <- subset(imp_ovlp, subset = Group == "intervention")
  
  ## create a data subset for group control
  dat0 <- subset(imp_ovlp, subset = Group == "control")
  
  ## regression model
  lmY1 <- lm(modmotscpost ~  Age + Gender + I(modmotscpre^2) + EQ5Dpre + I(TFC^2) + UHDRStV1 +SDMTpre, 
             data = dat1)
  lmY0 <- lm(modmotscpost ~  Age + Gender + I(modmotscpre^2) + EQ5Dpre + I(TFC^2) + UHDRStV1 +SDMTpre, 
             data = dat0)
  ## ATE
  Y1hats <- predict(lmY1, newdata = imp_ovlp)
  Y0hats <- predict(lmY0, newdata = imp_ovlp)
  ATE <- mean(Y1hats - Y0hats)
  cat("ATE: ", t.test(Y1hats - Y0hats, conf.level = 0.95)$conf.int, "\n")
  
  ## ATT
  Y0hatsATT <- predict(lmY0, newdata = dat1)
  ATT <- mean(dat1$modmotscpost - Y0hatsATT)
  cat("ATT: ", t.test(dat1$modmotscpost - Y0hatsATT, conf.level = 0.95)$conf.int, "\n")
}

imp1 <- complete(imp30, 1)
causal_effect(imp1)

imp2 <- complete(imp30, 2)
causal_effect(imp2)

imp3 <- complete(imp30, 3)
causal_effect(imp3)

imp4 <- complete(imp30, 4)
causal_effect(imp4)

imp5 <- complete(imp30, 5)
causal_effect(imp5)

imp_all <- imp1
for (i in 2:30){
  imp <- complete(imp30, i)
  imp_all <- rbind(imp_all, imp) 
}
causal_effect(imp_all)

# mean diff
ipa_dat1 <- complete(imp30, 1)
ipa_dat1$Group <- ifelse(ipa_dat1$Group == "control", 0, 1) 

pfe1 <- lm(modmotscpost ~ Group, data = ipa_dat1)
summary(pfe1)

pfe2 <- t.test(x = with(ipa_dat1, modmotscpost[Group == 0]),
               y = with(ipa_dat1, modmotscpost[Group == 1]),
               var.equal = TRUE)
diff(pfe2$estimate)
pfe2$p.value

pfe3 <- t.test(x = with(ipa_dat1, modmotscpost[Group == 0]),
               y = with(ipa_dat1, modmotscpost[Group == 1]),
               var.equal = FALSE)
diff(pfe3$estimate)
pfe3$p.value

boxplot(modmotscpost ~ Group, data = ipa_dat1, main = "Motor Function for People with HD", ylab = "Modified Motor Score", names = c("Controls", "Exercise"))
points(factor(ipa_dat1$Group), ipa_dat1$modmotscpost)

with(ipa_dat1, by(modmotscpost, INDICES = Group, FUN = var))
# var(with(ipa_dat1, modmotscpost[Group == 0]))
# var(ipa_dat1[which(ipa_dat1$Group==0), "modmotscpost"])
