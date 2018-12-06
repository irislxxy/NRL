library(tibble)
setwd("/Users/iris/Desktop/NRL/iWear")

df <- read.csv("iWear_complete.csv")
fields <- colnames(df)

whodas_temp <- grep("^whodas", fields)
whodas_idx <- whodas_temp[1:12]
whodas_score <- rep(NA,nrow(df))
df <- add_column(df, whodas_score, .after="whodas_15b")
df$whodas_score <- rowSums(df[whodas_idx])

write.csv(df, "iWear_complete_whodas_birth.csv", row.names=F)