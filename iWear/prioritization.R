setwd("/Users/iris/Desktop/NRL/iWear/Dual Task")
data <- read.csv("DTE_1028.csv", stringsAsFactors=FALSE)

fields <- colnames(data)
grep("dte", fields)
fields[grep("dte", fields)]
idx <- c(9, 38, 13, 41, 17, 44, 30, 48, 35, 49)
column <- c("as_correct", "hd_or_healthy", fields[idx])
df <- data[,column]

prior <- function(cognitive, motor){
  prior <- rep(NA, nrow(df))
  for (i in 1:nrow(df)){
    if (is.na(cognitive[i]) | is.infinite(cognitive[i])){
      prior[i] <- "cognitive NA"
    }

    else if (is.na(motor[i]) | is.infinite(motor[i])){
      prior[i] <- "motor NA"
    }
    
    else if (cognitive[i] == 0){
      prior[i] <- "cognitive 0"
    }
    
    else if (motor[i] == 0){
      prior[i] <- "motor 0"
    }
    
    else if (cognitive[i] > 0){
      if (motor[i] > 0){
        prior[i] <- "mutual facilitation"
      }
      else {
        prior[i] <- "cognitive priority"
      }
    }
    else{
      if (motor[i] > 0){
        prior[i] <- "motor priority"
      }
      else {
        prior[i] <- "mutual interference"
      }
    }
  }
  return(prior)
}

df$faeofs_prior <- prior(df$faeofs_dte, df$motor_faeofs_dte)
df$fteofs_prior <- prior(df$fteofs_dte, df$motor_fteofs_dte)
df$faeofoam_prior <- prior(df$faeofoam_dte, df$motor_faeofoam_dte)
df$alphabet_prior <- prior(df$wwt_dte, df$motor_alphabet_dte)
df$eol_prior <- prior(df$wwt_eol_dte, df$motor_eol_dte)

write.csv(df, "prioritization.csv", row.names = F)