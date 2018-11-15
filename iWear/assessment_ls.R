library(tibble)

setwd("/Users/iris/Desktop/NRL/iWear")
df <- read.csv("HDIWear_DATA_2018-10-22.csv", stringsAsFactors=FALSE, na.strings=c("",NA))
df_HD <- df[which(df$hd_or_healthy==1),]
fields <- colnames(df)

#LifeSpace for HD only (except germany) 
ls_temp <- grep("^lifespace", fields)
ls_idx <- seq(ls_temp[1]+1, ls_temp[2]-1)
ls <- c("as_correct", fields[ls_idx])
germany_idx <- which(df_HD$redcap_data_access_group=="germany")
df_HD_ls <- df_HD[-germany_idx,ls]

#Missing Value
##If ls1=0, set the following fields to zero.
ls1_idx <- which(df_HD_ls$ls1==0)
df_HD_ls[ls1_idx,c(3:21)][is.na(df_HD_ls[ls1_idx,c(3:21)])] <- 0

##If ls2=0, set the following fields to zero.
ls2_idx <- which(df_HD_ls$ls2==0)
df_HD_ls[ls2_idx,c(7:21)][is.na(df_HD_ls[ls2_idx,c(7:21)])] <- 0

##If ls3=0, set the following fields to zero.
ls3_idx <- which(df_HD_ls$ls3==0)
df_HD_ls[ls3_idx,c(11:21)][is.na(df_HD_ls[ls3_idx,c(11:21)])] <- 0

##If ls4=0, set the following fields to zero.
ls4_idx <- which(df_HD_ls$ls4==0)
df_HD_ls[ls4_idx,c(15:21)][is.na(df_HD_ls[ls4_idx,c(15:21)])] <- 0

##If ls5=0, set the following fields to zero.
ls5_idx <- which(df_HD_ls$ls5==0)
df_HD_ls[ls5_idx,c(19:21)][is.na(df_HD_ls[ls5_idx,c(19:21)])] <- 0

which(is.na(df_HD_ls), arr.ind=T)
df_HD_ls <- na.omit(df_HD_ls)

#LS1
##LS1A Did you use aids or equipment?
##LS1H Did you need help from another person?
##0-Yes 1-No 2-Don't know or refused
n <- nrow(df_HD_ls)
ls1_indep <- rep(NA,n)
df_HD_ls <- add_column(df_HD_ls, ls1_indep, .after="ls1h")
for (i in 1:n){
  #1-Personal assistance
  if (df_HD_ls$ls1h[i]==0){
    df_HD_ls$ls1_indep[i] <- 1
  }
  #1.5-Equipment only
  else if (df_HD_ls$ls1h[i]==1 & df_HD_ls$ls1a[i]==0){
    df_HD_ls$ls1_indep[i] <- 1.5
  }
  #2-No equipment or personal assistance
  else if (df_HD_ls$ls1h[i]==1 & df_HD_ls$ls1a[i]==1){
    df_HD_ls$ls1_indep[i] <- 2
  }
}

ls1_score <- rep(NA,n)
df_HD_ls <- add_column(df_HD_ls, ls1_score, .after="ls1_indep")
df_HD_ls$ls1_score <- df_HD_ls$ls1 * df_HD_ls$ls1f * df_HD_ls$ls1_indep

#LS2
ls2_indep <- rep(NA,n)
df_HD_ls <- add_column(df_HD_ls, ls2_indep, .after="ls2h")
for (i in 1:n){
  #1-Personal assistance
  if (df_HD_ls$ls2h[i]==0){
    df_HD_ls$ls2_indep[i] <- 1
  }
  #1.5-Equipment only
  else if (df_HD_ls$ls2h[i]==1 & df_HD_ls$ls2a[i]==0){
    df_HD_ls$ls2_indep[i] <- 1.5
  }
  #2-No equipment or personal assistance
  else if (df_HD_ls$ls2h[i]==1 & df_HD_ls$ls2a[i]==1){
    df_HD_ls$ls2_indep[i] <- 2
  }
}

ls2_score <- rep(NA,n)
df_HD_ls <- add_column(df_HD_ls, ls2_score, .after="ls2_indep")
df_HD_ls$ls2_score <- (df_HD_ls$ls2*2) * df_HD_ls$ls2f * df_HD_ls$ls2_indep

#LS3
ls3_indep <- rep(NA,n)
df_HD_ls <- add_column(df_HD_ls, ls3_indep, .after="ls3h")
for (i in 1:n){
  #1-Personal assistance
  if (df_HD_ls$ls3h[i]==0){
    df_HD_ls$ls3_indep[i] <- 1
  }
  #1.5-Equipment only
  else if (df_HD_ls$ls3h[i]==1 & df_HD_ls$ls3a[i]==0){
    df_HD_ls$ls3_indep[i] <- 1.5
  }
  #2-No equipment or personal assistance
  else if (df_HD_ls$ls3h[i]==1 & df_HD_ls$ls3a[i]==1){
    df_HD_ls$ls3_indep[i] <- 2
  }
}

ls3_score <- rep(NA,n)
df_HD_ls <- add_column(df_HD_ls, ls3_score, .after="ls3_indep")
df_HD_ls$ls3_score <- (df_HD_ls$ls3*3) * df_HD_ls$ls3f * df_HD_ls$ls3_indep

#LS4
ls4_indep <- rep(NA,n)
df_HD_ls <- add_column(df_HD_ls, ls4_indep, .after="ls4h")
for (i in 1:n){
  #1-Personal assistance
  if (df_HD_ls$ls4h[i]==0){
    df_HD_ls$ls4_indep[i] <- 1
  }
  #1.5-Equipment only
  else if (df_HD_ls$ls4h[i]==1 & df_HD_ls$ls4a[i]==0){
    df_HD_ls$ls4_indep[i] <- 1.5
  }
  #2-No equipment or personal assistance
  else if (df_HD_ls$ls4h[i]==1 & df_HD_ls$ls4a[i]==1){
    df_HD_ls$ls4_indep[i] <- 2
  }
}

ls4_score <- rep(NA,n)
df_HD_ls <- add_column(df_HD_ls, ls4_score, .after="ls4_indep")
df_HD_ls$ls4_score <- (df_HD_ls$ls4*4) * df_HD_ls$lsf4 * df_HD_ls$ls4_indep

#LS5
ls5_indep <- rep(NA,n)
df_HD_ls <- add_column(df_HD_ls, ls5_indep, .after="ls5h")
for (i in 1:n){
  #1-Personal assistance
  if (df_HD_ls$ls5h[i]==0){
    df_HD_ls$ls5_indep[i] <- 1
  }
  #1.5-Equipment only
  else if (df_HD_ls$ls5h[i]==1 & df_HD_ls$ls5a[i]==0){
    df_HD_ls$ls5_indep[i] <- 1.5
  }
  #2-No equipment or personal assistance
  else if (df_HD_ls$ls5h[i]==1 & df_HD_ls$ls5a[i]==1){
    df_HD_ls$ls5_indep[i] <- 2
  }
}

ls5_score <- rep(NA,n)
df_HD_ls <- add_column(df_HD_ls, ls5_score, .after="ls5_indep")
df_HD_ls$ls5_score <- (df_HD_ls$ls5*5) * df_HD_ls$ls5f * df_HD_ls$ls5_indep

#Total
total_score <- rep(NA,n)
df_HD_ls <- add_column(df_HD_ls, total_score, .after="ls5_score")
df_HD_ls$total_score <- df_HD_ls$ls1_score + df_HD_ls$ls2_score + df_HD_ls$ls3_score + df_HD_ls$ls4_score + df_HD_ls$ls5_score
write.csv(df_HD_ls, "assessment_ls_1022.csv", row.names=F)

#Statistics
mean(df_HD_ls$total_score)
sd(df_HD_ls$total_score)