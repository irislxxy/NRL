library(tibble)
library(lsr)
setwd("/Users/iris/Desktop/NRL/PreActiveHD")

df <- read.csv("PreActiveHD_DATA_2019-08-21_0622.csv")
df_label <- read.csv("PreActiveHD_DATA_LABELS_2019-06-28_0407.csv")
colnames(df_label)[87:105]
for (i in 87:105){
  colnames(df)[i] <- paste(colnames(df)[i], i-86, sep = "_")
}
fields <- colnames(df)
fields[87:105]

# IPAQ
## replace NA with 0
idx <- grep("ipaq",fields)[-1]
df_ipaq <- df[,c(1,idx)]
df_ipaq[is.na(df_ipaq)] <- 0

## Total MET-minutes/week at work = Vig (METs*min*days) + Mod (METs*min*days) + Walk (METs*min*days)
work <- 8.0*df_ipaq$ipaq_sl_ovday*(df_ipaq$ipaq_sl_ovdhrs*60+df_ipaq$ipaq_sl_ovdmin) + 
        4.0*df_ipaq$ipaq_sl_omday*(df_ipaq$ipaq_sl_omdhrs*60+df_ipaq$ipaq_sl_omdmin) +
        3.3*df_ipaq$ipaq_sl_owday*(df_ipaq$ipaq_sl_owdhrs*60+df_ipaq$ipaq_sl_owdmin)

## Total MET-minutes/week for transportation = Cycle (METs*min*days) + Walk (METs*min*days)
## Do not consider travel in a motor vehicle - tm Q8/Q9 
transportation <- 6.0*df_ipaq$ipaq_sl_tbday*(df_ipaq$ipaq_sl_tbwhrs*60+df_ipaq$ipaq_sl_tbwmin) +
                  3.3*df_ipaq$ipaq_sl_twday*(df_ipaq$ipaq_sl_twdhrs*60+df_ipaq$ipaq_sl_twdmin)

## Total MET-minutes/week from domestic and garden = Vig (METs*min*days) yard work + Mod (METs*min*days) yard work + Mod (METs*min*days) inside chores
chores <- 8.0*df_ipaq$ipaq_sl_gvday*(df_ipaq$ipaq_sl_gvdhrs*60+df_ipaq$ipaq_sl_gvmin) +
          4.0*df_ipaq$ipaq_sl_gmday*(df_ipaq$ipaq_sl_gmdhrs*60+df_ipaq$ipaq_sl_gmdmin) +
          4.0*df_ipaq$ipaq_sl_hmday*(df_ipaq$ipaq_sl_hmdhrs*60+df_ipaq$ipaq_sl_hmdmin)

## Total MET-minutes/week in leisure-time = Walk (METs*min*days) + Vig (METs*min*days) + Mod (METs*min*days)
leisure <- 3.3*df_ipaq$ipaq_sl_lwday*(df_ipaq$ipaq_sl_lwdhrs*60+df_ipaq$ipaq_sl_lwdmin) + 
           8.0*df_ipaq$ipaq_sl_lvday*(df_ipaq$ipaq_sl_lvdhrs*60+df_ipaq$ipaq_sl_lvdmin) +
           4.0*df_ipaq$ipaq_sl_lmday*(df_ipaq$ipaq_sl_lmdhrs*60+df_ipaq$ipaq_sl_lmdmin)

## Do not consider sitting - sd/se Q26/Q27

## Total
MET <- work + transportation + chores + leisure

# BREQ2
amotivation <- (df$breq2_why_5 + df$breq2_bother_9 + df$breq2_no_point_12 + df$breq2_waste_19)/4
external <- (df$breq2_others_say_1 + df$breq2_fam_fr_6 + df$breq2_others_please_11 + df$breq2_press_fam_16)/4
introjected <- (df$breq2_guilt_2 + df$breq2_asham_7 + df$breq2_failure_13)/3
identified <- (df$breq2_value_3 + df$breq2_imp_8 + df$breq2_ex_reg_14 + df$breq2_restless_17)/4
intrinsic <- (df$breq2_fun_4 + df$breq2_enjoy_10 + df$breq2_pleasure_15 + df$breq2_satisf_18)/4

# Cognition
cognition_start <- grep("hd_pro_keep_track", fields)
cognition_stop <- grep("hd_pro_two_tasks", fields)
which(is.na(df[,cognition_start:cognition_stop]), arr.ind=T)
cognition_raw_score <- rowSums(df[,cognition_start:cognition_stop])
cognition_final_score <- ((14*6)-cognition_raw_score) / 14

# Emotional and Behavioral
eb_start <- grep("hd_pro_behavior", fields)
eb_stop <- grep("hd_pro_temper", fields)
which(is.na(df[,eb_start:eb_stop]), arr.ind=T)
eb_raw_score <- rowSums(df[,eb_start:eb_stop])
eb_final_score <- eb_raw_score / 13 # question 9 is missing

# Motor
motor_a_start <- grep("hd_pro_unsteady",fields)
motor_a_stop <- grep("hd_pro_help",fields)
motor_b_start <- grep("hd_pro_cummunicate",fields)
motor_b_stop <- grep("hd_pro_phone",fields)
which(is.na(df[,motor_a_start:motor_b_stop]), arr.ind=T) # PREHD 9_follow-up:hd_pro_help
motor_a_score <- rowSums(df[,motor_a_start:motor_a_stop], na.rm = T) 
motor_b_score <- 4*6 - rowSums(df[,motor_b_start:motor_b_stop])
motor_final_score <- (motor_a_score + motor_b_score) / rowSums(!is.na(df[,motor_a_start:motor_b_stop]))

# Total
total_score <- cognition_final_score + eb_final_score + motor_final_score

# TMS
motor <- fields[grep("motor", fields)]
df_motor <- df[motor]
df_motor[df_motor >= 98] <- NA
TMS <- rowSums(df_motor)

## TFC
uhdrs <- fields[grep("uhdrs", fields)[2:6]]
df_uhdrs <- df[uhdrs]
df_uhdrs[df_uhdrs >= 98] <- NA
TFC <- rowSums(df_uhdrs)

# FA
func <- fields[248:272]
df_func <- df[func]
df_func[df_func >= 98] <- NA
FA <- rowSums(df_func)

# Add Column
df <- add_column(df, MET, .after = "ipaq_long_last_7_days_self_admin_complete")

df <- add_column(df, amotivation, .after = "breq2_waste_19")
df <- add_column(df, external, .after = "amotivation")
df <- add_column(df, introjected, .after = "external")
df <- add_column(df, identified, .after = "introjected")
df <- add_column(df, intrinsic, .after = "identified")

df <- add_column(df, cognition_final_score, .after = "hd_pro_two_tasks")
df <- add_column(df, eb_final_score, .after = "hd_pro_temper")
df <- add_column(df, motor_final_score, .after = "hd_pro_phone")
df <- add_column(df, total_score, .after = "motor_final_score")

df <- add_column(df, TMS, .after="motor_diagnostic")
df <- add_column(df, TFC, .after="uhdrs_carelevel")
df <- add_column(df, FA, .after="func_assess_25")

write.csv(df, "PreActiveHD_Score_0920.csv", row.names = F)

# Calculation
# difference
## PREHD 5/8/12 do not have follow up
df_cal <- df[-c(9,14,21),]
variableList <- c("MET",
                  "amotivation", "external", "introjected", "identified", "intrinsic",
                  "symbol_digit_correct",
                  "smwt_total_distance",
                  "verbalf_subtotal",
                  "gr_st2")
diff <- data.frame(row.names = paste("PREHD",c(1:4,6:7,9:11,13:14)))
for (i in 1:11){
  for (j in variableList){
    # post - pre
    diff[i,j] <- df_cal[i*2,j] - df_cal[i*2-1,j]
  }
}
write.csv(diff, "PreActiveHD_Diff_0820.csv", row.names = T)

cal <- function(df){
  for (i in variableList){
    cat(i,"\n")
    cat("Mean:", round(mean(df[,i],na.rm=T),2), "\n")
    cat("SD:", round(sd(df[,i],na.rm=T),2), "\n")
    cat("Confidence Interval:", t.test(df[,i])$conf.int, "\n\n")
  }
}
cal(diff)

# group
df_1 <- df_cal[which(df_cal$basic_assessnum == 1),]
cal(df_1)

df_2 <- df_cal[which(df_cal$basic_assessnum == 2),]
cal(df_2)

# effect size
for (i in variableList){
  cat(i,"\n")
  cat("Effect Size:", cohensD(df_2[,i], df_1[,i]), "\n\n")
}

# MET 
df_MET <- df[,c(1,81,idx)]
write.csv(df_MET, "PreActiveHD_MET_0820.csv", row.names = F)