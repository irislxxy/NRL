library(readxl)
setwd("/Users/iris/Desktop/NRL/IMUvsVicon")
df_side <- read_excel("paretic.xlsx")

df <- data.frame(Partic_ID=character(),
                 System=character(),
                 Day=character(), 
                 P_or_NP=character(),
                 Step_Count=numeric(),
                 Stance=numeric(),
                 Double_Support=numeric(),
                 Swing=numeric(),
                 Gait_Speed=numeric(),
                 Stride_Length=numeric(),
                 stringsAsFactors=FALSE)


df_avg <- data.frame(Partic_ID=character(),
                     System=character(),
                     Day=character(),
                     P_or_NP=character(),
                     Stance=numeric(),
                     Double_Support=numeric(),
                     Swing=numeric(),
                     Gait_Speed=numeric(),
                     Stride_Length=numeric(),
                     stringsAsFactors=FALSE)

for (i in 1:nrow(df_side)){
    id <- df_side[i,"Partic_ID"]
    s  <- df_side[i,"System"]
    
    # Day1
    if (s == "IMU"){
      f1name <- paste0("APDM/TPAD_RV_Sub",id,"Day1.csv")
      f1 <- read.csv(f1name, skip=1, stringsAsFactors=F)
    }
    else{
      f1name <- paste0("Vicon/sub",id,"day1.xlsx")
      f1 <- read_excel(f1name)
      colnames(f1)[-c(1:3)] <- paste0("X", colnames(f1)[-c(1:3)]) # check.names
    }
    print(f1name)
    
    # Paretic
    side <- df_side[i,"Paretic"]
    StanceName <- paste("Gait - Lower Limb - Stance", side, "(%GCT)")
    DoubleSupportName <- paste("Gait - Lower Limb - Double Support", side, "(%GCT)")
    SwingName <- paste("Gait - Lower Limb - Swing", side, "(%GCT)")
    GaitSpeedName <- paste("Gait - Lower Limb - Gait Speed", side, "(m/s)")
    StrideLengthName <- paste("Gait - Lower Limb - Stride Length", side, "(m)")
    
    for (j in 1:(ncol(f1)-3)){
      Stance <- f1[which(f1$Measure==StanceName), paste0("X",j)]
      DoubleSupport <- f1[which(f1$Measure==DoubleSupportName), paste0("X",j)]
      Swing <- f1[which(f1$Measure==SwingName), paste0("X",j)]
      GaitSpeed <- f1[which(f1$Measure==GaitSpeedName), paste0("X",j)]
      StrideLength <- f1[which(f1$Measure==StrideLengthName), paste0("X",j)]
      row <- c(id,s,1,"Paretic",j,Stance,DoubleSupport,Swing,GaitSpeed,StrideLength)
      df[nrow(df)+1,] <- row
    }
    
    df_avg[4*i-3,"Partic_ID"] <- id
    df_avg[4*i-3,"System"] <- s
    df_avg[4*i-3,"Day"] <- 1
    df_avg[4*i-3,"P_or_NP"] <- "Paretic"
    df_avg[4*i-3,"Stance"] <- f1[which(f1$Measure==StanceName), "Mean"]
    df_avg[4*i-3,"Double_Support"] <- f1[which(f1$Measure==DoubleSupportName), "Mean"]
    df_avg[4*i-3,"Swing"] <- f1[which(f1$Measure==SwingName), "Mean"]
    df_avg[4*i-3,"Gait_Speed"] <- f1[which(f1$Measure==GaitSpeedName), "Mean"]
    df_avg[4*i-3,"Stride_Length"] <- f1[which(f1$Measure==StrideLengthName), "Mean"]

    # Non_Paretic
    side_NP <- df_side[i,"Non_Paretic"]
    StanceName_NP <- paste("Gait - Lower Limb - Stance", side_NP, "(%GCT)")
    DoubleSupportName_NP <- paste("Gait - Lower Limb - Double Support", side_NP, "(%GCT)")
    SwingName_NP <- paste("Gait - Lower Limb - Swing", side_NP, "(%GCT)")
    GaitSpeedName_NP <- paste("Gait - Lower Limb - Gait Speed", side_NP, "(m/s)")
    StrideLengthName_NP <- paste("Gait - Lower Limb - Stride Length", side_NP, "(m)")
    
    for (j in 1:(ncol(f1)-3)){
      Stance_NP <- f1[which(f1$Measure==StanceName_NP), paste0("X",j)]
      DoubleSupport_NP <- f1[which(f1$Measure==DoubleSupportName_NP), paste0("X",j)]
      Swing_NP <- f1[which(f1$Measure==SwingName_NP), paste0("X",j)]
      GaitSpeed_NP <- f1[which(f1$Measure==GaitSpeedName_NP), paste0("X",j)]
      StrideLength_NP <- f1[which(f1$Measure==StrideLengthName_NP), paste0("X",j)]
      row <- c(id,s,1,"Non_Paretic",j,Stance_NP,DoubleSupport_NP,Swing_NP,GaitSpeed_NP,StrideLength_NP)
      df[nrow(df)+1,] <- row
    }
    
    df_avg[4*i-2,"Partic_ID"] <- id
    df_avg[4*i-2,"System"] <- s
    df_avg[4*i-2,"Day"] <- 1
    df_avg[4*i-2,"P_or_NP"] <- "Non_Paretic"
    df_avg[4*i-2,"Stance"] <- f1[which(f1$Measure==StanceName_NP), "Mean"]
    df_avg[4*i-2,"Double_Support"] <- f1[which(f1$Measure==DoubleSupportName_NP), "Mean"]
    df_avg[4*i-2,"Swing"] <- f1[which(f1$Measure==SwingName_NP), "Mean"]
    df_avg[4*i-2,"Gait_Speed"] <- f1[which(f1$Measure==GaitSpeedName_NP), "Mean"]
    df_avg[4*i-2,"Stride_Length"] <- f1[which(f1$Measure==StrideLengthName_NP), "Mean"]
    
    # Day2
    if (s == "IMU"){
      f2name <- paste0("APDM/TPAD_RV_Sub",id,"Day2.csv")
      f2 <- read.csv(f2name, skip=1, stringsAsFactors=F)
    }
    else{
      f2name <- paste0("Vicon/sub",id,"day2.xlsx")
      f2 <- read_excel(f2name)
      colnames(f2)[-c(1:3)] <- paste0("X", colnames(f2)[-c(1:3)]) # check.names
    }
    print(f2name)
    
    # Paretic
    for (j in 1:(ncol(f2)-3)){
      Stance <- f2[which(f2$Measure==StanceName), paste0("X",j)]
      DoubleSupport <- f2[which(f2$Measure==DoubleSupportName), paste0("X",j)]
      Swing <- f2[which(f2$Measure==SwingName), paste0("X",j)]
      GaitSpeed <- f2[which(f2$Measure==GaitSpeedName), paste0("X",j)]
      StrideLength <- f2[which(f2$Measure==StrideLengthName), paste0("X",j)]
      row <- c(id,s,2,"Paretic",j,Stance,DoubleSupport,Swing,GaitSpeed,StrideLength)
      df[nrow(df)+1,] <- row
    }
    
    df_avg[4*i-1,"Partic_ID"] <- id
    df_avg[4*i-1,"System"] <- s
    df_avg[4*i-1,"Day"] <- 2
    df_avg[4*i-1,"P_or_NP"] <- "Paretic"
    df_avg[4*i-1,"Stance"] <- f2[which(f2$Measure==StanceName), "Mean"]
    df_avg[4*i-1,"Double_Support"] <- f2[which(f2$Measure==DoubleSupportName), "Mean"]
    df_avg[4*i-1,"Swing"] <- f2[which(f2$Measure==SwingName), "Mean"]
    df_avg[4*i-1,"Gait_Speed"] <- f2[which(f2$Measure==GaitSpeedName), "Mean"]
    df_avg[4*i-1,"Stride_Length"] <- f2[which(f2$Measure==StrideLengthName), "Mean"]
    
    # Non_Paretic
    for (j in 1:(ncol(f2)-3)){
      Stance_NP <- f2[which(f2$Measure==StanceName_NP), paste0("X",j)]
      DoubleSupport_NP <- f2[which(f2$Measure==DoubleSupportName_NP), paste0("X",j)]
      Swing_NP <- f2[which(f2$Measure==SwingName_NP), paste0("X",j)]
      GaitSpeed_NP <- f2[which(f2$Measure==GaitSpeedName_NP), paste0("X",j)]
      StrideLength_NP <- f2[which(f2$Measure==StrideLengthName_NP), paste0("X",j)]
      row <- c(id,s,2,"Non_Paretic",j,Stance_NP,DoubleSupport_NP,Swing_NP,GaitSpeed_NP,StrideLength_NP)
      df[nrow(df)+1,] <- row
    }
    
    df_avg[4*i,"Partic_ID"] <- id
    df_avg[4*i,"System"] <- s
    df_avg[4*i,"Day"] <- 2
    df_avg[4*i,"P_or_NP"] <- "Non_Paretic"
    df_avg[4*i,"Stance"] <- f2[which(f2$Measure==StanceName_NP), "Mean"]
    df_avg[4*i,"Double_Support"] <- f2[which(f2$Measure==DoubleSupportName_NP), "Mean"]
    df_avg[4*i,"Swing"] <- f2[which(f2$Measure==SwingName_NP), "Mean"]
    df_avg[4*i,"Gait_Speed"] <- f2[which(f2$Measure==GaitSpeedName_NP), "Mean"]
    df_avg[4*i,"Stride_Length"] <- f2[which(f2$Measure==StrideLengthName_NP), "Mean"]
}

write.csv(df, "data.csv", row.names = F)
write.csv(df, "data_avg.csv", row.names = F)
