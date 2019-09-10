setwd("~/Desktop/NRL/EnrollHD/Raw Data")

# participation
participation <- read.csv("participation.csv", sep = "\t")
participation_sub <- participation[,c("subjid","subjstat","hdcat_0","hdcat_l","age_0")]

assessment <- read.csv("assessment.csv", sep = "\t")

# profile
profile <- read.csv("profile.csv", sep = "\t")
profile_sub <- profile[,c("subjid","sex","caghigh","sxsubj")]

# pharmacotx
pharmacotx <- read.csv("pharmacotx.csv", sep = "\t", stringsAsFactors=FALSE)
pharmacotx_sub <- pharmacotx[,c("subjid","cmtrt__ing","cmtrt__atc",
                                "cmstdy","cmenrf","cmendy")]
pharmacotx_sub[which(pharmacotx_sub$cmenrf == 1), "cmendy"] <- 9999
pharmacotx_sub[pharmacotx_sub == 9996] <- NA
pharmacotx_sub[pharmacotx_sub == 9997] <- NA
pharmacotx_sub[pharmacotx_sub == 9998] <- NA
pharmacotx_sub <- na.omit(pharmacotx_sub) # 76089-69884=6205

# if cmstdy<=visdy & (cmenrf=1|cmendy>visdy) then ondrug=1;
# Aspirin  = cmtrt__ing="Acetylsalicylic acid" & ondrug=1;
# Statin=(cmtrt__atc="C10AA") & ondrug=1;
# AntiDep = (substr(cmtrt__atc, 1,4)="N06A") & ondrug=1;
# AntiPsy = (substr(cmtrt__atc, 1,4)="N05A") & ondrug=1;
pharmacotx_sub$ondrug <- 0
for (i in 1:nrow(pharmacotx_sub)){
  if (pharmacotx_sub$cmstdy[i] <= 0 & (pharmacotx_sub$cmenrf[i] == 1|pharmacotx_sub$cmendy[i] > 0)){
      pharmacotx_sub$ondrug[i] <- 1
  }
}
pharmacotx_ondrug <- pharmacotx_sub[which(pharmacotx_sub$ondrug == 1),]

subjid <- unique(pharmacotx_ondrug$subjid)
drug <- as.data.frame(subjid)
drug$Aspirin <- 0  # boolean
drug$AspirinNum <- 0 # number
drug$Statin <- 0
drug$StatinNum <- 0
drug$AntiDep <- 0
drug$AntiDepNum <- 0
drug$AntiPsy <- 0
drug$AntiPsyNum <- 0

for (i in 1:nrow(pharmacotx_ondrug)){
  id <- pharmacotx_ondrug$subjid[i]
  if (pharmacotx_ondrug$cmtrt__ing[i] == "Acetylsalicylic acid"){
    drug[which(drug$subjid == id), "Aspirin"] = 1
    drug[which(drug$subjid == id), "AspirinNum"] = drug[which(drug$subjid == id), "AspirinNum"] + 1
  }
  if (pharmacotx_ondrug$cmtrt__atc[i] == "C10AA"){
    drug[which(drug$subjid == id), "Statin"] = 1
    drug[which(drug$subjid == id), "StatinNum"] = drug[which(drug$subjid == id), "StatinNum"] + 1
  }
  if (substr(pharmacotx_ondrug$cmtrt__atc[i], 1,4) == "N06A"){
    drug[which(drug$subjid == id), "AntiDep"] = 1
    drug[which(drug$subjid == id), "AntiDepNum"] = drug[which(drug$subjid == id), "AntiDepNum"] + 1
  }
  if (substr(pharmacotx_ondrug$cmtrt__atc[i], 1,4) == "N05A"){
    drug[which(drug$subjid == id), "AntiPsy"] = 1
    drug[which(drug$subjid == id), "AntiPsyNum"] = drug[which(drug$subjid == id), "AntiPsyNum"] + 1
  }
}

nonpharmacotx <- read.csv("nonpharmacotx.csv", sep = "\t")
nonpharmacotx_sub <- nonpharmacotx[,c("subjid","cmtrt","cmfrq","cmdosfrq","cmenrf")]

nutsuppl <- read.csv("nutsuppl.csv", sep = "\t")
nutsuppl_sub <- nutsuppl[,c("subjid","cmcat","cmtrt__modify","cmtrt__decod","cmtrt__atc","cmtrt__ing","cmenrf")]

comorbid <- read.csv("comorbid.csv", sep = "\t")
comorbid_sub <- comorbid[,c("subjid","mhterm__modify","mhterm__decod","mhbodsys","mhenrf")]

enroll <- read.csv("enroll.csv", sep = "\t")
fields <- colnames(enroll)
grep("alcunits",fields)
enroll_sub <- enroll[,c("subjid","seq",
                        # General
                        "height","weight","bmi",
                        # Group Drug use for non-medical resons? Group Marijuana
                        "mar","marfrq",
                        # UHDRS Motor
                        "motscore","diagconf",
                        # UHDRS TFC
                        "tfcscore",
                        # UHDRS Function
                        "fascore",
                        # Core Cognitive Assessment
                        "sdmt","sdmt1","sdmt2",        
                        "verfct","verfctd","verfct5","verfct6","verfct7",      
                        "scnt","scnt1","scnt2","scnt3",
                        "swrt","swrt1","swrt2","swrt3",
                        # Extended Cognitive Assessment
                        "sit","sit1",
                        # HADS-SIS
                        "anxscore","hads_depscore","irrscore","outscore","inwscore",
                        # PBA-s
                        "depscore","irascore","psyscore","aptscore","exfscore",
                        # SF-12
                        "pcs","mcs",
                        # TUG/STS
                        "tug","tug1","scst","scst1")]

df <- merge(participation_sub, profile_sub)
df <- merge(df, enroll_sub)
df <- merge(df, drug)

# df <- merge(df, nonpharmacotx_sub)
# df <- merge(df, nutsuppl_sub)
# df <- merge(df, comorbid_sub)

write.csv(df, "../enroll_merge2.csv", row.names = F)
# write.csv(df, "../enroll_tug.csv", row.names = F)