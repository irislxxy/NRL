setwd("~/Desktop/NRL/EnrollHD/Raw Data")

participation<- read.csv("participation.csv", sep = "\t")
participation_sub <- participation[,c("subjid","subjstat","hdcat_0","hdcat_l","age_0")]

profile <- read.csv("profile.csv", sep = "\t")
profile_sub <- profile[,c("subjid","sex","caghigh")]

pharmacotx <- read.csv("pharmacotx.csv", sep = "\t")
pharmacotx_sub <- pharmacotx[,c("subjid","cmtrt__atc")]
  
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

# df <- merge(df, pharmacotx_sub)
# df <- merge(df, nonpharmacotx_sub)
# df <- merge(df, nutsuppl_sub)
# df <- merge(df, comorbid_sub)

# write.csv(df, "../enroll_merge2.csv", row.names = F)
write.csv(df, "../enroll_tug.csv", row.names = F)