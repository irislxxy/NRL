setwd("~/Desktop/NRL/EnrollHD/Raw Data")

participation<- read.csv("participation.csv", sep = "\t")

profile <- read.csv("profile.csv", sep = "\t")
profile_sub <- profile[,c("subjid","sex","caghigh","ccmtr","ccmtrage",
                          "sxest","sxrater","sxestcfd","sxraterm","hddiagn")]

enroll <- read.csv("enroll.csv", sep = "\t")
fields <- colnames(enroll)
grep("alcunits",fields)
enroll_sub <- enroll[,c("subjid","seq",
                        # General 1
                        "height","weight","bmi",
                        "alcab","alcunits","tobab","packy","cafab","cafpd","drugab",
                        # Group Drug use for non-medical resons?
                        "mar","marfrq",
                        "her","herfrq",
                        "coc","cocfrq",
                        "clb","clbfrq",
                        "amp","ampfrq",
                        "rit","ritfrq",
                        "hal","halfrq",
                        "inh","inhfrq",
                        "opi","opifrq",
                        "pak","pakfrq",
                        "bar","barfrq",
                        "trq","trqfrq",
                        # General 2
                        "maristat","res","isced","jobclas",
                        # UHDRS Motor
                        "motscore","miscore",
                        "ocularh","ocularv",
                        "sacinith","sacinitv",      
                        "sacvelh","sacvelv","dysarth","tongue",
                        "fingtapr","fingtapl",
                        "prosupr","prosupl","luria",
                        "rigarmr","rigarml","brady",         
                        "dysttrnk","dystrue","dystlue","dystrle","dystlle",
                        "chorface","chorbol","chortrnk","chorrue","chorlue","chorrle","chorlle",       
                        "gait","tandem","retropls","diagconf",
                        # UHDRS TFC
                        "tfcscore","occupatn","finances","chores","adl","carelevl",
                        # UHDRS Function
                        "fascore","fiscore","emplusl","emplany","volunt",
                        "fafinan","grocery","cash","supchild","drive",
                        "housewrk","laundry","prepmeal","telephon","ownmeds",
                        "feedself","dress","bathe","pubtrans","walknbr",
                        "walkfall","walkhelp","comb","trnchair","bed",
                        "toilet","carehome","indepscl",
                        # Cognitive
                        "sdmt","sdmt1","sdmt2",        
                        "verfct","verfctd","verfct5","verfct6","verfct7",      
                        "scnt","scnt1","scnt2","scnt3",
                        "swrt","swrt1","swrt2","swrt3",
                        "sit","sit1","sit2","sit3",          
                        "trl","trla1","trla2","trla3","trlb1","trlb2","trlb3",
                        "verflt","verflt05","verflt06","verflt07",
                        # HADS-SIS
                        "anxscore","hads_depscore","irrscore","outscore","inwscore",
                        # Physiotherapy
                        "tug","tug1","scst","scst1",
                        # PBA-s
                        "depscore","irascore","psyscore","aptscore","exfscore",
                        # SF-12
                        "pf","rp","bp","gh", "vt","sf","re","mh","pcs","mcs")]

df <- merge(enroll_sub,profile_sub)
df2 <- df[,c(1,2,164:172,3:163)]

write.csv(df2, "../enroll.csv", row.names = F)