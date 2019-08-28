setwd("/Users/iris/Desktop/NRL/EnrollHD")
df <- read.csv("enroll_merge2.csv", stringsAsFactors=FALSE)

# Inclusion criteria
# Diagnosis of HD, confirmed by genetic testing 
## Q: Can I assume that manifest/motor-manifest partcipants HD are confirmed by genetic testing?
## A: YES MANIFEST AND MOTOR MANIFEST ARE INCLUDED 

# Above the age of 18
## Age at enrollment: age_0 >= 18
table(df$age_0)
df <- df[which(df$age_0 >= 18),]

# A current participant on the Enroll-HD study
## Participant status: subjstat = enrolled 
df <- df[which(df$subjsta == "enrolled"),]

# Up to and including stage 2 disease status (TFC 7-13) - not premanifest and are not a control
## Participant category at enrollment: hdcat_0 = 3
df <- df[which(df$hdcat_0 == 3),] # pre‐manifest/pre‐motor‐manifest HD - 2 
                                  # manifest/motor‐ manifest HD - 3
                                  # genotype negative - 4
                                  # family control - 5

# Exclusion criteria
# Diagnosis of juvenile onset HD
## Q: Can I assume that age 18+ partcipants are not juvenile onset HD? 
## A: ONSET BEFORE AGE 20 IS CONSIDERED JHD

# History of comorbid neurological conditions such as multiple sclerosis or stroke 
# Acute (within 1 month) orthopaedic conditions e.g. ankle sprain or fracture
## Q: There are various comorbid conditions, and I can't determine whether it is comorbid neurological conditions or acute orthopaedic conditions.
## A: LIST OF COMORBID CONDITIONS 
#Parkinson disease
#Schizophrenia
#Multiple sclerosis
#Essential tremor ----- Note* Must be together tremor is okay and there is essential hypertension also okay
#Epilepsy
#Tetraplegia
#Stroke
#amnesia
#Alzheimer
#Hydrocephalus
#Motor neuron disease
#Cerebral palsy
#Ataxia
#Hemiplegia
#Dystonia
#retardation
#Bell palsy
#Fracture -- Any 
#Sprain -- Any 
#Strain -- Any 

comorbid <- read.csv("Raw Data/comorbid.csv", sep = "\t")

#
table <- as.data.frame(table(comorbid$subjid))
table$Var1[which(table$Freq==66)]

condition <- comorbid$mhterm__modify
exclude_idx <- c()
exclude_idx <- c(exclude_idx, grep("Parkinson disease", condition))
exclude_idx <- c(exclude_idx, grep("[Ss]chizophrenia", condition))
exclude_idx <- c(exclude_idx, grep("Multiple sclerosis", condition))
exclude_idx <- c(exclude_idx, grep("Essential tremor", condition))
exclude_idx <- c(exclude_idx, grep("[Ee]pilepsy", condition))
exclude_idx <- c(exclude_idx, grep("[Tt]etraplegia", condition))
exclude_idx <- c(exclude_idx, grep("[Ss]troke", condition))
exclude_idx <- c(exclude_idx, grep("amnesia", condition))
exclude_idx <- c(exclude_idx, grep("Alzheimer", condition))
exclude_idx <- c(exclude_idx, grep("[Hh]ydrocephalus", condition))
exclude_idx <- c(exclude_idx, grep("Motor neuron disease", condition))
exclude_idx <- c(exclude_idx, grep("[Cc]erebral palsy", condition))
exclude_idx <- c(exclude_idx, grep("[Aa]taxia", condition))
exclude_idx <- c(exclude_idx, grep("[Hh]emiplegia", condition))
exclude_idx <- c(exclude_idx, grep("[Dd]ystonia", condition))
exclude_idx <- c(exclude_idx, grep("retardation", condition))
exclude_idx <- c(exclude_idx, grep("Bell palsy", condition))
exclude_idx <- c(exclude_idx, grep("[Ff]racture", condition))
exclude_idx <- c(exclude_idx, grep("[Ss]prain", condition))
exclude_idx <- c(exclude_idx, grep("strain", condition))
exclude_idx_sorted <- sort(exclude_idx)
exclude_idx_unique <- unique(exclude_idx_sorted) 

comorbid_check <- comorbid[-exclude_idx_unique,]  
write.csv(comorbid_check, "comorbid_check.csv", row.names = F)

exclude_subjid <- unique(comorbid$subjid[exclude_idx_unique])
df <- df[which(!(df$subjid %in% exclude_subjid)),]

# Inability or unwillingness of participant or legal guardian to give written informed consent
## Day of informed consent: rficdy (Number of days of informed consent before/after enrollment/baseline for Enroll‐HD. Not available for Adhoc study since substudy of Enroll.)
## Q: Does it mean that we need to exclude paritipants which have missing value in rficdy? 
## A: NO I THINK WE CAN ASSUME ALL HAVE CONSENTED IF WE HAVE THEIR DATA 

write.csv(df, "enroll_clean2.csv", row.names = F)