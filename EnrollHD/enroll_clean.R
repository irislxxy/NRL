setwd("/Users/iris/Desktop/NRL/EnrollHD")
df <- read.csv("enroll_merge.csv", stringsAsFactors=FALSE)

# Inclusion criteria
## Diagnosis of HD, confirmed by genetic testing 
## Can I assume that manifest/motor-manifest partcipants HD are confirmed by genetic testing?

## Above the age of 18
## Age at enrollment: age_0 >= 18
table(df$age_0)
df <- df[which(df$age_0 >= 18),]

## A current participant on the Enroll-HD study
## Participant status: subjstat = enrolled 
df <- df[which(df$subjsta == "enrolled"),]

## Up to and including stage 2 disease status (TFC 7-13) - not premanifest and are not a control
## Participant category at enrollment: hdcat_0 = 3
df <- df[which(df$hdcat_0 == 3),] # pre‐manifest/pre‐motor‐manifest HD - 2 
                                  # manifest/motor‐ manifest HD - 3
                                  # genotype negative - 4
                                  # family control - 5

# Exclusion criteria
## Diagnosis of juvenile onset HD
## Can I assume that age 18+ partcipants are not juvenile onset HD?

## History of comorbid neurological conditions such as multiple sclerosis or stroke & Acute (within 1 month) orthopaedic conditions e.g. ankle sprain or fracture
## There are various comorbid conditions, and I can't determine whether it is comorbid neurological conditions or acute orthopaedic conditions.

## Inability or unwillingness of participant or legal guardian to give written informed consent
## Day of informed consent: rficdy 
## (Number of days of informed consent before/after enrollment/baseline for Enroll‐HD. Not available for Adhoc study since substudy of Enroll.)
## Does it mean that we need to exclude paritipants which have missing value in rficdy?
is.na(df$rficdy) # no NA

write.csv(df, "enroll_clean.csv", row.names = F)