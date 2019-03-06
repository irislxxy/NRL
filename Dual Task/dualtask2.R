library(tibble)
setwd("/Users/iris/Desktop/NRL")

# iWear
df <- read.csv("iWear/HDIWear_DATA_2018-10-22.csv", stringsAsFactors=FALSE)

# APDM
df_walk <- read.csv("APDM/Walk.csv", header=T, stringsAsFactors=FALSE)
df_alphabet <- read.csv("APDM/Walk Alphabet.csv", header=T, stringsAsFactors=FALSE)
df_eol <- read.csv("APDM/Walk EOL.csv", header=T, stringsAsFactors=FALSE)

# DTE
dte <- as.data.frame(df_walk$X, stringsAsFactors=FALSE)
colnames(dte) <- "as_correct"
dte$as_correct[1:20] <- paste0("IW",substr(dte$as_correct[1:20],3,4),"GHI")
dte$hd_or_healthy <- df$hd_or_healthy[match(dte$as_correct,df$as_correct)]

# Lateral Step Variability
dte$Lateral.Step.Variability.Walk <- (df_walk$Gait...Lower.Limb...Lateral.Step.Variability.L..cm. + df_walk$Gait...Lower.Limb...Lateral.Step.Variability.R..cm.)/2
dte$Lateral.Step.Variability.Alphabet <- (df_alphabet$Gait...Lower.Limb...Lateral.Step.Variability.L..cm. + df_alphabet$Gait...Lower.Limb...Lateral.Step.Variability.R..cm.)/2
dte$Lateral.Step.Variability.EOL <- (df_eol$Gait...Lower.Limb...Lateral.Step.Variability.L..cm. + df_eol$Gait...Lower.Limb...Lateral.Step.Variability.R..cm.)/2

dte$Lateral.Step.Variability.Alphabet.DTE <- 100 * (dte$Lateral.Step.Variability.Alphabet - dte$Lateral.Step.Variability.Walk)/dte$Lateral.Step.Variability.Walk
dte$Lateral.Step.Variability.EOL.DTE <- 100 * (dte$Lateral.Step.Variability.EOL - dte$Lateral.Step.Variability.Walk)/dte$Lateral.Step.Variability.Walk

# Double Support
dte$Double.Support.Walk <- (df_walk$Gait...Lower.Limb...Double.Support.L...GCT. + df_walk$Gait...Lower.Limb...Double.Support.R...GCT.)/2
dte$Double.Support.Alphabet <- (df_alphabet$Gait...Lower.Limb...Double.Support.L...GCT. + df_alphabet$Gait...Lower.Limb...Double.Support.R...GCT.)/2
dte$Double.Support.EOL <- (df_eol$Gait...Lower.Limb...Double.Support.L...GCT. + df_eol$Gait...Lower.Limb...Double.Support.R...GCT.)/2

dte$Double.Support.Alphabet.DTE <- 100 * (dte$Double.Support.Alphabet - dte$Double.Support.Walk)/dte$Double.Support.Walk 
dte$Double.Support.EOL.DTE <- 100 * (dte$Double.Support.EOL - dte$Double.Support.Walk)/dte$Double.Support.Walk 

# Arm Range of Motion
dte$Arm.Range.of.Motion.Walk <- (df_walk$Gait...Upper.Limb...Arm.Range.of.Motion.L..degrees. + df_walk$Gait...Upper.Limb...Arm.Range.of.Motion.R..degrees.)/2
dte$Arm.Range.of.Motion.Alphabet <- (df_alphabet$Gait...Upper.Limb...Arm.Range.of.Motion.L..degrees. + df_alphabet$Gait...Upper.Limb...Arm.Range.of.Motion.R..degrees.)/2
dte$Arm.Range.of.Motion.EOL <- (df_eol$Gait...Upper.Limb...Arm.Range.of.Motion.L..degrees. + df_eol$Gait...Upper.Limb...Arm.Range.of.Motion.R..degrees.)/2

dte$Arm.Range.of.Motion.Alphabet.DTE <- 100 * (dte$Arm.Range.of.Motion.Alphabet - dte$Arm.Range.of.Motion.Walk)/dte$Arm.Range.of.Motion.Walk
dte$Arm.Range.of.Motion.EOL.DTE <- 100 * (dte$Arm.Range.of.Motion.EOL - dte$Arm.Range.of.Motion.Walk)/dte$Arm.Range.of.Motion.Walk

# Lumbar
## Coronal
dte$Lumbar.Coronal.Walk <- df_walk$Gait...Lumbar...Coronal.Range.of.Motion..degrees.
dte$Lumbar.Coronal.Alphabet <- df_alphabet$Gait...Lumbar...Coronal.Range.of.Motion..degrees.
dte$Lumbar.Coronal.EOL <- df_eol$Gait...Lumbar...Coronal.Range.of.Motion..degrees.

dte$Lumbar.Coronal.Alphabet.DTE <- 100 * (dte$Lumbar.Coronal.Alphabet - dte$Lumbar.Coronal.Walk)/dte$Lumbar.Coronal.Walk
dte$Lumbar.Coronal.EOL.DTE <- 100 * (dte$Lumbar.Coronal.EOL - dte$Lumbar.Coronal.Walk)/dte$Lumbar.Coronal.Walk

## Sagittal
dte$Lumbar.Sagittal.Walk <- df_walk$Gait...Lumbar...Sagittal.Range.of.Motion..degrees.
dte$Lumbar.Sagittal.Alphabet <- df_alphabet$Gait...Lumbar...Sagittal.Range.of.Motion..degrees.
dte$Lumbar.Sagittal.EOL <- df_eol$Gait...Lumbar...Sagittal.Range.of.Motion..degrees.

dte$Lumbar.Coronal.Alphabet.DTE <- 100 * (dte$Lumbar.Sagittal.Alphabet - dte$Lumbar.Sagittal.Walk)/dte$Lumbar.Sagittal.Walk
dte$Lumbar.Coronal.EOL.DTE <- 100 * (dte$Lumbar.Sagittal.EOL - dte$Lumbar.Sagittal.Walk)/dte$Lumbar.Sagittal.Walk

## Transverse
dte$Lumbar.Transverse.Walk <- df_walk$Gait...Lumbar...Transverse.Range.of.Motion..degrees.
dte$Lumbar.Transverse.Alphabet <- df_alphabet$Gait...Lumbar...Transverse.Range.of.Motion..degrees.
dte$Lumbar.Transverse.EOL <- df_eol$Gait...Lumbar...Transverse.Range.of.Motion..degrees.

dte$Lumbar.Transverse.Alphabet.DTE <- 100 * (dte$Lumbar.Transverse.Alphabet - dte$Lumbar.Transverse.Walk)/dte$Lumbar.Transverse.Walk
dte$Lumbar.Transverse.EOL.DTE <- 100 * (dte$Lumbar.Transverse.EOL - dte$Lumbar.Transverse.Walk)/dte$Lumbar.Transverse.Walk

# Trunk
## Coronal
dte$Trunk.Coronal.Walk <- df_walk$Gait...Trunk...Coronal.Range.of.Motion..degrees.
dte$Trunk.Coronal.Alphabet <- df_alphabet$Gait...Trunk...Coronal.Range.of.Motion..degrees.
dte$Trunk.Coronal.EOL <- df_eol$Gait...Trunk...Coronal.Range.of.Motion..degrees.

dte$Trunk.Coronal.Alphabet.DTE <- 100 * (dte$Trunk.Coronal.Alphabet - dte$Trunk.Coronal.Walk)/dte$Trunk.Coronal.Walk
dte$Trunk.Coronal.EOL.DTE <- 100 * (dte$Trunk.Coronal.EOL - dte$Trunk.Coronal.Walk)/dte$Trunk.Coronal.Walk

## Sagittal
dte$Trunk.Sagittal.Walk <- df_walk$Gait...Trunk...Sagittal.Range.of.Motion..degrees.
dte$Trunk.Sagittal.Alphabet <- df_alphabet$Gait...Trunk...Sagittal.Range.of.Motion..degrees.
dte$Trunk.Sagittal.EOL <- df_eol$Gait...Trunk...Sagittal.Range.of.Motion..degrees.

dte$Trunk.Coronal.Alphabet.DTE <- 100 * (dte$Trunk.Sagittal.Alphabet - dte$Trunk.Sagittal.Walk)/dte$Trunk.Sagittal.Walk
dte$Trunk.Coronal.EOL.DTE <- 100 * (dte$Trunk.Sagittal.EOL - dte$Trunk.Sagittal.Walk)/dte$Trunk.Sagittal.Walk

## Transverse
dte$Trunk.Transverse.Walk <- df_walk$Gait...Trunk...Transverse.Range.of.Motion..degrees.
dte$Trunk.Transverse.Alphabet <- df_alphabet$Gait...Trunk...Transverse.Range.of.Motion..degrees.
dte$Trunk.Transverse.EOL <- df_eol$Gait...Trunk...Transverse.Range.of.Motion..degrees.

dte$Trunk.Transverse.Alphabet.DTE <- 100 * (dte$Trunk.Transverse.Alphabet - dte$Trunk.Transverse.Walk)/dte$Trunk.Transverse.Walk
dte$Trunk.Transverse.EOL.DTE <- 100 * (dte$Trunk.Transverse.EOL - dte$Trunk.Transverse.Walk)/dte$Trunk.Transverse.Walk

write.csv(dte, "Dual Task/DTE2.csv", row.names = F)

#function: return the record and field of each missing value
f_na <- function(df){
  fields <- colnames(df)
  df_idx <- as.data.frame(which(is.na(df), arr.ind=T))
  df_idx <- df_idx[order(df_idx$row),]
  df_na <- data.frame(record=character(),
                      field=character(),
                      stringsAsFactors=F)
  for (i in 1:nrow(df_idx)){
    idx <- df_idx[i,]
    df_na[i,"record"] <- as.character(df[idx$row,"as_correct"])
    df_na[i,"field"] <- fields[idx$col]
  }
  return(df_na)
}

df_na <- f_na(dte)
df_na <- df_na[-grep("DTE",df_na$field),]

write.csv(df_na, "Dual Task/DTE2_NA.csv", row.names = F)