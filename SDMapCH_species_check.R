######################## PROJECT: BuzzLines connectivity
# Author: Julian Wittische (Musée National d'Histoire Naturelle Luxembourg)
# Request: Axel Hochkirch/Alexander Weigand/Balint Andrasi
# Start: Fall 2025
# Data: MNHNL
# Script objective : Deep check of which species from our expert-based, non-exhaustive is covered by SDMapCH models

############ Local configuration ----
source("config.R")

############ Libraries ----
library(readxl) # read xl files
library(taxize) # taxonomy management

############ Loading lists ---- 
buzz_list <- read_xlsx(paste0(ENVIPATH, "SDMapCH/BuzzLineSpeciesList_mod.xlsx"))
SDM_list <- read.csv2(paste0(ENVIPATH, "SDMapCH/metadata/1_SDMapCHv1_3_spinfo.csv"))

###### Separate habitat-based groups
dry_l <- buzz_list$Dry
wet_l <- buzz_list$Wet

############ Find the preferred name of a species in Catalog of Life ----

###### Global names verifier

### FUNCTION: split into chunks to respect API limit
chunks <- function(x){
  split(x, ceiling(seq_along(x) / 13))
}

### FUNCTION: apply gna_verifier() to each chunk
gna_big <- function(chunks_out){lapply(chunks_out, gna_verifier)}

### FUNCTION: FIX no current when EOL match
curr_eol_fix <- function(df) {
  i <- df$currentCanonicalSimple == "" | is.na(df$currentCanonicalSimple)
  df$currentCanonicalSimple[i] <- df$matchedCanonicalSimple[i]
  df
}

### BuzzLines
# Dryland
buzz_d_chunks <- chunks(dry_l)
buzz_d_gna_big <- gna_big(buzz_d_chunks)
buzz_d_gna_df <- do.call(rbind, buzz_d_gna_big)
buzz_d_gna_df <- curr_eol_fix(buzz_d_gna_df)

# Wetland
buzz_w_chunks <- chunks(wet_l)
buzz_w_gna_big <- gna_big(buzz_w_chunks)
buzz_w_gna_df <- do.call(rbind, buzz_w_gna_big)
buzz_w_gna_df <- curr_eol_fix(buzz_w_gna_df)
  
### SDMapCH
SDM_chunks <- chunks(SDM_list$species)
SDM_gna_big <- gna_big(SDM_chunks)
SDM_gna_df <- do.call(rbind, SDM_gna_big)
SDM_gna_df <- curr_eol_fix(SDM_gna_df)

# Checkpoint
table(SDM_gna_df$curation)
range(SDM_gna_df$sortScore)
table(SDM_gna_df$taxonomicStatus)
table(SDM_gna_df$parsingQualityScore)
table(SDM_gna_df$curatedDataScore)
table(SDM_gna_df$matchType)
View(SDM_gna_df[SDM_gna_df$matchType=="Fuzzy",]) #No problems
View(SDM_gna_df[SDM_gna_df$matchType=="PartialExact",]) # A few issues

###### Intersection
inter_d <- intersect(buzz_d_gna_df$currentCanonicalSimple, SDM_gna_df$currentCanonicalSimple)
inter_w <- intersect(buzz_w_gna_df$currentCanonicalSimple, SDM_gna_df$currentCanonicalSimple)

