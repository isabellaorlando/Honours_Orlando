#===========================================================
# Script for prosaccade data organisation - saccade reports
#===========================================================
# Orlando, O'Callaghan 2020

# Working directory is filepath of this script -------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dataDir_ps_amp <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/ProSaccade/PS_saccade")
resultsDir_PS <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/ProSaccade/Results")

# Load packages -------------------------------------------------------------------
require(tidyverse) # data organisation and visualisation
require(gazer)
require(data.table)
require(saccades) # for blink_detect function
require(ggforce)
require(viridis)
require(PupillometryR)
require(zoo)
library(raster)


# Read in data -------------------------------------------------------------------

Data_PS_A <- rbindlist(mapply(
  c,
  (
    list.files(
      path = dataDir_ps_amp,
      pattern = "*.txt",
      full.names = TRUE
    ) %>%
      lapply(
        read.delim,
        stringsAsFactors=FALSE,
        na.strings=c(".")
      )
  ),
  (
    list.files(
      path = dataDir_ps_amp,
      pattern = "*.txt",
      full.names = TRUE
    ) %>%
      basename() %>%
      as.list()
  ),
  SIMPLIFY = FALSE
),
fill = T)


#=============================================================================================
# Tidy and organise the df 
#=============================================================================================

#  Participant ID column = subject
Data_PS_A <- rename(Data_PS_A, subject = RECORDING_SESSION_LABEL)

# Remove ps from name
Data_PS_A$subject <- str_remove(Data_PS_A$subject, "ps")

#rename pd004_2_2
Data_PS_A$subject <- str_replace(Data_PS_A$subject, "pd004v2_2", "pd004v2")

# Split into subject and visit column  by v
Data_PS_A <- Data_PS_A %>% 
  separate(subject, into = c("subject", "visit"), 
           sep="v")


#replace NA in visit column with 1
Data_PS_A <- Data_PS_A %>% dplyr::mutate(visit = replace_na(visit, 1))

# Make subject and visit a factor
Data_PS_A$subject <- as.factor(Data_PS_A$subject)
Data_PS_A$visit <- as.factor(Data_PS_A$visit)

Data_PS_A <- Data_PS_A %>% dplyr::filter(TRIAL_INDEX != "1")

# create ID_trial column
cols3 <- c('subject', 'visit', 'TRIAL_INDEX')

Data_PS_A$ID_trial <- apply(Data_PS_A[ , cols3 ] , 1 , paste , collapse = "_" )

Data_PS_A$ID_trial <- as.factor(Data_PS_A$ID_trial)

Data_PS_A <- Data_PS_A[c(1:3, 221, 5, 4, 6:220)]


#write.csv(Data_PS_A, "Data_PS_A.csv")

#go to PS_analysis


