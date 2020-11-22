#================================================
# Script for antisaccade saccade report import
#================================================
# Orlando, O'Callaghan 2020

# Working directory is filepath of this script -------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dataDir_as_amp <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/AntiSaccade/AS_saccade")
resultsDir_AS <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/AntiSaccade/Results")

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

Data_AS_A <- rbindlist(mapply(
  c,
  (
    list.files(
      path = dataDir_as_amp,
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
      path = dataDir_as_amp,
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
Data_AS_A <- rename(Data_AS_A, subject = RECORDING_SESSION_LABEL)

# Remove ps from name
Data_AS_A$subject <- str_remove(Data_AS_A$subject, "as")

unique(Data_AS_A$subject)

#rename cn15
Data_AS_A$subject <- str_replace(Data_AS_A$subject, "cn15", "cn015")

# Split into subject and visit column  by v
Data_AS_A <- Data_AS_A %>% 
  separate(subject, into = c("subject", "visit"), 
           sep="v")


#replace NA in visit column with 1
Data_AS_A <- Data_AS_A %>% dplyr::mutate(visit = replace_na(visit, 1))

# Make subject and visit a factor
Data_AS_A$subject <- as.factor(Data_AS_A$subject)
Data_AS_A$visit <- as.factor(Data_AS_A$visit)

Data_AS_A <- Data_AS_A %>% dplyr::filter(TRIAL_INDEX != "1")

# create ID_trial column
cols3 <- c('subject', 'visit', 'TRIAL_INDEX')

Data_AS_A$ID_trial <- apply(Data_AS_A[ , cols3 ] , 1 , paste , collapse = "_" )

Data_AS_A$ID_trial <- as.factor(Data_AS_A$ID_trial)

Data_AS_A <- Data_AS_A[c(1:3, 221, 5, 4, 6:220)]


#write.csv(Data_AS_A, "Data_AS_A.csv")
