#================================================
# Script to attach demographics file to Baseline Pupil data
#================================================

# Working directory is filepath of this script -------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set directories CO -------------------------------------------------------------------
dataDir <- paste0(dirname(getwd()), "/BaselinePupil/ProcessedData")
resultsDir <- paste0(dirname(getwd()), "/BaselinePupil/Results")

# Load packages -------------------------------------------------------------------
library(stringr)
library(tidyverse)


# Read in data -------------------------------------------------------------------
df_DemogClin <- read.csv("DemogClinData.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(df_DemogClin)

df_pupil <- read.csv("df_pupil.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(df_DemogClin)

# Organise pupil data frame -------------------------------------------------------
df_pupil <- subset(df_pupil, select = -c(X))

#Subset df to only include PD
df_PD <- df_pupil %>%
  filter(str_detect(subject, "pd"))

#Con
df_CON <- df_pupil %>%
  filter(str_detect(subject, "cn")) 

#Keep relevant varibles in Demog data frame
#Varibles to keep that are relevant covariates: DxYears,AgeScan,Education,DDE,MMSE,MoCA,ACE_total, UPDRS_Part3_total,Plasma level, LC whole  
  
df_DemogClin_CON <-subset(df_DemogClin, Group_2=="Control")
df_DemogClin_CON<-df_DemogClin_CON[c(1,10,11,15,16,19,20,22,23,24,97,207,211)]
df_DemogClin <-subset(df_DemogClin, Group_2=="PD_Ato")
df_DemogClin<-df_DemogClin[c(1,10,11,15,16,19,20,22,23,24,97,207,211)]
  
#rename IDs and ID column
#needs library(stringr)
df_DemogClin$StudyID <- str_replace(df_DemogClin$StudyID, "_", "")
df_DemogClin$StudyID <- str_replace(df_DemogClin$StudyID, "PD", "pd")
names(df_DemogClin)[names(df_DemogClin) == "StudyID"] <- "subject"
  
df_DemogClin_CON$StudyID <- str_replace(df_DemogClin_CON$StudyID, "_", "")
df_DemogClin_CON$StudyID <- str_replace(df_DemogClin_CON$StudyID, "CON", "cn")
names(df_DemogClin_CON)[names(df_DemogClin_CON) == "StudyID"] <- "subject"
  
#Split dataframes by visit
df_DemogClin_visit1<-df_DemogClin[c(1,2,4:13)]
df_DemogClin_visit2<-df_DemogClin[c(1,3,4:13)]
  
df_PD_visit1<-subset(df_PD, visit=="1")
df_PD_visit2<-subset(df_PD, visit=="2")
  
#Rename condition varible
names(df_DemogClin_visit1)[names(df_DemogClin_visit1) == "SessionOneCondition"] <- "condition"
names(df_DemogClin_visit2)[names(df_DemogClin_visit2) == "SessionTwoCondition"] <- "condition"
  
names(df_DemogClin_CON)[names(df_DemogClin_CON) == "SessionOneCondition"] <- "condition"
#names(df_DemogClin_CON)[names(df_DemogClin_CON) == "SessionTwoCondition"] <- "condition"
df_DemogClin_CON<-subset(df_DemogClin_CON, select = -c(SessionTwoCondition))

#Merge data frames
visit1 <- left_join(df_DemogClin_visit1, df_PD_visit1, by = "subject")
visit2 <- left_join(df_DemogClin_visit2, df_PD_visit2, by = "subject")

ConMerge <-left_join(df_DemogClin_CON, df_CON, by = "subject") 
  
#Combine PD visits
df_temp <- rbind(visit1,visit2)
  
#Combine with controls
df <- rbind(df_temp,ConMerge)

#write.csv(df, "df_bp.csv")
#^df used for analysis

