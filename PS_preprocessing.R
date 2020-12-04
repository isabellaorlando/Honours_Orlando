#===========================================================
# Script for prosaccade data organisation - sample reports
#===========================================================
# Orlando, O'Callaghan 2020

# Working directory is filepath of this script -------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set directories -------------------------------------------------------------------
dataDir_PS <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/ProSaccade/PS_final")
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
library(stringr)
library(dplyr)
library(ggplot2)
library(ggpubr)
# Read in data -------------------------------------------------------------------

Data_PS <- rbindlist(mapply(
  c,
  (
    list.files(
      path = dataDir_PS,
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
      path = dataDir_PS,
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
Data_PS <- rename(Data_PS, subject = RECORDING_SESSION_LABEL)

# Remove ps from name
Data_PS$subject <- str_remove(Data_PS$subject, "ps")

#rename pd004_2_2
Data_PS$subject <- str_replace(Data_PS$subject, "pd004v2_2", "pd004v2")


# Split into subject and visit column  by v
Data_PS <- Data_PS %>% 
  separate(subject, into = c("subject", "visit"), 
           sep="v")


#replace NA in visit column with 1
Data_PS <- Data_PS %>% dplyr::mutate(visit = replace_na(visit, 1))


# Make subject and visit a factor
Data_PS$subject <- as.factor(Data_PS$subject)
Data_PS$visit <- as.factor(Data_PS$visit)


Data_PS2 <- Data_PS

# Make a single "gaze acceleration" column for x (i.e., combining left and right pupils)
Data_PS2 <- Data_PS2 %>% mutate(acceleration_x = ifelse(EYE_TRACKED == "Right", RIGHT_ACCELERATION_X, LEFT_ACCELERATION_X))

# Make a single "gaze acceleration" column for u (i.e., combining left and right pupils)
Data_PS2 <- Data_PS2 %>% mutate(acceleration_y = ifelse(EYE_TRACKED == "Right", RIGHT_ACCELERATION_Y, LEFT_ACCELERATION_Y))

#combine fix_index for left and right
Data_PS2 <- Data_PS2 %>% mutate(fix_index = ifelse(EYE_TRACKED == "Right", RIGHT_FIX_INDEX, LEFT_FIX_INDEX))

# Make a single "x_gaze" "y_gaze" columns
Data_PS2 <- Data_PS2 %>% mutate(x_gaze = ifelse(EYE_TRACKED == "Right", RIGHT_GAZE_X, LEFT_GAZE_X))

Data_PS2 <- Data_PS2 %>% mutate(y_gaze = ifelse(EYE_TRACKED == "Right", RIGHT_GAZE_Y, LEFT_GAZE_Y))

#single interest area
Data_PS2 <- Data_PS2 %>% mutate(interest_areas = ifelse(EYE_TRACKED == "Right", RIGHT_INTEREST_AREAS, LEFT_INTEREST_AREAS))

#single interest area data
Data_PS2 <- Data_PS2 %>% mutate(interest_area_data = ifelse(EYE_TRACKED == "Right", RIGHT_INTEREST_AREA_DATA, LEFT_INTEREST_AREA_DATA))

#single interest area distance
Data_PS2 <- Data_PS2 %>% mutate(interest_area_distance = ifelse(EYE_TRACKED == "Right", RIGHT_INTEREST_AREA_DISTANCE, LEFT_INTEREST_AREA_DISTANCE))

#single interest area ID
Data_PS2 <- Data_PS2 %>% mutate(interest_area_ID = ifelse(EYE_TRACKED == "Right", RIGHT_INTEREST_AREA_ID, LEFT_INTEREST_AREA_ID))

#single interest area label
Data_PS2 <- Data_PS2 %>% mutate(interest_area_label = ifelse(EYE_TRACKED == "Right", RIGHT_INTEREST_AREA_LABEL, LEFT_INTEREST_AREA_LABEL))

#single interest area pixel area
Data_PS2 <- Data_PS2 %>% mutate(IA_pixel_area = ifelse(EYE_TRACKED == "Right", RIGHT_INTEREST_AREA_PIXEL_AREA, LEFT_INTEREST_AREA_PIXEL_AREA))

#single blink column
Data_PS2 <- Data_PS2 %>% mutate(blink = ifelse(EYE_TRACKED == "Right", RIGHT_IN_BLINK, LEFT_IN_BLINK))

#single saccade column
Data_PS2 <- Data_PS2 %>% mutate(saccade = ifelse(EYE_TRACKED == "Right", RIGHT_IN_SACCADE, LEFT_IN_SACCADE))

#single saccade index column
Data_PS2 <- Data_PS2 %>% mutate(saccade_index = ifelse(EYE_TRACKED == "Right", RIGHT_SACCADE_INDEX, LEFT_SACCADE_INDEX))

#single pupil size column
Data_PS2 <- Data_PS2 %>% mutate(pupil_size = ifelse(EYE_TRACKED == "Right", RIGHT_PUPIL_SIZE, LEFT_PUPIL_SIZE))

#single x and y columns for velocity 
Data_PS2 <- Data_PS2 %>% mutate(x_velocity = ifelse(EYE_TRACKED == "Right", RIGHT_VELOCITY_X, LEFT_VELOCITY_X))

Data_PS2 <- Data_PS2 %>% mutate(y_velocity = ifelse(EYE_TRACKED == "Right", RIGHT_VELOCITY_Y, LEFT_VELOCITY_Y))


# Keep only relevant columns
Data_PS <- Data_PS2[c(1:5, 24:25, 44:51, 69:114)]
  
Data_PS <- Data_PS[c(1:28, 45:61)]


Data_PS <- Data_PS[c(1:15, 20:45)]

#remove IP_index, IP_label, IP_end_event_matched, IP_start_event_matched

Data_PS <- Data_PS[c(1:16, 18, 22:41)]

#replace "UNDEFINED" with NA in random_delay, direction, distance, t_x, t_y
Data_PS$Random_Delay <- na_if(Data_PS$Random_Delay, "UNDEFINED")
Data_PS$direction <- na_if(Data_PS$direction, "UNDEFINED")
Data_PS$distance <- na_if(Data_PS$distance, "UNDEFINED")
Data_PS$t_x <- na_if(Data_PS$t_x, "UNDEFINED")
Data_PS$t_y <- na_if(Data_PS$t_y, "UNDEFINED")


# create subject_visit column
cols <- c('subject', 'visit')

Data_PS$subject_visit <- apply(Data_PS[ , cols ] , 1 , paste , collapse = "_" )

Data_PS$subject_visit <- as.factor(Data_PS$subject_visit)

#rearrange column order
Data_PS <- Data_PS[c(1:2, 38, 3, 9, 4:8, 10:37)]

#delete 2nd trial column as it is repeated
Data_PS <- Data_PS[c(1:4, 6:38)]


#Create a "time" column for to put everyone in msec
#Sampling rate is 500 Hz (i.e., 1 sample every 2 msec)
Data_PS <- Data_PS %>% group_by(subject_visit) %>% mutate(time = TIMESTAMP - TRIAL_START_TIME) 
Data_PS <- Data_PS %>% group_by(subject_visit) %>% mutate(time_sec = (TIMESTAMP - TRIAL_START_TIME)/1000) 

#rearrange

Data_PS <- Data_PS %>% group_by(subject_visit) %>% arrange(subject_visit)

#number of samples for each subject_visit
df_ps_test <- Data_PS %>% group_by(subject_visit) %>% summarise(length(time))


Data_PS <- as.data.frame(Data_PS)

#=============================================================================================


df_ps <- Data_PS %>% mutate(saccade_new = ifelse(blink == "1", "0", saccade))
df_ps <- df_ps[c(1:5, 38, 6:32, 40, 34:37, 39)]

df_ps <- rename(df_ps, saccade = saccade_new)

#write.csv(df_ps, "df_ps_2.csv")


#=============================================================================================
#FILTER FOR DATA FROM TARGET DISPLAY
#=============================================================================================

#use df from above or from file saved to desktop
df_ps_2 <- read.csv("df_ps_2.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(df_ps_2)

df_ps_2 <- subset(df_ps_2, select = -c(X))

# create ID_trial column
cols3 <- c('subject', 'visit', 'TRIAL_INDEX')

df_ps_2$ID_trial <- apply(df_ps_2[ , cols3 ] , 1 , paste , collapse = "_" )

df_ps_2$ID_trial <- as.factor(df_ps_2$ID_trial)

#rearrange
df_ps_2 <- df_ps_2[c(1:4, 40, 5:39)]

#delete "trial column"
df_ps_2 <- df_ps_2[c(1:5, 7:39)]

df_ps_2_test1 <- df_ps_2 %>% group_by(ID_trial) %>% summarise(length(time))

#delete 1st trial for every p
df_ps_2 <- df_ps_2 %>% dplyr::filter(TRIAL_INDEX != "1")

df_ps_2_test2 <- df_ps_2 %>% group_by(ID_trial) %>% summarise(length(time))


#write.csv(df_ps_2, 'saccade_df_ps.csv')


#duplicate
df_ps_3 <- df_ps_2 %>% 
  mutate(SAMPLE_MESSAGE2 = SAMPLE_MESSAGE)

df_ps_3 <- df_ps_3 %>% 
  group_by(grp = cumsum(!is.na(SAMPLE_MESSAGE2))) %>% 
  mutate(SAMPLE_MESSAGE2 = replace(SAMPLE_MESSAGE2, first(SAMPLE_MESSAGE2) == 'Target_display', 'Saccade_target')) %>% 
  mutate(SAMPLE_MESSAGE2 = replace(SAMPLE_MESSAGE2, first(SAMPLE_MESSAGE2) == 'Saccade_target', 'End_trial_display')) %>% 
  ungroup() %>% 
  dplyr::select(-grp)


df_ps_3 <- df_ps_3 %>% filter(SAMPLE_MESSAGE2 == "End_trial_display")


#group_by (ID_trial) delete 40 rows after 1st row


#group_by (ID_trial) calculate distance to 1st saccade 

df_ps_3 <- df_ps_3 %>% group_by(ID_trial) %>% mutate(sample = row_number())


df_ps_3_test <- df_ps_3 %>% group_by(ID_trial) %>% summarise(length(time))

#reorder saccade_index to account for before 1st saccade
df_ps_4 <- df_ps_3 %>%
  dplyr::group_by(subject, TRIAL_INDEX) %>%
  mutate(saccades_inrange = with(rle(saccade), rep(seq_along(lengths), lengths)))



#create a saccade_index and blink column = sac_blink
cols11 <- c('saccade_index', 'blink')

df_ps_4$sac_blink <- apply(df_ps_4[ , cols11 ] , 1 , paste , collapse = "_" )

df_ps_4$sac_blink <- as.factor(df_ps_4$sac_blink)


# create saccade_index and ID_trial column

cols12 <- c('ID_trial', 'saccade_index')

df_ps_4$ID_sac <- apply(df_ps_4[ , cols12 ] , 1 , paste , collapse = "_" )

df_ps_4$ID_sac <- as.factor(df_ps_4$ID_sac)

#list ID_sac when sac_index_blink = 1_1, 2_1, 3_1, 4_1, 5_1

#unique(test5$sac_index_blink)
unique(df_ps_4$sac_blink)


#list all the saccades that include a blink


#list ID_sac when sac_blink = 1_1, 2_1, 3_1, 4_1, 5_1

df_ps_5 <- df_ps_4 %>% dplyr::select(ID_trial, ID_sac, saccade, blink, sac_blink, saccade_index, saccades_inrange)

df_ps_5_wrong <- df_ps_5 %>%
  filter(str_detect(sac_blink, "_1"))

wrong <- unique(df_ps_5_wrong$ID_sac)

#list of the saccades that include blinks - need to be deleted
#use cn002_1_6 as example
wrong2 <- as.data.frame(wrong)

wrong2 <- rename(wrong2, ID_sac = wrong)

wrong3 <- df_ps_5[!df_ps_5$ID_sac %in% wrong2$ID_sac,]


#rearrange saccades_inrange

rearrange_wrong3 <- wrong3 %>%
  dplyr::group_by(ID_trial) %>%
  mutate(saccade_number = with(rle(saccade), rep(seq_along(lengths), lengths)))


#repeat with full df - removed the saccades that contained blinks

final_test <- df_ps_4[!df_ps_4$ID_sac %in% wrong2$ID_sac,]

#rearrange saccades_number

final_test <- final_test %>%
  dplyr::group_by(ID_trial) %>%
  mutate(saccade_number = with(rle(saccade), rep(seq_along(lengths), lengths)))

#write.csv(final_test, 'final_ps_test.csv')


#go to PS_preprocessing_saccade reports
