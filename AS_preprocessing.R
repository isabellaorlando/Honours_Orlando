#================================================
# Script for antisaccade data organisation
#================================================
# Orlando, O'Callaghan 2020

# Working directory is filepath of this script -------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set directories -------------------------------------------------------------------
dataDir_AS <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/AntiSaccade/AS_final")
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

Data_AS <- rbindlist(mapply(
  c,
  (
    list.files(
      path = dataDir_AS,
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
      path = dataDir_AS,
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
Data_AS <- rename(Data_AS, subject = RECORDING_SESSION_LABEL)

# Remove ps from name
Data_AS$subject <- str_remove(Data_AS$subject, "as")

unique(Data_AS$subject)

#rename pd004_2_2
Data_AS$subject <- str_replace(Data_AS$subject, "cn15", "cn015")

#rename s with v
#Data_PS$subject <- str_replace(Data_PS$subject, "s", "v")

# Split into subject and visit column  by v
Data_AS <- Data_AS %>% 
  separate(subject, into = c("subject", "visit"), 
           sep="v")


#replace NA in visit column with 1
Data_AS <- Data_AS %>% dplyr::mutate(visit = replace_na(visit, 1))

# Make subject and visit a factor
Data_AS$subject <- as.factor(Data_AS$subject)
Data_AS$visit <- as.factor(Data_AS$visit)


Data_AS2 <- Data_AS

# Make a single "gaze acceleration" column for x (i.e., combining left and right pupils)
Data_AS2 <- Data_AS2 %>% mutate(acceleration_x = ifelse(EYE_TRACKED == "Right", RIGHT_ACCELERATION_X, LEFT_ACCELERATION_X))

# Make a single "gaze acceleration" column for u (i.e., combining left and right pupils)
Data_AS2 <- Data_AS2 %>% mutate(acceleration_y = ifelse(EYE_TRACKED == "Right", RIGHT_ACCELERATION_Y, LEFT_ACCELERATION_Y))

#combine fix_index for left and right
Data_AS2 <- Data_AS2 %>% mutate(fix_index = ifelse(EYE_TRACKED == "Right", RIGHT_FIX_INDEX, LEFT_FIX_INDEX))

# Make a single "x_gaze" "y_gaze" columns
Data_AS2 <- Data_AS2 %>% mutate(x_gaze = ifelse(EYE_TRACKED == "Right", RIGHT_GAZE_X, LEFT_GAZE_X))

Data_AS2 <- Data_AS2 %>% mutate(y_gaze = ifelse(EYE_TRACKED == "Right", RIGHT_GAZE_Y, LEFT_GAZE_Y))

#single interest area
Data_AS2 <- Data_AS2 %>% mutate(interest_areas = ifelse(EYE_TRACKED == "Right", RIGHT_INTEREST_AREAS, LEFT_INTEREST_AREAS))

#single interest area data
Data_AS2 <- Data_AS2 %>% mutate(interest_area_data = ifelse(EYE_TRACKED == "Right", RIGHT_INTEREST_AREA_DATA, LEFT_INTEREST_AREA_DATA))

#single interest area distance
Data_AS2 <- Data_AS2 %>% mutate(interest_area_distance = ifelse(EYE_TRACKED == "Right", RIGHT_INTEREST_AREA_DISTANCE, LEFT_INTEREST_AREA_DISTANCE))

#single interest area ID
Data_AS2 <- Data_AS2 %>% mutate(interest_area_ID = ifelse(EYE_TRACKED == "Right", RIGHT_INTEREST_AREA_ID, LEFT_INTEREST_AREA_ID))

#single interest area label
Data_AS2 <- Data_AS2 %>% mutate(interest_area_label = ifelse(EYE_TRACKED == "Right", RIGHT_INTEREST_AREA_LABEL, LEFT_INTEREST_AREA_LABEL))

#single interest area pixel area
Data_AS2 <- Data_AS2 %>% mutate(IA_pixel_area = ifelse(EYE_TRACKED == "Right", RIGHT_INTEREST_AREA_PIXEL_AREA, LEFT_INTEREST_AREA_PIXEL_AREA))

#single blink column
Data_AS2 <- Data_AS2 %>% mutate(blink = ifelse(EYE_TRACKED == "Right", RIGHT_IN_BLINK, LEFT_IN_BLINK))

#single saccade column
Data_AS2 <- Data_AS2 %>% mutate(saccade = ifelse(EYE_TRACKED == "Right", RIGHT_IN_SACCADE, LEFT_IN_SACCADE))

#single saccade index column
Data_AS2 <- Data_AS2 %>% mutate(saccade_index = ifelse(EYE_TRACKED == "Right", RIGHT_SACCADE_INDEX, LEFT_SACCADE_INDEX))

#single pupil size column
Data_AS2 <- Data_AS2 %>% mutate(pupil_size = ifelse(EYE_TRACKED == "Right", RIGHT_PUPIL_SIZE, LEFT_PUPIL_SIZE))

#single x and y columns for velocity 
Data_AS2 <- Data_AS2 %>% mutate(x_velocity = ifelse(EYE_TRACKED == "Right", RIGHT_VELOCITY_X, LEFT_VELOCITY_X))

Data_AS2 <- Data_AS2 %>% mutate(y_velocity = ifelse(EYE_TRACKED == "Right", RIGHT_VELOCITY_Y, LEFT_VELOCITY_Y))


# Keep only relevant columns
Data_AS <- Data_AS2[c(1:3, 76:78, 92:108, 20:22, 24:25, 28, 46:47, 65:68, 81:86)]

#remove IP_index, IP_label, IP_end_event_matched, IP_start_event_matched

#replace "UNDEFINED" with NA in random_delay, direction, distance, t_x, t_y

Data_AS$Random_Delay <- na_if(Data_AS$Random_Delay, "UNDEFINED")
Data_AS$direction <- na_if(Data_AS$direction, "UNDEFINED")
Data_AS$distance <- na_if(Data_AS$distance, "UNDEFINED")
Data_AS$t_x <- na_if(Data_AS$t_x, "UNDEFINED")
Data_AS$t_y <- na_if(Data_AS$t_y, "UNDEFINED")



# create subject_visit column
cols <- c('subject', 'visit')

Data_AS$subject_visit <- apply(Data_AS[ , cols ] , 1 , paste , collapse = "_" )

Data_AS$subject_visit <- as.factor(Data_AS$subject_visit)

#rearrange column order
Data_AS <- Data_AS[c(1:4, 42, 6, 33, 7:12, 18:24, 26:27, 29:32, 34:41)]

Data_AS <- Data_AS[c(1:25, 28:34)]



#Create a "time" column for to put everyone in msec
#Sampling rate is 500 Hz (i.e., 1 sample every 2 msec)
Data_AS <- Data_AS %>% group_by(subject_visit) %>% mutate(time = TIMESTAMP - TRIAL_START_TIME) 
Data_AS <- Data_AS %>% group_by(subject_visit) %>% mutate(time_sec = (TIMESTAMP - TRIAL_START_TIME)/1000) 

#time within each trial


#rearrage



#=============================================================================================
# check the number of samples/time for each subject to make sure they all have the same sampling rate and amount of data

#calculate length of each trial to make sure all the subjects/vists are for the same length of time
Data_AS_test <- Data_AS %>% group_by(subject_visit) %>% summarise(length(time))

##need to cut down cn002 and cn008 by deleteing every 2nd sample

#---------------------------------------------------------------------------------------------------------
# need to remove rows with time containing ".5" before we remove every 2nd row - for cn003 and cn009
#---------------------------------------------------------------------------------------------------------


Data_AS <- Data_AS %>% group_by(subject_visit) %>% arrange(subject_visit)


Data_AS <- as.data.frame(Data_AS)

#=============================================================================================



df_as <- Data_AS %>% mutate(saccade_new = ifelse(blink == "1", "0", saccade))
df_as <- df_as[c(1:14, 35, 16:34)]

df_as <- rename(df_as, saccade = saccade_new)

#write.csv(df_as, "df_as.csv")

#=============================================================================================
#FILTER FOR DATA FROM TARGET DISPLAY
#=============================================================================================

#use df from above or from file saved to desktop
df_as <- read.csv("df_as.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(df_as)

df_as <- subset(df_as, select = -c(X))


# create ID_trial column
cols3 <- c('subject', 'visit', 'TRIAL_INDEX')

df_as$ID_trial <- apply(df_as[ , cols3 ] , 1 , paste , collapse = "_" )

df_as$ID_trial <- as.factor(df_as$ID_trial)

#rearrange
df_as <- df_as[c(1:3, 35, 4:34)]

#delete "trial column"
df_as <- df_as[c(1:32, 34:35)]

as_df_test <- df_as %>% group_by(ID_trial) %>% summarise(length(time))

#delete 1st trial for every p
df_as <- df_as %>% dplyr::filter(TRIAL_INDEX != "1")

as_df_test2 <- df_as %>% group_by(ID_trial) %>% summarise(length(time))

as_df <- df_as

#write.csv(as_df, 'as_df_no1.csv')


#duplicate
as_df_2 <- as_df %>% 
  mutate(SAMPLE_MESSAGE2 = SAMPLE_MESSAGE)

as_df_2 <- as_df_2 %>% 
  group_by(grp = cumsum(!is.na(SAMPLE_MESSAGE2))) %>% 
  mutate(SAMPLE_MESSAGE2 = replace(SAMPLE_MESSAGE2, first(SAMPLE_MESSAGE2) == 'Target_display', 'Target_timeout')) %>% 
  mutate(SAMPLE_MESSAGE2 = replace(SAMPLE_MESSAGE2, first(SAMPLE_MESSAGE2) == 'Target_timeout', 'End_trial_display')) %>% 
  ungroup() %>% 
  dplyr::select(-grp)

as_df_3 <- as_df_2 %>% filter(SAMPLE_MESSAGE2 == "End_trial_display")


#group_by (ID_trial) delete 40 rows after 1st row


#group_by (ID_trial) calculate distance to 1st saccade 

as_df_3 <- as_df_3 %>% group_by(ID_trial) %>% mutate(sample = row_number())


as_df_3_test <- as_df_3 %>% group_by(ID_trial) %>% summarise(length(time))


as_df_4 <- as_df_3 %>%
  dplyr::group_by(subject, TRIAL_INDEX) %>%
  mutate(saccades_inrange = with(rle(saccade), rep(seq_along(lengths), lengths)))




#create a saccade_index and blink column
cols11 <- c('saccade_index', 'blink')

as_df_4$sac_blink <- apply(as_df_4[ , cols11 ] , 1 , paste , collapse = "_" )

as_df_4$sac_blink <- as.factor(as_df_4$sac_blink)


# create saccade_index and ID_trial column

cols12 <- c('ID_trial', 'saccade_index')

as_df_4$ID_sac <- apply(as_df_4[ , cols12 ] , 1 , paste , collapse = "_" )

as_df_4$ID_sac <- as.factor(as_df_4$ID_sac)

#list ID_sac when sac_index_blink = 1_1, 2_1, 3_1, 4_1, 5_1

#unique(test5$sac_index_blink)
unique(as_df_4$sac_blink)


#list ID_sac when sac_blink = 1_1, 2_1, 3_1, 4_1, 5_1

as_df_5 <- as_df_4 %>% dplyr::select(ID_trial, ID_sac, saccade, blink, sac_blink, saccade_index, saccades_inrange)

as_df_6 <- as_df_5 %>%
  filter(str_detect(sac_blink, "_1"))

wrong_as <- unique(as_df_6$ID_sac)

#list of the saccades that include blinks - need to be deleted
#use cn002_1_6 as example
wrong_as2 <- as.data.frame(wrong_as)

wrong_as2 <- rename(wrong_as2, ID_sac = wrong_as)

wrong_as3 <- as_df_5[!as_df_5$ID_sac %in% wrong_as2$ID_sac,]


#rearrange saccades_inrange

wrong_as3_rearrange <- wrong_as3 %>%
  dplyr::group_by(ID_trial) %>%
  mutate(saccade_number = with(rle(saccade), rep(seq_along(lengths), lengths)))


#repeat with full df - removed the saccades that contained blinks

final_as_test <- as_df_4[!as_df_4$ID_sac %in% wrong_as2$ID_sac,]


#rearrange saccades_number

final_as_test <- final_as_test %>%
  dplyr::group_by(ID_trial) %>%
  mutate(saccade_number = with(rle(saccade), rep(seq_along(lengths), lengths)))

#write.csv(final_as_test, 'final_as_test.csv')



