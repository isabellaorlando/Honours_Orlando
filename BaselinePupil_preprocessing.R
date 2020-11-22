#================================================
# Script for baseline pupil data organisation
#================================================
# Orlando, O'Callaghan 2020

# Working directory is filepath of this script -------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set directories -------------------------------------------------------------------
dataDir <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/BaselinePupil/ProcessedData")
resultsDir <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/BaselinePupil/Results")

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

Data <- rbindlist(mapply(
  c,
  (
    list.files(
      path = dataDir,
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
      path = dataDir,
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
Data <- rename(Data, subject = V1)

# Remove .txt from name
Data$subject <- str_remove(Data$subject, ".txt")

# Remove first "_"
Data$subject <- str_remove(Data$subject, "_")

# Split into subject and visit column
Data <- Data %>% 
  separate(subject, into = c("subject", "visit"), 
           sep="_")

# Make subject and visit a factor
Data$subject <- as.factor(Data$subject)
Data$visit <- as.factor(Data$visit)

# Make a single "pupil" column (i.e., combining left and right pupils)
Data <- Data %>%
  mutate(pupil = ifelse(EYE_TRACKED == "Right", RIGHT_PUPIL_SIZE, LEFT_PUPIL_SIZE ))

# Make a single "gaze_x" column
Data <- Data %>%
  mutate(x = ifelse(EYE_TRACKED == "Right", RIGHT_GAZE_X, LEFT_GAZE_X))

# Make a single "gaze_y" column
Data <- Data %>%
  mutate(y = ifelse(EYE_TRACKED == "Right", RIGHT_GAZE_Y, LEFT_GAZE_Y))

# Make a single "blink" column
Data <- Data %>%
  mutate(blink = ifelse(EYE_TRACKED == "Right", RIGHT_IN_BLINK , LEFT_IN_BLINK))

#Create a "time" column for to put everyone in msec
#Sampling rate is 500 Hz (i.e., 1 sample every 2 msec)
Data <- Data %>% group_by(subject) %>% mutate(time = TIMESTAMP - TRIAL_START_TIME) 
Data <- Data %>% group_by(subject) %>% mutate(time_sec = (TIMESTAMP - TRIAL_START_TIME)/1000) 

# Keep only relevant columns
Data_BP <- Data[c(3, 17:24)]

BasePupil <- Data_BP %>% dplyr::mutate(visit = replace_na(visit, 1))

#=============================================================================================
# check the number of samples/time for each subject to make sure they all have the same sampling rate and amount of data

#calculate length of each trial to make sure all the subjects/vists are for the same length of time
#BasePupil_length_test <- BasePupil %>% group_by(subject) %>% summarise(length(time))

##need to cut down cn003 and cn008 to 90000 samples
#cn003
reduced_BasePupil_cn003 <- BasePupil %>%
  dplyr::filter(subject == "cn003")

#delete every 2nd row 2x
Nth.delete_cn003 <- function(reduced_BasePupil_cn003, n)reduced_BasePupil_cn003[-(seq(n,to=nrow(reduced_BasePupil_cn003),by=n)),]
Nth.delete_cn003 <- Nth.delete_cn003(reduced_BasePupil_cn003, 2)
Nth.delete_cn003_2 <- function(reduced_BasePupil_cn003, n)reduced_BasePupil_cn003[-(seq(n,to=nrow(reduced_BasePupil_cn003),by=n)),]
Nth.delete_cn003_2 <- Nth.delete_cn003_2(Nth.delete_cn003, 2)
#now have 90000 samples

#cn008
reduced_BasePupil_cn008 <- BasePupil %>%
  dplyr::filter(subject == "cn008")

#delete every 2nd row (so time increases by 2)
Nth.delete_cn008 <- function(reduced_BasePupil_cn008, n)reduced_BasePupil_cn008[-(seq(n,to=nrow(reduced_BasePupil_cn008),by=n)),]
Nth.delete_cn008 <- Nth.delete_cn008(reduced_BasePupil_cn008, 2)
#now have 90000 samples

#=============================================================================================
#need to delete rows in BasePupil of cn003 and cn008 the replace with rows from Nth.delete_cn008 and Nth.delete_cn003_2

BasePupil_edit <- BasePupil %>% dplyr::filter(subject != "cn003" & subject != "cn008")

#merge dfs
Base_Pupil <- rbind(BasePupil_edit, Nth.delete_cn008, Nth.delete_cn003_2)

#Make new data frame
Base_Pupil <- as.data.frame(Base_Pupil)

#removes these variables from global environment
rm(list = ls(pattern = "Nth.delete."))
rm(list = ls(pattern = "reduced."))

#=============================================================================================
#Create a "time bin" so we can carve the 3 min up into 15 sec segements (i.e., 12 bins)

Base_Pupil$Bin <- cut(Base_Pupil$time, 12, include.lowest=TRUE, labels=c("1", "2", "3","4", "5", "6","7", "8","9","10", "11", "12"))

Base_Pupil <- as.data.frame(Base_Pupil)

#summary of bin length in msec = query
BasePupil_summary <- Base_Pupil %>% 
  group_by(subject, visit, Bin) %>%
  summarise(Binlength = length(time))

#write.csv(BasePupil_summary, "BasePupil_binlength.csv")

#=============================================================================================
#Create subject_visit column as a unique identifier
cols <- c('subject', 'visit')

Base_Pupil$subject_visit <- apply(Base_Pupil[ , cols ] , 1 , paste , collapse = "_" )

Base_Pupil$subject_visit <- as.factor(Base_Pupil$subject_visit)

#check length of each trial to make sure all the subjects/vists are for the same length of time
#BasePupil_length_test <- Base_Pupil %>% group_by(subject_visit) %>% summarise(length(time))

#write.csv(BasePupil_length_test, "BasePupil_triallength.csv")

#=============================================================================================
#Downsample data to 125 Hz

#Uses pupilometryR downsample_time_data, need to put in pupilometryR format
Base_Pupil <- make_pupillometryr_data(data = Base_Pupil,
                                       subject = subject_visit,
                                       trial = Bin,
                                       time = time,
                                       condition = visit)

#write.csv(Base_Pupil, "Base_Pupil.csv")

#Calculate median pupil size in each timebin
Base_Pupil_ds <- downsample_time_data(data = Base_Pupil,
                                  pupil = pupil,
                                  timebin_size = 8,
                                  option = 'median')

#write.csv(Base_Pupil_ds, "Base_Pupil_ds.csv")

#================================================================================================
# Gaze plots 
#================================================================================================

#Uses Base_Pupil df not the downsampled one

gazeplot <- ggplot(Base_Pupil, aes(x, y, colour = TIMESTAMP)) +
  geom_point(shape=".") +
  scale_color_viridis() +
  theme_bw() +
  theme(panel.grid = element_blank())

pdf("~/Atomoxetine/BaselinePupil/Results/GazePlots.pdf")
for(i in 1:20){
  print(gazeplot +
          facet_wrap_paginate(~ subject + visit, ncol = 1, nrow = 3, page = i))
  
}
dev.off()

#================================================================================================
#Calculate missing data
#================================================================================================

#Create df of missing data (i.e., NAs)

#summary of bin length in msec
missing_summary <- Base_Pupil_ds %>% 
  group_by(subject_visit, Bin) %>%
  summarise(missing = mean(is.na(pupil)))

missing_summary$missing <- missing_summary$missing*100

missing_summary <- as.data.frame(missing_summary)

#================================================================================================
# plot missing NAs

missing_data_plot <- ggplot(missing_summary, aes(x=Bin, y=missing)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(panel.grid = element_blank())

print(missing_data_plot)

pdf("~/Desktop/Atomoxetine/BaselinePupil/Results/missing_data_plot_ds.pdf")
for(i in 1:50){
  print(missing_data_plot +
          facet_wrap_paginate(~ subject_visit, ncol = 1, nrow = 2, page = i))
}
dev.off()

#================================================================================================
#Remove bins with more than 30% missing
#================================================================================================

#create new column (ID_bin for subject, visit and bin info)
miss <- c('subject_visit', 'Bin')
Base_Pupil_ds$ID_bin <- apply(Base_Pupil_ds[ , miss ] , 1 , paste , collapse = "_" )
Base_Pupil_ds$ID_bin <- as.factor(Base_Pupil_ds$ID_bin)

#merge missing_summary df with Base_Pupil_ds 
Base_Pupil_clean <- left_join(Base_Pupil_ds, missing_summary)
Base_Pupil_clean <- filter(Base_Pupil_clean, missing < 30) %>% 
                    droplevels()

#================================================================================================
# Extending Blinks
#================================================================================================

#uses downsampled data, i.e., 125 Hz not 500 Hz
pup_extend <- Base_Pupil_clean %>% 
  group_by(subject_visit) %>% 
  mutate(extendpupil = extend_blinks(pupil, fillback = 100, fillforward = 100, hz=125))

#plot histogram to inspect for extreme pupil values https://rpubs.com/jgeller1/438015
#can also check range like this: range(pup_extend$extendpupil, na.rm = TRUE)

puphist <- ggplot(pup_extend, aes(x = extendpupil)) + 
            geom_histogram(aes(y = ..count..), colour = "green", binwidth = 0.5) + 
            xlab("Pupil Size") + ylab("Count") + 
            theme_bw() 

print(puphist)

pupildata <- as.data.frame(pup_extend) 

#======================================================================================================
# Smoothing, filtering and interpolation
#======================================================================================================

# GAZE R - Smoothing with Hanning window and linear interpolation

#Performing linear interpolation (with hann smoothing)
##Grouped by ID_bin

pupildata_bin <- pupildata %>% group_by(subject_visit) %>% mutate(sample = row_number())

pupil_interp_hann_bin <- pupildata_bin %>%
  dplyr::group_by(subject_visit) %>%
  dplyr::mutate(pup_interp = zoo::na.approx(extendpupil, na.rm = FALSE, rule=2)) %>%
  dplyr::ungroup()

smooth_hann_linear <- as.data.frame(pupil_interp_hann_bin) %>%
  dplyr::group_by(subject_visit) %>%
  mutate(smoothed = hanning_filter(pup_interp, degree=11))

#------------------------------------------------------------------------------------------
#plot

hann_linear_plot_bin <- ggplot(smooth_hann_linear, aes(sample, smoothed)) +
  geom_line() +
  xlab("Time (msec)") + ylab("Pupil size (arbitrary units)") +
  theme_bw() +
  theme(panel.grid = element_blank())

pdf("~/Desktop/Atomoxetine/BaselinePupil/Results/hann_linear_plot_ID_bin_final.pdf")
for(i in 1:30){
  print(hann_linear_plot_bin +
          facet_wrap_paginate(~ subject_visit, ncol = 1, nrow = 2, page = i))
}
dev.off()


#==========================================================================
#Mean & coefficient of variation of pupil diameter 
#==========================================================================

#CoV uses the cv function from raster library, expressed as a percentage

#Hanning mean per bin
mean_BPperbin_hann <- smooth_hann_linear %>% 
  group_by(subject_visit, Bin) %>%
  summarise(mean = mean((smoothed)), cv = cv(smoothed))

#==========================================================================
# Create final df to export
#==========================================================================

#merge missing_summary df with mean_BPperbin_hann  
df1_pupil <- left_join(mean_BPperbin_hann, missing_summary)

# Split subject_vists into subject and visit column
df_pupil <- df1_pupil %>% 
  separate(subject_visit, into = c("subject", "visit"), 
           sep="_")


#write.csv(df_pupil, "df_pupil.csv")
#df used for attach_info (demog df)

#write.csv(smooth_hann_linear, "smooth_hann_linear.csv")   
#df used for temp deriv ^


#write.csv(df1_pupil, "df1_pupil.csv")
