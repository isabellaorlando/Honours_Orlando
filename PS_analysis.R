#==========================================================
# Script for combining prosaccade df and ps amplitude df - analysis
#==========================================================
# Orlando, O'Callaghan 2020


# Working directory is filepath of this script -------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set directories -------------------------------------------------------------------
dataDir <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/BaselinePupil/ProcessedData")
resultsDir <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/BaselinePupil/Results")

# Load packages -------------------------------------------------------------------
library(stringr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(ggforce)
library(viridis)
library(zoo)
library(raster)
library(ggpubr)
library(RVAideMemoire)
library(NCmisc)

#install.packages('RVAideMemoire')
#remotes::install_github("thomas-hinterecker/Ragbu")
#install.packages('NCmisc')

# Read in data -------------------------------------------------------------------
ps_file <- read.csv("final_ps_test.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(ps_file)

ps_sac_file <- read.csv("Data_PS_A.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(ps_sac_file)

# Organise data frame -------------------------------------------------------
ps_sac_file <- subset(ps_sac_file, select = -c(X))
ps_file <- subset(ps_file, select = -c(X))


#create IDsac2 column for amp file
cols12 <- c('ID_trial', 'CURRENT_SAC_INDEX')

ps_sac_file$ID_sac <- apply(ps_sac_file[ , cols12 ] , 1 , paste , collapse = "_" )

ps_sac_file$ID_sac <- as.factor(ps_sac_file$ID_sac)

ps_sac_file <- ps_sac_file[c(1:5, 8, 222, 9:14, 23, 27, 42:52, 215:219)]


#Merge data frames
merge_PS <- left_join(ps_file, ps_sac_file, by = "ID_sac")

#write.csv(merge_PS, "df_ps_combined.csv")


amplitude_ps_id <- merge_PS[c(1:7, 10, 12:16, 21:27, 33:44, 49, 54:59, 69)]

amplitude_ps_id <- rename(amplitude_ps_id, subject = 'subject.x')
amplitude_ps_id <- rename(amplitude_ps_id, visit = 'visit.x')
amplitude_ps_id <- rename(amplitude_ps_id, trial = 'TRIAL_INDEX.x')
amplitude_ps_id <- rename(amplitude_ps_id, ID_trial = 'ID_trial.x')
amplitude_ps_id <- rename(amplitude_ps_id, Random_Delay = 'Random_Delay.x')
amplitude_ps_id <- rename(amplitude_ps_id, direction = 'direction.x')
amplitude_ps_id <- rename(amplitude_ps_id, t_x = 't_x.x')
amplitude_ps_id <- rename(amplitude_ps_id, t_y = 't_y.x')


#saccades that need to be removed
reduced_amp2 <- amplitude_ps_id %>% filter(CURRENT_SAC_AMPLITUDE < 1.5)
reduced_amp4 <- amplitude_ps_id %>% filter(CURRENT_SAC_AMPLITUDE > 10)


#------------------------------------------------------------------------------------
#create new column - ID_trial and saccade number

cols10 <- c('ID_trial', 'saccade_number')

reduced_amp2$ID_sac_number <- apply(reduced_amp2[ , cols10 ] , 1 , paste , collapse = "_" )

reduced_amp2$ID_sac_number <- as.factor(reduced_amp2$ID_sac_number)


reduced_amp4$ID_sac_number <- apply(reduced_amp4[ , cols10 ] , 1 , paste , collapse = "_" )

reduced_amp4$ID_sac_number <- as.factor(reduced_amp4$ID_sac_number)


amplitude_ps_id$ID_sac_number <- apply(amplitude_ps_id[ , cols10 ] , 1 , paste , collapse = "_" )

amplitude_ps_id$ID_sac_number <- as.factor(amplitude_ps_id$ID_sac_number)

#------------------------------------------------------------------------------------


#remove the saccades with <1.5 and >10 amplitude from the df
remove_1 <- unique(reduced_amp2$ID_sac_number)
remove_2 <- unique(reduced_amp4$ID_sac_number)


#list of the saccades that are <1.5 amplitude 
remove_1 <- as.data.frame(remove_1)
remove_2 <- as.data.frame(remove_2)

#rename
remove_1 <- rename(remove_1, ID_sac_number = remove_1)
remove_2 <- rename(remove_2, ID_sac_number = remove_2)

#pd004_2_15_2, pd004_2_18_2, pd004_2_22_2, pd004_2_22_4, pd004_2_25_4,	pd007_2_ 5_4


#df with saccades with amplitude <1.5 removed
ps_edit <- amplitude_ps_id[!amplitude_ps_id$ID_sac_number %in% remove_1$ID_sac_number,]

#remove remove_2 manually
ps_edit <- ps_edit %>% filter(ID_sac_number != "pd004_2_15_ 2" & ID_sac_number != "pd004_2_18_ 2" 
                                    & ID_sac_number != "pd004_2_22_ 2" & ID_sac_number != "pd004_2_22_ 4" 
                                    & ID_sac_number != "pd004_2_25_ 4" & ID_sac_number != "pd007_2_ 5_ 4")




#------------------------------------------------------------------------------------
#rearrange saccade_number
#------------------------------------------------------------------------------------

PS_FIN <- ps_edit %>%
  dplyr::group_by(ID_trial) %>%
  mutate(saccade_number2 = with(rle(saccade), rep(seq_along(lengths), lengths)))

PS_FIN <- PS_FIN %>%
  dplyr::filter(subject != 'cn005')

PS_FIN <- PS_FIN %>%
  dplyr::filter(ID_trial != 'pd010_1_18' & ID_trial != 'pd010_1_19' & ID_trial != 'pd010_1_20'
                & ID_trial != 'pd010_1_25')


#------------------------------------------------------------------------------------

#sanity check
puphist <- ggplot(PS_FIN, aes(x = CURRENT_SAC_AMPLITUDE)) + 
  geom_histogram(aes(y = ..count..), colour = "green", binwidth = 0.5) + 
  xlab("Pupil Size") + ylab("Count") + 
  theme_bw() 

puphist


#getRversion()

#length tests
#PS_FIN_test <- PS_FIN %>% group_by(ID_trial) %>% mutate(sample = row_number())

#PS_FIN_test <- PS_FIN_test %>% group_by(subject_visit) %>% summarise(length(sample))

#PS_FIN_test2 <- PS_FIN %>% group_by(subject_visit) %>% summarise(length(time))



#=============================================================================================
# 1. CORRECT DIRECTION
#=============================================================================================

ps_correct <- PS_FIN
ps_correct$direction <- str_replace(ps_correct$direction , "Left", "LEFT")
ps_correct$direction <- str_replace(ps_correct$direction , "Right", "RIGHT")

ps_correct <- ps_correct %>% mutate(correct = ifelse(direction == CURRENT_SAC_DIRECTION, "1", "0"))

#------------------------------------------------------------------------------------
#calculate proportion of 1 (correct) and 0 (incorrect)
#------------------------------------------------------------------------------------

ps_correct_test2 <- ps_correct %>% dplyr::filter(saccade_number2 == "2")

ps_correct_percent <- ps_correct_test2 %>%
  group_by(ID_trial) %>%
  summarise(correct_sum = sum(correct==1))

ps_correct_percent <- ps_correct_percent %>% mutate(correct = ifelse(correct_sum != 0, "1", "0"))

#replace NA with 0
ps_correct_percent <- ps_correct_percent %>% dplyr::mutate(correct = replace_na(correct, 0))


ps_test <- ps_correct_percent %>% filter(correct == 0)
#22 trials have incorrect 1st saccades


#unique(ps_test$ID_trial)
#trials with negative 1st saccades - remove from df YES
# cn017_1_16, cn021_1_20, cn024_1_24, pd002_2_15, pd003_1_ 8
# pd003_1_22, pd003_1_24, pd006_1_ 9, pd007_2_ 3, pd007_2_ 4, pd010_1_17, pd010_1_21
# pd010_1_23, pd012_1_ 3, pd012_1_16, pd012_2_16, pd015_1_ 7, pd017_1_ 2, pd018_1_ 8
# pd018_1_18, pd020_1_ 8, pd020_1_25


ps_correct_percent2 <- ps_correct_percent %>% 
  separate(ID_trial, into = c("subject", "visit", "trial"), 
           sep="_")

cols <- c('subject', 'visit')

ps_correct_percent2$subject_visit <- apply(ps_correct_percent2[ , cols ] , 1 , paste , collapse = "_" )

ps_correct_percent2$subject_visit <- as.factor(ps_correct_percent2$subject_visit)



ps_correct_percent22 <- ps_correct_percent2 %>% group_by(subject_visit) %>% summarise(length(trial))


ps_correct_percent2 <- ps_correct_percent2 %>%
  group_by(subject_visit) %>%
  summarise(correct_sum = sum(correct==1))

ps_correct_percent2 <- left_join(ps_correct_percent2, ps_correct_percent22, by = "subject_visit")
ps_correct_percent2 <- rename(ps_correct_percent2, trial_total = 'length(trial)')

ps_correct_percent2$trial_total <- as.numeric(ps_correct_percent2$trial_total)


ps_correct_percent2 <- ps_correct_percent2 %>% mutate(correct_percent = trial_total/correct_sum) 
ps_correct_percent2 <- ps_correct_percent2 %>% mutate(correct_percent = correct_percent*100) 


ps_correct_percent2 <- as.data.frame(ps_correct_percent2)


#write.csv(ps_correct_percent2, "ps_correct_percent.csv")

#attach conditions and then re import 
ps_cor <- read.csv("ps_correct_percent2.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(ps_cor)

ps_cor <- subset(ps_cor, select = -c(X))


ps_cor$condition <- as.factor(ps_cor$condition)

ps_cor <- ps_cor %>% 
  separate(subject_visit, into = c("subject", "visit"), 
           sep="_")

ps_cor$condition <- str_replace(ps_cor$condition, "control", "Control")
ps_cor$condition <- str_replace(ps_cor$condition, "placebo", "Placebo")
ps_cor$condition <- str_replace(ps_cor$condition, "atomoxetine", "Atomoxetine")

ps_cor$condition <- as.factor(ps_cor$condition)

levels(ps_cor$condition)
ps_cor$condition <- factor(ps_cor$condition,levels(ps_cor$condition)[c(2,3,1)])
levels(ps_cor$condition)


ps_cor_PD <- ps_cor %>%
  filter(str_detect(subject, "pd"))


group_by(ps_cor, condition) %>%
  summarise(
    count = n(),
    mean = mean(correct_sum, na.rm = TRUE),
    sd = sd(correct_sum, na.rm = TRUE)
  )

#condition       count  mean    sd
#  1 Control        25  94.7  14.0
#  2 Placebo        19  82.0  23.6
#  3 Atomoxetine    19  90.4  16.9

#-----------------------------------------------------------------
#independent t-test
#-----------------------------------------------------------------
ps_cor_control <- subset(ps_cor, condition!="Atomoxetine")


res.ftest <- var.test(correct_sum ~ condition, data = ps_cor_control)
res.ftest

#F = 0.34904, num df = 24, denom df = 18, p-value = 0.01706

t_test_PvC <- t.test(correct_sum ~ condition, data = ps_cor_control, var.equal = TRUE)
t_test_PvC

#t = 2.2209, df = 42, p-value = 0.0318     significant!!


#-----------------------------------------------------------------
#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
diff <- with(ps_cor_PD, 
             correct_sum[condition == "Placebo"] - correct_sum[condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.88274, p-value = 0.02397 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(correct_sum ~ condition, data = ps_cor_PD, paired = TRUE)
t_test_PvA

#t = -1.58, df = 18, p-value = 0.1315


#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------

# create a vector of the p values:
p.value_co <- c(0.0318, 0.1315)
# run this:
p.adjust(p.value_co, method = "holm")
# you get this result:
# 0.0636 0.1315


#=============================================================================================
# 2. REMOVE INCORRECT TRIALS FROM DF
#=============================================================================================

PS_FIN <- PS_FIN %>%
  dplyr::filter(ID_trial != 'cn017_1_16' & ID_trial != 'cn021_1_20' & ID_trial != 'cn024_1_24'
                & ID_trial != 'pd002_2_15' & ID_trial != 'pd003_1_ 8' & ID_trial != 'pd003_1_22' 
                & ID_trial != 'pd003_1_24' & ID_trial != 'pd006_1_ 9' & ID_trial != 'pd007_2_ 3' 
                & ID_trial != 'pd007_2_ 4' & ID_trial != 'pd010_1_17' & ID_trial != 'pd010_1_21' 
                & ID_trial != 'pd010_1_23' & ID_trial != 'pd012_1_ 3' & ID_trial != 'pd012_1_16' 
                & ID_trial != 'pd012_2_16' & ID_trial != 'pd015_1_ 7' & ID_trial != 'pd017_1_ 2' 
                & ID_trial != 'pd018_1_ 8' & ID_trial != 'pd018_1_18' & ID_trial != 'pd020_1_ 8' 
                & ID_trial != 'pd020_1_25')

#22 trials removed

#=============================================================================================
# 3. ACCURACY
#=============================================================================================
#distance from the target (t_x)

#separate into left and right by 'direction' of target

#left
PS_FIN_left <- PS_FIN %>%
  dplyr::filter(direction == 'Left')

PS_FIN_left <- PS_FIN_left %>% filter(saccade_number2 == "2")

#right
PS_FIN_right <- PS_FIN %>%
  dplyr::filter(direction == 'Right')

PS_FIN_right <- PS_FIN_right %>% filter(saccade_number2 == "2")

#calculate the difference between 't_x' and 'current_sac_end_x"
# left  --> t_x' - 'current_sac_end_x": 
PS_FIN_left <- PS_FIN_left %>% mutate(difference = t_x - CURRENT_SAC_END_X) 


# right --> t_x' - 'current_sac_end_x":  
PS_FIN_right <- PS_FIN_right %>% mutate(difference = t_x - CURRENT_SAC_END_X) 

#identify if the saccade is positive of negative (will be different for left and right)

#if negative on left = hypo, if positive = hyper
#hypermetric column --> hyper = 1, hypo = -1
PS_FIN_left <- PS_FIN_left %>% mutate(hyper_hypo = ifelse(difference <= 0, '-1' , '1'))

#if in target area --> identify if saccade is in interest area ( = [1])
PS_FIN_left <- PS_FIN_left %>% mutate(on_target = ifelse(interest_areas == '[ 1 ]', '1' , ''))


#if negative on right = hyper, if positive = hypo
#hypermetric column --> hyper = 1, hypo = -1
PS_FIN_right <- PS_FIN_right %>% mutate(hyper_hypo = ifelse(difference <= 0, '1' , '-1'))

#if in target area --> identify if saccade is in interest area ( = [1])
PS_FIN_right <- PS_FIN_right %>% mutate(on_target = ifelse(interest_areas == '[ 1 ]', '1' , ''))


#merge hypometric and on_target columns
#hypo = -1
#hyper = 1
#on_target = 0
PS_FIN_left <- PS_FIN_left %>% mutate(off_target = ifelse(on_target == '1', '0' , hyper_hypo))

PS_FIN_right <- PS_FIN_right %>% mutate(off_target = ifelse(on_target == '1', '0' , hyper_hypo))


#if saccade contains some 0s, make it all 0
#left
PS_FIN_left <- PS_FIN_left %>% group_by(ID_sac_number) %>%
  mutate(truly_on = any(str_detect(off_target, '0')))

PS_FIN_left <- PS_FIN_left %>% mutate(target_missed = ifelse(truly_on == 'TRUE', '0' , off_target))

#right
PS_FIN_right <- PS_FIN_right %>% group_by(ID_sac_number) %>%
  mutate(truly_on = any(str_detect(off_target, '0')))

PS_FIN_right <- PS_FIN_right %>% mutate(target_missed = ifelse(truly_on == 'TRUE', '0' , off_target))



#then calculate percentage of on target, hypo and hyper saccades for each subject
PS_FIN_left_test <- PS_FIN_left %>% 
  group_by(ID_trial) %>%
  summarise(missed = first(target_missed))

#unique(PS_FIN_left_test$missed)
# "0"  "-1" NA

PS_FIN_right_test <- PS_FIN_right %>% 
  group_by(ID_trial) %>%
  summarise(missed = first(target_missed))

#unique(PS_FIN_right_test$missed)
# "0"  "-1" NA   "1" 


#merge the 2 dfs

PS_missed_target <- rbind(PS_FIN_right_test, PS_FIN_left_test)

#rearrange in order

PS_missed_target <- arrange(PS_missed_target, ID_trial)


PS_missed_target <- PS_missed_target %>% 
  separate(ID_trial, into = c("subject", "visit", "trial"), 
           sep="_")

cols <- c('subject', 'visit')

PS_missed_target$subject_visit <- apply(PS_missed_target[ , cols ] , 1 , paste , collapse = "_" )

PS_missed_target$subject_visit <- as.factor(PS_missed_target$subject_visit)


cols11 <- c('subject', 'visit', 'trial')

PS_missed_target$ID_trial <- apply(PS_missed_target[ , cols11 ] , 1 , paste , collapse = "_" )

PS_missed_target$ID_trial <- as.factor(PS_missed_target$ID_trial)


#number of trials performed for each subject
PS_missed_target_2 <- PS_missed_target %>% group_by(subject_visit) %>% summarise(length(trial))

PS_miss <- left_join(PS_missed_target, PS_missed_target_2, by = "subject_visit")
PS_miss <- rename(PS_miss, trial_total = 'length(trial)')


#calculate the number of '0', '1' and '-1' for each subject 
#divide by the length of trials each subject 
PS_miss_0 <- PS_miss %>% group_by(subject_visit) %>% summarise(hit = sum(missed==0))

PS_miss_hyper <- PS_miss %>% group_by(subject_visit) %>% summarise(hyper = sum(missed==1))

PS_miss_hypo <- PS_miss %>% group_by(subject_visit) %>% summarise(hypo = sum(missed=='-1'))

#join all the new calculations
PS_miss <- left_join(PS_miss, PS_miss_0, by = "subject_visit")
PS_miss <- left_join(PS_miss, PS_miss_hyper, by = "subject_visit")
PS_miss <- left_join(PS_miss, PS_miss_hypo, by = "subject_visit")


#calculate the percentages
PS_miss <- PS_miss %>% group_by(subject_visit) %>% mutate(target = hit/trial_total) 
PS_miss <- PS_miss %>% group_by(subject_visit) %>% mutate(target = target*100) 

PS_miss <- PS_miss %>% group_by(subject_visit) %>% mutate(hyper_percent = hyper/trial_total) 
PS_miss <- PS_miss %>% group_by(subject_visit) %>% mutate(hyper_percent = hyper_percent*100) 

PS_miss <- PS_miss %>% group_by(subject_visit) %>% mutate(hypo_percent = hypo/trial_total) 
PS_miss <- PS_miss %>% group_by(subject_visit) %>% mutate(hypo_percent = hypo_percent*100) 


#aggregate - one line per subject 
PS_miss_red <- PS_miss %>% group_by(subject_visit) %>% summarise(first(target))
PS_miss_red2 <- PS_miss %>% group_by(subject_visit) %>% summarise(first(hyper_percent))
PS_miss_red3 <- PS_miss %>% group_by(subject_visit) %>% summarise(first(hypo_percent))

PS_miss <- left_join(PS_miss, PS_miss_red, by = "subject_visit")
PS_miss <- left_join(PS_miss, PS_miss_red2, by = "subject_visit")
PS_miss <- left_join(PS_miss, PS_miss_red3, by = "subject_visit")

#write.csv(PS_miss_red, 'PS_miss_red.csv')
#write.csv(PS_miss_red2, 'PS_miss_red2.csv')
#write.csv(PS_miss_red3, 'PS_miss_red3.csv')

#join and then attach conditions

PS_accuracy <- read.csv("PS_miss_csv.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(PS_accuracy)

PS_accuracy <- subset(PS_accuracy, select = -c(X))


#analyse for conditional singificances 
PS_accuracy$condition <- as.factor(PS_accuracy$condition)

#remove pd004_2
PS_accuracy <- PS_accuracy %>% dplyr::filter(subject_visit != 'pd004_2') 


PS_accuracy <- PS_accuracy %>% 
  separate(subject_visit, into = c("subject", "visit"), 
           sep="_")

PS_accuracy$condition <- str_replace(PS_accuracy$condition, "control", "Control")
PS_accuracy$condition <- str_replace(PS_accuracy$condition, "placebo", "Placebo")
PS_accuracy$condition <- str_replace(PS_accuracy$condition, "atomoxetine", "Atomoxetine")

PS_accuracy$condition <- as.factor(PS_accuracy$condition)

levels(PS_accuracy$condition)
PS_accuracy$condition <- factor(PS_accuracy$condition,levels(PS_accuracy$condition)[c(2,3,1)])
levels(PS_accuracy$condition)

PS_accuracy_PD <- PS_accuracy %>%
  filter(str_detect(subject, "pd"))

#remove pd004_1
PS_accuracy_PD <- PS_accuracy_PD %>% dplyr::filter(subject != 'pd004') 
                                       


#total (not mean) - raw cumulative
bar_ontarget <- ggplot(PS_accuracy, aes(x=condition, y=on_target)) + geom_bar(stat='identity')
bar_ontarget

bar_hypo <- ggplot(PS_accuracy, aes(x=condition, y=hypo)) + geom_bar(stat='identity')
bar_hypo

bar_hyper <- ggplot(PS_accuracy, aes(x=condition, y=hyper)) + geom_bar(stat='identity')
bar_hyper


ontarget <- group_by(PS_accuracy, condition) %>%
  summarise(
    count = n(),
    mean = mean(on_target, na.rm = TRUE),
    sd = sd(on_target, na.rm = TRUE)
  )

ontarget
#  condition   count  mean    sd
#  1 Control        25  86.8  18.4
#  2 Placebo        19  84.6  14.3
#  3 Atomoxetine    19  75.0  20.5


hypo <- group_by(PS_accuracy, condition) %>%
  summarise(
    count = n(),
    mean = mean(hypo, na.rm = TRUE),
    sd = sd(hypo, na.rm = TRUE)
  )

hypo
#1 Control        25  13.2  18.4
#2 Placebo        19  15.4  14.3
#3 Atomoxetine    19  24.5  19.5

hyper <- group_by(PS_accuracy, condition) %>%
  summarise(
    count = n(),
    mean = mean(hyper, na.rm = TRUE),
    sd = sd(hyper, na.rm = TRUE)
  )

hyper
#1 Control        25 0      0   
#2 Placebo        19 0      0   
#3 Atomoxetine    19 0.458  1.99


#means
bar_ontarget2 <- ggplot(ontarget, aes(x=condition, y=mean)) + geom_bar(stat='identity')
bar_ontarget2 + xlab("") + ylab("Mean on-target saccades (%)")

bar_hypo2 <- ggplot(hypo, aes(x=condition, y=mean)) + geom_bar(stat='identity')
bar_hypo2 + xlab("") + ylab("Mean hypometric saccades (%)")

bar_hyper2 <- ggplot(hyper, aes(x=condition, y=mean)) + geom_bar(stat='identity')
bar_hyper2 + xlab("") + ylab("Mean hypermetric saccades (%)")


#-----------------------------------------------------------------
# ANOVA, sd and variance - ON TARGET
#-----------------------------------------------------------------

group_by(PS_accuracy, condition) %>%
  summarise(
    count = n(),
    mean = mean(on_target, na.rm = TRUE),
    sd = sd(on_target, na.rm = TRUE)
  )

#condition      count  mean    sd
#1 Control        25  86.8  18.4
#2 Placebo        18  84.6  14.3
#3 Atomoxetine    19  75.0  20.5

res.aov <- aov(on_target ~ condition, data = PS_accuracy)
# Summary of the analysis
summary(res.aov)

#Tukey multiple comparisons of means
#95% family-wise confidence level
TukeyHSD(res.aov)

#Placebo-Control      -2.228584 -15.62704 11.169872 0.9157543
#Atomoxetine-Control -11.739028 -24.93090  1.452840 0.0906063
#Atomoxetine-Placebo  -9.510444 -23.76700  4.746111 0.2520426

#-----------------------------------------------------------------
#independent t-test
#-----------------------------------------------------------------
ps_acc_control <- subset(PS_accuracy, condition!="Atomoxetine")


res.ftest <- var.test(on_target ~ condition, data = ps_acc_control)
res.ftest

#F = 1.6716, num df = 24, denom df = 17, p-value = 0.2781

t_test_PvC <- t.test(on_target ~ condition, data = ps_acc_control, var.equal = TRUE)
t_test_PvC

#t = 0.42826, df = 41, p-value = 0.6707

#-----------------------------------------------------------------
#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
diff <- with(PS_accuracy_PD, 
             on_target[condition == "Placebo"] - on_target[condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.91461, p-value = 0.1038 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(on_target ~ condition, data = PS_accuracy_PD, paired = TRUE)
t_test_PvA

#t = 1.5197, df = 17, p-value = 0.147


#-----------------------------------------------------------------
# ANOVA, sd and variance - HYPOSACCADES
#-----------------------------------------------------------------

group_by(PS_accuracy, condition) %>%
  summarise(
    count = n(),
    mean = mean(hypo, na.rm = TRUE),
    sd = sd(hypo, na.rm = TRUE)
  )

#condition      count  mean    sd
#1 Control        25  13.2  18.4
#2 Placebo        19  15.4  14.3
#3 Atomoxetine    19  24.5  19.5

res.aov <- aov(hypo ~ condition, data = PS_accuracy)
# Summary of the analysis
summary(res.aov)

#Tukey multiple comparisons of means
#95% family-wise confidence level
TukeyHSD(res.aov)

#Placebo-Control      2.228584 -10.921559 15.37873 0.9126928
#Atomoxetine-Control 11.281362  -1.666022 24.22875 0.0995934
#Atomoxetine-Placebo  9.052778  -4.939561 23.04512 0.2729547

#-----------------------------------------------------------------
#independent t-test
#-----------------------------------------------------------------
ps_acc_control <- subset(PS_accuracy, condition!="Atomoxetine")


res.ftest <- var.test(hypo ~ condition, data = ps_acc_control)
res.ftest

#F = 1.6716, num df = 24, denom df = 17, p-value = 0.2781

t_test_PvC <- t.test(hypo ~ condition, data = ps_acc_control, var.equal = TRUE)
t_test_PvC

#t = -0.42826, df = 41, p-value = 0.6707

#-----------------------------------------------------------------
#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------

diff <- with(PS_accuracy_PD, 
             hypo[condition == "Placebo"] - hypo[condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.92165, p-value = 0.1383 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(hypo ~ condition, data = PS_accuracy_PD, paired = TRUE)
t_test_PvA

#t = -1.5084, df = 17, p-value = 0.1498

# % of on_target, hypo and hyper for each condition 




#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------

# create a vector of the p values:
p.value_hypo <- c(0.6707, 0.1498)
# run this:
p.adjust(p.value_hypo, method = "holm")
# you get this result:
# 0.6707 0.2996

#=============================================================================================
# 4. LATENCY
#=============================================================================================

ps_latency <- PS_FIN %>% filter(saccade_number2 == "1")

#group_by (ID_trial) calculate distance to 1st saccade 
ps_latency <- ps_latency %>% group_by(ID_trial) %>% mutate(sample = row_number())

#summarise number of samples
ps_latency_test <- ps_latency %>% group_by(ID_trial, saccade_number2) %>% summarise(length(sample))

ps_latency_test <- rename(ps_latency_test, latency = 'length(sample)')

#since sampling is at 500Hz, there is a sample every 2ms therefore need to double the length
ps_latency_test$latency <- ps_latency_test$latency*2


ps_latency_test <- ps_latency_test %>% filter(ID_trial != "pd010_1_13")

#create subject_visit column
ps_latency_test <- ps_latency_test %>% 
  separate(ID_trial, into = c("subject", "visit", "trial"), 
           sep="_")

cols <- c('subject', 'visit')

ps_latency_test$subject_visit <- apply(ps_latency_test[ , cols ] , 1 , paste , collapse = "_" )

ps_latency_test$subject_visit <- as.factor(ps_latency_test$subject_visit)


ps_latency_out <- ps_latency_test %>%
  group_by(subject_visit) %>%
  filter(between(latency, mean(latency, na.rm=TRUE) - (2.5 * sd(latency, na.rm=TRUE)), 
                 mean(latency, na.rm=TRUE) + (2.5 * sd(latency, na.rm=TRUE))))

#=51 trials


#test to see how many saccades are anticipatory
ps_latency_out2 <- ps_latency_out %>% filter(latency < 80)
#=24 trials

#remove anticipatory saccades
ps_latency_out <- ps_latency_out %>% filter(latency > 80)



#sumamrise mean for each subject_visit
ps_latency_mean <- ps_latency_out %>% 
  group_by(subject_visit) %>%
  summarise(latency_mean = mean(latency), sd = sd(latency))

#write.csv(ps_latency_mean, 'ps_latency_mean.csv')


#=============================================================================================
#Analysis - latency
#=============================================================================================


#attach conditions 
ps_lat <- read.csv("ps_latency_mean2.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(ps_lat)

ps_lat <- subset(ps_lat, select = -c(X))


#analyse for conditional singificances 
ps_lat$condition <- as.factor(ps_lat$condition)

ps_lat <- ps_lat %>% 
  separate(subject_visit, into = c("subject", "visit"), sep="_")

ps_lat$condition <- str_replace(ps_lat$condition, "control", "Control")
ps_lat$condition <- str_replace(ps_lat$condition, "placebo", "Placebo")
ps_lat$condition <- str_replace(ps_lat$condition, "atomoxetine", "Atomoxetine")

ps_lat$condition <- as.factor(ps_lat$condition)
ps_lat$subject_visit <- as.factor(ps_lat$subject_visit)


levels(ps_lat$condition)
ps_lat$condition <- factor(ps_lat$condition,levels(ps_lat$condition)[c(2,3,1)])
levels(ps_lat$condition)


ps_lat_PD <- ps_lat %>%
  filter(str_detect(subject, "pd")) %>% 
  droplevels()

#outlier identification - IQR
boxplot(latency_mean ~ condition, data = ps_lat,  main = 'Latency')

# Paired plots -------------------------------------------------------------------

p20 <- ggpaired(ps_lat_PD, x = "condition", y = "latency_mean",
               color = "condition", line.color = "gray", line.size = 0.4, id = "subject", 
               xlab = FALSE, ylab = "", palette = c("#ffaa71", "#ff7171")) +
  theme(legend.position="none")

p20
# three group plots -------------------------------------------------------------------  

p21 <-  ggboxplot(ps_lat, x = "condition", y = "latency_mean",
                 color = "condition", line.color = "gray", line.size = 0.4,
                 xlab = FALSE, ylab = "Mean latency (ms)",
                 palette = c("#6e6d6d", "#ffaa71", "#ff7171"), add = "jitter") + 
  theme(legend.position="none") 

p21

ggarrange(p21, p20, ncol=2, nrow=1)


#-----------------------------------------------------------------
# ANOVA, sd and variance
#-----------------------------------------------------------------

group_by(ps_lat, condition) %>%
  summarise(
    count = n(),
    mean = mean(latency_mean, na.rm = TRUE),
    sd = sd(latency_mean, na.rm = TRUE)
  )

#condition      count  mean    sd
#1 Control        25  202.  37.3
#2 Placebo        19  242.  84.6
#3 Atomoxetine    19  192.  29.7

res.aov <- aov(latency_mean ~ condition, data = ps_lat)
# Summary of the analysis
summary(res.aov)

#Tukey multiple comparisons of means
#95% family-wise confidence level
TukeyHSD(res.aov)

#Atomoxetine-Placebo -50.11320 -92.5859561 -7.640439 0.0168937 !!!

#-----------------------------------------------------------------
#independent t-test
#-----------------------------------------------------------------
ps_lat_control <- ps_lat %>% filter(condition!="Atomoxetine")  %>% 
  droplevels()


res.ftest <- var.test(latency_mean ~ condition, data = ps_lat_control)
res.ftest

#F = 0.19498, num df = 24, denom df = 18, p-value = 0.0002794

t_test_PvC <- t.test(latency_mean ~ condition, data = ps_lat_control, var.equal = TRUE)
t_test_PvC

#t = -2.0893, df = 42, p-value = 0.04278 !!!! signif

#-----------------------------------------------------------------
#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
diff <- with(ps_lat_PD, 
             latency_mean[condition == "Placebo"] - latency_mean[condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.78193, p-value = 0.0006318 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(latency_mean ~ condition, data = ps_lat_PD, paired = TRUE)
t_test_PvA

#t = 2.8449, df = 18, p-value = 0.01075  !!! signif



# Unpaired test
perm.t.test(latency_mean ~ condition, nperm=5000, data = ps_lat_control)
# t = -2.0893, p-value = 0.04679   !!!! signif
str(ps_lat_control)

# Paired test
perm.t.test(latency_mean ~ condition, paired=TRUE,nperm=5000, data = ps_lat_PD,)
#t = 2.8449, p-value = 0.0112      !!!! signif


#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------

# create a vector of the p values:
p.value_lat <- c(0.04679, 0.0112)
# run this:
p.adjust(p.value_lat, method = "holm")
# you get this result:
# 0.04679 0.02240



#=============================================================================================
# 5. PEAK VELOCITY
#=============================================================================================

peakv_ps <- PS_FIN %>% dplyr::filter(saccade_number2 == "2")


peakv_ps <- peakv_ps %>% 
  group_by(ID_trial) %>%
  summarise(peakv = mean(CURRENT_SAC_PEAK_VELOCITY))

peakv_ps <- peakv_ps %>% filter(peakv != "NA")


peakv_ps_test <- peakv_ps %>% 
  separate(ID_trial, into = c("subject", "visit", "trial"), 
           sep="_")

cols <- c('subject', 'visit')

peakv_ps_test$subject_visit <- apply(peakv_ps_test[ , cols ] , 1 , paste , collapse = "_" )

peakv_ps_test$subject_visit <- as.factor(peakv_ps_test$subject_visit)


#remove extreme values
peakv_ps_out <- peakv_ps_test %>%
  group_by(subject_visit) %>%
  filter(between(peakv, mean(peakv, na.rm=TRUE) - (2.5 * sd(peakv, na.rm=TRUE)), 
                 mean(peakv, na.rm=TRUE) + (2.5 * sd(peakv, na.rm=TRUE))))
#removes 7 trials

#sumamrise mean for each subject_visit
peakv_ps_mean <- peakv_ps_out %>% 
  group_by(subject_visit) %>%
  summarise(peak_velocity = mean(peakv))


#write_csv(peakv_ps_mean, 'peakv_ps_mean.csv')

#=============================================================================================
#Analysis - peak velocity
#=============================================================================================


#attach conditions 
ps_pv <- read.csv("peakv_ps_mean2.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(ps_pv)

#analyse for conditional singificances 
ps_pv$condition <- as.factor(ps_pv$condition)

ps_pv <- ps_pv %>% 
  separate(subject_visit, into = c("subject", "visit"), 
           sep="_")

ps_pv$condition <- str_replace(ps_pv$condition, "control", "Control")
ps_pv$condition <- str_replace(ps_pv$condition, "placebo", "Placebo")
ps_pv$condition <- str_replace(ps_pv$condition, "atomoxetine", "Atomoxetine")

ps_pv$condition <- as.factor(ps_pv$condition)

levels(ps_pv$condition)
ps_pv$condition <- factor(ps_pv$condition,levels(ps_pv$condition)[c(2,3,1)])
levels(ps_pv$condition)

#ps_dur <- ps_dur %>% filter(subject != "pd010")


ps_pv_PD <- ps_pv %>%
  filter(str_detect(subject, "pd"))

boxplot(peak_velocity ~ condition, data = ps_pv,  main = 'Peak Velocity')


# Paired plots -------------------------------------------------------------------

p26 <- ggpaired(ps_pv_PD, x = "condition", y = "peak_velocity",
                color = "condition", line.color = "gray", line.size = 0.4, id = "subject", 
                xlab = FALSE, ylab = "", palette = c("#3e64ff", "#ff5151")) +
  theme(legend.position="none")

p26
# three group plots -------------------------------------------------------------------  

p27 <-  ggboxplot(ps_pv, x = "condition", y = "peak_velocity",
                  color = "condition", line.color = "gray", line.size = 0.4,
                  xlab = FALSE, ylab = "Peak velocity",
                  palette = c("#f3a953", "#3e64ff", "#ff5151"), add = "jitter") + 
  theme(legend.position="none") 

p27

ggarrange(p27, p26, ncol=2, nrow=1)


#-----------------------------------------------------------------
# ANOVA, sd and variance
#-----------------------------------------------------------------

group_by(ps_pv, condition) %>%
  summarise(
    count = n(),
    mean = mean(peak_velocity, na.rm = TRUE),
    sd = sd(peak_velocity, na.rm = TRUE)
  )


#condition       count  mean    sd
#Control           25  257.  38.4
#Placebo           19  267.  57.8
#Atomoxetine       19  255.  63.0


res.aov <- aov(peak_velocity ~ condition, data = ps_pv)
# Summary of the analysis
summary(res.aov)

#Tukey multiple comparisons of means
#95% family-wise confidence level
TukeyHSD(res.aov)

#Atomoxetine-Placebo -12.350192 -53.48609 28.78571 0.7517653


#-----------------------------------------------------------------
#independent t-test
#-----------------------------------------------------------------
ps_pv_control <- subset(ps_pv, condition!="Atomoxetine")


res.ftest <- var.test(peak_velocity ~ condition, data = ps_pv_control)
res.ftest

#F = 0.44072, num df = 24, denom df = 18, p-value = 0.06181

t_test_PvC <- t.test(peak_velocity ~ condition, data = ps_pv_control, var.equal = TRUE)
t_test_PvC

#t = -0.7244, df = 42, p-value = 0.4728

#-----------------------------------------------------------------
#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
diff <- with(ps_pv_PD, 
             peak_velocity[condition == "Placebo"] - peak_velocity[condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.93467, p-value = 0.211 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(peak_velocity ~ condition, data = ps_pv_PD, paired = TRUE)
t_test_PvA

#t = 1.3101, df = 18, p-value = 0.2066



#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------

# create a vector of the p values:
p.value_pv2 <- c(0.4728, 0.2066)
# run this:
p.adjust(p.value_pv2, method = "holm")
# you get this result:
# 0.4728 0.4132


#=============================================================================================
# 7. AVERAGE AMPLITUDE
#=============================================================================================

amp_ps <- PS_FIN %>% dplyr::filter(saccade_number2 == "2")

amp_ps <- amp_ps %>% 
  group_by(ID_trial) %>%
  summarise(amplitude = mean(CURRENT_SAC_AMPLITUDE))

amp_ps <- amp_ps %>% filter(amplitude != "NA")


amp_ps_test <- amp_ps %>% 
  separate(ID_trial, into = c("subject", "visit", "trial"), 
           sep="_")

cols <- c('subject', 'visit')

amp_ps_test$subject_visit <- apply(amp_ps_test[ , cols ] , 1 , paste , collapse = "_" )

amp_ps_test$subject_visit <- as.factor(amp_ps_test$subject_visit)


#remove extreme values
amp_ps_out <- amp_ps_test %>%
  group_by(subject_visit) %>%
  filter(between(amplitude, mean(amplitude, na.rm=TRUE) - (2.5 * sd(amplitude, na.rm=TRUE)), 
                 mean(amplitude, na.rm=TRUE) + (2.5 * sd(amplitude, na.rm=TRUE))))
#removes 2 trials

#sumamrise mean for each subject_visit
amp_ps_mean <- amp_ps_test %>% 
  group_by(subject_visit) %>%
  summarise(amp_mean = mean(amplitude))


#write_csv(amp_ps_mean, 'amp_ps_mean.csv')




#=============================================================================================
#Analysis - amplitude
#=============================================================================================


#attach conditions 
ps_amplitude <- read.csv("amp_ps_mean2.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(ps_amplitude)

#analyse for conditional singificances 
ps_amplitude$condition <- as.factor(ps_amplitude$condition)

ps_amplitude <- ps_amplitude %>% 
  separate(subject_visit, into = c("subject", "visit"), 
           sep="_")

ps_amplitude$condition <- str_replace(ps_amplitude$condition, "control", "Control")
ps_amplitude$condition <- str_replace(ps_amplitude$condition, "placebo", "Placebo")
ps_amplitude$condition <- str_replace(ps_amplitude$condition, "atomoxetine", "Atomoxetine")

ps_amplitude$condition <- as.factor(ps_amplitude$condition)

levels(ps_amplitude$condition)
ps_amplitude$condition <- factor(ps_amplitude$condition,levels(ps_amplitude$condition)[c(2,3,1)])
levels(ps_amplitude$condition)


ps_amplitude_PD <- ps_amplitude %>%
  filter(str_detect(subject, "pd"))

boxplot(amp_mean ~ condition, data = ps_amplitude,  main = 'Amplitude')


# Paired plots -------------------------------------------------------------------

p24 <- ggpaired(ps_amplitude_PD, x = "condition", y = "amp_mean",
                color = "condition", line.color = "gray", line.size = 0.4, id = "subject", 
                xlab = FALSE, ylab = "", palette = c("#3e64ff", "#ff5151")) +
  theme(legend.position="none")

p24
# three group plots -------------------------------------------------------------------  

p25 <-  ggboxplot(ps_amplitude, x = "condition", y = "amp_mean",
                  color = "condition", line.color = "gray", line.size = 0.4,
                  xlab = FALSE, ylab = "Mean amplitude (degrees)",
                  palette = c("#f3a953", "#3e64ff", "#ff5151"), add = "jitter") + 
  theme(legend.position="none") 

p25

ggarrange(p25, p24, ncol=2, nrow=1)




#-----------------------------------------------------------------
# ANOVA, sd and variance
#-----------------------------------------------------------------

group_by(ps_amplitude, condition) %>%
  summarise(
    count = n(),
    mean = mean(amp_mean, na.rm = TRUE),
    sd = sd(amp_mean, na.rm = TRUE)
  )

#condition      count  mean    sd
# 1 Control        25  4.88 0.377
# 2 Placebo        19  4.83 0.456
# 3 Atomoxetine    19  4.77 0.675

res.aov <- aov(amp_mean ~ condition, data = ps_amplitude)
# Summary of the analysis
summary(res.aov)

#Tukey multiple comparisons of means
#95% family-wise confidence level
TukeyHSD(res.aov)

# Atomoxetine-Placebo -0.06147126 -0.4560103 0.3330678 0.9257290

#-----------------------------------------------------------------
#independent t-test
#-----------------------------------------------------------------
ps_amplitude_con <- subset(ps_amplitude, condition!="Atomoxetine")


res.ftest <- var.test(amp_mean ~ condition, data = ps_amplitude_con)
res.ftest

#F = 0.68172, num df = 24, denom df = 18, p-value = 0.3762


t_test_PvC <- t.test(amp_mean ~ condition, data = ps_amplitude_con, var.equal = TRUE)
t_test_PvC

#t = 0.39816, df = 42, p-value = 0.6925

#-----------------------------------------------------------------
#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
diff <- with(ps_amplitude_PD, 
             amp_mean[condition == "Placebo"] - amp_mean[condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.96082, p-value = 0.5886 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(amp_mean ~ condition, data = ps_amplitude_PD, paired = TRUE)
t_test_PvA

#t = 0.39045, df = 18, p-value = 0.7008


#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------

# create a vector of the p values:
p.value_amp2 <- c(0.6925, 0.7008)
# run this:
p.adjust(p.value_amp2, method = "holm")
# you get this result:
# 0.4728 0.4132








