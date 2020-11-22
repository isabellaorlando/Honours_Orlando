#==========================================================
# Script for combining antisaccade df and as amplitude df - analysis
#==========================================================
# Orlando, O'Callaghan 2020

# Working directory is filepath of this script -------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set directories IO -------------------------------------------------------------------

dataDir_as_amp <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/AntiSaccade/AS_saccade")
resultsDir_AS <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/AntiSaccade/Results")

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
library(afex)
library(afex)
library(devtools)


# Read in data -------------------------------------------------------------------
as_file <- read.csv("final_as_test.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(as_file)

as_amp_file <- read.csv("Data_AS_A.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(as_amp_file)

# Organise data frame -------------------------------------------------------
as_amp_file <- subset(as_amp_file, select = -c(X))
as_file <- subset(as_file, select = -c(X))


as_amp_file <- as_amp_file[c(1:5, 8, 9:14, 23, 27, 42:52, 215:219)]

#create IDsac2 column for amp file
cols12 <- c('ID_trial', 'CURRENT_SAC_INDEX')

as_amp_file$ID_sac <- apply(as_amp_file[ , cols12 ] , 1 , paste , collapse = "_")

as_amp_file$ID_sac <- as.factor(as_amp_file$ID_sac)


#Merge data frames
df_as_fin <- left_join(as_file, as_amp_file, by = "ID_sac")

#write.csv(df_as_fin, "df_as_combined.csv")


df_as_fin <- df_as_fin[c(1:40, 45, 47, 50:65)]
df_as_fin <- df_as_fin[c(1:21, 27:48, 57:58)]
df_as_fin <- df_as_fin[c(1:36, 38:45)]


df_as_fin_test <- df_as_fin %>% group_by(subject_visit) %>% summarise(length(time))
df_as_fin_test2 <- df_as_fin %>% group_by(subject_visit) %>% summarise(length(sample))

#cn003 latency / 4

df_as_fin2 <- df_as_fin

df_as_fin2 <- rename(df_as_fin2, subject = 'subject.x')
df_as_fin2 <- rename(df_as_fin2, visit = 'visit.x')
df_as_fin2 <- rename(df_as_fin2, trial = 'TRIAL_INDEX.x')
df_as_fin2 <- rename(df_as_fin2, ID_trial = 'ID_trial.x')
df_as_fin2 <- rename(df_as_fin2, Random_Delay = 'Random_Delay.x')
df_as_fin2 <- rename(df_as_fin2, direction = 'direction.x')
df_as_fin2 <- rename(df_as_fin2, t_x = 't_x.x')
df_as_fin2 <- rename(df_as_fin2, t_y = 't_y.x')




#-------------------------------------------------------
#find amplitude of each saccade - thresholds
#-------------------------------------------------------


#saccades that need to be removed

reduced_amp_AS <- df_as_fin2 %>% filter(CURRENT_SAC_AMPLITUDE < 1.5)
reduced_amp_AS2 <- df_as_fin2 %>% filter(CURRENT_SAC_AMPLITUDE > 10)


#------------------------------------------------------------------------------------
#create new column - ID_trial and saccade number

cols10 <- c('ID_trial', 'saccade_number')

reduced_amp_AS$ID_sac_number <- apply(reduced_amp_AS[ , cols10 ] , 1 , paste , collapse = "_" )

reduced_amp_AS$ID_sac_number <- as.factor(reduced_amp_AS$ID_sac_number)


reduced_amp_AS2$ID_sac_number <- apply(reduced_amp_AS2[ , cols10 ] , 1 , paste , collapse = "_" )

reduced_amp_AS2$ID_sac_number <- as.factor(reduced_amp_AS2$ID_sac_number)


df_as_fin2$ID_sac_number <- apply(df_as_fin2[ , cols10 ] , 1 , paste , collapse = "_" )

df_as_fin2$ID_sac_number <- as.factor(df_as_fin2$ID_sac_number)

#------------------------------------------------------------------------------------


#remove the saccades with <1.5 and >10 amplitude from the df
remove <- unique(reduced_amp_AS$ID_sac_number)
remove2 <- unique(reduced_amp_AS2$ID_sac_number)


#list of the saccades that are <1.5 amplitude 
remove <- as.data.frame(remove)
remove2 <- as.data.frame(remove2)

#rename
remove <- rename(remove, ID_sac_number = remove)
remove2 <- rename(remove2, ID_sac_number = remove2)


#df_as_fin2_test <- df_as_fin2 %>% group_by(subject_visit) %>% summarise(length(sample))


#df with saccades with amplitude <1.5 removed
edit_amp_as <- df_as_fin2[!df_as_fin2$ID_sac_number %in% remove$ID_sac_number,]
edit_amp_as <- edit_amp_as[!edit_amp_as$ID_sac_number %in% remove2$ID_sac_number,]



#------------------------------------------------------------------------------------
#rearrange saccade_number


AS_FIN <- edit_amp_as %>%
  dplyr::group_by(ID_trial) %>%
  mutate(saccade_number2 = with(rle(saccade), rep(seq_along(lengths), lengths)))

#cn005 has only 1,700 samples! remove
AS_FIN <- AS_FIN %>%
  dplyr::filter(subject != 'cn005')

AS_FIN <- AS_FIN %>%
  dplyr::filter(ID_trial != 'pd010_1_18' & ID_trial != 'pd010_1_19' & ID_trial != 'pd010_1_20'
                & ID_trial != 'pd010_1_25')

#------------------------------------------------------------------------------------


#sanity check
puphist <- ggplot(AS_FIN, aes(x = CURRENT_SAC_AMPLITUDE)) + 
  geom_histogram(aes(y = ..count..), colour = "green", binwidth = 0.5) + 
  xlab("Pupil Size") + ylab("Count") + 
  theme_bw() 

puphist


#test length of each 
AS_FIN_test <- AS_FIN %>% group_by(subject_visit) %>% summarise(length(sample))


#average sample length = 10,000 - 30,000

#cn003 has 100,000 samples!

#pd020_2 has 7700 samples

#cn011 has weird results



#=============================================================================================
#CORRECT DIRECTION
#=============================================================================================

as_correct <- AS_FIN
as_correct$direction <- str_replace(as_correct$direction , "Left", "LEFT")
as_correct$direction <- str_replace(as_correct$direction , "Right", "RIGHT")

as_correct <- as_correct %>% mutate(correct = ifelse(direction == CURRENT_SAC_DIRECTION, "0", "1"))

#------------------------------------------------------------------------------------
#calculate proportion or 1 (correct) and 0 (incorrect)

as_correct_test <- as_correct %>% dplyr::filter(saccade_number2 == "2")

#as_correct_test5 <- count(as_correct_test2, correct==1)
#as_correct_test5 <- rename(as_correct_test5, 'correct' = 'correct == 1')
#1354 rows total 800 rows wrong


as_correct_percent <- as_correct_test %>%
  group_by(ID_trial) %>%
  summarise(correct_sum = sum(correct==1))

as_correct_percent <- as_correct_percent %>% mutate(correct = ifelse(correct_sum != 0, "1", "0"))

#replace NA with 0
as_correct_1 <- as_correct_percent %>% dplyr::mutate(correct = replace_na(correct, 0))


#------------------------------------------------------------------------------------
#Looking at the correct proportion of 2nd saccade

as_correct_test2 <- as_correct %>% dplyr::filter(saccade_number2 == "4")

as_correct_2 <- as_correct_test2 %>%
  group_by(ID_trial) %>%
  summarise(correct_sum2 = sum(correct==1))

as_correct_2 <- as_correct_2 %>% mutate(correct2 = ifelse(correct_sum2 != 0, "1", "0"))

#replace NA with 0
as_correct_2 <- as_correct_2 %>% dplyr::mutate(correct2 = replace_na(correct2, 0))





#=============================================================================================
#Joining 1st and 2nd saccades

as_correct_join <- left_join(as_correct_1, as_correct_2, by = "ID_trial")
as_correct_join <- as_correct_join %>% dplyr::mutate(correct2 = replace_na(correct2, 0))



cols20 <- c('correct', 'correct2')

as_correct_join$both <- apply(as_correct_join[ , cols20 ] , 1 , paste , collapse = "_" )

as_correct_join$both <- as.factor(as_correct_join$both)


as_correct_join <- as_correct_join %>% mutate(corrective = ifelse(both == "0_1", "1", "0"))
as_correct_join <- as_correct_join %>% mutate(no_correct = ifelse(both == "0_0", "1", "0"))


#write.csv(as_correct_join, 'as_correct_df.csv')

as_correct_join2 <- as_correct_join %>% 
  separate(ID_trial, into = c("subject", "visit", "trial"), 
           sep="_")

cols <- c('subject', 'visit')

as_correct_join2$subject_visit <- apply(as_correct_join2[ , cols ] , 1 , paste , collapse = "_" )

as_correct_join2$subject_visit <- as.factor(as_correct_join2$subject_visit)



as_correct_join2 <- as_correct_join2 %>% group_by(subject_visit) %>% mutate(sample = row_number())

#write.csv(as_correct_join2, 'as_correct_join2.csv')

as_correct_join22 <- as_correct_join2 %>% group_by(subject_visit) %>% summarise(length(sample))



as_correct_join_sum <- as_correct_join2 %>%
  group_by(subject_visit) %>%
  summarise(corrective_sum = sum(corrective==1))

as_correct_join_sum2 <- as_correct_join2 %>%
  group_by(subject_visit) %>%
  summarise(correct_1st = sum(correct==1))

as_correct_join_sum3 <- as_correct_join2 %>%
  group_by(subject_visit) %>%
  summarise(correct_2nd = sum(correct2==1))

as_correct_join_sum4 <- as_correct_join2 %>%
  group_by(subject_visit) %>%
  summarise(no_correct = sum(no_correct==1))


AS_sums <- left_join(as_correct_join_sum2, as_correct_join_sum3, by = "subject_visit")

AS_sums <- left_join(AS_sums, as_correct_join_sum, by = "subject_visit")

AS_sums <- left_join(AS_sums, as_correct_join_sum4, by = "subject_visit")



#total error rate of 1st saccade
#total number of number of trials

AS_sums <- left_join(AS_sums, as_correct_join22, by = "subject_visit")

AS_sums <- rename(AS_sums, total_trials = 'length(sample)')


AS_sums <- AS_sums %>% group_by(subject_visit) %>% mutate(total_error = total_trials - correct_1st) 

AS_sums <- AS_sums %>% group_by(subject_visit) %>% mutate(error_1st = total_error/total_trials) 
AS_sums <- AS_sums %>% group_by(subject_visit) %>% mutate(error_1st = error_1st*100) 


#total error rate of no saccades (0 1st and 0 2nd - no corrective)
AS_sums <- AS_sums %>% group_by(subject_visit) %>% mutate(no_corrective = no_correct/total_trials) 
AS_sums <- AS_sums %>% group_by(subject_visit) %>% mutate(no_corrective = no_corrective*100) 


#write.csv(AS_sums, 'AS_sum.csv')

#=============================================================================================

#attach conditions 
AS_sum2 <- read.csv("AS_sum2.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(AS_sum2)

AS_sum2 <- subset(AS_sum2, select = -c(X))


#analyse for conditional singificances 
AS_sum2$condition <- as.factor(AS_sum2$condition)

AS_sum2$condition <- str_replace(AS_sum2$condition, "control", "Control")
AS_sum2$condition <- str_replace(AS_sum2$condition, "placebo", "Placebo")
AS_sum2$condition <- str_replace(AS_sum2$condition, "atomoxetine", "Atomoxetine")

AS_sum2$condition <- as.factor(AS_sum2$condition)

levels(AS_sum2$condition)
AS_sum2$condition <- factor(AS_sum2$condition,levels(AS_sum2$condition)[c(2,3,1)])
levels(AS_sum2$condition)

#create a column total_trials - correct 1st = trials_incorrect
#corrective_percent = corrective_sum/ total_incorrect *100

AS_sum2 <- AS_sum2 %>% group_by(subject_visit) %>% mutate(trials_incorrect = total_trials - correct_1st) 
AS_sum2 <- AS_sum2 %>% group_by(subject_visit) %>% mutate(corrective_percent = corrective_sum/trials_incorrect) 
AS_sum2 <- AS_sum2 %>% group_by(subject_visit) %>% mutate(corrective_percent = corrective_percent*100) 


#AS_sum2 <- AS_sum2 %>% group_by(subject_visit) %>% mutate(total_corrective = corrective_sum/total_trials) 
#AS_sum2 <- AS_sum2 %>% group_by(subject_visit) %>% mutate(total_corrective = total_corrective*100) 



AS_sum2 <- AS_sum2 %>% 
  separate(subject_visit, into = c("subject", "visit"), 
           sep="_")


AS_sum2_PD <- AS_sum2 %>%
  filter(str_detect(subject, "pd"))


error_rat_plot <- boxplot(error_1st ~ condition, data = AS_sum2, border=(c("#f3a953", "#3e64ff", "#ff5151")), main = 'Error rate (%)')

error_rat_plot + xlab("Condition")

# Paired plots -------------------------------------------------------------------

p_error <- ggpaired(AS_sum2_PD, x = "condition", y = "error_1st",
                color = "condition", line.color = "gray", line.size = 0.4, id = "subject", 
                xlab = FALSE, ylab = "", bxp.errorbar = TRUE, palette = c("#3e64ff", "#ff5151")) +
  theme(legend.position="none")

p_error
# three group plots -------------------------------------------------------------------  

p_error2 <-  ggboxplot(AS_sum2, x = "condition", y = "error_1st",
                  color = "condition", line.color = "gray", line.size = 0.4,
                  xlab = FALSE, ylab = "Mean error rate (%)", bxp.errorbar = TRUE,
                  palette = c("#f3a953", "#3e64ff", "#ff5151"), add = "jitter") + 
  theme(legend.position="none") 

p_error2

ggarrange(p_error2, p_error, ncol=2, nrow=1)


boxplot(corrective_percent ~ condition, data = AS_sum2,  main = 'Corrective AS')
boxplot(no_corrective ~ condition, data = AS_sum2,  main = 'No corrective AS')


#=============================================================================================
#corrective saccade analysis
#=============================================================================================


as_corrective <- group_by(AS_sum2, condition) %>%
  summarise(
    count = n(),
    mean = mean(corrective_percent, na.rm = TRUE),
    sd = sd(corrective_percent, na.rm = TRUE)
  )

as_corrective

#condition        count  mean    sd
#1 Control        25    54.0  31.0
#2 Placebo        18    56.7  25.0
#3 Atomoxetine    18    57.1  29.0


#-----------------------------------------------------------------
#independent t-test
#-----------------------------------------------------------------
AS_sum_control <- subset(AS_sum2, condition!="Atomoxetine")


res.ftest <- var.test(corrective_percent ~ condition, data = AS_sum_control)
res.ftest

#F = 1.5395, num df = 24, denom df = 17, p-value = 0.3628


t_test_PvC <- t.test(corrective_percent ~ condition, data = AS_sum_control, var.equal = TRUE)
t_test_PvC

#t = -0.30625, df = 41, p-value = 0.761

#-----------------------------------------------------------------
#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
AS_sum_PD <- subset(AS_sum2, condition!="Control")

diff <- with(AS_sum_PD, 
             corrective_percent[condition == "Placebo"] - corrective_percent[condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.95902, p-value = 0.5829 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(corrective_percent ~ condition, data = AS_sum_PD, paired = TRUE)
t_test_PvA

#t = -0.10699, df = 17, p-value = 0.9161




#=============================================================================================
#error rate in 1st saccade analysis
#=============================================================================================

error_rate <-group_by(AS_sum2, condition) %>%
  summarise(
    count = n(),
    mean = mean(error_1st, na.rm = TRUE),
    sd = sd(error_1st, na.rm = TRUE)
  )

error_rate
#  condition   count  mean    sd
#1 Control        25  47.1  21.8
#2 Placebo        18  53.8  24.3
#3 Atomoxetine    18  65.3  19.3


#-----------------------------------------------------------------
#independent t-test
#-----------------------------------------------------------------
AS_sum_control <- subset(AS_sum2, condition!="Atomoxetine")


res.ftest <- var.test(error_1st ~ condition, data = AS_sum_control)
res.ftest

#F = 0.80686, num df = 24, denom df = 17, p-value = 0.6164


t_test_PvC <- t.test(error_1st ~ condition, data = AS_sum_control, var.equal = TRUE)
t_test_PvC

#t = -0.95134, df = 41, p-value = 0.347

#-----------------------------------------------------------------
#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
AS_sum_PD <- subset(AS_sum2, condition!="Control")

diff <- with(AS_sum_PD, 
             error_1st[condition == "Placebo"] - error_1st[condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.97727, p-value = 0.9176 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(error_1st ~ condition, data = AS_sum_PD, paired = TRUE)
t_test_PvA

#t = -2.6444, df = 17, p-value = 0.01704  !!!! signif



#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------

# create a vector of the p values:
p.value_er <- c(0.347, 0.01704)
# run this:
p.adjust(p.value_er, method = "holm")
# you get this result:
# 0.34700 0.03408


#=============================================================================================
#no correct saccade analysis
#=============================================================================================

nocorrect <- group_by(AS_sum2, condition) %>%
  summarise(
    count = n(),
    mean = mean(no_correct_sac, na.rm = TRUE),
    sd = sd(no_correct_sac, na.rm = TRUE)
  )

nocorrect
#  condition   count  mean    sd
#1 Control        25  22.6  18.2
#2 Placebo        18  23.0  17.3
#3 Atomoxetine    18  27.6  20.7


#-----------------------------------------------------------------
#independent t-test
#-----------------------------------------------------------------
AS_sum_control <- subset(AS_sum2, condition!="Atomoxetine")


res.ftest <- var.test(no_correct_sac ~ condition, data = AS_sum_control)
res.ftest

#F = 1.1053, num df = 24, denom df = 17, p-value = 0.846


t_test_PvC <- t.test(no_correct_sac ~ condition, data = AS_sum_control, var.equal = TRUE)
t_test_PvC

#t = -0.076006, df = 41, p-value = 0.9398

#-----------------------------------------------------------------
#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
AS_sum_PD <- subset(AS_sum2, condition!="Control")

diff <- with(AS_sum_PD, 
             no_correct_sac[condition == "Placebo"] - no_correct_sac[condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.9643, p-value = 0.6862 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(no_correct_sac ~ condition, data = AS_sum_PD, paired = TRUE)
t_test_PvA

#t = -1.6265, df = 17, p-value = 0.1222

#mean

bar_nosac2 <- ggplot(nocorrect, aes(x=condition, y=mean)) + geom_bar(stat='identity')
bar_nosac2 + xlab("") + ylab("Mean saccade incorrect 1st and 2nd (%)")

bar_error1st2 <- ggplot(error_rate, aes(x=condition, y=mean)) + geom_bar(stat='identity')
bar_error1st2 + xlab("") + ylab("Mean error rate in 1st saccade (%)")


bar_corrective2 <- ggplot(as_corrective, aes(x=condition, y=mean)) + geom_bar(stat='identity')
bar_corrective2 + xlab("") + ylab("Mean corrective saccades (%)")








#=============================================================================================
#LATENCY
#=============================================================================================

as_latency <- AS_FIN %>% filter(saccade_number2 == "1")

#group_by (ID_trial) calculate distance to 1st saccade 
#summarise number of samples
as_latency_test <- as_latency %>% group_by(ID_trial) %>% summarise(length(sample))

as_latency_test <- rename(as_latency_test, latency = 'length(sample)')

#since sampling is at 500Hz, there is a sample every 2ms therefore need to double the length
as_latency_test$latency <- as_latency_test$latency*2


#create subject_visit column
as_latency_test <- as_latency_test %>% 
  separate(ID_trial, into = c("subject", "visit", "trial"), 
           sep="_")

cols <- c('subject', 'visit')

as_latency_test$subject_visit <- apply(as_latency_test[ , cols ] , 1 , paste , collapse = "_" )

as_latency_test$subject_visit <- as.factor(as_latency_test$subject_visit)

#write.csv(as_latency_test2, 'as_latency_test2.csv')

as_latency_test <- as_latency_test %>% mutate(latency_2 = ifelse(subject == 'cn003', latency/4, latency))
as_latency_test <- as_latency_test %>% mutate(latency_3 = ifelse(subject == 'cn009', latency/2, latency_2))


as_latency_out <- as_latency_test %>%
  group_by(subject_visit) %>%
  filter(between(latency_3, mean(latency_3, na.rm=TRUE) - (2.5 * sd(latency_3, na.rm=TRUE)), 
                 mean(latency_3, na.rm=TRUE) + (2.5 * sd(latency_3, na.rm=TRUE))))

#remove 53 trials


#test to see how many saccades are anticipatory
as_latency_out2 <- as_latency_out %>% filter(latency_3 < 80)
#= 44 trials 

as_latency_out3 <- as_latency_out %>% filter(latency_3 > 2500)
#= 28 trials


#remove anticipatory saccades
as_latency_out <- as_latency_out %>% filter(latency_3 > 80)
as_latency_out <- as_latency_out %>% filter(latency_3 < 2500)
#still 1332 trials

#sumamrise mean for each subject_visit
as_latency_mean <- as_latency_out %>% 
  group_by(subject_visit) %>%
  summarise(latency_mean = mean(latency_3))


#write.csv(as_latency_mean, 'as_latency_mean2.csv')


#=============================================================================================
#Analysis - latency
#=============================================================================================


#attach conditions and divide cn003 by 4
as_lat <- read.csv("as_latency_mean2.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(as_lat)

as_lat <- subset(as_lat, select = -c(X))


#analyse for conditional singificances 
as_lat$condition <- as.factor(as_lat$condition)

as_lat <- as_lat %>% 
  separate(subject_visit, into = c("subject", "visit"), 
           sep="_")

as_lat$condition <- str_replace(as_lat$condition, "control", "Control")
as_lat$condition <- str_replace(as_lat$condition, "placebo", "Placebo")
as_lat$condition <- str_replace(as_lat$condition, "atomoxetine", "Atomoxetine")

as_lat$condition <- as.factor(as_lat$condition)

levels(as_lat$condition)
as_lat$condition <- factor(as_lat$condition,levels(as_lat$condition)[c(2,3,1)])
levels(as_lat$condition)

#unique(as_lat2$subject)

#remove outliers cn009 and cn011 or divide by 2?

as_lat_PD <- as_lat %>% filter(condition!="Control")  %>% 
  droplevels()

boxplot(latency_mean ~ condition, data = as_lat,  main = 'Mean Latency')


# Paired plots -------------------------------------------------------------------

p30 <- ggpaired(as_lat_PD, x = "condition", y = "latency_mean",
                color = "condition", line.color = "gray", line.size = 0.4, id = "subject", 
                xlab = FALSE, ylab = "", bxp.errorbar = TRUE, palette = c("#3e64ff", "#ff5151")) +
  theme(legend.position="none")

p30
# three group plots -------------------------------------------------------------------  

p31 <-  ggboxplot(as_lat, x = "condition", y = "latency_mean",
                  color = "condition", line.color = "gray", line.size = 0.4,
                  xlab = FALSE, ylab = "Mean latency (ms)", bxp.errorbar = TRUE,
                  palette = c("#f3a953", "#3e64ff", "#ff5151"), add = "jitter") + 
  theme(legend.position="none") 

p31

ggarrange(p31, p30, ncol=2, nrow=1)


#-----------------------------------------------------------------
# ANOVA, sd and variance
#-----------------------------------------------------------------

group_by(as_lat, condition) %>%
  summarise(
    count = n(),
    mean = mean(latency_mean, na.rm = TRUE),
    sd = sd(latency_mean, na.rm = TRUE)
  )

#condition   count  mean    sd
#Control        25  369. 172. 
#2 Placebo        18  298. 108. 
#3 Atomoxetine    18  284.  93.7

res.aov <- aov(latency_mean ~ condition, data = as_lat)
# Summary of the analysis
summary(res.aov)

#Tukey multiple comparisons of means
#95% family-wise confidence level
TukeyHSD(res.aov)


#-----------------------------------------------------------------
#independent t-test
#-----------------------------------------------------------------
as_lat_control <- as_lat %>% filter(condition!="Atomoxetine")  %>% 
  droplevels()



res.ftest <- var.test(latency_mean ~ condition, data = as_lat_control)
res.ftest

#F = 2.5577, num df = 24, denom df = 17, p-value = 0.05019

#without outliers
#F = 1.559, num df = 22, denom df = 17, p-value = 0.3536

t_test_PvC <- t.test(latency_mean ~ condition, data = as_lat_control, var.equal = TRUE)
t_test_PvC

#t = 1.5447, df = 41, p-value = 0.1301


#-----------------------------------------------------------------
#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
diff <- with(as_lat_PD, 
             latency_mean[condition == "Placebo"] - latency_mean[condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

# W = 0.87905, p-value = 0.02517 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(latency_mean ~ condition, data = as_lat_PD, paired = TRUE)
t_test_PvA

#t = 1.065, df = 17, p-value = 0.3018



# Unpaired test
perm.t.test(latency_mean ~ condition, nperm=5000, data = as_lat_control)
# t = 1.5447, p-value = 0.1256


# Paired test
perm.t.test(latency_mean ~ condition, paired=TRUE,nperm=5000, data = as_lat_PD,)
#t = 1.065, p-value = 0.3523



#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------

# create a vector of the p values:
p.value_la <- c(0.1256, 0.3523)
# run this:
p.adjust(p.value_la, method = "holm")
# you get this result:
# 0.2512 0.3523




#=============================================================================================
#PEAK VELOCITY
#=============================================================================================

peakv_as <- AS_FIN %>% dplyr::filter(saccade_number2 == "2")

peakv_as <- peakv_as %>% 
  group_by(ID_trial) %>%
  summarise(peakv = mean(CURRENT_SAC_PEAK_VELOCITY))

peakv_as <- peakv_as %>% filter(peakv != "NA")

# #remove outliers
# peakv_as <- peakv_as %>% filter(ID_trial != "pd010_2_ 6" & ID_trial != "pd010_2_ 9" 
#                                 & ID_trial != "pd010_2_19" & ID_trial != "pd010_2_21")


peakv_as_test <- peakv_as %>% 
  separate(ID_trial, into = c("subject", "visit", "trial"), 
           sep="_")

cols <- c('subject', 'visit')

peakv_as_test$subject_visit <- apply(peakv_as_test[ , cols ] , 1 , paste , collapse = "_" )

peakv_as_test$subject_visit <- as.factor(peakv_as_test$subject_visit)


peakv_as_out <- peakv_as_test %>%
  group_by(subject_visit) %>%
  filter(between(peakv, mean(peakv, na.rm=TRUE) - (2.5 * sd(peakv, na.rm=TRUE)), 
                 mean(peakv, na.rm=TRUE) + (2.5 * sd(peakv, na.rm=TRUE))))
#10 trials removed

#sumamrise mean for each subject_visit
peakv_as_mean <- peakv_as_out %>% 
  group_by(subject_visit) %>%
  summarise(peak_velocity = mean(peakv))


#write_csv(peakv_as_mean, 'peakv_as_mean.csv')

#=============================================================================================
#Analysis - peak velocity
#=============================================================================================


#attach conditions 
as_pv <- read.csv("peakv_as_mean2.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(as_pv)

as_pv <- subset(as_pv, select = -c(X))


#analyse for conditional singificances 
as_pv$condition <- as.factor(as_pv$condition)

as_pv <- as_pv %>% 
  separate(subject_visit, into = c("subject", "visit"), 
           sep="_")

as_pv$condition <- str_replace(as_pv$condition, "control", "Control")
as_pv$condition <- str_replace(as_pv$condition, "placebo", "Placebo")
as_pv$condition <- str_replace(as_pv$condition, "atomoxetine", "Atomoxetine")

as_pv$condition <- as.factor(as_pv$condition)

levels(as_pv$condition)
as_pv$condition <- factor(as_pv$condition,levels(as_pv$condition)[c(2,3,1)])
levels(as_pv$condition)

as_pv_PD <- as_pv %>%
  filter(str_detect(subject, "pd"))

boxplot(peak_velocity ~ condition, data = as_pv,  main = 'Mean Peak Velocity')


# Paired plots -------------------------------------------------------------------

p32 <- ggpaired(as_pv_PD, x = "condition", y = "peak_velocity",
                color = "condition", line.color = "gray", line.size = 0.4, id = "subject", 
                xlab = FALSE, ylab = "", palette = c("#3e64ff", "#ff5151")) +
  theme(legend.position="none")

p32
# three group plots -------------------------------------------------------------------  

p33 <-  ggboxplot(as_pv, x = "condition", y = "peak_velocity",
                  color = "condition", line.color = "gray", line.size = 0.4,
                  xlab = FALSE, ylab = "Peak velocity",
                  palette = c("#f3a953", "#3e64ff", "#ff5151"), add = "jitter") + 
  theme(legend.position="none") 

p33

ggarrange(p33, p32, ncol=2, nrow=1)


#-----------------------------------------------------------------
# ANOVA, sd and variance
#-----------------------------------------------------------------

group_by(as_pv, condition) %>%
  summarise(
    count = n(),
    mean = mean(peak_velocity, na.rm = TRUE),
    sd = sd(peak_velocity, na.rm = TRUE)
  )

#condition       count  mean    sd
# 1 Control        25  247.  41.6
# 2 Placebo        18  272.  71.2
# 3 Atomoxetine    18  241.  44.8

res.aov <- aov(peak_velocity ~ condition, data = as_pv)
# Summary of the analysis
summary(res.aov)

#Tukey multiple comparisons of means
#95% family-wise confidence level
TukeyHSD(res.aov)

# Atomoxetine-Placebo -31.307893 -73.65088 11.03509 0.1857991

#-----------------------------------------------------------------
#independent t-test
#-----------------------------------------------------------------
as_pv_control <- subset(as_pv, condition!="Atomoxetine")


res.ftest <- var.test(peak_velocity ~ condition, data = as_pv_control)
res.ftest

#F = 0.3409, num df = 24, denom df = 17, p-value = 0.0158

t_test_PvC <- t.test(peak_velocity ~ condition, data = as_pv_control, var.equal = TRUE)
t_test_PvC

#t = -1.4892, df = 41, p-value = 0.1441

#-----------------------------------------------------------------
#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
diff <- with(as_pv_PD, 
             peak_velocity[condition == "Placebo"] - peak_velocity[condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.9086, p-value = 0.08129 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(peak_velocity ~ condition, data = as_pv_PD, paired = TRUE)
t_test_PvA

#t = 2.7673, df = 17, p-value = 0.01318  !!!!! significant
# p 0.02636




#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------

# create a vector of the p values:
p.value_pv <- c(0.1441, 0.01318)
# run this:
p.adjust(p.value_pv, method = "holm")
# you get this result:
# 0.14410 0.02636


#=============================================================================================
#AVERAGE AMPLITUDE
#=============================================================================================

amp_as <- AS_FIN %>% dplyr::filter(saccade_number2 == "2")

amp_as <- amp_as %>% 
  group_by(ID_trial) %>%
  summarise(amplitude = mean(CURRENT_SAC_AMPLITUDE))

amp_as <- amp_as %>% filter(amplitude != "NA")


amp_as_test <- amp_as %>% 
  separate(ID_trial, into = c("subject", "visit", "trial"), 
           sep="_")

cols <- c('subject', 'visit')

amp_as_test$subject_visit <- apply(amp_as_test[ , cols ] , 1 , paste , collapse = "_" )

amp_as_test$subject_visit <- as.factor(amp_as_test$subject_visit)

amp_as_out <- amp_as_test %>%
  group_by(subject_visit) %>%
  filter(between(amplitude, mean(amplitude, na.rm=TRUE) - (2.5 * sd(amplitude, na.rm=TRUE)), 
                 mean(amplitude, na.rm=TRUE) + (2.5 * sd(amplitude, na.rm=TRUE))))

#7 trials removed

#sumamrise mean for each subject_visit
amp_as_mean <- amp_as_out %>% 
  group_by(subject_visit) %>%
  summarise(amp_mean = mean(amplitude))


#write_csv(amp_as_mean, 'amp_as_mean.csv')





#=============================================================================================
#Analysis - amplitude
#=============================================================================================


#attach conditions 
as_amplitude <- read.csv("amp_as_mean2.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(as_amplitude)

as_amplitude <- subset(as_amplitude, select = -c(X))


#analyse for conditional singificances 
as_amplitude$condition <- as.factor(as_amplitude$condition)

as_amplitude <- as_amplitude %>% 
  separate(subject_visit, into = c("subject", "visit"), 
           sep="_")

as_amplitude$condition <- str_replace(as_amplitude$condition, "control", "Control")
as_amplitude$condition <- str_replace(as_amplitude$condition, "placebo", "Placebo")
as_amplitude$condition <- str_replace(as_amplitude$condition, "atomoxetine", "Atomoxetine")

as_amplitude$condition <- as.factor(as_amplitude$condition)

levels(as_amplitude$condition)
as_amplitude$condition <- factor(as_amplitude$condition,levels(as_amplitude$condition)[c(2,3,1)])
levels(as_amplitude$condition)


as_amplitude_PD <- as_amplitude %>%
  filter(str_detect(subject, "pd"))

boxplot(amp_mean ~ condition, data = as_amplitude,  main = 'Mean Peak Velocity')


# Paired plots -------------------------------------------------------------------

p36 <- ggpaired(as_amplitude_PD, x = "condition", y = "amp_mean",
                color = "condition", line.color = "gray", line.size = 0.4, id = "subject", 
                xlab = FALSE, ylab = "", palette = c("#3e64ff", "#ff5151")) +
  theme(legend.position="none")

p36
# three group plots -------------------------------------------------------------------  

p37 <-  ggboxplot(as_amplitude, x = "condition", y = "amp_mean",
                  color = "condition", line.color = "gray", line.size = 0.4,
                  xlab = FALSE, ylab = "Mean amplitude (degrees)",
                  palette = c("#f3a953", "#3e64ff", "#ff5151"), add = "jitter") + 
  theme(legend.position="none") 

p37

ggarrange(p37, p36, ncol=2, nrow=1)




#-----------------------------------------------------------------
# ANOVA, sd and variance
#-----------------------------------------------------------------

group_by(as_amplitude, condition) %>%
  summarise(
    count = n(),
    mean = mean(amp_mean, na.rm = TRUE),
    sd = sd(amp_mean, na.rm = TRUE)
  )

# #with pd010
# #condition      count  mean    sd
# 1 Control        25  4.62 0.704
# 2 Placebo        18  4.60 0.601
# 3 Atomoxetine    18  4.33 0.558

res.aov <- aov(amp_mean ~ condition, data = as_amplitude)
# Summary of the analysis
summary(res.aov)

#Tukey multiple comparisons of means
#95% family-wise confidence level
TukeyHSD(res.aov)


#-----------------------------------------------------------------
#independent t-test
#-----------------------------------------------------------------
as_amplitude_con <- subset(as_amplitude, condition!="Atomoxetine")


res.ftest <- var.test(amp_mean ~ condition, data = as_amplitude_con)
res.ftest

#F = 1.3712, num df = 24, denom df = 17, p-value = 0.5077


t_test_PvC <- t.test(amp_mean ~ condition, data = as_amplitude_con, var.equal = TRUE)
t_test_PvC

#t = 0.083662, df = 41, p-value = 0.9337

#-----------------------------------------------------------------
#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
diff <- with(as_amplitude_PD, 
             amp_mean[condition == "Placebo"] - amp_mean[condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.9394, p-value = 0.283 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(amp_mean ~ condition, data = as_amplitude_PD, paired = TRUE)
t_test_PvA

#t = 1.9986, df = 17, p-value = 0.0619   * nearly signif




#calculate proportion of saccades <1.5 degrees
#calculate corectness - direction of saccade opposite to target presented

#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------

# create a vector of the p values:
p.value_am <- c(0.9337, 0.0619)
# run this:
p.adjust(p.value_am, method = "holm")
# you get this result:
# 0.9337 0.1238







#=============================================================================================
#3x2 analysis -  based on correct/incorrect
#=============================================================================================

#join based on ID_trial

as_lat_correct <- as_latency_out

as_cor <- as_correct_join2

as_pv_correct <- peakv_as_out
  
as_amp_correct <- amp_as_out

#add similar factor to join by
cols11 <- c('subject', 'visit', 'trial')

as_lat_correct$ID_trial <- apply(as_lat_correct[ , cols11 ] , 1 , paste , collapse = "_" )
as_lat_correct$ID_trial <- as.factor(as_lat_correct$ID_trial)

as_cor$ID_trial <- apply(as_cor[ , cols11 ] , 1 , paste , collapse = "_" )
as_cor$ID_trial <- as.factor(as_cor$ID_trial)

as_pv_correct$ID_trial <- apply(as_pv_correct[ , cols11 ] , 1 , paste , collapse = "_" )
as_pv_correct$ID_trial <- as.factor(as_pv_correct$ID_trial)

as_amp_correct$ID_trial <- apply(as_amp_correct[ , cols11 ] , 1 , paste , collapse = "_" )
as_amp_correct$ID_trial <- as.factor(as_amp_correct$ID_trial)

#join
as_correct_analysis <- left_join(as_cor, as_lat_correct, by = "ID_trial")
as_correct_analysis <- left_join(as_correct_analysis, as_pv_correct, by = "ID_trial")
as_correct_analysis <- left_join(as_correct_analysis, as_amp_correct, by = "ID_trial")




#remove repeated columns

as_correct_analysis <- as_correct_analysis[c(1:13, 20, 24, 29)]



as_correct_analysis <- rename(as_correct_analysis, subject = 'subject.x')
as_correct_analysis <- rename(as_correct_analysis, visit = 'visit.x')
as_correct_analysis <- rename(as_correct_analysis, trial = 'trial.x')
as_correct_analysis <- rename(as_correct_analysis, subject_visit = 'subject_visit.x')
as_correct_analysis <- rename(as_correct_analysis, latency = 'latency_3')



totallength_test <- as_correct_analysis %>% group_by(subject_visit) %>% summarise(length(trial))

as_correct_analysis <- left_join(as_correct_analysis, totallength_test, by = "subject_visit")
as_correct_analysis <- rename(as_correct_analysis, trial_total = 'length(trial)')




#correct saccades
correct_list <- as_correct_analysis %>%filter(str_detect(correct, "1"))
#599 trials

correct_list <- correct_list %>% dplyr::mutate(latency = replace_na(latency, 'x'))
correct_list <- correct_list %>% filter(latency != 'x')
#removed 33 trials

correct_list <- correct_list %>% dplyr::mutate(amplitude = replace_na(amplitude, 'x'))
correct_list <- correct_list %>% filter(amplitude != 'x')
#4 trials removed

correct_list_test <- correct_list %>% group_by(subject_visit) %>% summarise(length(trial))
correct_list_test <- left_join(correct_list, correct_list_test, by = "subject_visit")
correct_list <- rename(correct_list_test, trial_total_correct = 'length(trial)')



#incorrect saccades
incorrect_list <- as_correct_analysis %>%filter(str_detect(correct, "0"))
#748 trials

incorrect_list <- incorrect_list %>% dplyr::mutate(latency = replace_na(latency, 'x'))
incorrect_list <- incorrect_list %>% filter(latency != 'x')
#removed 26 trials

incorrect_list <- incorrect_list %>% dplyr::mutate(amplitude = replace_na(amplitude, 'x'))
incorrect_list <- incorrect_list %>% filter(amplitude != 'x')
#5 trials removed


incorrect_list <- incorrect_list %>% dplyr::mutate(peakv = replace_na(peakv, 'x'))
incorrect_list <- incorrect_list %>% filter(peakv != 'x')
#5 trials removed


incorrect_list_test <- incorrect_list %>% group_by(subject_visit) %>% summarise(length(trial))
incorrect_list_test <- left_join(incorrect_list, incorrect_list_test, by = "subject_visit")
incorrect_list <- rename(incorrect_list_test, trial_total_incorrect = 'length(trial)')


correct_list$latency <- as.numeric(correct_list$latency)
correct_list$amplitude <- as.numeric(correct_list$amplitude)
correct_list$peakv <- as.numeric(correct_list$peakv)


#find average latency and duration for subject_visit
correct_list_mean <- correct_list %>% 
  group_by(subject_visit) %>%
  summarise(latency_mean = mean(latency))

correct_list_mean2 <- correct_list %>% 
  group_by(subject_visit) %>%
  summarise(peak_velocity_mean = mean(peakv))

correct_list_mean3 <- correct_list %>% 
  group_by(subject_visit) %>%
  summarise(amplitude_mean = mean(amplitude))

correct_mean <- left_join(correct_list_mean, correct_list_mean2, by = "subject_visit")
correct_mean <- left_join(correct_mean, correct_list_mean3, by = "subject_visit")



#write.csv(correct_mean, 'correct_as.csv')


incorrect_list$latency <- as.numeric(incorrect_list$latency)
incorrect_list$amplitude <- as.numeric(incorrect_list$amplitude)
incorrect_list$peakv <- as.numeric(incorrect_list$peakv)


incorrect_list_mean <- incorrect_list %>% 
  group_by(subject_visit) %>%
  summarise(latency_mean = mean(latency))

incorrect_list_mean2 <- incorrect_list %>% 
  group_by(subject_visit) %>%
  summarise(peak_velocity_mean = mean(peakv))

incorrect_list_mean3 <- incorrect_list %>% 
  group_by(subject_visit) %>%
  summarise(amplitude_mean = mean(amplitude))

incorrect_mean <- left_join(incorrect_list_mean, incorrect_list_mean2, by = "subject_visit")
incorrect_mean <- left_join(incorrect_mean, incorrect_list_mean3, by = "subject_visit")

#write.csv(incorrect_mean, 'incorrect_as.csv')




#------------------------------------------------------------------------
# ANALYSIS BASED ON CORRECT V INCORRECT - 3x2 ANOVA
#------------------------------------------------------------------------

#create new rows to identify if correct or incorrect - just using duration mean randomly here
correct_mean <- correct_mean %>% mutate(on_target = ifelse(latency_mean != 0, "on", "on"))

incorrect_mean <- incorrect_mean %>% mutate(on_target = ifelse(latency_mean != 0, "off", "off"))

correct_long <- rbind(correct_mean, incorrect_mean)


#long/vertical join
correct_long <- rbind(correct_mean, incorrect_mean)
write.csv(correct_long, 'correct_vertical.csv')
#divide cn003 latency by 4 and cn009 latency by 2
#attach conditions






#--------------------------------------------------------------------------------------------------------------
#start from here
correct_long <- read.csv("correct_vertical.csv", header = TRUE, quote="\"", 
                         stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(correct_long)
#divide cn003 latency by 4 and cn009 by 2

correct_long <- subset(correct_long, select = -c(X))

correct_long$on_target <- as.factor(correct_long$on_target)
correct_long$condition <- as.factor(correct_long$condition)
correct_long$group <- as.factor(correct_long$group)
correct_long$latency_mean <- as.numeric(correct_long$latency_mean)



levels(correct_long$condition)
correct_long$condition <- factor(correct_long$condition,levels(correct_long$condition)[c(2,3,1)])
levels(correct_long$condition)

correct_long <- rename(correct_long, Direction = 'on_target')
correct_long$Direction <- str_replace(correct_long$Direction , "on", "Correct")
correct_long$Direction <- str_replace(correct_long$Direction , "off", "Incorrect")

correct_sum <- correct_long %>%
  group_by(condition, Direction) %>%
  summarise( 
    n=n(),
    mean=mean(latency_mean),
    sd=sd(latency_mean)
  ) %>%
  mutate(se=sd/sqrt(n))  %>%
  mutate(ic=se * qt((1-0.05)/2 + .5, n-1))

#latency
plot_1 <- ggplot(correct_long, aes(x=condition, y=latency_mean, color=Direction, label=Direction)) +
  geom_boxplot(outlier.shape=NA)

plot_1



plot_1_1 <- ggplot(correct_long, aes(x=condition, y=latency_mean, color=Direction, label=Direction)) +
  geom_boxplot(outlier.shape=NA) +
  geom_point(pch = 20, position = position_jitterdodge()) +
  labs(x="Condition", y = "Mean latency (msec)")
  #geom_errorbar( aes(x=condition, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.5)

plot_1_1 + scale_color_manual(values=c("#fdcb6e", "#6c5ce7")) + theme_light()



#repeated measures/mixed design analysis 
res.aov2 <- aov(latency_mean ~ condition + on_target, data = correct_long)
summary(res.aov2)


#               Df  Sum Sq Mean Sq  F value   Pr(>F)    
# condition     2   98640   49320   3.128    0.0475 *  
# on_target     1  371753  371753  23.574   3.75e-06 ***
# Residuals   117 1845033   15770       

#The output includes the columns F value and Pr(>F) corresponding to the p-value of the test.

# Repeated measures ANOVA
aov1 <- afex::aov_ez(id = "subject_visit", dv = "latency_mean", within = "condition", 
                     between = "group", data = correct_long)


knitr::kable(nice(aov1))

#test from claire
LC_CNR <- read.csv("LC_CNR_new_ATO.csv",
                   header = TRUE, stringsAsFactors = TRUE, strip.white = TRUE,
                   na.strings = c("NA", "-", "?"))

# melt into long format
LC_CNR_long <- melt(LC_CNR, id=c("Group_1", "WBICNumber", "StudyID"),
                    variable.name="LC_segment",
                    value.name="CNR"
)

t.test(LC_CNR$Whole ~ LC_CNR$Group_1, alternative = "two.sided", var.equal = FALSE)

aov1 <- afex::aov_ez(id = "StudyID", dv = "CNR", within = "LC_segment", between = "Group_1", data = LC_CNR_long %>% dplyr::filter(LC_segment != "Whole"))
knitr::kable(nice(aov1))

# Main effect of segment
m1 <- emmeans(aov1, ~ LC_segment)
pairs(m1)

# Interaction
m2 <- emmeans(aov1, "Group_1", by = "LC_segment")
pairs(m2)











p35 <-  ggboxplot(as_dur, x = "condition", y = "duration_mean",
                  color = "condition", line.color = "gray", line.size = 0.4,
                  xlab = FALSE, ylab = "Mean duration (ms)",
                  palette = c("#f3a953", "#3e64ff", "#ff5151"), add = "jitter") + 
  theme(legend.position="none") 


#peak velocity
plot_2 <- ggplot(correct_long, aes(x=condition, y=peak_velocity_mean, color=on_target, label=on_target)) +
  geom_boxplot(outlier.shape=NA) +
  geom_point(pch = 20, position = position_jitterdodge())

plot_2


plot_2_1 <- ggplot(correct_long, aes(x=condition, y=peak_velocity_mean, color=on_target, label=on_target)) +
  geom_boxplot(outlier.shape=NA)

plot_2_1


#amplitude
plot_3 <- ggplot(correct_long, aes(x=condition, y=amplitude_mean, color=on_target, label=on_target)) +
  geom_boxplot(outlier.shape=NA) +
  geom_point(pch = 20, position = position_jitterdodge())

plot_3


plot_3_1 <- ggplot(correct_long, aes(x=condition, y=amplitude_mean, color=on_target, label=on_target)) +
  geom_boxplot(outlier.shape=NA)

plot_3_1


