#================================================
#Temporal derivative functions and analysis
#================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set directories -------------------------------------------------------------------
dataDir <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/BaselinePupil/ProcessedData")
resultsDir <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/BaselinePupil/Results")

# Load packages -------------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(stringr)
library(rstatix)
library(devtools)
library(afex)

# Read in data & organise -------------------------------------------------------------------
df_td <- read.csv("smooth_hann_linear_3.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(df_td)

df_td <- subset(df_td, select = -c(X))

#===========================================================================
# Temporal derivative
#===========================================================================
#Brink et al 2016 
# temporal derivative = average difference between each 2 consecutive samples within a window
# OR difference in baseline diameter between the first and last samples of the window

#range within each bin
tempdiff <- df_td %>% group_by(ID_bin) %>% mutate(range = diff(range(smoothed)))

#consecutive difference between each sample within each bin (with every first sample = 0)
tempdiff <- tempdiff %>% group_by(ID_bin) %>% 
  mutate(consec_diff = smoothed - lag(smoothed, default = first(smoothed)))

#average difference between each 2 consecutive samples within a bin
mean_tempdiff <- group_by(tempdiff, ID_bin) %>%
  summarise(
    count = n(),
    mean = mean(consec_diff, na.rm = TRUE),
    sd = sd(consec_diff, na.rm = TRUE)
  )

mean_tempdiff <- mean_tempdiff %>% 
  separate(ID_bin, into = c("subject", "visit", "bin"), 
           sep="_")

cols <- c('subject', 'visit')
mean_tempdiff$subject_visit<- apply(mean_tempdiff[ , cols ] , 1 , paste , collapse = "_" )
mean_tempdiff$subject_visit <- as.factor(mean_tempdiff$subject_visit)


mean_tempdiff <- mean_tempdiff %>% group_by(subject_visit) %>% mutate(sample = row_number())


#write.csv(mean_tempdiff, "mean_tempdiff.csv")

#=======================================================================================

mean_tempdiff_plot <- ggplot(mean_tempdiff, aes(sample, meandiff)) +
  geom_line() +
  xlab("Time (msec)") + ylab("Mean difference in pupil size") +
  theme_bw() +
  theme(panel.grid = element_blank())

pdf("~/Desktop/Atomoxetine/BaselinePupil/Results/mean_tempdiff_plot.pdf")
for(i in 1:30){
  print(mean_tempdiff_plot +
          facet_wrap_paginate(~ subject_visit, ncol = 1, nrow = 2, page = i))
}
dev.off()

#=======================================================================================


mean_tempdiff2 <- mean_tempdiff %>% 
  group_by(subject_visit) %>%
  summarise(mean = mean((mean)), cv = cv(mean))

mean_tempdiff2 <- rename(mean_tempdiff2, average = mean)

#write.csv(mean_tempdiff2, 'mean_tempdiff2.csv')

#===========================================================================
# Organising df - need to order by bin ???????
#===========================================================================
mean_tempdiff$bin <- str_pad(mean_tempdiff$bin, width=2, side="left", pad="0")

cols2 <- c('subject', 'visit', 'bin')

mean_tempdiff$ID <- apply(mean_tempdiff[ , cols2 ] , 1 , paste , collapse = "_" )

mean_tempdiff$ID <- as.factor(mean_tempdiff$ID)

mean_tempdiff$subject_visit <- apply(mean_tempdiff[ , cols ] , 1 , paste , collapse = "_" )

mean_tempdiff$subject_visit <- as.factor(mean_tempdiff$subject_visit)


mean_tempdiff <- mean_tempdiff  %>% group_by(subject_visit) %>% arrange(ID)

mean_tempdiff <- rename(mean_tempdiff, meandiff = mean)

dftempdiff <- mean_tempdiff[c(7, 5:6)]


#===========================================================================
#Load df from preprocessing script of mean BP per bin
#===========================================================================
df_pupil_td <- read.csv("df1_pupil.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(df_pupil_td)

df_pupil_td <- subset(df_pupil_td, select = -c(X))


#===========================================================================
#organise df in order to merge with mean tempdiff
#===========================================================================
df_pupil_td$Bin <- str_pad(df_pupil_td$Bin, width=2, side="left", pad="0")

df_pupil_td <- rename(df_pupil_td, bin = Bin)

cols4 <- c('subject_visit', 'bin')

df_pupil_td$ID <- apply(df_pupil_td[ , cols4 ] , 1 , paste , collapse = "_" )

df_pupil_td$ID <- as.factor(df_pupil_td$ID)


df_pupil_td <- df_pupil_td[c(6, 3:5)]


df_11 <- merge(df_pupil_td, dftempdiff)

write.csv(df_11, "meandifferences.csv")

#meandiff = average difference between data points within each bin 
#------------------------------------------------------------------------
#Match visit with demographic info for condition (eg) ato v placebo conditions) - did it manually in excel
#------------------------------------------------------------------------

tempdiff_df <- read.csv("meandifferences_edit.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(tempdiff_df)

tempdiff_df <- subset(tempdiff_df, select = -c(X))

#===========================================================================
#Mean BP plots against time
#===========================================================================

tempdiff_df <- tempdiff_df %>% 
  separate(ID, into = c("subject", "visit", "bin"), 
           sep="_")

tempdiff_df <- rename(tempdiff_df, pupil_mean = mean)

#make different dfs for control, placebo and atomoxetine
#------------------------------------------------------------------------

tempdiff_control <- subset(tempdiff_df, condition!="Atomoxetine" & condition!="Placebo")
tempdiff_placebo <- subset(tempdiff_df, condition!="Atomoxetine" & condition!="control")
tempdiff_atomoxetine <- subset(tempdiff_df, condition!="Placebo" & condition!="control")

#------------------------------------------------------------------------
#Average BP for each bin for each control 
#------------------------------------------------------------------------

mean_BP_control <- group_by(tempdiff_control, bin) %>%
  summarise(
    count = n(),
    mean = mean(pupil_mean, na.rm = TRUE),
    sd = sd(pupil_mean, na.rm = TRUE)
  )

mean_BP_control$mean <- as.factor(mean_BP_control$mean)
mean_BP_control$bin <- as.factor(mean_BP_control$bin)

#remove leading 0s
mean_BP_control$bin = str_remove(mean_BP_control$bin, "^0+")

mean_BP_control$mean=as.numeric(levels(mean_BP_control$mean))[mean_BP_control$mean]


mean_BP_control_plot <- ggplot(mean_BP_control, aes(x=bin, y=mean)) +
                                 geom_point() + ggtitle("Control") + xlab("Time (15sec bins)") + 
                                 ylab("Mean pupil size (arbitrary units)") + ylim(350, 600)

mean_BP_control_plot

#------------------------------------------------------------------------
#Average BP for each bin for each placebo 
#------------------------------------------------------------------------

mean_BP_placebo <- group_by(tempdiff_placebo, bin) %>%
  summarise(
    count = n(),
    mean = mean(pupil_mean, na.rm = TRUE),
    sd = sd(pupil_mean, na.rm = TRUE)
  )

mean_BP_placebo$mean <- as.factor(mean_BP_placebo$mean)
mean_BP_placebo$bin <- as.factor(mean_BP_placebo$bin)

#remove leading 0s
mean_BP_placebo$bin = str_remove(mean_BP_placebo$bin, "^0+")

mean_BP_placebo$mean=as.numeric(levels(mean_BP_placebo$mean))[mean_BP_placebo$mean]


mean_BP_placebo_plot <- ggplot(mean_BP_placebo, aes(x=bin, y=mean)) +
  geom_point() + ggtitle("Placebo") + xlab("Time (15sec bins)") + 
  ylab("Mean pupil size (arbitrary units)") + ylim(350, 600)

mean_BP_placebo_plot

#------------------------------------------------------------------------
#Average BP for each bin for each atomoxetine 
#------------------------------------------------------------------------

mean_BP_atomoxetine <- group_by(tempdiff_atomoxetine, bin) %>%
  summarise(
    count = n(),
    mean = mean(pupil_mean, na.rm = TRUE),
    sd = sd(pupil_mean, na.rm = TRUE)
  )

mean_BP_atomoxetine$mean <- as.factor(mean_BP_atomoxetine$mean)
mean_BP_atomoxetine$bin <- as.factor(mean_BP_atomoxetine$bin)

#remove leading 0s
mean_BP_atomoxetine$bin = str_remove(mean_BP_atomoxetine$bin, "^0+")

mean_BP_atomoxetine$mean=as.numeric(levels(mean_BP_atomoxetine$mean))[mean_BP_atomoxetine$mean]


mean_BP_atomoxetine_plot <- ggplot(mean_BP_atomoxetine, aes(x=bin, y=mean)) +
  geom_point() +
  geom_point(shape=21, color="black", fill="#69b3a2", size=2) + ggtitle("Atomoxetine") + xlab("Time (15sec bins)") + 
  ylab("Mean pupil size (arbitrary units)") + ylim(350, 600)

mean_BP_atomoxetine_plot


#------------------------------------------------------------------------

ggarrange(mean_BP_control_plot, mean_BP_placebo_plot, mean_BP_atomoxetine_plot, ncol=3, nrow=1)


#------------------------------------------------------------------------
#Average change in BP for each bin for each control 
#------------------------------------------------------------------------

mean_diff_control <- group_by(tempdiff_control, bin) %>%
  summarise(
    count = n(),
    mean = mean(meandiff, na.rm = TRUE),
    sd = sd(meandiff, na.rm = TRUE)
  )

mean_diff_control

mean_diff_control$mean <- as.factor(mean_diff_control$mean)
mean_diff_control$bin <- as.factor(mean_diff_control$bin)

#remove leading 0s
mean_diff_control$bin = str_remove(mean_diff_control$bin, "^0+")

mean_diff_control$mean=as.numeric(levels(mean_diff_control$mean))[mean_diff_control$mean]

mean_diff_control$bin <- as.integer(as.character(mean_diff_control$bin))

mean_diff_control_plot <- ggplot(mean_diff_control, aes(x=bin, y=mean)) +
  geom_point() + ggtitle("Control") + xlab("Time (15sec bins)") + 
  ylab("Mean change in pupil size") + ylim(-0.1, 0.1)

mean_diff_control_plot


#------------------------------------------------------------------------
#Average change in BP for each bin for each placebo 
#------------------------------------------------------------------------

mean_diff_placebo <- group_by(tempdiff_placebo, bin) %>%
  summarise(
    count = n(),
    mean = mean(meandiff, na.rm = TRUE),
    sd = sd(meandiff, na.rm = TRUE)
  )

mean_diff_placebo$mean <- as.factor(mean_diff_placebo$mean)
mean_diff_placebo$bin <- as.factor(mean_diff_placebo$bin)

#remove leading 0s
mean_diff_placebo$bin = str_remove(mean_diff_placebo$bin, "^0+")

mean_diff_placebo$mean=as.numeric(levels(mean_diff_placebo$mean))[mean_diff_placebo$mean]

mean_diff_placebo$bin <- as.integer(as.character(mean_diff_placebo$bin))

mean_diff_placebo_plot <- ggplot(mean_diff_placebo, aes(x=bin, y=mean)) +
  geom_point() + ggtitle("Placebo") + xlab("Time (15sec bins)") + 
  ylab("Mean change in pupil size") + ylim(-0.1, 0.1)

mean_diff_placebo_plot

#------------------------------------------------------------------------
#Average change in BP for each bin for each atomoxetine 
#------------------------------------------------------------------------


mean_diff_atomoxetine <- group_by(tempdiff_atomoxetine, bin) %>%
  summarise(
    count = n(),
    mean = mean(meandiff, na.rm = TRUE),
    sd = sd(meandiff, na.rm = TRUE)
  )

mean_diff_atomoxetine$mean <- as.factor(mean_diff_atomoxetine$mean)
mean_diff_atomoxetine$bin <- as.factor(mean_diff_atomoxetine$bin)

#remove leading 0s
mean_diff_atomoxetine$bin = str_remove(mean_diff_atomoxetine$bin, "^0+")

mean_diff_atomoxetine$mean=as.numeric(levels(mean_diff_atomoxetine$mean))[mean_diff_atomoxetine$mean]

mean_diff_atomoxetine$bin <- as.integer(as.character(mean_diff_atomoxetine$bin))

mean_diff_atomoxetine_plot <- ggplot(mean_diff_atomoxetine, aes(x=bin, y=mean)) +
  geom_point() + ggtitle("Atomoxetine") + xlab("Time (15sec bins)") + 
  ylab("Mean change in pupil size") + ylim(-0.1, 0.1)

mean_diff_atomoxetine_plot

#===========================================================================
#Group plots
#===========================================================================


ggarrange(mean_diff_control_plot, mean_diff_placebo_plot, mean_diff_atomoxetine_plot, ncol=3, nrow=1)

#mean per subject - control
control_meanderiv <- group_by(tempdiff_control, subject) %>%
  summarise(
    count = n(),
    mean = mean(meandiff, na.rm = TRUE),
    sd = sd(meandiff, na.rm = TRUE)
  )


#mean per subject - placebo
placebo_meanderiv <- group_by(tempdiff_placebo, subject) %>%
  summarise(
    count = n(),
    mean = mean(meandiff, na.rm = TRUE),
    sd = sd(meandiff, na.rm = TRUE)
  )


#mean per subject - placebo
ato_meanderiv <- group_by(tempdiff_atomoxetine, subject) %>%
  summarise(
    count = n(),
    mean = mean(meandiff, na.rm = TRUE),
    sd = sd(meandiff, na.rm = TRUE)
  )



#R Function to Find Derivative of Every Point in Time Series

df_TD_edit <- tempdiff_df

df_TD_edit <- df_TD_edit[c(1:3, 7, 9)]

#Create subject_visit column as a unique identifier
cols <- c('subject', 'visit')

df_TD_edit$subject_visit <- apply(df_TD_edit[ , cols ] , 1 , paste , collapse = "_" )

df_TD_edit$subject_visit <- as.factor(df_TD_edit$subject_visit)


#write.csv(df_TD_edit2, 'df_TD_edit2.csv')
#add in conditions



##### ANALYSIS - main part - I can send you this file if you would like to start from here

df_TD_final <- read.csv("df_TD_final.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(df_TD_final)

df_TD_final <- subset(df_TD_final, select = -c(X))


df_TD_final <- rename(df_TD_final, tempD = mean)

levels(df_TD_final$condition)
df_TD_final$condition <- factor(df_TD_final$condition,levels(df_TD_final$condition)[c(2,3,1)])
levels(df_TD_final$condition)

df_TD_final_mean <- group_by(df_TD_final, condition) %>%
  summarise(
    count = n(),
    mean = mean(tempD, na.rm = TRUE),
    sd = sd(tempD, na.rm = TRUE)
  )
df_TD_final_mean
# 
# # condition     count     mean    sd
# 1 control        25 -0.00343 0.00381
# 2 placebo        17 -0.00781 0.00775
# 3 atomoxetine    18 -0.00356 0.0122 

summarise


df_TD_final <- df_TD_final %>% group_by(subject_visit) %>% mutate(mean2 = mean**2) 

df_TD_final$condition <- str_replace(df_TD_final$condition, "atomoxetine", "Atomoxetine")
df_TD_final$condition <- str_replace(df_TD_final$condition, "control", "Control")
df_TD_final$condition <- str_replace(df_TD_final$condition, "placebo", "Placebo")


plot_TD <- ggboxplot(df_TD_final, x = "condition", y = "mean", 
                     color = "condition", line.color = "gray", line.size = 0.4,
                     xlab = FALSE, ylab = "Temporal derivative of pupil diameter (mean change)",
                     palette = c("#f3a953", "#3e64ff", "#ff5151"), add = "jitter") + 
  theme(legend.position="none") 

plot_TD


plot_TD2 <- ggboxplot(df_TD_final, x = "condition", y = "mean2", 
                     color = "condition", line.color = "gray", line.size = 0.4,
                     xlab = FALSE, ylab = "Temporal derivative (mean pupil size)",
                     palette = c("#f3a953", "#3e64ff", "#ff5151"), add = "jitter") + 
  theme(legend.position="none") 

plot_TD2




df_TD_final_PD <- df_TD_final %>% filter(str_detect(subject_visit, "pd"))

df_TD_final_PD <- df_TD_final_PD %>% 
  separate(subject_visit, into = c("subject", "visit"), 
           sep="_")


# had to delete pd013 as v1 pupil)mean was missing
df_TD_final_PD <- subset(df_TD_final_PD, subject!="pd013")


df_TD_paired_plot <- ggpaired(df_TD_final_PD, x = "condition", y = "mean",
               color = "condition", line.color = "gray", line.size = 0.4, id = "subject", 
               xlab = FALSE, ylab = 'Temporal derivative of pupil diameter (mean change)', palette = c("#3e64ff", "#ff5151")) + theme(legend.position="none")

df_TD_paired_plot



#Create df of control and placebo conditions
#-----------------------------------------------------------------

df_TD_control <- subset(df_TD_final, condition!="Atomoxetine")

#-----------------------------------------------------------------
#Independent samples t-test (placebo vs control)
#-----------------------------------------------------------------


res.ftest2 <- var.test(mean ~ condition, data = df_TD_control)
res.ftest2

t_test_PvC2 <- t.test(mean ~ condition, data = df_TD_control, var.equal = TRUE)
t_test_PvC2

#PvC = placebo and control comparison

#temporal derivative mean by condition
#t = 2.4415, df = 40, p-value = 0.01915
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.000755724 0.008022234
#sample estimates:
#  mean in group control mean in group placebo 
#-0.003425391          -0.007814370 



#-----------------------------------------------------------------
#Paired samples t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
# compute the difference

diff_2 <- with(df_TD_final_PD, 
             mean[condition == "Placebo"] - mean[condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff_2)

#W = 0.80571, p-value = 0.002432 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA_2 <- t.test(mean ~ condition, data = df_TD_final_PD, paired = TRUE)
t_test_PvA_2

#pupil_mean by condition
#t = -1.2957, df = 16, p-value = 0.2135



#-----------------------------------------------------------------

df_TD_atoxcontrol <- subset(df_TD_final, condition!="Placebo")

#-----------------------------------------------------------------
#Independent samples t-test (atomoxetine vs control)
#-----------------------------------------------------------------


res.ftest3 <- var.test(mean ~ condition, data = df_TD_atoxcontrol)
res.ftest3

t_test_PvC3 <- t.test(mean ~ condition, data = df_TD_atoxcontrol, var.equal = TRUE)
t_test_PvC3

#t = 0.052666, df = 41, p-value = 0.9583


#mean of placebo and atomoxetine mean diff for each p
group_by(df_TD_final, condition) %>%
  summarise(
    count = n(),
    mean = mean(mean, na.rm = TRUE),
    sd = sd(mean, na.rm = TRUE)
  )


# Atomoxetine    n=18     mean = -0.0036    
# Control        n=25     mean = -0.0034  
# Placebo        n=17     mean = -0.0078




#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------

# create a vector of the p values:
p.value <- c(0.9583, 0.2135)
# run this:
p.adjust(p.value, method = "holm")
# you get this result:
# 0.9583 0.4270
