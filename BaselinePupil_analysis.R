#================================================
# Script for Baseline Pupil analysis
#================================================
# Orlando, O'Callaghan 2020

# Working directory is filepath of this script -------------------------------------
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
library(lmerTest)

# Read in data & organise -------------------------------------------------------------------
df <- read.csv("df_bp.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(df)

df <- subset(df, select = -c(X))

#remove IDs that don't have any pupil data
#Missing pupil data files: cn005; pd007 - both; pd013 - session 2

df <- df %>% filter(subject != "cn005" & subject != "pd007")

# fill in condition NAs for controls
df$condition <- as.character(df$condition)
df$condition <- df$condition %>% replace_na("Control")
df$condition <- as.factor(df$condition)


#change order of condition factor, better for later plotting
levels(df$condition)
df$condition <- factor(df$condition,levels(df$condition)[c(2,3,1)])
levels(df$condition)

#create summary df
df_summary <- df %>% 
  group_by(subject, condition, DxYears, AgeScan, Education, DDE, MMSE, MoCA, ACE_total, UPDRS_Part3_total, PlasmaLevel_ATO, Whole, visit) %>%
  summarise(pupil_mean = mean(mean), pupil_cv = mean(cv))  

df_summary_PD <- df_summary %>%
  filter(str_detect(subject, "pd"))



# Paired plots -------------------------------------------------------------------

p1 <- ggpaired(df_summary_PD, x = "condition", y = "pupil_mean",
         color = "condition", line.color = "gray", line.size = 0.4, id = "subject", 
         xlab = FALSE, ylab = "", palette = c("#3e64ff", "#ff5151")) +
  theme(legend.position="none")
  
p1
# three group plots -------------------------------------------------------------------  

p2 <-  ggboxplot(df_summary, x = "condition", y = "pupil_mean",
          color = "condition", line.color = "gray", line.size = 0.4,
          xlab = FALSE, ylab = "Mean pupil (arbitary units)",
          palette = c("#f3a953", "#3e64ff", "#ff5151"), add = "jitter") + 
  theme(legend.position="none") 

p2

ggarrange(p2, p1, ncol=2, nrow=1)

#----------------------------------------------------------------------------------- 
# ANOVA, sd and variance
# Data analysis of variance (anova) with multiple comparisons.
#http://www.sthda.com/english/wiki/one-way-anova-test-in-r#multiple-pairwise-comparison-between-the-means-of-groups


group_by(df_summary, condition) %>%
  summarise(
    count = n(),
    mean = mean(pupil_mean, na.rm = TRUE),
    sd = sd(pupil_mean, na.rm = TRUE)
  )
  
## A tibble: 3 x 4
#condition   count    mean    sd
#<fct>       <int>   <dbl>   <dbl>
#  1 Control      25  423.   97.6
#2 Placebo        18  402.   88.7
#3 Atomoxetine    18  518.   188. 

ggline(df_summary, x = "condition", y = "pupil_mean", 
       add = c("mean_se", "jitter"), 
       order = c("Control", "Placebo", "Atomoxetine"),
       ylab = "Mean Pupil Diameter", xlab = "Treatment", 
       color = "condition", line.color = "gray", line.size = 0.4,
       palette = c("#ffa931", "#f54291", "#12cad6"))

#-----------------------------------------------------------------
# Compute the analysis of variance - ANOVA
#-----------------------------------------------------------------

res.aov <- aov(pupil_mean ~ condition, data = df_summary)
# Summary of the analysis
summary(res.aov)

#Tukey multiple comparisons of means
#95% family-wise confidence level
TukeyHSD(res.aov)

#                         diff         lwr      upr     p adj
#Placebo-Control     -21.58507 -119.415535  76.2454 0.8564966
#Atomoxetine-Control  94.57973   -1.619575 190.7790 0.0549433
#Atomoxetine-Placebo 116.16479   10.915969 221.4136 0.0272367

#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

bxp <- ggboxplot(df_summary, x = "condition", y = "pupil_mean", add = "point", 
                 ylab = "Mean Pupil Diameter", xlab = "Treatment", 
                 color = "condition", line.color = "gray", line.size = 0.4,
                 palette = c("#ffa931", "#f54291", "#12cad6"))
bxp


#outliers
df_summary %>%
  group_by(condition) %>%
  identify_outliers(pupil_mean)


#Normality 
#The normality assumption can be checked by computing Shapiro-Wilk test for each time point. 
#If the data is normally distributed, the p-value should be greater than 0.05.
df_summary %>%
  group_by(condition) %>%
  shapiro_test(pupil_mean)

ggqqplot(df_summary, "pupil_mean", facet.by = "condition")

# From the plot above, if all the points fall approximately along 
# the reference line, we can assume normality.


#Create df of control and placebo conditions
#-----------------------------------------------------------------

df_summary_control <- subset(df_summary, condition!="Atomoxetine")

#-----------------------------------------------------------------
#Independent samples t-test (placebo vs control)
#-----------------------------------------------------------------


res.ftest <- var.test(pupil_mean ~ condition, data = df_summary_control)
res.ftest

t_test_PvC <- t.test(pupil_mean ~ condition, data = df_summary_control, var.equal = TRUE)
t_test_PvC

#PvC = placebo and control comparison

#pupil_mean by condition
#t = 0.72936, df = 40, p-value = 0.47
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -38.22800  81.39813
#sample estimates:
# mean in group Control = 423.2272
# mean in group Placebo = 401.6422 

#-----------------------------------------------------------------
#Paired samples t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
# compute the difference

diff <- with(df_summary_PD, 
          pupil_mean[condition == "Placebo"] - pupil_mean[condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.91435, p-value = 0.1185 --> above p=0.05 therefore can assume normality 

# had to delete pd013 as v1 pupil)mean was missing
df_summary_PD_2 <- subset(df_summary_PD, subject!="pd013")

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(pupil_mean ~ condition, data = df_summary_PD_2, paired = TRUE)
t_test_PvA

#pupil_mean by condition
#t = -4.1293, df = 16, p-value = 0.0007868


ggpaired(df_summary_PD_2, x = "condition", y = "pupil_mean",
         color = "condition", line.color = "gray", line.size = 0.4, id = "subject", 
         xlab = FALSE, ylab = "Mean pupil (arbitary units)", palette = c("#f54291", "#12cad6")) +
  theme(legend.position="none")




#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------
t_test_PvC
#pupil_mean by condition
#t = 0.72936, df = 40, p-value = 0.47
t_test_PvA
#pupil_mean by condition
#t = -4.1293, df = 16, p-value = 0.0007868
# create a vector of the p values:
p.value <- c(0.47, 0.0007868)
# run this:
p.adjust(p.value, method = "holm")
# you get this result:
# 0.4700000 0.0015736


#--------------------------------------------------------------------------------------------------------------------


#create new columns placebo and atomoxetine

df_mean_diff <- df_summary_PD_2[c(1:2, 14)]
#write.csv(df_mean_diff, "df_mean_diff.csv")

#edit to combine subjects horizontally
mean_diff_edit <- read.csv("mean_diff_edit.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, strip.white = TRUE)
attach(mean_diff_edit)

mean_diff_edit <- subset(mean_diff_edit, select = -c(X))

#-------------------------------------------------------------
#calculate mean difference
#-------------------------------------------------------------
mean_diff_edit <- mean_diff_edit %>% group_by(subject) %>% mutate(difference = Atomoxetine - Placebo) 
mean(mean_diff_edit$difference)
# mean difference (Atomoxetine - placebo) = 129.4282

#-------------------------------------------------------------
#percentage change 
#-------------------------------------------------------------
# (ato - placebo)/placebo x 100

mean_diff_edit <- mean_diff_edit %>% group_by(subject) %>% mutate(change = difference / Placebo) 
mean_diff_edit <- mean_diff_edit %>% group_by(subject) %>% mutate(percentage_change = change * 100) 
mean(mean_diff_edit$percentage_change)
#mean percentage change = 31.20601


#Backward elimination of fixed effects in the linear mixed model predicting mean_pupil

#Inclusion probabilities and Bayes factors for the inclusion of fixed effects in the linear mixed model predicting mean_pupil

#-----------------------------------------------------------------------------------------------
#AFEX linear models
#-----------------------------------------------------------------------------------------------
#put pd013 back into 
# 
# 
# model1 <- afex::mixed(pupil_mean ~ condition + (1|subject), data = df_summary_PD)
# model1
# summary(model1)
# nice(model1) #Mixed Model Anova Table #KR = kenward-roger method
# 
# # Effect: condition   df =16.32   F = 15.59    p.value = 0.001
# 
# model2 <- afex::mixed(pupil_mean ~ condition + AgeScan + UPDRS_Part3_total + (1|subject) , data = df_summary_PD)
# model2
# summary(model2)
# nice(model2)
# 
# #Effect: AgeScan             df: 14.84     F: 0.85    p.value: .371
# #Effect: UPDRS_Part3_total   df: 14.69     F: 0.01    p.value: .923
# 
# #backward elimination
# 
# model3 <- afex::mixed(pupil_mean ~ condition + UPDRS_Part3_total + AgeScan + DxYears + DDE + PlasmaLevel_ATO + visit + (1|subject) , data = df_summary_PD)
# model3
# summary(model3)
# nice(model3)
# 
# #  Effect                 df         F    p.value
# #         condition 1, 15.23     16.80    *** <.001
# # UPDRS_Part3_total 1, 11.77      0.00    .956
# #           AgeScan 1, 11.94      0.57    .463
# #           DxYears 1, 11.93      0.02    .878
# #               DDE 1, 11.77      0.00    .986
# #   PlasmaLevel_ATO 1, 11.82      0.03    .856
# #             visit 1, 15.23      2.58    .129
# 



#--------------------------------------------------------------------
# GLMM - claire
#--------------------------------------------------------------------


#set sum-to-zero contrast for your categorical predictors (ie.., visit and drug). 
#This is something afex does automatically,
#so we want to keep consistent. It is also generally advised to do 
#http://atyre2.github.io/2016/09/03/sum-to-zero-contrasts.html


df_summary_PD$condition <- as.factor(df_summary_PD$condition)
df_summary_PD$visit <- as.factor(df_summary_PD$visit)


# assign sum-to-zero contrasts to categorical predictors
contrasts(df_summary_PD$condition) <- contr.sum
contrasts(df_summary_PD$visit) <- contr.sum

# Test for potential role of clinical covariates / demographics in explaining SSRT
# Supplementary Table 4: Frequentist model selection
# Fit full model, with all potential covariates

pupil_condition_mixed <- lmerTest::lmer(pupil_mean ~ condition + UPDRS_Part3_total + 
                                          AgeScan + visit + DxYears + DDE + PlasmaLevel_ATO + 
                                          (1 | subject),data = df_summary_PD)

# Stepwise backward elimination of fixed effects
pupil_condition_mixed_step <- lmerTest::step(pupil_condition_mixed, reduce.random = FALSE)


# To get beta estimates and p-values for each of the model variants considered by `lmerTest::step`
# above, we have to fit those models separately
mixed_formulae <- within(list(), {
  step1 <- formula(pupil_mean ~ condition + visit + DxYears +
                     AgeScan + UPDRS_Part3_total + DDE + PlasmaLevel_ATO + (1 | subject))
  step2 <- update(step1, . ~ . - UPDRS_Part3_total)
  step3 <- update(step2, . ~ . - AgeScan)
  step4 <- update(step3, . ~ . - DxYears)
  step5 <- update(step4, . ~ . - DDE)
  step6 <- update(step5, . ~ . - PlasmaLevel_ATO)
  step7 <- update(step6, . ~ . - visit)
})

# Use `afex::mixed` to get p-values for the fixed effects, for each model variant
pupil_condition_mixed_step_variants <- lapply(mixed_formulae, FUN = function(formula) {
  afex::mixed(formula = formula, data = df_summary_PD)
})

# Use `lme4::lmer` to get standardised beta's and AIC / BIC values for the fixed effects, for each
# model variant
pupil_condition_mixed_step_variants_IC <- lapply(mixed_formulae, FUN = function(formula) {
  lme4::lmer(formula = formula, data = df_summary_PD, REML = FALSE)
})

#pupil_condition_mixed_step_variants_IC ??? - zoom at 3

#find p values
#pupil_condition_mixed_step_variants[["step7"]]


#beta values, AIC and BIC
# pupil_condition_mixed_step_variants_IC[["step7"]]
# fixed effects = beta values
























#-----------------------------------------------------------------------------------------------
# Z-score - mess
#-----------------------------------------------------------------------------------------------

# z-score = (x-μ)/σ
#x is a raw score to be standardized; 
#μ is the mean of the population; 
#σ is the standard deviation of the population. 

# overall mean of each condition
#mean(mean_diff_edit$Placebo)
# = 401.6422

#mean_diff_edit <- mean_diff_edit %>% group_by(subject) %>% mutate(variance = (Placebo - 401.6422)) 

# calculate SD for each subject

#mean_diff_edit2 <- mean_diff_edit %>%
#  summarise(sd = sd(Atomoxetine))


#drug = mean_diff_edit$Atomoxetine 
#sd(drug)
#= 184.5502

#mean(mean_diff_edit$Atomoxetine)
# = 531.0704

#mean_diff_edit <- mean_diff_edit %>% group_by(subject) %>% mutate(varianceA = (Atomoxetine - 531.0704)) 

#SD???
#mean_diff_edit <- mean_diff_edit %>% group_by(subject) %>% mutate(sd = (Atomoxetine - 531.0704)**2) 

#sum(mean_diff_edit$sd)
#= 544940.6

#544940.6/16
#= 34,058.7875



#-----------------------------------------------------------------------------------------------
#GLMM - winter methods
#-----------------------------------------------------------------------------------------------

#install.packages(“lme4”)
#library(lme4)

#pupilsize_test.model = lmer(pupil_mean ~ condition + (1|subject), data=df_summary_PD)
#pupilsize_test.model

#REML criterion at convergence: 422.0688
#Random effects:
#  Groups   Name        Std.Dev.
#  subject  (Intercept) 117.61  
#Residual              92.16  
#Number of obs: 35, groups:  subject, 18
#Fixed Effects:
#  (Intercept)  conditionAtomoxetine  
#    393.4                 124.4 


#pupilsize.model2 = lmer(pupil_mean ~ condition + (1|subject) + (1|DxYears), data=df_summary_PD)
#pupilsize.model2



#null model
#pupilsize.null = lmer(pupil_mean ~ AgeScan +
#                         (1|subject), data=df_summary_PD,
#REML=FALSE)
#pupilsize.null


#model
#pupilsize.model = lmer(pupil_mean ~ condition +
#                          AgeScan +
#                          (1|subject), data=df_summary_PD,
#                        REML=FALSE)
#pupilsize.model

#likelihood ratio test using the anova to compare the two models
#anova(pupilsize.null,pupilsize.model)

#                 npar      AIC    BIC    logLik    deviance      Chisq   Df  Pr(>Chisq)    
#pupilsize.null     4     457.47  463.69  -224.74   449.47                         
#pupilsize.model    5     448.23  456.00  -219.11   438.23        11.248  1   0.0007971 ***

#condition affected pupilsize (χ2(1)=11.25, p=0.0008)


