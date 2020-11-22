#=================================================================================
# Script for HRV 
#=================================================================================
# Orlando, O'Callaghan 2020


# Working directory is filepath of this script -------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#https://github.com/cliffordlab/PhysioNet-Cardiovascular-Signal-Toolbox/
#blob/master/Tools/HRV_Metrics_Tools/EvalTimeDomainHRVstats.m
# Set directories IO -------------------------------------------------------------------

dataDir_HRV <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/HRV/Analysis")
resultsDir_HRV <- paste0(dirname(getwd()), "/Desktop/Atomoxetine/HRV/Results")

# Load packages -------------------------------------------------------------------

library(pander)
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
library(devtools)
library(reshape2)
library("wesanderson")
#library("kableExtra")
#library("emmeans")
#library("multcomp")

#Set working dir & subset datasheet
# ------------------------------------------------------------------------------------------
df <- read.csv("AllPatients_HRV_results_allwindows_20190618.csv", header = TRUE, 
               quote="\"", stringsAsFactors= TRUE, 
               strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(df)

# ------------------------------------------------------------------------------------------
#Organise PD ato dataframe to provide session & group values 
#& also to match ID values with DemogClin data file

#subset
df_Con <-subset(df, patID%in%df$patID[grep("con", df$patID) ])
df_PD_light <-subset(df, patID%in%df$patID[grep("pd_light", df$patID) ])
df_PD_ato <-subset(df, patID%in%df$patID[grep("v", df$patID) ])

#rename IDs
df_Con$patID <- str_replace(df_Con$patID, "con_", "CON_")
df_PD_light$patID <- str_replace(df_PD_light$patID, "pd_light_", "PDL_")
df_PD_ato$patID <- str_replace(df_PD_ato$patID, "pd_", "PD_")

#Create new group columns
df_Con$Group_1<-"Control"
df_Con$Group_2<-"Control"

df_PD_light$Group_1<-"PD"
df_PD_light$Group_2<-"PD_Light"

df_PD_ato$Group_1<-"PD"
df_PD_ato$Group_2<-"PD_Ato"

#Need further manipulate PD_ato to link it with Demog file that give drug/session informtaion
out <- do.call(rbind, str_split(df_PD_ato$patID, '_v'))
out <- as.data.frame(out)
names(out) <- c("patID", "Session")
out$observation <- 1:nrow(out) 

names(df_PD_ato)[names(df_PD_ato) == "patID"] <- "patID_orig"
df_PD_ato$observation <- 1:nrow(df_PD_ato)
df_PD_ato <- merge(df_PD_ato, out, by = "observation")
names(df_PD_ato)[names(df_PD_ato) == "patID"] <- "StudyID"

# ------------------------------------------------------------------------------------------
#Read in Demogclin file
df_DemogClin <- read.csv("DemogClinData.csv", header = TRUE, quote="\"", stringsAsFactors= TRUE, 
                         strip.white = TRUE, na.strings=c("NA", "-", "?"))
attach(df_DemogClin)


#change id names and col values so merge can work
#df_DemogClin$StudyID

#Keep relevant varibles
df_DemogClin<-df_DemogClin[-c(2,5,6,7,8,9,12,13,15,17,18,19,21:94,101:206)]

###Can add more clin/demog varibles here later for covariates###
df_DemogClin_control<-subset(df_DemogClin, Group_2=="Control")
df_DemogClin_light<-subset(df_DemogClin, Group_2=="PD_Light")
df_DemogClin_ato<-subset(df_DemogClin, Group_2=="PD_Ato")
#df_DemogClin<-df_DemogClin[c(1,10,11)]

#remove Group cols
df_DemogClin_control<-df_DemogClin_control[-c(2,3)]
df_DemogClin_light<-df_DemogClin_light[-c(2,3)]
df_DemogClin_ato<-df_DemogClin_ato[-c(2,3)]


#Split dataframes by session
df_DemogClin_session1<-df_DemogClin_ato[-c(3)]
df_DemogClin_session2<-df_DemogClin_ato[-c(2)]

df_PD_ato_session1<-subset(df_PD_ato, Session=="1")
df_PD_ato_session2<-subset(df_PD_ato, Session=="2")

#Rename condition varible
names(df_DemogClin_session1)[names(df_DemogClin_session1) == "SessionOneCondition"] <- "Condition"
names(df_DemogClin_session2)[names(df_DemogClin_session2) == "SessionTwoCondition"] <- "Condition"

#Remove unecessary cols from df_PD_ato dataframes
df_PD_ato_session1 <- df_PD_ato_session1[-c(1:4)]
df_PD_ato_session2 <- df_PD_ato_session2[-c(1:4)]

#Merge data frames
df_PD_ato_session1 <- merge(df_PD_ato_session1, df_DemogClin_session1, by = "StudyID")
df_PD_ato_session2 <- merge(df_PD_ato_session2, df_DemogClin_session2, by = "StudyID")

#Melt into long format; requires rehape2

#df_PD_ato_session1_long<-melt(df_PD_ato_session1, id=c("StudyID","Session", "Condition","Group_1", 
#"Group_2"))

df_PD_ato_session1_long<-melt(df_PD_ato_session1, id=c(1,31,32,29,30,33:46))

#df_PD_ato_session2_long<-melt(df_PD_ato_session2, id=c("StudyID","Session", "Condition","Group_1", 
#"Group_2"))

df_PD_ato_session2_long<-melt(df_PD_ato_session2, id=c(1,31,32,29,30,33:46))

#merge dataframes by column
Final_df_PD_ato_long <- rbind(df_PD_ato_session1_long,df_PD_ato_session2_long)

#Final_df_PD_ato_long$value<-as.numeric(Final_df_PD_ato_long$value)


#Organise Control dataframe to be able to bind it with PD ato
names(df_Con)[names(df_Con) == "patID"] <- "StudyID"

#Create Sesssion & Condition columns
df_Con$Session<-"0"
#df_Con$Condition<-"Control"

#Remove unecessary cols 
df_Con <- df_Con[-c(2,3)]

#Bind with demog file
#Rename condition varible
df_DemogClin_control<-df_DemogClin_control[-c(3)]
names(df_DemogClin_control)[names(df_DemogClin_control) == "SessionOneCondition"] <- "Condition"
df_DemogClin_control$Condition<-"Control"

df_Con_merged <- merge(df_Con, df_DemogClin_control, by = "StudyID")


#Melt into long format; requires rehape2
#df_Con_long<-melt(df_Con, id=c("StudyID","Session", "Condition","Group_1", "Group_2"))

df_Con_long<-melt(df_Con_merged, id=c(1,31,32,29,30,33:46))

#merge dataframes by column
Final_df_CON_PD_ato_long <- rbind(Final_df_PD_ato_long,df_Con_long)



# ------------------------------------------------------------------------------------------


#Organise PD light dataframe to be able to bind it with PD ato
names(df_PD_light)[names(df_PD_light) == "patID"] <- "StudyID"

#Create Sesssion & Condition columns
df_PD_light$Session<-"0"
#df_PD_light$Condition<-"Control"

#Remove unecessary cols 
df_PD_light <- df_PD_light[-c(2,3)]

#Bind with demog file
#Rename condition varible
df_DemogClin_light<-df_DemogClin_light[-c(3)]
names(df_DemogClin_light)[names(df_DemogClin_light) == "SessionOneCondition"] <- "Condition"
df_DemogClin_light$Condition<-"Light"

df_light_merged <- merge(df_PD_light, df_DemogClin_light, by = "StudyID")

#Melt into long format; requires rehape2
#df_PD_light_long<-melt(df_PD_light, id=c("StudyID","Session", "Condition","Group_1", "Group_2"))

df_PD_light_long<-melt(df_light_merged, id=c(1,31,32,29,30,33:46))

#merge dataframes by column
Final_df_ControlvsPD_long <- rbind(Final_df_CON_PD_ato_long,df_PD_light_long)

#Remove ATO condition
Final_df_ControlvsPD_long <- subset(Final_df_ControlvsPD_long,!(Condition=="Atomoxetine"))

#Note: Final_df_PD_ato_long for ATO vs PLA comparison; Final_df_ControlvsPD_long for all PD (light + placebo vs. Controls); Final_df_CON_PD_ato_long for ATO vs PLA vs Control




levels(Final_df_PD_ato_long$Condition)
Final_df_PD_ato_long$Condition <- factor(Final_df_PD_ato_long$Condition,levels(Final_df_PD_ato_long$Condition)[c(2,1)])
levels(Final_df_PD_ato_long$Condition)




# ------------------------------------------------------------------------------------------
#Plots of Atomoxetine-Placebo comparisons
# ------------------------------------------------------------------------------------------
#Subset to plot
SDNN <-subset(Final_df_PD_ato_long, variable=="SDNN")
RMSSD <-subset(Final_df_PD_ato_long, variable=="RMSSD")
HF <-subset(Final_df_PD_ato_long, variable=="hf")
LF <-subset(Final_df_PD_ato_long, variable=="lf")
VLF <-subset(Final_df_PD_ato_long, variable=="vlf")
LFHF <-subset(Final_df_PD_ato_long, variable=="lfhf")
SampEn <-subset(Final_df_PD_ato_long, variable=="SampEn")

# ------------------------------------------------------------------------------------------
SDNN <- ggpaired(SDNN, x = "Condition", y = "value",
                 color = "Condition", line.color = "gray", line.size = 0.4,
                 palette = "jco", title = "SDNN") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                          axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =2)) 
#+ stat_compare_means(paired = TRUE)

SDNN

# ------------------------------------------------------------------------------------------

RMSSD <- ggpaired(RMSSD, x = "Condition", y = "value",
                  color = "Condition", line.color = "gray", line.size = 0.4,
                  palette = "jco", title = "RMSSD") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                            axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =2)) 
#+ stat_compare_means(paired = TRUE)
RMSSD

# ------------------------------------------------------------------------------------------

HF <- ggpaired(HF, x = "Condition", y = "value",
               color = "Condition", line.color = "gray", line.size = 0.4,
               palette = "jco", title = "High frequency") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                                  axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =2)) 
#+ stat_compare_means(paired = TRUE)
HF

# ------------------------------------------------------------------------------------------

LF <- ggpaired(LF, x = "Condition", y = "value",
               color = "Condition", line.color = "gray", line.size = 0.4,
               palette = "jco", title = "Low frequency") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                                 axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =2)) 
#+ stat_compare_means(paired = TRUE)
LF

# ------------------------------------------------------------------------------------------
VLF <- ggpaired(VLF, x = "Condition", y = "value",
                color = "Condition", line.color = "gray", line.size = 0.4,
                palette = "jco", title = "Very low frequency") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                                       axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =2)) 
#+ stat_compare_means(paired = TRUE)

VLF
# ------------------------------------------------------------------------------------------

LFHF <- ggpaired(LFHF, x = "Condition", y = "value",
                 color = "Condition", line.color = "gray", line.size = 0.4,
                 palette = "jco", title = "Low/high frequency ratio") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                                              axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =2)) 
#+ stat_compare_means(paired = TRUE)

LFHF

# ------------------------------------------------------------------------------------------

SampEn <- ggpaired(SampEn, x = "Condition", y = "value",
                   color = "Condition", line.color = "gray", line.size = 0.4,
                   palette = "jco", title = "Sample entropy") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                                      axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =2)) 
#+ stat_compare_means(paired = TRUE)

SampEn
# ------------------------------------------------------------------------------------------


fin2 <- as.data.frame(Final_df_CON_PD_ato_long)

levels(fin2$Condition)
fin2$Condition <- factor(fin2$Condition,levels(fin2$Condition)[c(3,2,1)])
levels(fin2$Condition)


Final_df_CON_PD_ato_long <- as.data.frame(fin2)



# ------------------------------------------------------------------------------------------
#Plots of Atomoxetine-Placebo-Control comparisons
# ------------------------------------------------------------------------------------------
#Subset to plot
SDNN_2 <-subset(Final_df_CON_PD_ato_long, variable=="SDNN")
RMSSD_2 <-subset(Final_df_CON_PD_ato_long, variable=="RMSSD")
HF_2 <-subset(Final_df_CON_PD_ato_long, variable=="hf")

#test without outliers cn006 and pd002
HF_2_2 <- HF_2 %>% dplyr::filter(StudyID!="PD_002" & StudyID!="CON_006")


LF_2 <-subset(Final_df_CON_PD_ato_long, variable=="lf")
VLF_2 <-subset(Final_df_CON_PD_ato_long, variable=="vlf")
LFHF_2 <-subset(Final_df_CON_PD_ato_long, variable=="lfhf")
SampEn_2 <-subset(Final_df_CON_PD_ato_long, variable=="SampEn")

# ------------------------------------------------------------------------------------------
SDNN2 <- ggboxplot(SDNN_2, x = "Condition", y = "value",
                  color = "Condition", line.color = "gray", line.size = 0.4,
                  palette = c("#f3a953", "#3e64ff", "#ff5151"), title = "SDNN", add = "jitter") + theme(legend.position="none")

SDNN2

SDNN2 + xlab("Condition") + ylab("Mean SDNN")

palette = c("#ffaa71", "#ff7171")
SDNN2

#+ stat_compare_means(paired = TRUE)
# ------------------------------------------------------------------------------------------

RMSSD_2 <- ggboxplot(RMSSD_2, x = "Condition", y = "value",
                   color = "Condition", line.color = "gray", line.size = 0.4,
                   palette = c("#f3a953", "#3e64ff", "#ff5151"), title = "RMSSD", add = "jitter") + theme(legend.position="none") 

RMSSD_2

RMSSD_2

# ------------------------------------------------------------------------------------------
HF_2 <- ggboxplot(HF_2, x = "Condition", y = "value",
                color = "Condition", line.color = "gray", line.size = 0.4,
                palette = c("#f3a953", "#3e64ff", "#ff5151"), title = "High frequency", add = "jitter") + theme(legend.position="none") 

HF_2 + xlab("Condition") + ylab("Mean HF")

#without cn006 and pd002
HF_2_2plot <- ggboxplot(HF_2_2, x = "Condition", y = "value",
                  color = "Condition", line.color = "gray", line.size = 0.4,
                  palette = c("#f3a953", "#3e64ff", "#ff5151"), title = "High frequency", add = "jitter") + theme(legend.position="none") 

HF_2_2plot + xlab("Condition") + ylab("Mean HF")

# ------------------------------------------------------------------------------------------
LF_2 <- ggboxplot(LF_2, x = "Condition", y = "value",
                color = "Condition", line.color = "gray", line.size = 0.4,
                palette = c("#f3a953", "#3e64ff", "#ff5151"), title = "Low frequency", add = "jitter") + theme(legend.position="none") 
#+ stat_compare_means(paired = TRUE)

LF_2 + xlab("Condition") + ylab("Mean LF")
# ------------------------------------------------------------------------------------------
VLF_2 <- ggboxplot(VLF_2, x = "Condition", y = "value",
                 color = "Condition", line.color = "gray", line.size = 0.4,
                 palette = "jco", title = "Vey low frequency", add = "jitter") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                                                       axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =3)) 
#+ stat_compare_means(paired = TRUE)
VLF_2
# ------------------------------------------------------------------------------------------
LFHF_2 <- ggboxplot(LFHF_2, x = "Condition", y = "value",
                  color = "Condition", line.color = "gray", line.size = 0.4,
                  palette = "jco", title = "Low/high frequency ratio", add = "jitter") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                                                               axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =3)) 
#+ stat_compare_means(paired = TRUE)

LFHF_2
# ------------------------------------------------------------------------------------------

SampEn_2 <- ggboxplot(SampEn_2, x = "Condition", y = "value",
                    color = "Condition", line.color = "gray", line.size = 0.4,
                    palette = "jco", title = "Sample entropy", add = "jitter") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                                                       axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =3)) 
#+ stat_compare_means(paired = TRUE)
SampEn_2
# ------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------
#Plots of PD (N = 25) vs. Control comparisons
# ------------------------------------------------------------------------------------------

#Subset to plot
SDNN_3 <-subset(Final_df_ControlvsPD_long, variable=="SDNN")
RMSSD_3 <-subset(Final_df_ControlvsPD_long, variable=="RMSSD")
HF_3 <-subset(Final_df_ControlvsPD_long, variable=="hf")
LF_3 <-subset(Final_df_ControlvsPD_long, variable=="lf")
VLF_3 <-subset(Final_df_ControlvsPD_long, variable=="vlf")
LFHF_3 <-subset(Final_df_ControlvsPD_long, variable=="lfhf")
SampEn_3 <-subset(Final_df_ControlvsPD_long, variable=="SampEn")

# ------------------------------------------------------------------------------------------
SDNN_3 <- ggboxplot(SDNN_3, x = "Group_1", y = "value",
                  color = "Group_1", line.color = "gray", line.size = 0.4,
                  palette = "jco", title = "SDNN", add = "jitter") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                                           axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =2)) 
#+ stat_compare_means(paired = TRUE)
SDNN_3
# ------------------------------------------------------------------------------------------

RMSSD_3 <- ggboxplot(RMSSD_3, x = "Group_1", y = "value",
                   color = "Group_1", line.color = "gray", line.size = 0.4,
                   palette = "jco", title = "RMSSD", add = "jitter") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                                             axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =2)) 
#+ stat_compare_means(paired = TRUE)
RMSSD_3
# ------------------------------------------------------------------------------------------

HF_3 <- ggboxplot(HF_3, x = "Group_1", y = "value",
                color = "Group_1", line.color = "gray", line.size = 0.4,
                palette = "jco", title = "High frequency", add = "jitter") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                                                   axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =2)) 
#+ stat_compare_means(paired = TRUE)
HF_3
# ------------------------------------------------------------------------------------------

LF_3 <- ggboxplot(LF_3, x = "Group_1", y = "value",
                color = "Group_1", line.color = "gray", line.size = 0.4,
                palette = "jco", title = "Low frequency", add = "jitter") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                                                  axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =2)) 
#+ stat_compare_means(paired = TRUE)
LF_3
# ------------------------------------------------------------------------------------------

VLF_3 <- ggboxplot(VLF_3, x = "Group_1", y = "value",
                 color = "Group_1", line.color = "gray", line.size = 0.4,
                 palette = "jco", title = "Very low frequency", add = "jitter") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                                                        axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =2)) 
#+ stat_compare_means(paired = TRUE)
VLF_3
# ------------------------------------------------------------------------------------------

LFHF_3 <- ggboxplot(LFHF_3, x = "Group_1", y = "value",
                  color = "Group_1", line.color = "gray", line.size = 0.4,
                  palette = "jco", title = "Low/High frequency ratio", add = "jitter") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                                                               axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =2)) 
#+ stat_compare_means(paired = TRUE)
LFHF_3

# ------------------------------------------------------------------------------------------
SampEn_3 <- ggboxplot(SampEn_3, x = "Group_1", y = "value",
                    color = "Group_1", line.color = "gray", line.size = 0.4,
                    palette = "jco", title = "Sample entropy", add = "jitter") + theme(legend.position="none") + theme(axis.title.x = element_blank(),
                                                                                                                       axis.title.y = element_blank()) + scale_color_manual(values=wes_palette(name="GrandBudapest1", n =2)) 
#+ stat_compare_means(paired = TRUE)
SampEn_3



# ------------------------------------------------------------------------------------------
#ANOVAS/T-tests
# ------------------------------------------------------------------------------------------
#create a df with all the means of each value for each subject_visit

#dfs
# Final_df_PD_ato_long        - PD groups only
# Final_df_CON_PD_ato_long    - main file
# Final_df_ControlvsPD_long   - not really needed

hrv_df <- as.data.frame(Final_df_CON_PD_ato_long)

hrv_df <- hrv_df[c(1:3, 6:8, 11, 14:15, 20:21)]


hrv_SDNN <-subset(hrv_df, variable=="SDNN")
hrv_RMSSD <-subset(hrv_df, variable=="RMSSD")
hrv_HF <-subset(hrv_df, variable=="hf")
hrv_LF <-subset(hrv_df, variable=="lf")
hrv_SampEn <-subset(hrv_df, variable=="SampEn")
hrv_pnn50 <-subset(hrv_df, variable=="pnn50")
hrv_lfhfr <-subset(hrv_df, variable=="lfhf")


#-----------------------------------------------------------------

SDNN_mean <- group_by(hrv_SDNN, Condition) %>%
  summarise(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

SDNN_mean

#Condition      count  mean    sd
#1 Atomoxetine    19  29.0  11.9
#2 Placebo        19  31.7  10.2
#3 Control        26  47.5  15.6


#independent t-test
#-----------------------------------------------------------------
hrv_SDNN_con <- subset(hrv_SDNN, Condition!="Atomoxetine") %>% 
  droplevels()


res.ftest <- var.test(value ~ Condition, data = hrv_SDNN_con)
res.ftest

#F = 0.4263, num df = 18, denom df = 25, p-value = 0.06643


t_test_PvC <- t.test(value ~ Condition, data = hrv_SDNN_con, var.equal = TRUE)
t_test_PvC

#t = -3.8321, df = 43, p-value = 0.0004089   !!!!


#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
hrv_SDNN_PD <- subset(hrv_SDNN, Condition!="Control") %>% 
  droplevels()

diff <- with(hrv_SDNN_PD, 
             value[Condition == "Placebo"] - value[Condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.89003, p-value = 0.03223 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(value ~ Condition, data = hrv_SDNN_PD, paired = TRUE)
t_test_PvA

#t = -0.83559, df = 18, p-value = 0.4143


# Paired test
perm.t.test(value ~ Condition, paired=TRUE,nperm=5000, data = hrv_SDNN_PD)
#t = -0.83559, p-value = 0.43237


#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------

# create a vector of the p values:
p.value <- c(0.0004089, 0.43237)
# run this:
p.adjust(p.value, method = "holm")
# you get this result:
# 0.0008178 0.4323700

#-----------------------------------------------------------------


RMSSD_mean <- group_by(hrv_RMSSD, Condition) %>%
  summarise(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

RMSSD_mean

#Condition      count  mean    sd
#1 Atomoxetine    19  19.8  15.6 
#2 Placebo        19  20.3  9.43
#3 Control        26  26.7  14.1 

#independent t-test
#-----------------------------------------------------------------
hrv_RMSSD_con <- subset(hrv_RMSSD, Condition!="Atomoxetine") %>% 
  droplevels()


res.ftest <- var.test(value ~ Condition, data = hrv_RMSSD_con)
res.ftest

#F = 0.44806, num df = 18, denom df = 25, p-value = 0.08332


t_test_PvC <- t.test(value ~ Condition, data = hrv_RMSSD_con, var.equal = TRUE)
t_test_PvC

#t = -1.7103, df = 43, p-value = 0.09443


#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
hrv_RMSSD_PD <- subset(hrv_RMSSD, Condition!="Control") %>% 
  droplevels()

diff <- with(hrv_RMSSD_PD, 
             value[Condition == "Placebo"] - value[Condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.90782, p-value = 0.06752 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(value ~ Condition, data = hrv_RMSSD_PD, paired = TRUE)
t_test_PvA

#t = -0.10328, df = 18, p-value = 0.9189

#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------

# create a vector of the p values:
p.value <- c(0.09443, 0.9189)
# run this:
p.adjust(p.value, method = "holm")
# you get this result:
# 0.18886 0.91890

#-----------------------------------------------------------------

HF_mean <- group_by(hrv_HF, Condition) %>%
  summarise(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

HF_mean
#Condition        count  mean    sd
# 1 Atomoxetine     19  170.  244.
# 2 Placebo         19  126.  125.
# 3 Control         26  255.  229.

#independent t-test
#-----------------------------------------------------------------
hrv_HF_con <- subset(hrv_HF, Condition!="Atomoxetine") %>% 
  droplevels()


res.ftest <- var.test(value ~ Condition, data = hrv_HF_con)
res.ftest

#F = 0.29688, num df = 18, denom df = 25, p-value = 0.01023


t_test_PvC <- t.test(value ~ Condition, data = hrv_HF_con, var.equal = TRUE)
t_test_PvC

#t = -2.2051, df = 43, p-value = 0.03285 !!!!


#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
hrv_HF_PD <- subset(hrv_HF, Condition!="Control") %>% 
  droplevels()

diff <- with(hrv_HF_PD, 
             value[Condition == "Placebo"] - value[Condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.80743, p-value = 0.001468 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(value ~ Condition, data = hrv_HF_PD, paired = TRUE)
t_test_PvA

#t = 0.64472, df = 18, p-value = 0.5272

# Paired test
perm.t.test(value ~ Condition, paired=TRUE,nperm=5000, data = hrv_HF_PD)
#t = 0.64472, p-value = 0.5327


#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------

# create a vector of the p values:
p.value <- c(0.03285, 0.5327)
# run this:
p.adjust(p.value, method = "holm")
# you get this result:
# 0.0657 0.5327

#-----------------------------------------------------------------

LF_mean <- group_by(hrv_LF, Condition) %>%
  summarise(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

LF_mean

#Condition   count  mean    sd
#1 Atomoxetine    19  279.  327.
#2 Placebo        19  390.  407.
#3 Control        26  822.  599.

#independent t-test
#-----------------------------------------------------------------
hrv_LF_con <- subset(hrv_LF, Condition!="Atomoxetine") %>% 
  droplevels()


res.ftest <- var.test(value ~ Condition, data = hrv_LF_con)
res.ftest

#F = 0.46071, num df = 18, denom df = 25, p-value = 0.09426


t_test_PvC <- t.test(value ~ Condition, data = hrv_LF_con, var.equal = TRUE)
t_test_PvC

#t = -2.7136, df = 43, p-value = 0.009536   !!!


#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
hrv_LF_PD <- subset(hrv_LF, Condition!="Control") %>% 
  droplevels()

diff <- with(hrv_LF_PD, 
             value[Condition == "Placebo"] - value[Condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.88939, p-value = 0.0314 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(value ~ Condition, data = hrv_LF_PD, paired = TRUE)
t_test_PvA

#t = -0.90651, df = 18, p-value = 0.3766


# Paired test
perm.t.test(value ~ Condition, paired=TRUE,nperm=5000, data = hrv_LF_PD)
#t = -0.90651, p-value = 0.3847
#-----------------------------------------------------------------

#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------

# create a vector of the p values:
p.value <- c(0.009536, 0.3847)
# run this:
p.adjust(p.value, method = "holm")
# you get this result:
# 0.019072 0.384700




#--------------------------------------------------------------------------------------------------------
#LF:HF ratio
#--------------------------------------------------------------------------------------------------------

#left join
#make subject_visit columns

hrv_HF <-subset(hrv_df, variable=="hf")
hrv_LF <-subset(hrv_df, variable=="lf")


cols13 <- c('StudyID', 'Session')

hrv_HF$subject_visit <- apply(hrv_HF[ , cols13 ] , 1 , paste , collapse = "_" )
hrv_HF$subject_visit <- as.factor(hrv_HF$subject_visit)

hrv_LF$subject_visit <- apply(hrv_LF[ , cols13 ] , 1 , paste , collapse = "_" )
hrv_LF$subject_visit <- as.factor(hrv_LF$subject_visit)

hrv_HF <- rename(hrv_HF, HF = value)
hrv_LF <- rename(hrv_LF, LF = value)


hrv_HF <- hrv_HF[c(12, 3:11)]
hrv_LF2 <- hrv_LF[c(12, 11)]


LF_HF_merge <- left_join(hrv_HF, hrv_LF2, by = "subject_visit")

#Ratio = LF:HF = LF (HF = 1, LF = ? in comparison to HF)
LF_HF_merge$Ratio <-
  case_when(
    is.na(LF_HF_merge$LF) & is.na(LF_HF_merge$HF) ~ 1,
    is.na(LF_HF_merge$LF) & LF_HF_merge$HF >= 0 ~ 1,
    LF_HF_merge$LF >= 0 & is.na(LF_HF_merge$HF) ~ 0,
    TRUE ~ LF_HF_merge$LF / LF_HF_merge$HF
  )

LF_HF_mean <- group_by(LF_HF_merge, Condition) %>%
  summarise(
    count = n(),
    mean = mean(Ratio, na.rm = TRUE),
    sd = sd(Ratio, na.rm = TRUE), 
    var = var(Ratio, na.rm = TRUE)
  )


LF_HF_mean

#Condition   count  mean    sd  var
#Control        26  5.07  3.86  14.9
#Placebo        19  4.83  5.42  29.4
#Atomoxetine    19  3.95  5.74  33.0

LF_HF_mean2 <- LF_HF_merge %>% 
  group_by(Condition) %>%
  summarise(mean = mean((Ratio)), cv = cv(Ratio))

LF_HF_mean2

#   Condition    mean    cv
# 1 Control      5.07   76.3
# 2 Placebo      4.83   112. 
# 3 Atomoxetine  3.95   145.


#independent t-test
#-----------------------------------------------------------------
LF_HF_con <- subset(LF_HF_merge, Condition!="Atomoxetine") %>% 
  droplevels()


res.ftest <- var.test(Ratio ~ Condition, data = LF_HF_con)
res.ftest

#F = 1.9691, num df = 18, denom df = 25, p-value = 0.1165


t_test_PvC <- t.test(Ratio ~ Condition, data = LF_HF_con, var.equal = TRUE)
t_test_PvC

#t = -0.1746, df = 43, p-value = 0.8622   


#paired t-test (placebo vs atomoxetine)
#-----------------------------------------------------------------
LF_HF_PD <- subset(LF_HF_merge, Condition!="Control") %>% 
  droplevels()

diff <- with(LF_HF_PD, 
             Ratio[Condition == "Placebo"] - Ratio[Condition == "Atomoxetine"])

# Shapiro-Wilk normality test for the differences
shapiro.test(diff)

#W = 0.82683, p-value = 0.002882 --> below p=0.05 therefore can't assume normality 

#PvA = placebo and atomoxetine comparison

t_test_PvA <- t.test(Ratio ~ Condition, data = LF_HF_PD, paired = TRUE)
t_test_PvA

#t = -0.45921, df = 18, p-value = 0.6516


# Paired test
perm.t.test(Ratio ~ Condition, paired=TRUE,nperm=5000, data = LF_HF_PD)
#t = -0.45921, p-value = 0.6671
#-----------------------------------------------------------------

LFHF_plot <- ggboxplot(LF_HF_merge, x = "Condition", y = "Ratio",
                  color = "Condition", line.color = "gray", line.size = 0.4,
                  palette = c("#f3a953", "#3e64ff", "#ff5151"), title = "LF:HF Ratio", add = "jitter") + theme(legend.position="none") 

LFHF_plot

#---------------------------------------------------------------------------------
# multiple comp correction
#---------------------------------------------------------------------------------

# create a vector of the p values:
p.value <- c(0.6516, 0.6671)
# run this:
p.adjust(p.value, method = "holm")
# you get this result:
# 1 1



lfhfr_mean <- group_by(hrv_lfhfr, Condition) %>%
  summarise(
    count = n(),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

lfhfr_mean
#Condition   count  mean    sd
# 1 Control        26  5.07  3.86
# 2 Placebo        19  4.83  5.42
# 3 Atomoxetine    19  3.95  5.74

