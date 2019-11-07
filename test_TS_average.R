# comparing time series averages of all physiological data! (test script)

library(cowplot)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(vcdExtra)
library(gridExtra)
library(nlme)
library(Hmisc)
library(dplyr)
library(lubridate)
library(knitr)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(stats)
library(utils)
library(rmarkdown)
library(reshape2)
library(tidyr)
library(stringr)
library(xts)
library(zoo)
library(data.table)
library(reshape2)
library(scales)
library(rprojroot)
library(outliers)
library(lme4)
library(bigmemory)
library(biganalytics)
library(DescTools)
library(hms)
library(chron)
library(anytime)
library(purrr)
library(bayesbio)
library(RcppRoll)
library(binaryLogic)
library(tools)
library(matrixStats)
library(RColorBrewer)

#load data
HR <- fread(file="/Users/emiliagrzesiak/Cohort_Combined/HR.csv",
            header = TRUE, sep = ",")
#HR <- HR[HR$subject_id %in% c("PROM001", "PROM002", "PROM003", "PROM004"),]
#edit datasets
HR <- setDT(HR, keep.rownames=TRUE, key=NULL, check.names=FALSE)
`%notin%` <- Negate(`%in%`)
HR <- subset(HR, subject_id %notin% c("PROM023","PROM026"))
HR$ftime <- anytime(HR$tod, tz='America/New_York')
HR$fday <- day(HR$ftime)
HR <- HR[,index_day := .GRP, by = .(subject_id, fday)]
HR <- HR[, index_day:=index_day-index_day[1]+1L, by="subject_id"]
#HR <- HR %>% group_by(subject_id) %>% mutate(index_day = row_number(fday))
HR$is_baseline <- "False"
HR$is_baseline[HR$index_day < 2] <- "True"

SHED <- fread(file="/Users/emiliagrzesiak/Downloads/vw_PR_SHEDDING.csv",
              header = TRUE, sep = "\t")
SYMP <- fread(file="/Users/emiliagrzesiak/Downloads/vw_PR_SYMPTOM.csv",
              header=TRUE, sep= "\t")
SHED <- subset(SHED, subject_id %notin% c("PROM023","PROM026"))
SYMP <- subset(SYMP, subject_id %notin% c("PROM023","PROM026"))
#SYMPTOMS + SHEDDING ANALYSIS

#calculate symptom/shedding scores
#symptom/shedding/PCR data manipulation
SYMP$sx_day <- substring(SYMP$sx_day, 2)
SYMP$sx_day <- as.numeric(SYMP$sx_day)+2
colnames(SYMP)[2] <- "timepoint"
SYMP$symp_score <- rowSums(SYMP[,7:24])
SYMP[is.na(SYMP)] <- 0
SHED$timepoint <- substring(SHED$timepoint, 2)
SHED$timepoint <- as.numeric(SHED$timepoint)+2

SYMP <- SYMP %>% group_by(subject_id, timepoint) %>% 
  dplyr::summarize(symp_score = mean(symp_score), pcr_at_discharge = min(pcr_at_discharge),
                   feeling_symp = sum(sx_musclesoreness,sx_fatigue,sx_headache,sx_ear_pain,
                                      sx_throat_discomfort,sx_chest_pain,sx_chills,sx_malaise,
                                      sx_itchy_eyes),
                   showing_symp = sum(sx_fever,sx_stuffy_nose,sx_runny_nose,sx_sneezing,sx_coughing,
                                      sx_soba,sx_hoarseness,sx_diarrhea,sx_wheezy_chest))
score_per_person <- SYMP %>% group_by(subject_id) %>%
  dplyr::summarize(total_symptom_score=sum(symp_score), PCR=min(pcr_at_discharge), 
                   total_showing_symp=sum(showing_symp), total_feeling_symp = sum(feeling_symp))
shed_per_person <- SHED %>% group_by(subject_id) %>%
  dplyr::summarize(total_shed_score=sum(shedding_value))
shed_score_pcr <- merge(score_per_person, shed_per_person, by="subject_id")
shed_score_pcr_perDay <- merge(SYMP, SHED, by=c("subject_id", "timepoint"))

#criteria for being labelled "sick"

sickDef <- shed_score_pcr_perDay %>% group_by(subject_id) %>% 
  dplyr::summarize(days_symp = count((symp_score>0)), days_shed = count((shedding_value>0)),
                   pcr_at_discharge = min(pcr_at_discharge), days_feeling = count((feeling_symp>0)),
                   days_showing=count((showing_symp>0)))
sickDef$pcr_at_discharge <- as.factor(sickDef$pcr_at_discharge)
sickDefmelt <- melt(sickDef, id.vars=c("subject_id", "pcr_at_discharge"))

sickDefmelt$subject_id <- as.factor(index(unique(sickDefmelt$subject_id)))
pcr_at_discharge.labs <- c("Negative Discharge PCR", "Positive Discharge PCR")
names(pcr_at_discharge.labs) <- c("Negative", "Positive")
sickDefplot <- ggplot(sickDefmelt, aes(x=subject_id, y=value, fill=variable))+
  geom_bar(stat="identity",position=position_dodge())+
  theme_bw()+
  theme(legend.position = "top")+
  facet_wrap(~pcr_at_discharge, scales = "free_x", 
             labeller = labeller(pcr_at_discharge = pcr_at_discharge.labs))+
  xlab("Subject ID")+
  ylab("Number of Days Experienced")+
  scale_fill_discrete(name = "Infection Definition", 
                      labels = c("Total Symptoms", "Shedding", "Subjective Symptoms", "Objective Symptoms"))
print(sickDefplot)

#heat map of day # versus sick metrics
totalSymp_heat <- ggplot(shed_score_pcr_perDay, aes(timepoint, subject_id, fill=symp_score))+
  geom_tile() +
  scale_fill_gradient(low="white", high="blue")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  scale_x_continuous(breaks = sort(shed_score_pcr_perDay$timepoint[1:12]), 
                     labels=  c("Baseline",3,4,5,6,7,8,9,10,11,12,"Discharge Date"))+
  xlab("Day")+
  ylab("Subject ID")+
  ggtitle("Total Symptom Score per Day")+
  labs(fill="Total Symptom Score")
print(totalSymp_heat)
rm(totalSymp_heat)

feelingSymp_heat <- ggplot(shed_score_pcr_perDay, aes(timepoint, subject_id, fill=feeling_symp))+
  geom_tile() +
  scale_fill_gradient(low="white", high="red")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  scale_x_continuous(breaks = sort(shed_score_pcr_perDay$timepoint[1:12]), 
                     labels=  c("Baseline",3,4,5,6,7,8,9,10,11,12,"Discharge Date"))+
  xlab("Day")+
  ylab("Subject ID")+
  ggtitle("Subjective Symptom Score per Day")+
  labs(fill="Subjective Symptom Score")
print(feelingSymp_heat)
rm(feelingSymp_heat)

showingSymp_heat <- ggplot(shed_score_pcr_perDay, aes(timepoint, subject_id, fill=showing_symp))+
  geom_tile() +
  scale_fill_gradient(low="white", high="green")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  scale_x_continuous(breaks = sort(shed_score_pcr_perDay$timepoint[1:12]), 
                     labels=  c("Baseline",3,4,5,6,7,8,9,10,11,12,"Discharge Date"))+
  xlab("Day")+
  ylab("Subject ID")+
  ggtitle("Objective Symptom Score per Day")+
  labs(fill="Objective Symptom Score")
print(showingSymp_heat)
rm(showingSymp_heat)

shedding_heat <- ggplot(shed_score_pcr_perDay, aes(timepoint, subject_id, fill=shedding_value))+
  geom_tile() +
  scale_fill_gradient(low="white", high="black")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  scale_x_continuous(breaks = sort(shed_score_pcr_perDay$timepoint[1:12]), 
                     labels=  c("Baseline",3,4,5,6,7,8,9,10,11,12,"Discharge Date"))+
  xlab("Day")+
  ylab("Subject ID")+
  ggtitle("Shedding Score per Day")+
  labs(fill="Shedding Symptom Score")
print(shedding_heat)
rm(shedding_heat)

  
#combine HR and daily shed/symp/pcr info
HR_shed <- merge(HR, shed_score_pcr_perDay, by.x=c("subject_id", "index_day"), 
                 by.y=c("subject_id", "timepoint"))
rm(HR)
HR_shed$got_sick <- "False"
HR_shed$is_baseline[HR_shed$index_day < 2] <- "True"
HR_shed$total_score <- HR_shed$symp_score + HR_shed$shedding_value
HR_baseVsSick <-HR_shed %>% filter(is_baseline == 'True' | total_score > 0)
HR_baseVsSick_test <- HR_baseVsSick %>% group_by(subject_id, fday) %>% 
  filter(any(max(total_score)))
HR_baseVsSick_baseline <- HR_baseVsSick %>% filter(is_baseline == 'True')
### this dataset contains: all baseline data + all data from day of max total_score
HR_baseVsSick_final <- rbindlist(list(HR_baseVsSick_baseline, HR_baseVsSick_test))
rm(HR_baseVsSick_test, HR_baseVsSick_baseline)


#compare relative frequencies of HR data from baseline day vs all other days
# PCR NEGATIVE
HR_spread_plot_neg <- ggplot(HR_shed[HR_shed$pcr_at_discharge=='Negative',], 
                             aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')+
  ggtitle("Negative PCR, Baseline Day + Night vs Post-Innoculation Days + Nights")
print(HR_spread_plot_neg)
rm(HR_spread_plot_neg)

# PCR POSITIVE
HR_spread_plot_pos <- ggplot(HR_shed[HR_shed$pcr_at_discharge=='Positive',], 
                             aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')+
  ggtitle("Positive PCR, Baseline Day + Night vs Post-Innoculation Days + Nights")
print(HR_spread_plot_pos)
rm(HR_spread_plot_pos)

#compare relative frequencies of HR data from baseline day vs max shedding day
HR_spread_plot_spec_neg <- ggplot(HR_baseVsSick_final[HR_baseVsSick_final$pcr_at_discharge=='Negative',],
  aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')+
  ggtitle("Negative PCR, Baseline Day + Night vs Sickest Day + Night")
print(HR_spread_plot_spec_neg)
rm(HR_spread_plot_spec_neg)

HR_spread_plot_spec_pos <- ggplot(HR_baseVsSick_final[HR_baseVsSick_final$pcr_at_discharge=='Positive',],
                                  aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')+
  ggtitle("Positive PCR, Baseline Day + Night vs Sickest Day + Night")
print(HR_spread_plot_spec_pos)
rm(HR_spread_plot_spec_pos)

# look at hours of day measured
HR_hours_perSubject <- HR_shed %>% group_by(subject_id, index_day) %>%
  dplyr::summarize(min.hour = min(as.numeric(format(ftime, format = "%H"))), 
                   max.hour = max(as.numeric(format(ftime, format = "%H"))))


# plot hours of day measured for each day and each subject
HR_time_measured_plot <- ggplot(HR_hours_perSubject, aes(index_day)) +
  geom_rect(aes(x = index_day, xmin= index_day - 0.45, 
                                     xmax = index_day + 0.45, 
                                     ymin = min.hour, ymax = max.hour, 
                                     fill = factor(ifelse(index_day==1,"Highlighted","Normal"))))+
  scale_fill_manual(name = "index_day", values=c("red","grey50")) +
  theme(legend.position="none")+
  xlab('day')+
  scale_x_continuous(breaks = HR_hours_perSubject$index_day, 
                   labels= HR_hours_perSubject$index_day)+
  facet_wrap(~subject_id)
print(HR_time_measured_plot)
rm(HR_time_measured_plot)

# look into day vs night 
HR_night <- subset(HR_shed, format(ftime, '%H') %in% 
                     c("02","03","04","05","06"))
HR_day <- subset(HR_shed, format(ftime, '%H') %notin% 
                     c("02","03","04","05","06"))

#include only subjects with night or day baseline measurements
HR_night <- HR_night %>% group_by(subject_id) %>% 
  filter(any(index_day == 1 ))
HR_day <- HR_day %>% group_by(subject_id) %>% 
  filter(any(index_day == 1 ))

#compare night HR on baseline vs other days
HRnight_neg_plot <- ggplot(HR_night[HR_night$pcr_at_discharge=='Negative',], 
                              aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')+
  ggtitle("Negative PCR, Night Baseline vs Nights Post-Innoculation")
print(HRnight_neg_plot)
rm(HRnight_neg_plot)

HRnight_pos_plot <- ggplot(HR_night[HR_night$pcr_at_discharge=='Positive',], 
                              aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')+
  ggtitle("Positive PCR, Night Baseline vs Nights Post-Innoculation")
print(HRnight_pos_plot)
rm(HRnight_pos_plot)

HRday_neg_plot <- ggplot(HR_day[HR_day$pcr_at_discharge=='Negative',], 
                           aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')+
  ggtitle("Negative PCR, Day Baseline vs Days Post-Innoculation")
print(HRday_neg_plot)
rm(HRday_neg_plot)

HRday_pos_plot <- ggplot(HR_day[HR_day$pcr_at_discharge=='Positive',], 
                           aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')+
  ggtitle("Positive PCR, Day Baseline vs Days Post-Innoculation")
print(HRday_pos_plot)
rm(HRday_pos_plot)

#zoom in on just max total score day vs baseline day
HR_night_sickest <- subset(HR_baseVsSick_final, format(ftime, '%H') %in% 
                     c("02","03","04","05","06"))
HR_day_sickest <- subset(HR_baseVsSick_final, format(ftime, '%H') %notin% 
                   c("02","03","04","05","06"))
HR_night_sickest <- HR_night_sickest %>% group_by(subject_id) %>% 
  filter(any(index_day == 1 ))
HR_day_sickest <- HR_day_sickest %>% group_by(subject_id) %>% 
  filter(any(index_day == 1 ))

#compare night HR on baseline vs sickest day
HRsickestNight_neg_plot <- ggplot(HR_night_sickest[HR_night_sickest$pcr_at_discharge=='Negative',], 
                           aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')+
  ggtitle("Negative PCR, Night Baseline vs Sickest Night")
print(HRsickestNight_neg_plot)
rm(HRsickestNight_neg_plot)

HRsickestNight_pos_plot <- ggplot(HR_night_sickest[HR_night_sickest$pcr_at_discharge=='Positive',], 
                           aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')+
  ggtitle("Positive PCR, Night Baseline vs Sickest Night")
print(HRsickestNight_pos_plot)
rm(HRsickestNight_pos_plot)

#compare day HR on baseline vs sickest day
HRsickestDay_neg_plot <- ggplot(HR_day_sickest[HR_day_sickest$pcr_at_discharge=='Negative',], 
                                  aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')+
  ggtitle("Negative PCR, Baseline Day vs Sickest Day")
print(HRsickestDay_neg_plot)
rm(HRsickestDay_neg_plot)

HRsickestDay_pos_plot <- ggplot(HR_day_sickest[HR_day_sickest$pcr_at_discharge=='Positive',], 
                                  aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')+
  ggtitle("Positive PCR, Baseline Day vs Sickest Day")
print(HRsickestDay_pos_plot)
rm(HRsickestDay_pos_plot)

#Average Pos vs Neg comparisons
HRsickestDay_neg_AVG <- ggplot(HR_day_sickest[HR_day_sickest$pcr_at_discharge=='Negative',], 
                                aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  ggtitle("Negative PCR, Baseline Day vs Sickest Day")
print(HRsickestDay_neg_AVG)
rm(HRsickestDay_neg_AVG)

HRsickestDay_pos_AVG <- ggplot(HR_day_sickest[HR_day_sickest$pcr_at_discharge=='Positive',], 
                                aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  ggtitle("Positive PCR, Baseline Day vs Sickest Day")
print(HRsickestDay_pos_AVG)
rm(HRsickestDay_pos_AVG)

HRsickestNight_neg_AVG <- ggplot(HR_night_sickest[HR_night_sickest$pcr_at_discharge=='Negative',], 
                                  aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  ggtitle("Negative PCR, Night Baseline vs Sickest Night")
print(HRsickestNight_neg_AVG)
rm(HRsickestNight_neg_AVG)

HRsickestNight_pos_AVG <- ggplot(HR_night_sickest[HR_night_sickest$pcr_at_discharge=='Positive',], 
                                  aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  ggtitle("Positive PCR, Night Baseline vs Sickest Night")
print(HRsickestNight_pos_AVG)
rm(HRsickestNight_pos_AVG)

#compare day HR on baseline vs other days (outdated!!!! don't use)
# HRday_spread_plot <- ggplot(HR_day, aes(measure, fill = is_baseline)) + 
#   geom_density(alpha = 0.2)+
#   facet_wrap(~subject_id, scale='free_x')
# print(HRday_spread_plot)