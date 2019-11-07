## Script just for making patient summary data table (standard for all papers)

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

#read in local, relevant files
physexam <- fread(file="/Users/emiliagrzesiak/Downloads/vw_PR_PHYSEXAM.csv", 
                  header = TRUE, sep = "\t")
screen <- fread(file="/Users/emiliagrzesiak/Downloads/vw_PR_SCREEN.csv", 
                header = TRUE, sep = "\t")
SHED <- fread(file="/Users/emiliagrzesiak/Downloads/vw_PR_SHEDDING.csv",
              header = TRUE, sep = "\t")
SYMP <- fread(file="/Users/emiliagrzesiak/Downloads/vw_PR_SYMPTOM.csv",
              header=TRUE, sep= "\t")

#exclude bad subjects
`%notin%` <- Negate(`%in%`)
SHED <- subset(SHED, subject_id %notin% c("PROM023","PROM026"))
SYMP <- subset(SYMP, subject_id %notin% c("PROM023","PROM026"))
physexam <- subset(physexam, subject_id %notin% c("PROM023","PROM026"))
screen <- subset(screen, subject_id %notin% c("PROM023","PROM026"))

#calculate symptom/shedding scores
#symptom/shedding/PCR data manipulation
SYMP$sx_day <- substring(SYMP$sx_day, 2)
SYMP$sx_day <- as.numeric(SYMP$sx_day)+2
colnames(SYMP)[2] <- "timepoint"
SYMP$symp_score <- rowSums(SYMP[,7:24])
SYMP[is.na(SYMP)] <- 0
SHED$timepoint <- substring(SHED$timepoint, 2)
#SHED$timepoint <- as.numeric(SHED$timepoint)+2
SYMP <- SYMP %>% group_by(subject_id, timepoint) %>% 
  dplyr::summarize(symp_score = mean(symp_score), pcr_at_discharge = min(pcr_at_discharge))
score_per_person <- SYMP %>% group_by(subject_id) %>%
  dplyr::summarize(total_symptom_score=sum(symp_score), PCR=min(pcr_at_discharge))
shed_per_person <- SHED %>% group_by(subject_id) %>%
  dplyr::summarize(total_shed_score=sum(shedding_value))
shed_score_pcr <- merge(score_per_person, shed_per_person, by="subject_id")

#Exclude columns I don't care about
screen <- screen[,c("subject_id", "gender", "dob", "consent_date", 
                    "race_white_0", "race_white_1", "race_white_2",
                    "race_twoorm_3", "race_twoorm_4", "race_twoorm_5", 
                    "race_twoorm_6", "race_asian_7",
                    "race_asian_8", "race_asian_9", "race_asian_10", 
                    "race_black_11", "race_black_12",
                   "race_black_13", "race_asian_14", "race_other_15")]
#make columns I do want
screen$age <- time_length(difftime(as.Date(screen$consent_date), 
                                   as.Date(screen$dob)), unit="years")
screen$white <- as.integer(screen$race_white_0+screen$race_white_1+
                             screen$race_white_2>0) 
screen$black <- as.integer(screen$race_black_11+screen$race_black_12+
                             screen$race_black_13>0)
screen$asian <- as.integer(screen$race_asian_7+screen$race_asian_8+
                             screen$race_asian_9+screen$race_asian_10>0)
screen$other_race <- as.integer(screen$race_twoorm_3+screen$race_twoorm_4+
                                  screen$race_twoorm_5+screen$race_twoorm_6
                                +screen$race_other_15>0)
#exclude columns I don't care about
screen <- screen[,c("subject_id", "gender", "age", "white", 
                    "black", "asian", "other_race")]

#putting it all together...
pre_table1 <- merge(screen, shed_score_pcr, by=c("subject_id"))
pre_table1$age <- floor(pre_table1$age)
median_age <- median(pre_table1$age)
range_age <- range(pre_table1$age)

number_male <- count(pre_table1$gender==0)
number_female <- count(pre_table1$gender==1)

number_white <- count(pre_table1$white)
number_black <- count(pre_table1$black)
number_asian <- count(pre_table1$asian)
number_other_race <- count(pre_table1$other_race)

number_pos_PCR <- count(pre_table1$PCR=='Positive')
number_neg_PCR <- count(pre_table1$PCR=='Negative')
median_symp <- median(pre_table1$total_symptom_score)
range_symp <- range(pre_table1$total_symptom_score)

#table 2
median_age_posPCR <- median(pre_table1$age[pre_table1$PCR=='Positive'])
median_age_negPCR <- median(pre_table1$age[pre_table1$PCR=='Negative'])
range_age_posPCR <- range(pre_table1$age[pre_table1$PCR=='Positive'])
range_age_negPCR <- range(pre_table1$age[pre_table1$PCR=='Negative'])

number_male_posPCR <- count(pre_table1$gender[pre_table1$PCR=='Positive']==0)
number_male_negPCR <- count(pre_table1$gender[pre_table1$PCR=='Negative']==0)
number_female_posPCR <- count(pre_table1$gender[pre_table1$PCR=='Positive']==1)
number_female_negPCR <- count(pre_table1$gender[pre_table1$PCR=='Negative']==1)

number_white_posPCR <- count(pre_table1$white[pre_table1$PCR=='Positive']==1)
number_white_negPCR <- count(pre_table1$white[pre_table1$PCR=='Negative']==1)
number_black_posPCR <- count(pre_table1$black[pre_table1$PCR=='Positive']==1)
number_black_negPCR <- count(pre_table1$black[pre_table1$PCR=='Negative']==1)
number_asian_posPCR <- count(pre_table1$asian[pre_table1$PCR=='Positive']==1)
number_asian_negPCR <- count(pre_table1$asian[pre_table1$PCR=='Negative']==1)
number_other_posPCR <- count(pre_table1$other_race[pre_table1$PCR=='Positive']==1)
number_other_negPCR <- count(pre_table1$other_race[pre_table1$PCR=='Negative']==1)

number_shed_posPCR <- count(pre_table1$total_shed_score[pre_table1$PCR=='Positive']>0)
number_shed_negPCR <- count(pre_table1$total_shed_score[pre_table1$PCR=='Negative']>0)

number_symp_posPCR <- median(
  pre_table1$total_symptom_score[pre_table1$PCR=='Positive'])
number_symp_negPCR <- median(
  pre_table1$total_symptom_score[pre_table1$PCR=='Negative'])
range_symp_posPCR <- range(
  pre_table1$total_symptom_score[pre_table1$PCR=='Positive'])
range_symp_negPCR <- range(
  pre_table1$total_symptom_score[pre_table1$PCR=='Negative'])

shed_days_posPCR <- subset(SHED, subject_id %in% 
                      pre_table1$subject_id[pre_table1$PCR=='Positive'])
symp_days_posPCR <- subset(SYMP, subject_id %in% 
                             pre_table1$subject_id[pre_table1$PCR=='Positive'])
symp_days_negPCR <- subset(SYMP, subject_id %in% 
                             pre_table1$subject_id[pre_table1$PCR=='Negative'])
shed_days_posPCR <- shed_days_posPCR[which(shed_days_posPCR$shedding_value>0),]
symp_days_posPCR <- symp_days_posPCR[which(symp_days_posPCR$symp_score>0),]
symp_days_negPCR <- symp_days_negPCR[which(symp_days_negPCR$symp_score>0),]

shed_days_perSubject <-shed_days_posPCR %>% group_by(subject_id) %>% 
  dplyr::summarize(count = n())
symp_days_posPCR <-symp_days_posPCR %>% group_by(subject_id) %>% 
  dplyr::summarize(count = n())
symp_days_negPCR <-symp_days_negPCR %>% group_by(subject_id) %>% 
  dplyr::summarize(count = n())

median_symp_days_posPCR <- median(symp_days_posPCR$count)
range_symp_days_posPCR <- range(symp_days_posPCR$count)
median_symp_days_negPCR <- median(symp_days_negPCR$count)
range_symp_days_negPCR <- range(symp_days_negPCR$count)
