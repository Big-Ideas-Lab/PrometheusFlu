#test PROM_rest script just coh 1


library("cowplot") 
library("ggplot2")
library("tidyverse")
library("magrittr")
library("vcdExtra")
library("gridExtra")
library("nlme")
library("Hmisc")
library("dplyr")
library("lubridate")
library("knitr")
library("datasets")
library("graphics")
library("grDevices")
library("methods")
library("stats")
library("utils")
library("rmarkdown")
library("reshape2")
library("tidyr")
library("stringr")
library("xts")
library("zoo")
library("data.table")
library("reshape2")
library("scales")
library("rprojroot")
library("outliers")
library("lme4")
library("bigmemory")
library("biganalytics")
library("DescTools")
library("hms")
library("chron")
library("anytime")
library("purrr")
library("bayesbio")
library("RcppRoll")
library("binaryLogic")
library("tools")
library("matrixStats")
library("RColorBrewer")

#setting up directories
#dir1 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort1/"
#dir2 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort2/"
#dir3 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort3/"
#dir4 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort4/"
# dir5 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort5/"
# dir6 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort6/"
# dir7 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort7/"
# dir8 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort8/"
# dir9 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort9/"
# dir10 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort10/"
# 
# ACC_1 <- read.csv(paste0(dir1, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# ACC_2 <- read.csv(paste0(dir2, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# ACC_3 <- read.csv(paste0(dir3, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# ACC_4 <- read.csv(paste0(dir4, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# ACC_5 <- read.csv(paste0(dir5, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# ACC_6 <- read.csv(paste0(dir6, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# ACC_7 <- read.csv(paste0(dir7, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# ACC_8 <- read.csv(paste0(dir8, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# ACC_9 <- read.csv(paste0(dir9, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# ACC_10 <- read.csv(paste0(dir10, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# 
# HR_1 <- read.csv(paste0(dir1, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# HR_2 <- read.csv(paste0(dir2, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# HR_3 <- read.csv(paste0(dir3, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# HR_4 <- read.csv(paste0(dir4, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# HR_5 <- read.csv(paste0(dir5, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# HR_6 <- read.csv(paste0(dir6, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# HR_7 <- read.csv(paste0(dir7, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# HR_8 <- read.csv(paste0(dir8, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# HR_9 <- read.csv(paste0(dir9, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
# HR_10 <- read.csv(paste0(dir10, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)

#heart rate values, steps threshold, and window size of baseline day
#personalized method, results from scheduler
# RHR_day0 <- data.frame("subject_id" = as.character(c("PROM001", "PROM002", "PROM003", "PROM004", "PROM005",
#                                         "PROM006", "PROM007", "PROM008", "PROM009", "PROM010",
#                                         "PROM011", "PROM012", "PROM013", "PROM014", "PROM015",
#                                         "PROM016", "PROM017", "PROM018", "PROM019", "PROM020",
#                                         "PROM021", "PROM022", "PROM023", "PROM024", "PROM025",
#                                         "PROM026", "PROM027", "PROM028", "PROM029", "PROM030",
#                                         "PROM031", "PROM032", "PROM033", "PROM034", "PROM035",
#                                         "PROM036", "PROM037", "PROM038", "PROM039")),
#                        "RHR_median" = c(73.575, 68.95, 66.18, 79.3, 48.4, 66.25, 187.375,
#                                         48.93, 76.035, 63.975, 87.08, 57.02, 57.345, 65.08,
#                                         131.095, 51.48, 66.015, 50.92, 61.35, 64.675, 117.32,
#                                         104.315, 63.8, 70.47, 54.75, 60.45, 99.86, 45.55, 69.195,
#                                         57.305, 47.3, 64.785, 71.01, 68.65, 145.87, 58.795,
#                                         78.1, 67.36, 58.42),
#                        "steps_threshold" = c(63.6, 62.7, 61.8, 62.6, 63.4, 63.2, 62.2, 61.3,
#                                              63.7, 62.2, 63.4, 63.2, 62.1, 63.9, 63.2, 62.1,
#                                              63.7, 61.8, 62.6, 62.2, 63.2, 61.5, 61.2, 62.5,
#                                              63, 62, 62.4, 61.8, 61.8, 63.1, 62.9, 62, 61, 62.3,
#                                              63.3, 60.9, 61.4, 61.5, 63.3),
#                        "window_size" = c(840, 2400, 360, 2220, 3420, 2460, 180, 120, 2160,
#                                          420, 1980, 5940, 360, 5220, 1260, 780, 3240, 960,
#                                          1800, 1920, 1080, 180, 60, 60, 1440, 1140, 600, 120,
#                                          240, 5520, 2760, 4140, 180, 3540, 60, 120, 540, 60, 5460))

RHR_day0 <- data.frame("subject_id" = as.character(c("PROM001", "PROM002", "PROM003", "PROM004", "PROM005",
                                                     "PROM006", "PROM007", "PROM008", "PROM009", "PROM010",
                                                     "PROM011", "PROM012", "PROM013", "PROM014", "PROM015",
                                                     "PROM016", "PROM017", "PROM018", "PROM019", "PROM020",
                                                     "PROM021", "PROM022", "PROM023", "PROM024", "PROM025",
                                                     "PROM026", "PROM027", "PROM028", "PROM029", "PROM030",
                                                     "PROM031", "PROM032", "PROM033", "PROM034", "PROM035",
                                                     "PROM036", "PROM037", "PROM038", "PROM039")),
                       "RHR_median" = c(75.62, 65.88, 65.975, 98.385, 57.27, 87.335, 63.12,
                                        62.02, 82.97, 62.28, 72.035, 69.28, 75.93, 65.055,
                                        115.53, 122.52, 94.22, 49.92, 63.86, 77.16, 77.55,
                                        94.13, 67.905, 51.07, 62.015, 58.03, 98.885, 51.3, 90.9,
                                        57.7, 65.27, 65.645, 53.14, 89.5, 77.58, 78.75,
                                         97.975, 63.625, 61.965),
                       "steps_threshold" = c(61.4, 61.3, 61.1, 61.8, 61.4, 61.8, 62.1, 61,
                                             61.4, 61.7, 61.5, 62.1, 62.4, 61.2, 61, 61.5,
                                             61.8, 61.6, 60.8, 60.2, 62.3, 61.3, 61, 62.1,
                                             62, 61.8, 61.6, 61.1, 61.9, 61.6, 62.2, 61.4, 61, 60.9,
                                             61.3, 61.3, 61.1, 61.7, 60.2),
                       "window_size" = c(900, 2100, 300, 240, 780, 900, 1620, 1620, 300,
                                         600, 300, 1200, 1920, 720, 1680, 240, 1320, 1680,
                                         360, 1560, 1320, 1560, 720, 1740, 1260, 1500, 240, 240,
                                         1860, 840, 960, 480, 360, 240, 300, 1500, 1500, 1020, 1380))


#standard method
#RHR_day0_standard <-data.frame("steps_threshold" = median(RHR_day0$steps_threshold), 
                               #"window_size" = median(RHR_day0$window_size))
RHR_day0_standard <-data.frame("steps_threshold" = 0.01, 
                               "window_size" = 900)

#allHR <- rbindlist(list(HR_1, HR_2, HR_3, HR_4, HR_5, HR_6, HR_7, HR_8, HR_9, HR_10))
#allACC <- rbindlist(list(ACC_1, ACC_2, ACC_3, ACC_4, ACC_5, ACC_6, ACC_7, 
                         #ACC_8, ACC_9, ACC_10))

#load in data sets, only for cohort 1 for local drive
allACC <- fread(file="/Users/emiliagrzesiak/ACC_out.csv",
                        header = FALSE, sep = "\t")
HR <- fread(file="/Users/emiliagrzesiak/Cohort_Combined/HR.csv",
              header = TRUE, sep = ",")
allHR <- HR[HR$subject_id %in% c("PROM001", "PROM002", "PROM003", "PROM004"),]
allHR$ftime <- anytime(allHR$tod-7200, tz="UTC")
allACC$ftime <- anytime(allACC$V2-7200, tz="UTC")
allACC$sum_squares <- sqrt(1/3*(allACC$V3^2 + allACC$V4^2 + allACC$V5^2))
# letsee <- allACC %>% group_by(V1, ftime=cut(ftime, "1 sec")) %>%
#   dplyr::summarize(tod = min(V2), med_x = median(V3), med_y = median(V4), med_z = median(V5),
#                    med_root = median(sum_squares))
# 
# colnames(letsee)[1] <- "subject_id"
# allHR$ftime <- as.factor(allHR$ftime)
# letsee_HR <- dt_steps_hr_perSec <- merge(allHR, letsee, 
#                                          by=c("subject_id", "ftime"))
# letsee_HR[ , diff_x := med_x - shift(med_x), by = subject_id]
# letsee_HR[ , diff_y := med_y - shift(med_y), by = subject_id]  
# letsee_HR[ , diff_z := med_z - shift(med_z), by = subject_id]  
# letsee_HR[ , diff_root := med_root - shift(med_root), by = subject_id]  
# letsee_HR[is.na(letsee_HR)] <- 0
# letsee_HR.cor = cor(letsee_HR[,c("med_x", "med_y", "med_z", "med_root", 
#                                  "diff_x", "diff_y", "diff_z", "diff_root", "measure")])

allHR <- setDT(allHR)
allACC <- setDT(allACC)
#allHR[, join_time := ftime]
#allACC[, join_time := ftime]

#Average ACC values (1 per second!)
dt_steps_perSec <- allACC %>% group_by(V1, ftime=cut(ftime, "1 sec")) %>%
  dplyr::summarize(tod = min(V2), avg_per_sec = IQR(sum_squares))

dt_steps_perMin <- allACC %>% group_by(V1, ftime=cut(ftime, "1 min")) %>%
  dplyr::summarize(tod = min(V2), avg_per_sec = IQR(sum_squares))

colnames(dt_steps_perMin) <- c("subject_id", "ftime", "tod", "avg_per_sec", "sd_per_sec")
colnames(dt_steps_perSec) <- c("subject_id", "ftime", "tod", "avg_per_sec", "sd_per_sec")

dt_hr_perMin <- allHR %>% group_by(subject_id, ftime=cut(ftime, "1 min")) %>%
  dplyr::summarize(tod = min(tod), avg_per_sec = median(measure), 
                   sd_per_sec = sd(measure))

dt_steps_hr <- allHR %>% group_by(subject_id, ftime=cut(ftime, "1 sec")) %>%
  dplyr::summarize(tod = min(tod), measure = median(measure))
dt_steps_hr <- dt_steps_hr[,c("subject_id", "ftime", "avg_per_sec", "measure")]
dt_steps_hr <- setDT(dt_steps_hr)
#validating SD method instead of median/mean method
dt_steps_hr_perMin <- merge(dt_steps_perMin, dt_hr_perMin, 
                            by=c("subject_id", "ftime", "tod"))
dt_steps_hr <- merge(dt_steps_hr, dt_steps_perSec, by.x=c("subject_id", "ftime"), by.y =c("V1", "ftime"))

steps_hr_sd_corr_min <- ggplot(data=dt_steps_hr_perMin, aes(x=sd_per_sec.x, y=avg_per_sec.y)) +
  geom_point(colour='red') +
  geom_smooth(method='lm') +
  stat_density(aes(x=sd_per_sec.x,y=..scaled..),position='identity',geom='line') +
  facet_wrap(~subject_id,scale='free_x')
print(steps_hr_sd_corr_min)

steps_hr_avg_corr_min <- ggplot(data=dt_steps_hr_perMin, aes(x=avg_per_sec.x, y=avg_per_sec.y)) +
  geom_point(colour='red') +
  geom_smooth(method='lm') +
  stat_density(aes(x=avg_per_sec.x,y=..scaled..),position='identity',geom='line') +
  facet_wrap(~subject_id,scale='free_x')
print(steps_hr_avg_corr_min)

steps_hr_sd_corr_sec <- ggplot(data=dt_steps_hr_perSec, aes(x=sd_per_sec.x, y=avg_per_sec.y)) +
  geom_point(colour='red') +
  geom_smooth(method='lm') +
  stat_density(aes(x=sd_per_sec.x,y=..scaled..),position='identity',geom='line') +
  facet_wrap(~subject_id,scale='free_x')
print(steps_hr_sd_corr_sec)

steps_hr_avg_corr_sec <- ggplot(data=dt_steps_hr_perSec, aes(x=avg_per_sec.x, y=avg_per_sec.y)) +
  geom_point(colour='red') +
  geom_smooth(method='lm') +
  stat_density(aes(x=avg_per_sec.x,y=..scaled..),position='identity',geom='line') +
  facet_wrap(~subject_id,scale='free_x')
print(steps_hr_avg_corr_sec)


#FUNCTION: for getting resting heart rate values per subject based off...
  ##... personalized window size and accelerometer thresholds
index_by_acc <- function(subjectID, HR_group){
  #isolate one subject
  subject_acc<- dt_steps_perSec[dt_steps_perSec$V1 %in% subjectID,]
  subject_rest<- RHR_day0[RHR_day0$subject_id %in% subjectID,]
  #leave in only accelerometer values under or = to threshold
  subject_acc <- subject_acc[(subject_acc$avg_per_sec <= subject_rest$steps_threshold),]
  #leave in only HR values that are consecutive & within the personalized time window
  subject_acc<- subject_acc %>% 
    mutate(tod = as.integer(tod)) %>% 
    group_by(grp = cumsum(c(1, diff(tod) != 1))) %>% 
    filter(n() >= subject_rest$window_size)
  return(subject_acc)
}

#same function but for standard window + steps threshold
index_by_acc_standard <- function(subjectID, HR_group){
  #isolate one subject
  subject_acc<- dt_steps_perSec[dt_steps_perSec$V1 %in% subjectID,]
  #leave in only accelerometer values under or = to threshold
  subject_acc <- subject_acc[(subject_acc$avg_per_sec <= RHR_day0_standard$steps_threshold),]
  #leave in only HR values that are consecutive & within the personalized time window
  subject_acc<- subject_acc %>% 
    mutate(tod = as.integer(tod)) %>% 
    group_by(grp = cumsum(c(1, diff(tod) != 1))) %>% 
    filter(n() >= RHR_day0_standard$window_size)
  return(subject_acc)
}

ACC_1_noOut<- index_by_acc("PROM001", allHR)
ACC_2_noOut<- index_by_acc("PROM002", allHR)
ACC_3_noOut<- index_by_acc("PROM003", allHR)
ACC_4_noOut<- index_by_acc("PROM004", allHR)
ACC_5_noOut<- index_by_acc("PROM005", allHR)
ACC_6_noOut<- index_by_acc("PROM006", allHR)
ACC_7_noOut<- index_by_acc("PROM007", allHR)
ACC_8_noOut<- index_by_acc("PROM008", allHR)
ACC_9_noOut<- index_by_acc("PROM009", allHR)
ACC_10_noOut<- index_by_acc("PROM010", allHR)
ACC_11_noOut<- index_by_acc("PROM011", allHR)
ACC_12_noOut<- index_by_acc("PROM012", allHR)
ACC_13_noOut<- index_by_acc("PROM013", allHR)
ACC_14_noOut<- index_by_acc("PROM014", allHR)
ACC_15_noOut<- index_by_acc("PROM015", allHR)
ACC_16_noOut<- index_by_acc("PROM016", allHR)
ACC_17_noOut<- index_by_acc("PROM017", allHR)
ACC_18_noOut<- index_by_acc("PROM018", allHR)
ACC_19_noOut<- index_by_acc("PROM019", allHR)
ACC_20_noOut<- index_by_acc("PROM020", allHR)
ACC_21_noOut<- index_by_acc("PROM021", allHR)
ACC_22_noOut<- index_by_acc("PROM022", allHR)
ACC_23_noOut<- index_by_acc("PROM023", allHR)
ACC_24_noOut<- index_by_acc("PROM024", allHR)
ACC_25_noOut<- index_by_acc("PROM025", allHR)
ACC_26_noOut<- index_by_acc("PROM026", allHR)
ACC_27_noOut<- index_by_acc("PROM027", allHR)
ACC_28_noOut<- index_by_acc("PROM028", allHR)
ACC_29_noOut<- index_by_acc("PROM029", allHR)
ACC_30_noOut<- index_by_acc("PROM030", allHR)
ACC_31_noOut<- index_by_acc("PROM031", allHR)
ACC_32_noOut<- index_by_acc("PROM032", allHR)
ACC_33_noOut<- index_by_acc("PROM033", allHR)
ACC_34_noOut<- index_by_acc("PROM034", allHR)
ACC_35_noOut<- index_by_acc("PROM035", allHR)
ACC_36_noOut<- index_by_acc("PROM036", allHR)
ACC_37_noOut<- index_by_acc("PROM037", allHR)
ACC_38_noOut<- index_by_acc("PROM038", allHR)
ACC_39_noOut<- index_by_acc("PROM039", allHR)

ACC_1_noOut_standard<- index_by_acc_standard("PROM001", allHR)
ACC_2_noOut_standard<- index_by_acc_standard("PROM002", allHR)
ACC_3_noOut_standard<- index_by_acc_standard("PROM003", allHR)
ACC_4_noOut_standard<- index_by_acc_standard("PROM004", allHR)
ACC_5_noOut_standard<- index_by_acc_standard("PROM005", allHR)
ACC_6_noOut_standard<- index_by_acc_standard("PROM006", allHR)
ACC_7_noOut_standard<- index_by_acc_standard("PROM007", allHR)
ACC_8_noOut_standard<- index_by_acc_standard("PROM008", allHR)
ACC_9_noOut_standard<- index_by_acc_standard("PROM009", allHR)
ACC_10_noOut_standard<- index_by_acc_standard("PROM010", allHR)
ACC_11_noOut_standard<- index_by_acc_standard("PROM011", allHR)
ACC_12_noOut_standard<- index_by_acc_standard("PROM012", allHR)
ACC_13_noOut_standard<- index_by_acc_standard("PROM013", allHR)
ACC_14_noOut_standard<- index_by_acc_standard("PROM014", allHR)
ACC_15_noOut_standard<- index_by_acc_standard("PROM015", allHR)
ACC_16_noOut_standard<- index_by_acc_standard("PROM016", allHR)
ACC_17_noOut_standard<- index_by_acc_standard("PROM017", allHR)
ACC_18_noOut_standard<- index_by_acc_standard("PROM018", allHR)
ACC_19_noOut_standard<- index_by_acc_standard("PROM019", allHR)
ACC_20_noOut_standard<- index_by_acc_standard("PROM020", allHR)
ACC_21_noOut_standard<- index_by_acc_standard("PROM021", allHR)
ACC_22_noOut_standard<- index_by_acc_standard("PROM022", allHR)
ACC_23_noOut_standard<- index_by_acc_standard("PROM023", allHR)
ACC_24_noOut_standard<- index_by_acc_standard("PROM024", allHR)
ACC_25_noOut_standard<- index_by_acc_standard("PROM025", allHR)
ACC_26_noOut_standard<- index_by_acc_standard("PROM026", allHR)
ACC_27_noOut_standard<- index_by_acc_standard("PROM027", allHR)
ACC_28_noOut_standard<- index_by_acc_standard("PROM028", allHR)
ACC_29_noOut_standard<- index_by_acc_standard("PROM029", allHR)
ACC_30_noOut_standard<- index_by_acc_standard("PROM030", allHR)
ACC_31_noOut_standard<- index_by_acc_standard("PROM031", allHR)
ACC_32_noOut_standard<- index_by_acc_standard("PROM032", allHR)
ACC_33_noOut_standard<- index_by_acc_standard("PROM033", allHR)
ACC_34_noOut_standard<- index_by_acc_standard("PROM034", allHR)
ACC_35_noOut_standard<- index_by_acc_standard("PROM035", allHR)
ACC_36_noOut_standard<- index_by_acc_standard("PROM036", allHR)
ACC_37_noOut_standard<- index_by_acc_standard("PROM037", allHR)
ACC_38_noOut_standard<- index_by_acc_standard("PROM038", allHR)
ACC_39_noOut_standard<- index_by_acc_standard("PROM039", allHR)

HR_ACC_1_standard <- merge(ACC_1_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_2_standard <- merge(ACC_2_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_3_standard <- merge(ACC_3_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_4_standard <- merge(ACC_4_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_5_standard <- merge(ACC_5_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_6_standard <- merge(ACC_6_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_7_standard <- merge(ACC_7_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_8_standard <- merge(ACC_8_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_9_standard <- merge(ACC_9_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_10_standard <- merge(ACC_10_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_11_standard <- merge(ACC_11_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_12_standard <- merge(ACC_12_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_13_standard <- merge(ACC_13_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_14_standard <- merge(ACC_14_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_15_standard <- merge(ACC_15_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_16_standard <- merge(ACC_16_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_17_standard <- merge(ACC_17_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_18_standard <- merge(ACC_18_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_19_standard <- merge(ACC_19_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_20_standard <- merge(ACC_20_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_21_standard <- merge(ACC_21_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_22_standard <- merge(ACC_22_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_23_standard <- merge(ACC_23_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_24_standard <- merge(ACC_24_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_25_standard <- merge(ACC_25_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_26_standard <- merge(ACC_26_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_27_standard <- merge(ACC_27_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_28_standard <- merge(ACC_28_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_29_standard <- merge(ACC_29_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_30_standard <- merge(ACC_30_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_31_standard <- merge(ACC_31_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_32_standard <- merge(ACC_32_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_33_standard <- merge(ACC_33_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_34_standard <- merge(ACC_34_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_35_standard <- merge(ACC_35_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_36_standard <- merge(ACC_36_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_37_standard <- merge(ACC_37_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_38_standard <- merge(ACC_38_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))
HR_ACC_39_standard <- merge(ACC_39_noOut_standard[,c("V1", "ftime", "tod", "avg_per_sec")], 
                           allHR[,c("subject_id", "tod", "measure")], 
                           by.x=c("V1", "tod"), 
                           by.y=c("subject_id", "tod"))

HR_ACC_standard <- rbindlist(list(HR_ACC_1_standard, HR_ACC_2_standard, HR_ACC_3_standard, HR_ACC_4_standard))

# HR_ACC_1_standard %>% mutate(days = date(ftime)) %>%
#   group_by(V1, days) %>% dplyr::summarize(RHR_day = median(measure))


RHRdaily_standard <- HR_ACC_standard %>% mutate(days = date(ftime)) %>%
  group_by(V1, days) %>% dplyr::summarize(RHR_day = median(measure))
RHRdaily_standard <- data.table(RHRdaily_standard)
RHRdaily_standard <- RHRdaily_standard[,index:= order(days), by="V1"]
print(RHRdaily_standard)
# test1 <- dt_steps_perSec[dt_steps_perSec$V1 %in% "PROM001",]
# test2<- RHR_day0[RHR_day0$subject_id %in% "PROM001",]
# test3 <- test1[(test1$avg_per_sec <= test2$steps_threshold),]
# test3 %>%
#   mutate(tod = as.integer(tod)) %>%
#   group_by(grp = cumsum(c(1, diff(tod) != 1))) %>%
#   filter(n() >= test2$window_size)