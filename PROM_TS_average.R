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
dir1 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort1/"
dir2 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort2/"
dir3 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort3/"
dir4 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort4/"
dir5 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort5/"
dir6 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort6/"
dir7 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort7/"
dir8 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort8/"
dir9 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort9/"
dir10 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort10/"

ACC_1 <- read.csv(paste0(dir1, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
ACC_2 <- read.csv(paste0(dir2, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
ACC_3 <- read.csv(paste0(dir3, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
ACC_4 <- read.csv(paste0(dir4, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
ACC_5 <- read.csv(paste0(dir5, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
ACC_6 <- read.csv(paste0(dir6, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
ACC_7 <- read.csv(paste0(dir7, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
ACC_8 <- read.csv(paste0(dir8, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
ACC_9 <- read.csv(paste0(dir9, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
ACC_10 <- read.csv(paste0(dir10, "ACC_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)

#edit datasets
ACC <- rbindlist(list(ACC_1, ACC_2, ACC_3, ACC_4, ACC_5, ACC_6, ACC_7, ACC_8, ACC_9, ACC_10))
rm(ACC_1, ACC_2, ACC_3, ACC_4, ACC_5, ACC_6, ACC_7, ACC_8, ACC_9, ACC_10)
ACC <- setDT(ACC, keep.rownames=TRUE, key=NULL, check.names=FALSE)
ACC$ftime <- anytime(ACC$tod-7200, tz="UTC")
ACC$fday <- day(ACC$ftime)
ACC$sum_squares <- sqrt(1/3*(ACC$data_type^2 + ACC$ACC_x^2 + ACC$ACC_y^2))
ACC <- ACC[,index_day := .GRP, by = .(subject_id, fday)]
ACC <- ACC[, index_day:=index_day-index_day[1]+1L, by="subject_id"]
ACC$is_baseline <- "False"
ACC$is_baseline[HR$index_day < 2] <- "True"


#compare relative frequencies of HR data from baseline day vs all other days
ACC_spread_plot <- ggplot(ACC, aes(sum_squares, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')
print(ACC_spread_plot)
rm(ACC)

HR_1 <- read.csv(paste0(dir1, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
HR_2 <- read.csv(paste0(dir2, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
HR_3 <- read.csv(paste0(dir3, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
HR_4 <- read.csv(paste0(dir4, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
HR_5 <- read.csv(paste0(dir5, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
HR_6 <- read.csv(paste0(dir6, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
HR_7 <- read.csv(paste0(dir7, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
HR_8 <- read.csv(paste0(dir8, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
HR_9 <- read.csv(paste0(dir9, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
HR_10 <- read.csv(paste0(dir10, "HR_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)

#edit datasets
HR <- rbindlist(list(HR_1, HR_2, HR_3, HR_4, HR_5, HR_6, HR_7, HR_8, HR_9, HR_10))
rm(HR_1, HR_2, HR_3, HR_4, HR_5, HR_6, HR_7, HR_8, HR_9, HR_10)
HR <- setDT(HR, keep.rownames=TRUE, key=NULL, check.names=FALSE)
HR$ftime <- anytime(HR$tod-7200, tz="UTC")
HR$fday <- day(HR$ftime)
HR <- HR[,index_day := .GRP, by = .(subject_id, fday)]
HR <- HR[, index_day:=index_day-index_day[1]+1L, by="subject_id"]
#HR <- HR %>% group_by(subject_id) %>% mutate(index_day = row_number(fday))
HR$is_baseline <- "False"
HR$is_baseline[HR$index_day < 2] <- "True"


#compare relative frequencies of HR data from baseline day vs all other days
HR_spread_plot <- ggplot(HR, aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')
print(HR_spread_plot)
rm(HR)

TEMP_1 <- read.csv(paste0(dir1, "TEMP_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
TEMP_2 <- read.csv(paste0(dir2, "TEMP_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
TEMP_3 <- read.csv(paste0(dir3, "TEMP_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
TEMP_4 <- read.csv(paste0(dir4, "TEMP_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
TEMP_5 <- read.csv(paste0(dir5, "TEMP_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
TEMP_6 <- read.csv(paste0(dir6, "TEMP_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
TEMP_7 <- read.csv(paste0(dir7, "TEMP_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
TEMP_8 <- read.csv(paste0(dir8, "TEMP_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
TEMP_9 <- read.csv(paste0(dir9, "TEMP_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
TEMP_10 <- read.csv(paste0(dir10, "TEMP_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)

#edit datasets
TEMP <- setDT(TEMP, keep.rownames=TRUE, key=NULL, check.names=FALSE)
TEMP$ftime <- anytime(TEMP$tod-7200, tz="UTC")
TEMP$fday <- day(TEMP$ftime)
TEMP <- TEMP[,index_day := .GRP, by = .(subject_id, fday)]
TEMP <- TEMP[, index_day:=index_day-index_day[1]+1L, by="subject_id"]
#HR <- HR %>% group_by(subject_id) %>% mutate(index_day = row_number(fday))
TEMP$is_baseline <- "False"
TEMP$is_baseline[TEMP$index_day < 2] <- "True"


#compare relative frequencies of HR data from baseline day vs all other days
TEMP_spread_plot <- ggplot(TEMP, aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')
print(TEMP_spread_plot)
rm(TEMP)

EDA_1 <- read.csv(paste0(dir1, "EDA_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
EDA_2 <- read.csv(paste0(dir2, "EDA_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
EDA_3 <- read.csv(paste0(dir3, "EDA_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
EDA_4 <- read.csv(paste0(dir4, "EDA_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
EDA_5 <- read.csv(paste0(dir5, "EDA_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
EDA_6 <- read.csv(paste0(dir6, "EDA_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
EDA_7 <- read.csv(paste0(dir7, "EDA_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
EDA_8 <- read.csv(paste0(dir8, "EDA_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
EDA_9 <- read.csv(paste0(dir9, "EDA_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
EDA_10 <- read.csv(paste0(dir10, "EDA_out.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)

#edit datasets
EDA <- rbindlist(list(EDA_1, EDA_2, EDA_3, EDA_4, EDA_5, EDA_6, EDA_7, EDA_8, EDA_9, EDA_10))
rm(EDA_1, EDA_2, EDA_3, EDA_4, EDA_5, EDA_6, EDA_7, EDA_8, EDA_9, EDA_10)
EDA <- setDT(EDA, keep.rownames=TRUE, key=NULL, check.names=FALSE)
EDA$ftime <- anytime(EDA$tod-7200, tz="UTC")
EDA$fday <- day(EDA$ftime)
EDA <- EDA[,index_day := .GRP, by = .(subject_id, fday)]
EDA <- EDA[, index_day:=index_day-index_day[1]+1L, by="subject_id"]
EDA$is_baseline <- "False"
EDA$is_baseline[EDA$index_day < 2] <- "True"


#compare relative frequencies of HR data from baseline day vs all other days
EDA_spread_plot <- ggplot(EDA, aes(measure, fill = is_baseline)) + 
  geom_density(alpha = 0.2)+
  facet_wrap(~subject_id, scale='free_x')
print(EDA_spread_plot)
rm(EDA)