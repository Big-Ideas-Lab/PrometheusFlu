#For DCC Running 1st time!
#Emilia Grzesiak

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

#setting up directories
dir1 = "../E4_processed/Cohort1"
dir2 = "../E4_processed/Cohort2"
dir3 = "../E4_processed/Cohort3"
dir4 = "../E4_processed/Cohort4"
dir5 = "../E4_processed/Cohort5"
dir6 = "../E4_processed/Cohort6"
dir7 = "../E4_processed/Cohort7"
dir8 = "../E4_processed/Cohort8"
dir9 = "../E4_processed/Cohort9"
dir10 = "../E4_processed/Cohort10"

ACC_1 <- read.csv(paste0(dir1, "ACC_out.csv", header = FALSE, sep = "\t"))
ACC_2 <- read.csv(paste0(dir2, "ACC_out.csv", header = FALSE, sep = "\t"))
ACC_3 <- read.csv(paste0(dir3, "ACC_out.csv", header = FALSE, sep = "\t"))
ACC_4 <- read.csv(paste0(dir4, "ACC_out.csv", header = FALSE, sep = "\t"))
ACC_5 <- read.csv(paste0(dir5, "ACC_out.csv", header = FALSE, sep = "\t"))
ACC_6 <- read.csv(paste0(dir6, "ACC_out.csv", header = FALSE, sep = "\t"))
ACC_7 <- read.csv(paste0(dir7, "ACC_out.csv", header = FALSE, sep = "\t"))
ACC_8 <- read.csv(paste0(dir8, "ACC_out.csv", header = FALSE, sep = "\t"))
ACC_9 <- read.csv(paste0(dir9, "ACC_out.csv", header = FALSE, sep = "\t"))
ACC_10 <- read.csv(paste0(dir10, "ACC_out.csv", header = FALSE, sep = "\t"))

HR_1 <- read.csv(paste0(dir1, "HR_out.csv", header = FALSE, sep = "\t"))
HR_2 <- read.csv(paste0(dir2, "HR_out.csv", header = FALSE, sep = "\t"))
HR_3 <- read.csv(paste0(dir3, "HR_out.csv", header = FALSE, sep = "\t"))
HR_4 <- read.csv(paste0(dir4, "HR_out.csv", header = FALSE, sep = "\t"))
HR_5 <- read.csv(paste0(dir5, "HR_out.csv", header = FALSE, sep = "\t"))
HR_6 <- read.csv(paste0(dir6, "HR_out.csv", header = FALSE, sep = "\t"))
HR_7 <- read.csv(paste0(dir7, "HR_out.csv", header = FALSE, sep = "\t"))
HR_8 <- read.csv(paste0(dir8, "HR_out.csv", header = FALSE, sep = "\t"))
HR_9 <- read.csv(paste0(dir9, "HR_out.csv", header = FALSE, sep = "\t"))
HR_10 <- read.csv(paste0(dir10, "HR_out.csv", header = FALSE, sep = "\t"))

#all cohort data in 1
allHR <- rbindlist(list(HR_1, HR_2, HR_3, HR_4, HR_5, HR_6, HR_7, HR_8, HR_9, HR_10))
allACC <- rbindlist(list(ACC_1, ACC_2, ACC_3, ACC_4, ACC_5, ACC_6, ACC_7, 
                         ACC_8, ACC_9, ACC_10))
allHR$ftime <- anytime(allHR$tod-7200, tz="UTC")
allACC$ftime <- anytime(allACC$tod-7200, tz="UTC")
allACC$sum_squares <- sqrt(allACC$ACC_x^2 + allACC$ACC_y^2 + allACC$ACC_z^2)

#Merge HR and ACC data
allHR[, join_time := ftime]
allACC[, join_time := ftime]
ACC_day <- allACC %>% group_by(subject_id) %>% 
  filter(as_date(join_time) == min(as_date(join_time)))
ACC_day <- setDT(ACC_day)
setkey(allHR, subject_id, join_time)
setkey(ACC_day, subject_id, join_time)

dt_steps_perSec <- ACC_day %>% group_by(subject_id, join_time=cut(join_time, "1 sec")) %>%
  dplyr::summarize(avg_per_sec = mean(sum_squares))
dt_steps_perSec$join_time <- as.POSIXct(dt_steps_perSec$join_time)
dt_steps_perSec <- data.table(dt_steps_perSec)
colnames(dt_steps_perSec) <- c("subject_id", "join_time", "avg_per_sec")

dt_steps_hr <- dt_steps_perSec[allHR, nomatch=FALSE, on = c("subject_id", "join_time")]
subject_id_list <- dt_steps_hr$subject_id %>% unique()
dt_steps_hr <- dt_steps_hr[,c("subject_id", "join_time", "avg_per_sec", "measure")]
dt_steps_hr <- dt_steps_hr[order(dt_steps_hr$join_time),]
colnames(dt_steps_hr) <- c("Id", "ActivityMin", "Steps", "Value")
setkey(dt_steps_hr, Id, ActivityMin)

#Chentian's code
boxplot <- boxplot(Value ~ Id, dt_steps_hr)
# 
# # progress bar
pb <- txtProgressBar(min = 0, max = length(boxplot$names), style = 3)
i = 1
for (group_num in 1:length(boxplot$names)) {
  id <- boxplot$names[group_num]
  out <- boxplot$out[boxplot$group == group_num]
  
  #   # want to remove outliers for this participant, i.e. rows where Id == id and Value %in% out
  #   # thus, we will keep the complement:
  dt_steps_hr <- dt_steps_hr[!((Id == id) & (Value %in% out))]
  
  setTxtProgressBar(pb, i)
  i = i + 1
}
close(pb)


show_linePerGroup <- function(dt, x_colStr, y_colStr, group_colStr = NULL) {
  # returns a ggplot line plot (one line per unique value in the column with name group_colStr)
  
  # dt : data.table
  # x_colStr: name (character) of variable to plot on the x-axis
  # y_colStr: name (character) of variable to plot on the y-axis
  # group_colStr: name (character) of variable for which we want to plot one line per unique value
  
  plt <- 
    ggplot(data = dt) + 
    geom_line(mapping = aes_string(x = x_colStr, y = y_colStr, group = group_colStr, color = group_colStr)) +
    theme(legend.position = "none") 
  
  return(plt)
}


gridSearch_diff_window_steps <- function(dt_steps_hr, window_size_list, steps_threshold_list) {
  # this function searches for the largest sensitivity value produced from all combinations of parameters
  # specified by window_size_list and steps_threshold_list.
  
  # initialize table with all possible combinations of the step thresholds and window sizes specified above
  dt_results <- data.table(steps_threshold = rep(steps_threshold_list, each = length(window_size_list)),
                           window_size = window_size_list,  # will be recycled
                           sensitivity = as.numeric(rep(NA, length(steps_threshold_list)*length(window_size_list))))
  
  # range of HR values for each participant
  dt_rangeHR_byId <- dt_steps_hr[, 
                                 .(rangeHR = range(Value, na.rm = TRUE)[2] - range(Value, na.rm = TRUE)[1]), 
                                 by = Id]
  
  num_participants <- length(dt_rangeHR_byId$Id)
  
  # progress bar
  pb <- txtProgressBar(min = 0, max = nrow(dt_results), style = 3)
  i = 1
  for (n in window_size_list) {
    # for each row, keep track of the rolling sum of num. steps within a time window that occurs BEFORE the current row
    # (the table is already sorted by Id and then sorted by ActivityMin within each unique Id value)
    # (the Steps column has no missing values: `dt_steps_hr$Steps %>% is.na() %>% any()`` returns FALSE)
    
    # cleanup
    #dt_steps_hr[, rolling_sum_steps := NULL]
    
    dt_steps_hr[, 
                rolling_sum_steps := roll_mean(Steps, n, align = "right", fill = NA),
                by = Id]
    
    for (m in steps_threshold_list) {
      # summary HR where rolling_sum_steps <= steps_threshold for each participant
      dt_low_summaryHR_byId <- dt_steps_hr[rolling_sum_steps <= m,
                                           .(low_summaryHR = median(Value, na.rm = TRUE)),
                                           by = Id]
      
      # summary HR where rolling_sum_steps > steps_threshold for each participant
      dt_high_summaryHR_byId <- dt_steps_hr[rolling_sum_steps > m,
                                            .(high_summaryHR = median(Value, na.rm = TRUE)),
                                            by = Id]
      
      # only consider participants with who have associated values in BOTH the "low" and "high" tables above
      id_list <- intersect(dt_low_summaryHR_byId$Id, dt_high_summaryHR_byId$Id)
      dt_id <- data.table(Id = id_list)
      
      if (nrow(dt_id) >= ceiling(num_participants/2)) {
        # continue only if we can calculate sensitivity for at least half of the participants
        
        dt_low_summaryHR_byId <- dt_low_summaryHR_byId[dt_id, on = "Id"]
        dt_high_summaryHR_byId <- dt_high_summaryHR_byId[dt_id, on = "Id"]
        
        dt_combined <- copy(dt_rangeHR_byId)
        dt_combined <- dt_combined[dt_id, on = "Id"]
        
        stopifnot(all(dt_low_summaryHR_byId$Id == dt_high_summaryHR_byId$Id) && all(dt_low_summaryHR_byId$Id == dt_combined$Id))
        
        dt_combined[, low_summaryHR := dt_low_summaryHR_byId$low_summaryHR]
        dt_combined[, high_summaryHR := dt_high_summaryHR_byId$high_summaryHR]
        
        # difference between high_summaryHR (above threshold) and low_summaryHR (below threshold) for each participant
        dt_combined[, diff_summaryHR := high_summaryHR - low_summaryHR]
        
        # how significant is this difference compared to the range of HR values for each participant?
        # --> find ratio of diff_summaryHR/rangeHR for each participant
        dt_combined[, ratio_diff_range := diff_summaryHR/rangeHR]
        
        # average over this ratio to find an overall measure for how much this window-threshold combination
        # causes change in HR values between HR values for steps below the threshold and HR values for steps above the threshold
        # let's call this measure "sensitivity"
        sensitivity <- dt_combined[, mean(ratio_diff_range)]
        
        # append to results table
        dt_results[window_size == n & steps_threshold == m]$sensitivity <- sensitivity
      } else {
        dt_results[window_size == n & steps_threshold == m]$sensitivity <- NA
      }
      
      setTxtProgressBar(pb, i)
      i = i + 1
    }
  }
  
  close(pb)
  
  return(dt_results)
}

gridSearch_deviation_window_steps <- function(dt_steps_hr, window_size_list, steps_threshold_list) {
  # initialize table with all possible combinations of the step thresholds and window sizes specified above
  dt_results <- data.table(steps_threshold = rep(steps_threshold_list, each = length(window_size_list)),
                           window_size = window_size_list,  # will be recycled
                           RHR_dev = as.numeric(rep(NA, length(steps_threshold_list)*length(window_size_list))),
                           notRHR_dev = as.numeric(rep(NA, length(steps_threshold_list)*length(window_size_list))),
                           RHR_median = as.numeric(rep(NA, length(steps_threshold_list)*length(window_size_list))),
                           notRHR_median = as.numeric(rep(NA, length(steps_threshold_list)*length(window_size_list)))
  )
  
  num_participants <- dt_steps_hr$Id %>% unique() %>% length()
  
  # progress bar
  pb <- txtProgressBar(min = 0, max = nrow(dt_results), style = 3)
  i = 1
  for (n in window_size_list) {
    # cleanup
    #dt_steps_hr[, rolling_sum_steps := NULL]
    
    dt_steps_hr[, 
                rolling_sum_steps := roll_mean(Steps, n, align = "right", fill = NA),
                by = Id]
    
    # TODO: get rid of multi-id logic
    for (m in steps_threshold_list) {
      # summary HR where rolling_sum_steps <= steps_threshold for each participant
      dt_low_deviationHR_byId <- dt_steps_hr[rolling_sum_steps <= m,
                                             .(low_deviationHR = sd(Value, na.rm = TRUE)),
                                             by = Id]
      
      # summary HR where rolling_sum_steps > steps_threshold for each participant
      dt_high_deviationHR_byId <- dt_steps_hr[rolling_sum_steps > m,
                                              .(high_deviationHR = sd(Value, na.rm = TRUE)),
                                              by = Id]
      
      # only consider participants with who have associated values in BOTH the "low" and "high" tables above
      # id_list <- intersect(dt_low_deviationHR_byId$Id, dt_high_deviationHR_byId$Id)
      # dt_id <- data.table(Id = id_list)
      
      # if (nrow(dt_id) >= ceiling(num_participants/2)) {
      dt_low_deviationHR_byId <- dt_low_deviationHR_byId  #[dt_id, on = "Id"]
      dt_high_deviationHR_byId <- dt_high_deviationHR_byId  #[dt_id, on = "Id"]
      
      if (nrow(dt_low_deviationHR_byId) == 1) {
        dt_results[window_size == n & steps_threshold == m]$RHR_dev <- dt_low_deviationHR_byId$low_deviationHR 
      }
      
      if (nrow(dt_high_deviationHR_byId) == 1) {
        dt_results[window_size == n & steps_threshold == m]$notRHR_dev <- dt_high_deviationHR_byId$high_deviationHR 
      }
      
      dt_low_medianHR <- dt_steps_hr[rolling_sum_steps <= m,
                                     .(low_medianHR = median(Value, na.rm = TRUE)),
                                     by = Id]
      dt_high_medianHR <-  dt_steps_hr[rolling_sum_steps > m,.
                                       (high_medianHR = median(Value, na.rm = TRUE)),
                                       by = Id]
      
      if (nrow(dt_low_medianHR) == 1) {
        dt_results[window_size == n & steps_threshold == m]$RHR_median <- dt_low_medianHR$low_medianHR
      }
      
      if (nrow(dt_high_medianHR) == 1) {
        dt_results[window_size == n & steps_threshold == m]$notRHR_median <- dt_high_medianHR$high_medianHR 
      }
      
      setTxtProgressBar(pb, i)
      i = i + 1
    }
  }
  
  close(pb)
  return(dt_results)
}

# Best window + step threshold combination for each participant ----
# TODO: comparison across arms
gridSearch_savePlot_perId <- function(id_list, gridSearch_func, window_size_list, steps_threshold_list, soft = TRUE, save = FALSE) {
  gridSearch_func_name <- substitute(gridSearch_func) %>% as.character()
  
  plots <- list()
  dt_metrics <- data.table(Id = id_list,
                           penalty = as.numeric(rep(NA, length(id_list))),
                           steps_threshold = as.numeric(rep(NA, length(id_list))),
                           window_size = as.numeric(rep(NA, length(id_list))),
                           RHR_median = as.numeric(rep(NA, length(id_list))),
                           notRHR_median = as.numeric(rep(NA, length(id_list))),
                           RHR_mean = as.numeric(rep(NA, length(id_list))),
                           notRHR_mean = as.numeric(rep(NA, length(id_list))),
                           RHR_size = as.numeric(rep(NA, length(id_list))),
                           notRHR_size = as.numeric(rep(NA, length(id_list))),
                           RHR_max = as.numeric(rep(NA, length(id_list))),
                           notRHR_max = as.numeric(rep(NA, length(id_list))),
                           RHR_min = as.numeric(rep(NA, length(id_list))),
                           notRHR_min = as.numeric(rep(NA, length(id_list)))
  )
  for(i in 1:length(id_list)) {
    id <- id_list[i]
    print(sprintf("Searching for participant with id %s", id))
    
    if (soft == TRUE) {
      softmin <- dt_steps_hr[Id == id]$Value %>% quantile(0.05, na.rm = TRUE)
      softmax <- dt_steps_hr[Id == id]$Value %>% quantile(0.95, na.rm = TRUE)
      
      dt_results <- gridSearch_func(dt_steps_hr[Id == id & Value < softmax & Value > softmin], window_size_list, steps_threshold_list)  
    } else {
      dt_results <- gridSearch_func(dt_steps_hr[Id == id], window_size_list, steps_threshold_list)  
    }
    
    best_index <- dt_results[, 3] %>% unlist() %>% as.numeric() %>% which.min()
    dt_best <- dt_results[best_index]
    print(dt_best)
    
    window_size <- dt_best$window_size
    steps_threshold <- dt_best$steps_threshold
    
    #dt_metrics[Id == id]$penalty <- dt_best[, 3]
    #dt_metrics[Id == id]$steps_threshold <- steps_threshold
    #dt_metrics[Id == id]$window_size <- window_size
    
    #dt_steps_hr[, rolling_sum_steps := NULL]
    dt_steps_hr[Id == id, rolling_sum_steps := roll_mean(Steps, window_size, align = "right", fill = NA)]
    
    #dt_steps_hr[, isRHR := NULL]
    dt_steps_hr[(Id == id) & (rolling_sum_steps <= steps_threshold),
                isRHR := TRUE]
    dt_steps_hr[(Id == id) & (rolling_sum_steps > steps_threshold),
                isRHR := FALSE]
    
    # median
    dt_metrics[Id == id]$RHR_median <- dt_steps_hr[(Id == id) & isRHR == TRUE & !is.na(Value)]$Value %>% median()
    dt_metrics[Id == id]$notRHR_median <- dt_steps_hr[(Id == id) & isRHR == FALSE & !is.na(Value)]$Value %>% median()
    
    # mean
    dt_metrics[Id == id]$RHR_mean <- dt_steps_hr[(Id == id) & isRHR == TRUE & !is.na(Value)]$Value %>% mean()
    dt_metrics[Id == id]$notRHR_mean <- dt_steps_hr[(Id == id) & isRHR == FALSE & !is.na(Value)]$Value %>% mean()
    
    # size
    dt_metrics[Id == id]$RHR_size <- dt_steps_hr[(Id == id) & isRHR == TRUE & !is.na(Value)]$Value %>% length()
    dt_metrics[Id == id]$notRHR_size <- dt_steps_hr[(Id == id) & isRHR == FALSE & !is.na(Value)]$Value %>% length()
    
    # min
    dt_metrics[Id == id]$RHR_min <- dt_steps_hr[(Id == id) & isRHR == TRUE & !is.na(Value)]$Value %>% min()
    dt_metrics[Id == id]$notRHR_min <- dt_steps_hr[(Id == id) & isRHR == FALSE & !is.na(Value)]$Value %>% min()
    
    # max
    dt_metrics[Id == id]$RHR_max <- dt_steps_hr[(Id == id) & isRHR == TRUE & !is.na(Value)]$Value %>% max()
    dt_metrics[Id == id]$notRHR_max <- dt_steps_hr[(Id == id) & isRHR == FALSE & !is.na(Value)]$Value %>% max()
    
    # plots
    plots[[(i-1)*2 + 1]] <- show_linePerGroup(dt_steps_hr[isRHR == TRUE & Id == id], "ActivityMin", "Value", "Id") +
      scale_color_manual(values = "#66C2A5") +
      ylim(c(20, 220)) +
      theme(plot.title = element_text(hjust = 0.5, size = 9), axis.title = element_text(size = 7)) +
      labs(title = sprintf("%s: estimated RHR, window=%f, steps=%f", id, window_size, steps_threshold),
           x = "Minutes",
           y = "Heart Rate")
    
    if (save == TRUE) {
      sprintf("%s_window=%f_steps=%f_isRHR=TRUE_%s_soft=%s.png", id, window_size, steps_threshold, gridSearch_func_name, as.character(soft)) %>%
        ggsave(path = "./temp_plots/", width = 8, height = 6, units = "in")
    }
    
    plots[[(i-1)*2 + 2]] <- show_linePerGroup(dt_steps_hr[isRHR == FALSE & Id == id], "ActivityMin", "Value", "Id") +
      scale_color_manual(values = "#FC8D62") +
      ylim(c(20, 220)) +
      theme(plot.title = element_text(hjust = 0.5, size = 9), axis.title = element_text(size = 7)) +
      labs(title = sprintf("%s: estimated regular HR: window=%f, steps=%f", id, window_size, steps_threshold),
           x = "Minutes",
           y = "Heart Rate")
    
    if (save == TRUE) {
      sprintf("%s_window=%f_steps=%f_isRHR=FALSE_%s_soft=%s.png", id, window_size, steps_threshold, gridSearch_func_name, as.character(soft)) %>%
        ggsave(path = "./temp_plots/", width = 8, height = 6, units = "in")
    }
  }
  
  return(list("plots" = plots, "dt_metrics" = dt_metrics))
}

# search ----
# sample 3 participants
#set.seed(0)
id_sample <-unique(allHR$subject_id)
#id_sample <- c("PROM001")

# step threshold search over hundreds
window_size_list <- as.integer(c(1, 2, 5, 10, 30, 60, 120)*60)  # seconds
#window_size_list <- c(1*60)
steps_threshold_list <- seq(60, 80, .1)  # num. steps

plots_metrics_list <- gridSearch_savePlot_perId(id_sample, gridSearch_deviation_window_steps, window_size_list, steps_threshold_list, save = FALSE)
