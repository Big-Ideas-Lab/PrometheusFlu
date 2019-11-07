# Removing Outliers from HR and TEMP

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
library("pracma")

HR <- fread(file="/Users/emiliagrzesiak/Cohort_Combined/HR.csv",
            header = TRUE, sep = ",")
TEMP <- fread(file="/Users/emiliagrzesiak/Cohort_Combined/TEMP.csv",
              header = TRUE, sep = ",")

HR <- setDT(HR, keep.rownames=TRUE, key=NULL, check.names=FALSE)
TEMP <- setDT(TEMP, keep.rownames=TRUE, key=NULL, check.names=FALSE)

HR_1 <- HR[HR$subject_id %in% c("PROM001", "PROM002", "PROM003", "PROM004"),]

outlierMAD <- function (x, k){
  n <- length(x)
  y <- x
  ind <- c()
  L <- 1.4826
  t0 <- 5
  for (i in (k + 1):(n - k)) {
    x0 <- median(x[(i - k):(i + k)])
    S0 <- L * median(abs(x[(i - k):(i + k)] - x0))
    if (abs(x[i] - x0) > t0 * S0) {
      y[i] <- x0
      ind <- c(ind, i)
    }
  }
  list(y = y, ind = ind)
}

#removes and replaces outliers per cohort 
HR_out_free<- HR_1 %>% 
  group_by(subject_id) %>%
  do(data.table(subject_id = HR_1$subject_id, tod = HR_1$tod,
                measure=(outlierMAD(HR_1$measure, 300)[["y"]])))

#function to isolate one subject's data to remove & replace outliers

# function(HR_cohortNum, subjectID, outlierMAD){
#   NoOut_HR <- outlierMAD(HR_cohortNum[HR_cohortNum$subject_id %in% subjectID]$measure, 300)
#   return(NoOut_HR)
# }

time_window <- 43200 #one minute to one hour
n= length(time_window)
# test_func <- function(HR_cohortNum, subjectID, window, outlierMAD){
#   result <- matrix(NA, nrow=length(window), ncol=2)
#   NoOut_HR <- list()
#   for (i in 1:n){
#    NoOut_HR[i,] <- outlierMAD(HR_cohortNum[HR_cohortNum$subject_id %in% subjectID]$measure, window[i])
#    result <- NoOut_HR
#   }
#   return(result)
# }

#testing

sens_function <- function(HR_cohortNum, subjectID, window,outlierMAD){
  sens <- data.table()
  #NoOut_HR <- list()
  number_outliers <- vector("numeric", n)
  for (i in 1:n){
   NoOut_HR <- outlierMAD(HR_cohortNum[HR_cohortNum$subject_id %in% subjectID]$measure, window[i])
   number_outliers[i] <- length(NoOut_HR[["ind"]])
  }
  sens <- data.table(time_window_size = window, n_outliers = number_outliers)
  return(sens)
}

sensitivity_HRp1<- sens_function(HR_1, "PROM001", time_window, outlierMAD)

sens_pers1 <- data.frame(time_window_size = c(60, 120, 180, 240, 300, 360, 420, 480, 540, 600, 660, 720,
                                780, 840, 900, 960, 1020, 1080, 1140, 1200, 1260, 1320, 1380,
                                1440, 1500, 1560, 1620, 1680, 1740, 1800), 
           n_outliers = c(147, 2260, 5302, 6890, 8503, 9402, 9756, 9926, 10337, 10376, 10308, 10120, 
                          10168, 9918, 9954, 9946, 9905, 9849, 9984, 10213, 10154, 10172, 10353,
                          10356, 10421, 10448, 10631, 10838, 11004, 11082))

sens_plot <- ggplot(sens_pers1, aes(x= time_window_size, y=n_outliers))+
  geom_point()+
  theme_bw()+
  xlab("Time window (in seconds)")+
  ylab("Number of outliers replaced")
print(sens_plot)

#test_func_ans <- test_func(HR_1, "PROM001", time_window, outlierMAD)

HR_1 <- HR[HR$subject_id %in% c("PROM001", "PROM002", "PROM003", "PROM004"),]
HR_2 <- HR[HR$subject_id %in% c("PROM005", "PROM006", "PROM007"),]
HR_3 <- HR[HR$subject_id %in% c("PROM008", "PROM009", "PROM010", "PROM011"),]
HR_4 <- HR[HR$subject_id %in% c("PROM012", "PROM013"),]
HR_5 <- HR[HR$subject_id %in% c("PROM014", "PROM015", "PROM016", "PROM017"),]
HR_6 <- HR[HR$subject_id %in% c("PROM018", "PROM019", "PROM020"),]
HR_7 <- HR[HR$subject_id %in% c("PROM021", "PROM022", "PROM023", "PROM024", "PROM025"),]
HR_8 <- HR[HR$subject_id %in% c("PROM026", "PROM027"),]
HR_9 <- HR[HR$subject_id %in% c("PROM028", "PROM029", "PROM030", "PROM031", "PROM032", "PROM033"),]
HR_10 <- HR[HR$subject_id %in% c("PROM034", "PROM035", "PROM036", "PROM037", "PROM038", "PROM039"),]

HRp1_noOut <- replaceOutCoh(HR_1, "PROM001", outlierMAD)
HRp2_noOut <- replaceOutCoh(HR_1, "PROM002", outlierMAD)
HRp3_noOut <- replaceOutCoh(HR_1, "PROM003", outlierMAD)
HRp4_noOut <- replaceOutCoh(HR_1, "PROM004", outlierMAD)
HRp5_noOut <- replaceOutCoh(HR_2, "PROM005", outlierMAD)
HRp6_noOut <- replaceOutCoh(HR_2, "PROM006", outlierMAD)
HRp7_noOut <- replaceOutCoh(HR_2, "PROM007", outlierMAD)
HRp8_noOut <- replaceOutCoh(HR_3, "PROM008", outlierMAD)
HRp9_noOut <- replaceOutCoh(HR_3, "PROM009", outlierMAD)
HRp10_noOut <- replaceOutCoh(HR_3, "PROM010", outlierMAD)
HRp11_noOut <- replaceOutCoh(HR_3, "PROM011", outlierMAD)
HRp12_noOut <- replaceOutCoh(HR_4, "PROM012", outlierMAD)
HRp13_noOut <- replaceOutCoh(HR_4, "PROM013", outlierMAD)
HRp14_noOut <- replaceOutCoh(HR_5, "PROM014", outlierMAD)
HRp15_noOut <- replaceOutCoh(HR_5, "PROM015", outlierMAD)
HRp16_noOut <- replaceOutCoh(HR_5, "PROM016", outlierMAD)
HRp17_noOut <- replaceOutCoh(HR_5, "PROM017", outlierMAD)
HRp18_noOut <- replaceOutCoh(HR_6, "PROM018", outlierMAD)
HRp19_noOut <- replaceOutCoh(HR_6, "PROM019", outlierMAD)
HRp20_noOut <- replaceOutCoh(HR_6, "PROM020", outlierMAD)
HRp21_noOut <- replaceOutCoh(HR_7, "PROM021", outlierMAD)
HRp22_noOut <- replaceOutCoh(HR_7, "PROM022", outlierMAD)
HRp23_noOut <- replaceOutCoh(HR_7, "PROM023", outlierMAD)
HRp24_noOut <- replaceOutCoh(HR_7, "PROM024", outlierMAD)
HRp25_noOut <- replaceOutCoh(HR_7, "PROM025", outlierMAD)
HRp26_noOut <- replaceOutCoh(HR_8, "PROM026", outlierMAD)
HRp27_noOut <- replaceOutCoh(HR_8, "PROM027", outlierMAD)
HRp28_noOut <- replaceOutCoh(HR_9, "PROM028", outlierMAD)
HRp29_noOut <- replaceOutCoh(HR_9, "PROM029", outlierMAD)
HRp30_noOut <- replaceOutCoh(HR_9, "PROM030", outlierMAD)
HRp31_noOut <- replaceOutCoh(HR_9, "PROM031", outlierMAD)
HRp32_noOut <- replaceOutCoh(HR_9, "PROM032", outlierMAD)
HRp33_noOut <- replaceOutCoh(HR_9, "PROM033", outlierMAD)
HRp34_noOut <- replaceOutCoh(HR_10, "PROM034", outlierMAD)
HRp35_noOut <- replaceOutCoh(HR_10, "PROM035", outlierMAD)
HRp36_noOut <- replaceOutCoh(HR_10, "PROM036", outlierMAD)
HRp37_noOut <- replaceOutCoh(HR_10, "PROM037", outlierMAD)
HRp38_noOut <- replaceOutCoh(HR_10, "PROM038", outlierMAD)
HRp39_noOut <- replaceOutCoh(HR_10, "PROM039", outlierMAD)



HR_p1 <- HR[HR$subject_id %in% "PROM001"]
HR_p1NO <- outlierMAD(HR_p1$measure, k= 300) #10 min window (bc window -k to k)
HR_p1NOx <- data.frame(tod = HR_p1$tod, measure = HR_p1NO[["y"]])

p1_nofilt <- ggplot(HR_p1, aes(tod, measure))+
  geom_line()
print(p1_nofilt)

p1_filt <- ggplot(HR_p1NOx, aes(tod, measure))+
  geom_line()
print(p1_filt)

dailyHRbox <- function(HR_cohortNum, subjectID, graph_name_output){
  #HR_cohortNum$ftime <- anytime(HR_cohortNum$tod-7200, tz="UTC")
  graph_name_output <- ggplot(HR_cohortNum[HR_cohortNum$subject_id == subjectID,], 
                              aes(x = format(ftime, format = "%d"), 
                                  y = measure, group = format(ftime, format = "%d"))) + 
    geom_boxplot(fill = "grey80", colour = "#3366FF", outlier.alpha = 1/40, outlier.size=.5) +
    #stat_summary(fun.data = n_fun, geom = "text", 
    #fun.y = median, colour = "black", size= 2, fontface = "bold") +
    theme_bw()+
    theme(axis.text.x= element_text(size=5))+
    #stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red") +
    xlab("Day of Sept") +
    ylab ("HR (bpm)")
  return(graph_name_output) }