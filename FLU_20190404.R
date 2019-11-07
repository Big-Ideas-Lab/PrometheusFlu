#03/23/19
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

#all data
HR <- fread(file="/Users/emiliagrzesiak/Cohort_Combined/HR.csv",
                  header = TRUE, sep = ",")
TEMP <- fread(file="/Users/emiliagrzesiak/Cohort_Combined/TEMP.csv",
                     header = TRUE, sep = ",")
INOC <- fread(file="/Users/emiliagrzesiak/Cohort_Combined/INOC.csv",
              header = TRUE, sep = ",")
QRY <- fread(file="/Users/emiliagrzesiak/Cohort_Combined/qry_e4_sessionOnset_wInnoc.csv",
              header = TRUE, sep = "\t")
VW <-  fread(file="/Users/emiliagrzesiak/Cohort_Combined/vw_E4_ATTRIBUTE.csv",
             header = TRUE, sep = "\t")

#just cohort 1
ACC_1 <- fread(file="/Users/emiliagrzesiak/Cohort1/ACC_out.csv",
            header = FALSE, sep = "\t")
#BVP_1 <- fread(file="/Users/emiliagrzesiak/Cohort1/BVP_out.csv",
              #header = TRUE, sep = ",")
EDA_1 <- fread(file="/Users/emiliagrzesiak/Cohort1/EDA_out.csv",
              header = TRUE, sep = "\t")
E4_1 <- fread(file="/Users/emiliagrzesiak/Cohort1/E4ATTR_out.csv",
              header = TRUE, sep = "|")

#data prepping
cols_HR <- colnames(HR)
cols_HR
tail(HR)
HR <- setDT(HR, keep.rownames=TRUE, key=NULL, check.names=FALSE)
cols_TEMP <- colnames(TEMP)
cols_TEMP
tail(TEMP)
TEMP <- setDT(TEMP, keep.rownames=TRUE, key=NULL, check.names=FALSE)
cols_INOC <- colnames(INOC)
cols_INOC
INOC <- setDT(INOC, keep.rownames=TRUE, key=NULL, check.names=FALSE)
cols_QRY <- colnames(QRY)
cols_QRY
tail(QRY)
QRY <- setDT(QRY, keep.rownames=TRUE, key=NULL, check.names=FALSE)
cols_VW <- colnames(VW)
cols_VW
tail(VW)
VW <- setDT(VW, keep.rownames=TRUE, key=NULL, check.names=FALSE)
cols_ACC1 <- colnames(ACC_1)
cols_ACC1
tail(ACC_1)
ACC_1 <- setDT(ACC_1, keep.rownames=TRUE, key=NULL, check.names=FALSE)

HR$ftime <- anytime(HR$tod-7200, tz="UTC")
#HR$date <- as.POSIXct(HR$tod, origin="1970-01-01")
# DAILY HR BOXPLOTS ALL USERS

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
HRsub1_graph <- dailyHRbox(HR_1, "PROM001", HRsub1_graph)
HRsub2_graph <- dailyHRbox(HR_1, "PROM002", HRsub2_graph)
HRsub3_graph <- dailyHRbox(HR_1, "PROM003", HRsub3_graph)
HRsub4_graph <- dailyHRbox(HR_1, "PROM004", HRsub4_graph)
HRsub5_graph <- dailyHRbox(HR_2, "PROM005", HRsub5_graph)
HRsub6_graph <- dailyHRbox(HR_2, "PROM006", HRsub6_graph)
HRsub7_graph <- dailyHRbox(HR_2, "PROM007", HRsub7_graph)
HRsub8_graph <- dailyHRbox(HR_3, "PROM008", HRsub8_graph)
HRsub9_graph <- dailyHRbox(HR_3, "PROM009", HRsub9_graph)
HRsub10_graph <- dailyHRbox(HR_3, "PROM010", HRsub10_graph)
HRsub11_graph <- dailyHRbox(HR_3, "PROM011", HRsub11_graph)
HRsub12_graph <- dailyHRbox(HR_4, "PROM012", HRsub12_graph)
HRsub13_graph <- dailyHRbox(HR_4, "PROM013", HRsub13_graph)
HRsub14_graph <- dailyHRbox(HR_5, "PROM014", HRsub14_graph)
HRsub15_graph <- dailyHRbox(HR_5, "PROM015", HRsub15_graph)
HRsub16_graph <- dailyHRbox(HR_5, "PROM016", HRsub16_graph)
HRsub17_graph <- dailyHRbox(HR_5, "PROM017", HRsub17_graph)
HRsub18_graph <- dailyHRbox(HR_6, "PROM018", HRsub18_graph)
HRsub19_graph <- dailyHRbox(HR_6, "PROM019", HRsub19_graph)
HRsub20_graph <- dailyHRbox(HR_6, "PROM020", HRsub20_graph)
HRsub21_graph <- dailyHRbox(HR_7, "PROM021", HRsub21_graph)
HRsub22_graph <- dailyHRbox(HR_7, "PROM022", HRsub22_graph)
HRsub23_graph <- dailyHRbox(HR_7, "PROM023", HRsub23_graph)
HRsub24_graph <- dailyHRbox(HR_7, "PROM024", HRsub24_graph)
HRsub25_graph <- dailyHRbox(HR_7, "PROM025", HRsub25_graph)
HRsub26_graph <- dailyHRbox(HR_8, "PROM026", HRsub26_graph)
HRsub27_graph <- dailyHRbox(HR_8, "PROM027", HRsub27_graph)
HRsub28_graph <- dailyHRbox(HR_9, "PROM028", HRsub28_graph)
HRsub29_graph <- dailyHRbox(HR_9, "PROM029", HRsub29_graph)
HRsub30_graph <- dailyHRbox(HR_9, "PROM030", HRsub30_graph)
HRsub31_graph <- dailyHRbox(HR_9, "PROM031", HRsub31_graph)
HRsub32_graph <- dailyHRbox(HR_9, "PROM032", HRsub32_graph)
HRsub33_graph <- dailyHRbox(HR_9, "PROM033", HRsub33_graph)
HRsub34_graph <- dailyHRbox(HR_10, "PROM034", HRsub34_graph)
HRsub35_graph <- dailyHRbox(HR_10, "PROM035", HRsub35_graph)
HRsub36_graph <- dailyHRbox(HR_10, "PROM036", HRsub36_graph)
HRsub37_graph <- dailyHRbox(HR_10, "PROM037", HRsub37_graph)
HRsub38_graph <- dailyHRbox(HR_10, "PROM038", HRsub38_graph)
HRsub39_graph <- dailyHRbox(HR_10, "PROM039", HRsub39_graph)

print(ggdraw() + draw_plot(HRsub1_graph, x= 0, y=.8, width = .125, height=.2) +
        draw_plot(HRsub2_graph, x=0, y=.6, width = .125, height=.2)+
        draw_plot(HRsub3_graph, x= 0, y=.4, width = .125, height=.2)+
        draw_plot(HRsub4_graph, x= 0, y=.2, width = .125, height=.2)+
        draw_plot(HRsub5_graph, x= 0, y=0, width = .125, height=.2)+
        draw_plot(HRsub6_graph, x= 0.125, y=.8, width = .125, height=.2)+
        draw_plot(HRsub7_graph, x= 0.125, y=.6, width = .125, height=.2)+
        draw_plot(HRsub8_graph, x= 0.125, y=.4, width = .125, height=.2)+
        draw_plot(HRsub9_graph, x= 0.125, y=.2, width = .125, height=.2)+
        draw_plot(HRsub10_graph, x= 0.125, y=0, width = .125, height=.2)+
        draw_plot(HRsub11_graph, x= 0.25, y=.8, width = .125, height=.2)+
        draw_plot(HRsub12_graph, x= 0.25, y=.6, width = .125, height=.2)+
        draw_plot(HRsub13_graph, x= 0.25, y=.4, width = .125, height=.2)+
        draw_plot(HRsub14_graph, x= 0.25, y=.2, width = .125, height=.2)+
        draw_plot(HRsub15_graph, x= 0.25, y=0, width = .125, height=.2)+
        draw_plot(HRsub16_graph, x= 0.375, y=.8, width = .125, height=.2)+
        draw_plot(HRsub17_graph, x= 0.375, y=.6, width = .125, height=.2)+
        draw_plot(HRsub18_graph, x= 0.375, y=.4, width = .125, height=.2)+
        draw_plot(HRsub19_graph, x= 0.375, y=.2, width = .125, height=.2)+
        draw_plot(HRsub20_graph, x= 0.375, y=0, width = .125, height=.2)+
        draw_plot(HRsub21_graph, x= 0.5, y=.8, width = .125, height=.2)+
        draw_plot(HRsub22_graph, x= 0.5, y=.6, width = .125, height=.2)+
        draw_plot(HRsub23_graph, x= 0.5, y=.4, width = .125, height=.2)+
        draw_plot(HRsub24_graph, x= 0.5, y=.2, width = .125, height=.2)+
        draw_plot(HRsub25_graph, x= 0.5, y=0, width = .125, height=.2)+
        draw_plot(HRsub26_graph, x= 0.625, y=.8, width = .125, height=.2)+
        draw_plot(HRsub27_graph, x= 0.625, y=.6, width = .125, height=.2)+
        draw_plot(HRsub28_graph, x= 0.625, y=.4, width = .125, height=.2)+
        draw_plot(HRsub29_graph, x= 0.625, y=.2, width = .125, height=.2)+
        draw_plot(HRsub30_graph, x= 0.625, y=0, width = .125, height=.2)+
        draw_plot(HRsub31_graph, x= 0.75, y=.8, width = .125, height=.2)+
        draw_plot(HRsub32_graph, x= 0.75, y=.6, width = .125, height=.2)+
        draw_plot(HRsub33_graph, x= 0.75, y=.4, width = .125, height=.2)+
        draw_plot(HRsub34_graph, x= 0.75, y=.2, width = .125, height=.2)+
        draw_plot(HRsub35_graph, x= 0.75, y=0, width = .125, height=.2)+
        draw_plot(HRsub36_graph, x= 0.875, y=.8, width = .125, height=.2)+
        draw_plot(HRsub37_graph, x= 0.875, y=.6, width = .125, height=.2)+
        draw_plot(HRsub38_graph, x= 0.875, y=.4, width = .125, height=.2)+
        draw_plot(HRsub39_graph, x= 0.875, y=.2, width = .125, height=.2))

# ACCELEROMETRY ANALYSIS
ACC_1$sum_squares <- sqrt(ACC_1$V3^2 + ACC_1$V4^2 + ACC_1$V5^2)
ACC_1$ftime <- anytime(ACC_1$V2-7200, tz="UTC")
#ACC_1$date <- as.POSIXct(ACC_1$V2, origin="1970-01-01")

  ##accelerometer box plots per hour for cohort 1
hourlyAccbox <- function(Acc_cohortNum, subjectID, graph_name_output){
  #HR_cohortNum$ftime <- anytime(HR_cohortNum$tod-7200, tz="UTC")
  graph_name_output <- ggplot(Acc_cohortNum[Acc_cohortNum$V1 == subjectID,], 
                              aes(x = format(ftime,"%d:%H"), 
                                  y = sum_squares, group=format(ftime,"%d:%H")))+ 
    geom_boxplot(fill = "grey80", colour = "#3366FF", outlier.alpha = 1/40, outlier.size=.5) +
    #stat_summary(fun.data = n_fun, geom = "text", 
    #fun.y = median, colour = "black", size= 2, fontface = "bold") +
    theme_bw()+
    #scale_x_discrete(labels=format.Date(ACC_1$ftime, "%H"))+
    theme(axis.text.x= element_text(size=7, angle=-90, hjust=1))+
    #stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red") +
    xlab("Day of Sept:Hour of day") +
    ylab ("xyz root squared")
  return(graph_name_output) }

Accsub1_graph <- hourlyAccbox(ACC_1, "PROM001", Accsub1_graph)
Accsub2_graph <- hourlyAccbox(ACC_1, "PROM002", Accsub2_graph)
Accsub3_graph <- hourlyAccbox(ACC_1, "PROM003", Accsub3_graph)
Accsub4_graph <- hourlyAccbox(ACC_1, "PROM004", Accsub4_graph)

print(ggdraw() + draw_plot(Accsub1_graph, x= 0, y=.5, width = .5, height=.5) +
        draw_plot(Accsub2_graph, x= 0, y=0, width = .5, height=.5)+
        draw_plot(Accsub3_graph, x= .5, y=.5, width = .5, height=.5)+
        draw_plot(Accsub4_graph, x= .5, y=0, width = .5, height=.5))

  ##relating HR and ACC data for correlation
ACC_1 <- setkey(ACC_1, V1, ftime)
HR_1 <- setkey(HR_1, subject_id, ftime)

#ACC_HR_1 = HR_1[ACC_1, roll="nearest"]
#ACC_1_sec<- aggregate(ACC_1$sum_squares, by = list(ACC_1$V1, ACC_1$ftime), )
ACC_1.1_sec<- ACC_1[ACC_1$V1 == "PROM001"] %>% group_by(ftime=cut(ftime, "1 sec")) %>%
  dplyr::summarize(max_diff = (max(sum_squares)-min(sum_squares)), V1 = "PROM001")
ACC_1.2_sec<- ACC_1[ACC_1$V1 == "PROM002"] %>% group_by(ftime=cut(ftime, "1 sec")) %>%
  dplyr::summarize(max_diff = (max(sum_squares)-min(sum_squares)), V1 = "PROM002")
ACC_1.3_sec<- ACC_1[ACC_1$V1 == "PROM003"] %>% group_by(ftime=cut(ftime, "1 sec")) %>%
  dplyr::summarize(max_diff = (max(sum_squares)-min(sum_squares)), V1 = "PROM003")
ACC_1.4_sec<- ACC_1[ACC_1$V1 == "PROM004"] %>% group_by(ftime=cut(ftime, "1 sec")) %>%
  dplyr::summarize(max_diff = (max(sum_squares)-min(sum_squares)), V1 = "PROM004")
ACC_sec <-  rbindlist(list(ACC_1.1_sec, ACC_1.2_sec, ACC_1.3_sec, ACC_1.4_sec))
  
  ##plotting delta sum squares over time
# deltaAcc <- ggplot(ACC_1.1_sec, aes(x=as_datetime(ftime), y=max_diff, group = 1))+
#   geom_point() +
#   theme_bw() +
#   ggtitle("Movement per second") +
#   xlab("Date") +
#   ylab("Delta Squared Sum of Squares")
# print(deltaAcc)

Hist_ACC1<- ggplot(ACC_1, aes(x= sum_squares, fill=V1)) +
  geom_histogram(aes(y=..density..), position="dodge", alpha=.5, binwidth=1)+
  geom_density(alpha=.64)+
  xlim(60, 70) +
  xlab("Sum Squares per Subject")+
  ggtitle("Histogram")+
  facet_grid(V1~.)
print(Hist_ACC1)

Hist_diffACC <- ggplot(ACC_sec, aes(x= max_diff, fill=V1)) +
  geom_histogram(aes(y=..density..), position="dodge", alpha=.5, binwidth=1)+
  geom_density(alpha=.64)+
  xlim(0, 10) +
  xlab("Sum Squares Difference per Subject")+
  ggtitle("Histogram")+
  facet_grid(V1~.)
print(Hist_diffACC)

# ACC_HR_1$interpol = unique(ACC_HR_1$subject_id) %>%
#   map_df(~ approx(HR_1$ftime[HR_1$subject_id==.x], HR_1$measure[HR_1$subject_id==.x],
#                   xout=ACC_HR_1$ftime[ACC_HR_1$subject_id==.x]), .id="subject_id") %>% .$y

# ACC_HR_1 %>% group_by(subject_id) %>%
#   summarise(r_interp = cor(sum_squares, interpol, use="pairwise.complete.obs"),
#             r_roll = cor(sum_squares, measure, use="pairwise.complete.obs"))

#RESTING HR ANALYSIS BASED OF ACCELEROMETRY DATA (MOST EFFECTIVE)
gridSearch_diff_window_acc <- function(dt_acc_hr, window_size_list, acc_threshold_list) {
  dt_results <- data.table(acc_threshold = rep(acc_threshold_list, each = length(window_size_list)),
                           window_size = window_size_list,  # will be recycled
                           sensitivity = as.numeric(rep(NA, length(acc_threshold_list)*length(window_size_list))))
  
  dt_rangeHR_byId <- dt_steps_hr[, 
                                 .(rangeHR = range(measure, na.rm = TRUE)[2] - range(measure, na.rm = TRUE)[1]), 
                                 by = subject_id]
  
  num_participants <- length(dt_rangeHR_byId$subject_id)
  
}
#RESTING HR ANALYSIS ARBITRARY 
  ##function for labelling high, medium, low HR values
RestingHR_func <- function(HR_cohort, output_name){
output_name <- HR_cohort %>% mutate(date = ymd_hms(ftime), 
                                            day = floor_date(ftime, 'day')) %>%
  group_by(subject_id, day) %>%
  mutate(HR_level = .bincode(measure, quantile(measure, c(0, .25, .75, 1)),
                             #labels=c('Low', 'Medium', 'High'),
                             include.lowest = TRUE))
  #levels: 1=low, 2=medium, 3=high
#return(output_name())
}
HRlevels_1 <- RestingHR_func(HR_1, levels_1)
HRlevels_2 <- RestingHR_func(HR_2, levels_2)
HRlevels_3 <- RestingHR_func(HR_3, levels_3)
HRlevels_4 <- RestingHR_func(HR_4, levels_4)
HRlevels_5 <- RestingHR_func(HR_5, levels_5)
HRlevels_6 <- RestingHR_func(HR_6, levels_6)
HRlevels_7 <- RestingHR_func(HR_7, levels_7)
HRlevels_8 <- RestingHR_func(HR_8, levels_8)
HRlevels_9 <- RestingHR_func(HR_9, levels_9)
HRlevels_10 <- RestingHR_func(HR_10, levels_10)

    ##combine all cohort data when HR is low value
quartile1HR <- rbindlist(list(HRlevels_1[HRlevels_1$HR_level == 1,],
                              HRlevels_2[HRlevels_2$HR_level == 1,],
                              HRlevels_3[HRlevels_3$HR_level == 1,],
                              HRlevels_4[HRlevels_4$HR_level == 1,],
                              HRlevels_5[HRlevels_5$HR_level == 1,],
                              HRlevels_6[HRlevels_6$HR_level == 1,],
                              HRlevels_7[HRlevels_7$HR_level == 1,],
                              HRlevels_8[HRlevels_8$HR_level == 1,],
                              HRlevels_9[HRlevels_9$HR_level == 1,],
                              HRlevels_10[HRlevels_10$HR_level == 1,]))
#quartile1HR <- no_HR_outliers[no_HR_outliers$HR_level == 1,]

quartile1_summary <- group_by(quartile1HR, subject_id) %>% 
  dplyr::summarize(lower_quartile_HR = mean(measure), stan_dev=sd(measure))
summary(quartile1_summary)

noOut_avgHR<- quartile1HR %>%
  group_by(subject_id, day) %>%
  dplyr::summarize(lower_quartile_HR = mean(measure), stan_dev = sd(measure))

noOut_avgHR <- data.table(noOut_avgHR)
noOut_avgHR[,index:= order(day), by="subject_id"]

  ##title: Resting HR per User
HR_noOut_plot <- ggplot(quartile1_summary, 
                        aes(x=subject_id, y=lower_quartile_HR))+
  geom_bar(stat = "identity", width=0.8,fill="steelblue")+
  theme_bw()+
  xlab("userId")+
  ylab("Resting HR (bpm)")+
  theme(axis.text.x = element_text(angle=-90, hjust=1))+
  geom_errorbar(aes(ymin=lower_quartile_HR-stan_dev, ymax=lower_quartile_HR+stan_dev),
                width=.2,position=position_dodge(.9))
  #scale_x_discrete(labels=as.factor(index(unique(UserInfo_HR$userId))))
print(HR_noOut_plot)

  ##title: Resting HR per User per day
plotHR_noOut <- ggplot(noOut_avgHR, 
                       aes(x=index, y=lower_quartile_HR, colour=subject_id, group = subject_id)) +
  geom_point() + geom_line() +
  theme_bw() +
  xlab("Number of days") +
  ylab("Heart rate (beats/min)")+
  # geom_errorbar(aes(ymin=average_HR-Standard_Dev, ymax=average_HR+Standard_Dev),
  # width=.2,position=position_dodge(.9))
  scale_color_hue(labels= index(unique(noOut_avgHR$subject_id)))
print(plotHR_noOut)

  ## title: Relationship Between RHR SD and Resting HR
fit1 <- lm(stan_dev ~ lower_quartile_HR, data = quartile1_summary)
SDvsRestHR <- ggplot(quartile1_summary,
                     aes(x=stan_dev, y=lower_quartile_HR, colour=subject_id))+
  geom_point()+
  stat_smooth(method = 'lm', aes(group = 'linear'), se = FALSE) +
  theme_bw()+
  xlab("Standard Deviation of HR")+
  ylab("Resting HR (bpm)")+
  scale_color_hue(labels= index(unique(quartile1_summary$subject_id)))+
  #scale_x_discrete(labels=as.factor(index(unique(quartile1_summary$userId))))
  labs(title = paste("R^2 = ",signif(summary(fit1)$adj.r.squared, 3), ",",
                     " P =",signif(summary(fit1)$coef[2,4], 4)))
print(SDvsRestHR)

#DELTA HR ANALYSIS


# SAVING VARIABLES CALLED IN RMARKDOWN FILE
#save(list=c(
            #"HRsub1_graph", "HRsub2_graph", "HRsub3_graph", "HRsub4_graph",
            #"HRsub5_graph", "HRsub6_graph", "HRsub7_graph", "HRsub8_graph", "HRsub9_graph",
            #"HRsub10_graph", "HRsub11_graph", "HRsub12_graph", "HRsub13_graph", "HRsub14_graph",
            #"HRsub15_graph", "HRsub16_graph", "HRsub17_graph", "HRsub18_graph", "HRsub19_graph",
            #"HRsub20_graph", "HRsub21_graph", "HRsub22_graph", "HRsub23_graph", "HRsub24_graph",
            #"HRsub25_graph", "HRsub26_graph", "HRsub27_graph", "HRsub28_graph", "HRsub29_graph",
            #"HRsub30_graph", "HRsub31_graph", "HRsub32_graph", "HRsub33_graph", "HRsub34_graph",
            #"HRsub35_graph", "HRsub36_graph", "HRsub37_graph", "HRsub38_graph", "HRsub39_graph"
            #), file = "20190329_flu.Rdata")