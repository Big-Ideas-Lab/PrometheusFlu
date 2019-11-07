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

#raw HR graphing
  #coh 1
# HR_1 <- HR[HR$subject_id %in% c("PROM001", "PROM002", "PROM003", "PROM004"),]
# HR_1$ftime <- anytime(HR_1$tod, tz="UTC")
# HR_graph <- ggplot(HR_1, aes(x=ftime, y=measure, colour=subject_id))+
#   geom_line()+
#   theme(axis.text.x = element_text(angle=-90, hjust=1))+
#   facet_wrap(~subject_id)
# print(HR_graph)
# 
# HR_sub1 <- HR_1[HR_1$subject_id == "PROM001",]
# HR_sub1$ftime <- anytime(HR_sub1$tod-7200, tz="UTC")
# HRsub1_graph <- ggplot(HR_sub1, 
#                    aes(x = format(ftime, format = "%d"), 
#                        y = measure, group = format(ftime, format = "%d"))) + 
#   geom_boxplot(fill = "grey80", colour = "#3366FF") +
#   #stat_summary(fun.data = n_fun, geom = "text", 
#                #fun.y = median, colour = "black", size= 2, fontface = "bold") +
#   theme_bw()+
#   #stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red") +
#   xlab("Day of Sept") +
#   ylab ("HR (bpm)")
# 
# HR_sub2 <- HR_1[HR_1$subject_id == "PROM002",]
# HR_sub2$ftime <- anytime(HR_sub2$tod-7200, tz="UTC")
# HRsub2_graph <- ggplot(HR_sub2, 
#                        aes(x = format(ftime, format = "%d"), 
#                            y = measure, group = format(ftime, format = "%d"))) + 
#   geom_boxplot(fill = "grey80", colour = "#3366FF") +
#   #stat_summary(fun.data = n_fun, geom = "text", 
#   #fun.y = median, colour = "black", size= 2, fontface = "bold") +
#   theme_bw()+
#   #stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red") +
#   xlab("Day of Sept") +
#   ylab ("HR (bpm)")
# 
# HR_sub3 <- HR_1[HR_1$subject_id == "PROM003",]
# #VW_sub3 <- VW[VW$subject_id == "PROM003",]
# HR_sub3$ftime <- anytime(HR_sub3$tod-7200, tz="UTC")
# HRsub3_graph <- ggplot(HR_sub3, 
#                        aes(x = format(ftime, format = "%d"), 
#                            y = measure, group = format(ftime, format = "%d"))) + 
#   geom_boxplot(fill = "grey80", colour = "#3366FF") +
#   #stat_summary(fun.data = n_fun, geom = "text", 
#   #fun.y = median, colour = "black", size= 2, fontface = "bold") +
#   theme_bw()+
#   #stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red") +
#   xlab("Day of Sept") +
#   ylab ("HR (bpm)")
# 
# HR_sub4 <- HR_1[HR_1$subject_id == "PROM004",]
# HR_sub4$ftime <- anytime(HR_sub4$tod-7200, tz="UTC")
# HRsub4_graph <- ggplot(HR_sub4, 
#                        aes(x = format(ftime, format = "%d"), 
#                            y = measure, group = format(ftime, format = "%d"))) + 
#   geom_boxplot(fill = "grey80", colour = "#3366FF") +
#   #stat_summary(fun.data = n_fun, geom = "text", 
#   #fun.y = median, colour = "black", size= 2, fontface = "bold") +
#   theme_bw()+
#   #stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red") +
#   xlab("Day of Sept") +
#   ylab ("HR (bpm)")
# 
#   #coh2
# HR_2 <- HR[HR$subject_id %in% c("PROM005", "PROM006", "PROM007"),]
# HR_sub5 <- HR_2[HR_2$subject_id == "PROM005",]
# HR_sub5$ftime <- anytime(HR_sub5$tod-7200, tz="UTC")
# HRsub5_graph <- ggplot(HR_sub5, 
#                        aes(x = format(ftime, format = "%d"), 
#                            y = measure, group = format(ftime, format = "%d"))) + 
#   geom_boxplot(fill = "grey80", colour = "#3366FF") +
#   #stat_summary(fun.data = n_fun, geom = "text", 
#   #fun.y = median, colour = "black", size= 2, fontface = "bold") +
#   theme_bw()+
#   #stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red") +
#   xlab("Day of Sept") +
#   ylab ("HR (bpm)")
# 
# HR_sub6 <- HR_2[HR_2$subject_id == "PROM006",]
# HR_sub6$ftime <- anytime(HR_sub6$tod-7200, tz="UTC")
# HRsub6_graph <- ggplot(HR_sub6, 
#                        aes(x = format(ftime, format = "%d"), 
#                            y = measure, group = format(ftime, format = "%d"))) + 
#   geom_boxplot(fill = "grey80", colour = "#3366FF") +
#   #stat_summary(fun.data = n_fun, geom = "text", 
#   #fun.y = median, colour = "black", size= 2, fontface = "bold") +
#   theme_bw()+
#   #stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red") +
#   xlab("Day of Sept") +
#   ylab ("HR (bpm)")

# let's make a function for plotting daily HR boxplots because I dont want to copy/paste 39 times

dailyHRbox <- function(HR_cohortNum, subjectID, graph_name_output){
  HR_cohortNum$ftime <- anytime(HR_cohortNum$tod-7200, tz="UTC")
  graph_name_output <- ggplot(HR_cohortNum[HR_cohortNum$subject_id == subjectID,], 
                         aes(x = format(ftime, format = "%d"), 
                             y = measure, group = format(ftime, format = "%d"))) + 
    geom_boxplot(fill = "grey80", colour = "#3366FF") +
    #stat_summary(fun.data = n_fun, geom = "text", 
    #fun.y = median, colour = "black", size= 2, fontface = "bold") +
    theme_bw()+
    #stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red") +
    xlab("Day of Sept") +
    ylab ("HR (bpm)")
  return(graph_name_output)
}
HR_1 <- HR[HR$subject_id %in% c("PROM001", "PROM002", "PROM003", "PROM004"),]
HR_2 <- HR[HR$subject_id %in% c("PROM005", "PROM006", "PROM007"),]
HR_3 <- HR[HR$subject_id %in% c("PROM008", "PROM009", "PROM010", "PROM011"),]
HR_4 <- HR[HR$subject_id %in% c("PROM012", "PROM13"),]
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
HRsub34_graph <- dailyHRbox(HR_9, "PROM034", HRsub34_graph)
HRsub35_graph <- dailyHRbox(HR_9, "PROM035", HRsub35_graph)
HRsub36_graph <- dailyHRbox(HR_9, "PROM036", HRsub36_graph)
HRsub37_graph <- dailyHRbox(HR_9, "PROM037", HRsub37_graph)
HRsub38_graph <- dailyHRbox(HR_9, "PROM038", HRsub38_graph)
HRsub39_graph <- dailyHRbox(HR_9, "PROM039", HRsub39_graph)

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

ACC_1$sum_squares <- sqrt(ACC_1$V3^2 + ACC_1$V4^2 + ACC_1$V5^2)
ACC_1$V2 <- anytime(ACC_1$V2-7200, tz="UTC")



save(list=c(
            "HRsub1_graph", "HRsub2_graph", "HRsub3_graph", "HRsub4_graph",
            "HRsub5_graph", "HRsub6_graph", "HRsub7_graph", "HRsub8_graph", "HRsub9_graph",
            "HRsub10_graph", "HRsub11_graph", "HRsub12_graph", "HRsub13_graph", "HRsub14_graph",
            "HRsub15_graph", "HRsub16_graph", "HRsub17_graph", "HRsub18_graph", "HRsub19_graph",
            "HRsub20_graph", "HRsub21_graph", "HRsub22_graph", "HRsub23_graph", "HRsub24_graph",
            "HRsub25_graph", "HRsub26_graph", "HRsub27_graph", "HRsub28_graph", "HRsub29_graph",
            "HRsub30_graph", "HRsub31_graph", "HRsub32_graph", "HRsub33_graph", "HRsub34_graph",
            "HRsub35_graph", "HRsub36_graph", "HRsub37_graph", "HRsub38_graph", "HRsub39_graph"
            ), file = "20190329_flu.Rdata")