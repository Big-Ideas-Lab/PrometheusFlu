#sleep validation with accelerometer data, HR analysis will be incorporated into this later

#libraries
library("hms")
library("rlang")
library("caret")
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
library("bigmemory")
library("biganalytics")
library("DescTools")
library("chron")
library("anytime")
library("purrr")
library("bayesbio")
library("RcppRoll")
library("binaryLogic")
library("tools")
library("matrixStats")
library("RColorBrewer")
library("fda.usc")

#load in data
HR <- fread(file="/Users/emiliagrzesiak/Cohort_Combined/HR.csv",
            header = TRUE, sep = ",")
SHED <- fread(file="/Users/emiliagrzesiak/Downloads/vw_PR_SHEDDING.csv",
              header = TRUE, sep = "\t")
SYMP <- fread(file="/Users/emiliagrzesiak/Downloads/vw_PR_SYMPTOM.csv",
              header=TRUE, sep= "\t")
ACC <- fread(file="/Users/emiliagrzesiak/ACC_out.csv",
             header = FALSE, sep = "\t")

#fix wonky data and exclude bad participants
colnames(ACC) <- c("subject_id", "tod", "ACC_x", "ACC_y", "ACC_z")
HR <- setDT(HR, keep.rownames=TRUE, key=NULL, check.names=FALSE)
ACC <- setDT(ACC, keep.rownames=TRUE, key=NULL, check.names=FALSE)
`%notin%` <- Negate(`%in%`)
HR <- subset(HR, subject_id %notin% c("PROM023","PROM026"))
ACC <- subset(ACC, subject_id %notin% c("PROM023","PROM026"))
SHED <- subset(SHED, subject_id %notin% c("PROM023","PROM026"))
SYMP <- subset(SYMP, subject_id %notin% c("PROM023","PROM026"))

#fix time stamps
HR$ftime <- anytime(HR$tod, tz='America/New_York')
ACC$ftime <- anytime(ACC$tod, tz='America/New_York')

#calculate root mean squared for ACC values
ACC$measure <- sqrt((ACC$ACC_x^2 + ACC$ACC_y^2 + ACC$ACC_z^2))

#index days to standardize dates across cohorts
HR <- HR[,index_day := .GRP, by = .(subject_id, day(HR$ftime))]
HR <- HR[, index_day:=index_day-index_day[1]+1L, by="subject_id"]
ACC <- ACC[,index_day := .GRP, by = .(subject_id, day(ACC$ftime))]
ACC <- ACC[, index_day:=index_day-index_day[1]+1L, by="subject_id"]

HR$ftime <- format(HR$ftime, format="%H:%M:%S")
HR$index_date <- as.Date(HR$index_day, format = "%j", origin = "1.1.2014")
HR$ftime <- as.POSIXct(paste(HR$index_date, HR$ftime), format="%Y-%m-%d %H:%M:%S")

test_HR <- HR[HR$subject_id==c("PROM001", "PROM002")]

hourDay_boxplots <- function(timeseries_dataframe){
  graph_output <- ggplot(timeseries_dataframe, 
                              aes(x = format(as.POSIXct(ftime),"%d:%H"), 
                                  y = measure, group=format(as.POSIXct(ftime),"%d:%H")))+ 
    geom_boxplot(fill = "grey80", colour = "#3366FF", outlier.alpha = 1/40, outlier.size=-1) +
    #stat_summary(fun.data = n_fun, geom = "text", 
    #fun.y = median, colour = "black", size= 2, fontface = "bold") +
    theme_bw()+
    facet_wrap(subject_id~.)+
    #scale_x_discrete(labels=format.Date(ACC_1$ftime, "%H"))+
    theme(axis.text.x= element_text(size=7, angle=-90, hjust=1))+
    #coord_cartesian(ylim=c(mean(timeseries_dataframe$measure)-2*sd(timeseries_dataframe$measure), 
    #                       mean(timeseries_dataframe$measure)+3*sd(timeseries_dataframe$measure)))+
    #stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, colour = "red") +
    xlab("Day:hour") +
    ylab ("magnitude")
  return(graph_output) 
}

ACC_tiny <- ACC[ACC$index_day==3 | ACC$index_day==4]
HR_tiny <- HR[(HR$index_day==3 | HR$index_day==4)&(
  HR$subject_id=="PROM001" | HR$subject_id=="PROM002" | 
    HR$subject_id=="PROM003" | HR$subject_id=="PROM004")]
ACC_tinybox<- hourDay_boxplots(ACC_tiny)
print(ACC_tinybox)
HR_tinybox <- hourDay_boxplots(HR_tiny)
print(HR_tinybox)
