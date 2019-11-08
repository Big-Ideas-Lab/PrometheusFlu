# Functional Clustering Script, Other Script will be for DCC
# Emilia Grzesiak, 09/09/2019

#libraries:

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
library("lme4")
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
library("funFEM")
library("funHDDC")
library("funcy")
library("sm")
library("SnowballC")
library("tm")
library("fclust")
library("fda.usc")

# all Cohort data:
HR <- fread(file="/Users/emiliagrzesiak/Cohort_Combined/HR.csv",
            header = TRUE, sep = ",")
SHED <- fread(file="/Users/emiliagrzesiak/Downloads/vw_PR_SHEDDING.csv",
              header = TRUE, sep = "\t")
SYMP <- fread(file="/Users/emiliagrzesiak/Downloads/vw_PR_SYMPTOM.csv",
              header=TRUE, sep= "\t")
#HR_1 <- HR[HR$subject_id %in% c("PROM001", "PROM002", "PROM003", "PROM004"),]

#HR MANIPULATION

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

#symptom/shedding/PCR MANIPULATION
SHED <- subset(SHED, subject_id %notin% c("PROM023","PROM026"))
SYMP <- subset(SYMP, subject_id %notin% c("PROM023","PROM026"))
SYMP$sx_day <- substring(SYMP$sx_day, 2)
SYMP$sx_day <- as.numeric(SYMP$sx_day)+2
colnames(SYMP)[2] <- "timepoint"
SYMP$symp_score <- rowSums(SYMP[,7:24])
SYMP[is.na(SYMP)] <- 0
SHED$timepoint <- substring(SHED$timepoint, 2)
SHED$timepoint <- as.numeric(SHED$timepoint)+2

SYMP <- SYMP %>% group_by(subject_id, timepoint) %>% 
  dplyr::summarize(symp_score = mean(symp_score), pcr_at_discharge = min(pcr_at_discharge))
score_per_person <- SYMP %>% group_by(subject_id) %>%
  dplyr::summarize(total_symptom_score=sum(symp_score), PCR=min(pcr_at_discharge))
shed_per_person <- SHED %>% group_by(subject_id) %>%
  dplyr::summarize(total_shed_score=sum(shedding_value))
shed_score_pcr <- merge(score_per_person, shed_per_person, by="subject_id")

#criteria for being labelled "sick"
sick.SYMP <- subset(SYMP, pcr_at_discharge == 'Positive')
sickDays <- merge(sick.SYMP, SHED, by= c('subject_id', 'timepoint'))

#HR_day0 <- HR_1[,c('subject_id', 'measure', 'ftime')] %>% group_by(subject_id) %>%
#  filter(as_date(ftime) == min(as_date(ftime)))  #change to include more days later
HR_InterestDay <- HR[,c('subject_id', 'measure', 'ftime', 'index_day')] %>% group_by(subject_id) %>%
  filter(index_day != 1)
HR_InterestDay <- setDT(HR_InterestDay)
#plot median + IQR HR per hour every day, line plot
HR_hourly <- HR_InterestDay %>% group_by(subject_id, ftime=cut(ftime, "60 min")) %>%
  dplyr::summarize(measure = median(measure), index_day = min(index_day))
HR_hourly$ftime <- as.POSIXct(HR_hourly$ftime)
HR_hourly$ftime <- format(HR_hourly$ftime, format="%H:%M:%S")
HR_hourly$ftime <- as.POSIXct(paste(HR_hourly$index_day, HR_hourly$ftime), format="%d %H")

HR_hourly_plot <- ggplot(HR_hourly, aes(x=factor(ftime), y=median_measure, group=subject_id))+
  geom_line(aes(colour=subject_id))
print(HR_hourly_plot)

#rm(HR)
#TEMP_day0$ftime <-format(TEMP_day0$ftime, format="%H:%M:%S")
#reshape data to accomodate fda.usa package's needs before functional clustering
matrix2cluster <- function(input_dataframe, number_of_clusters){
  HR_hourly_test <- dcast(data = input_dataframe[,c("subject_id", "ftime", "measure")], 
                          formula = ftime~subject_id, value.var = "measure")
  ftime_HR <- HR_hourly_test[,-1]
  row.names(ftime_HR) <- unique(HR_hourly_test$ftime)
  HR_matrix <- data.matrix(ftime_HR)
  HR_matrix[HR_matrix == 0] <- NA
  for(i in 1:ncol(HR_matrix)){
    HR_matrix[is.na(HR_matrix[,i]), i] <- mean(HR_matrix[,i], na.rm = TRUE)
  }
  rownames(HR_matrix) <- NULL
  HR_matrix <- t(HR_matrix)
  HR_matrix_norm <- t(apply(HR_matrix, 1, function(x)(x-min(x))/(max(x)-min(x))))
  # Unsupervised classification
  cluster=kmeans.fd(HR_matrix, ncl=number_of_clusters, draw=TRUE, par.ini=list(method="exact"))
  return(cluster)
}

out.fd1 <- matrix2cluster(HR_hourly, 2)
#include_list <- c("PROM001", "PROM002", "PROM003", "PROM004", "PROM005",
#                  "PROM006", "PROM008", "PROM009", "PROM010", "PROM012",
#                  "PROM014", "PROM016", "PROM017", "PROM018", "PROM019",
#                  "PROM020", "PROM022", "PROM024", "PROM025", "PROM028",
#                  "PROM030", "PROM031", "PROM032", "PROM033", "PROM035",
#                  "PROM036", "PROM037", "PROM038", "PROM039") 
#not_include_list <- c("PROM007", "PROM011", "PROM013", "PROM015", "PROM021", 
#                      "PROM027", "PROM029", "PROM034") 
#cluster2 <- HR_matrix[include_list, ]
#cluster3 <- HR_matrix[not_include_list, ]
#out.fd2=kmeans.fd(cluster2, ncl=2, draw=TRUE, par.ini=list(method="exact"))
#out.fd3=kmeans.fd(cluster3, ncl=2, draw=TRUE, par.ini=list(method="exact"))
#out.fd4=kmeans.fd(HR_matrix_norm, ncl=2, draw=TRUE, par.ini=list(method="exact"))

#now looking at days/night individually
HR_sameTime <- HR[,c("subject_id", "ftime", "measure", "index_day")]
HR_sameTime$ftime <- format(HR_sameTime$ftime, format="%H:%M:%S")
HR_sameTime$index_date <- as.Date(HR_sameTime$index_day, format = "%j", origin = "1.1.2014")
HR_sameTime$ftime <- as.POSIXct(paste(HR_sameTime$index_date, 
                                      HR_sameTime$ftime), format="%Y-%m-%d %H:%M:%S")

HR_night <- subset(HR_sameTime, format(ftime, '%H') %in%  c("01","02","03","04","05","06"))
HR_day <- subset(HR_sameTime, format(ftime, '%H') %notin% c("01", "02","03","04","05","06"))
#day/night 2
HR_night_2 <- subset(HR_night, index_day==2)
HR_day_2 <- subset(HR_day, index_day==2)

out.fd5 <- matrix2cluster(HR_night_2,3)

#test<- t(HR_matrix)
#HR_fdata <- fdata(test)
#is.fdata(HR_fdata)

#plot((pc<-create.pc.basis(HR_fdata$data,l=1:2))[[1]])

#bsp5<-create.bspline.basis(HR_fdata$rangeval,nbasis=5)
#S.bsp5  <-  S.basis(HR_fdata$argvals, bsp5)
#TEMP_matrix <- data.matrix(ftime_TEMP)
#HR_matrix <- HR_matrix[12839:74038,]

##uses package funFEM, might not be working/up-to-date, ignore this section
#basis_HR <- create.bspline.basis(rangeval = c(0, dim(HR_matrix)[1]))#, nbasis=4,  norder=4) # norder=4 : cubic spline
#plot(basis_HR)
#fdobj_HR <- smooth.basis(HR_vals, HR_matrix, basis_HR)$fd
#res_HR <- funFEM(fdobj_HR, K=4)

#uses package fda
#kmeans_HR <- kmeans.fd(HR_matrix,ncl=3, metric=metric.lp, dfunc=func.trim.FM,
#          max.iter=100, par.metric=NULL, par.dfunc=list(trim=0.05),
#          par.ini=list(method="sample"),draw=TRUE)
#kmeans.center.ini(HR_matrix,ncl=3,metric=metric.lp,
#                  draw=TRUE,method="sample",iter=100,par.metric=NULL,...)

#is.fdata(t(HR_matrix))
#test <- t(HR_matrix)
##uses packages funHDDC and funcy
#HR_matrix[HR_matrix == 0] <- NA
#funcy_HR <- formatFuncy(HR_matrix, format = "Format1")
#funcy_HR <- na.omit(funcy_HR)
#ctrl <- new("funcyCtrlMbc", baseType = "splines", dimBase = 3, eps = 0.01)
#res_HR <- funcit(data = funcy_HR, k = 2, methods = "fitfclust") #funcyCtrl = ctrl)