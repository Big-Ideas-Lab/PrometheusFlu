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
library("abind")

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
SHED <- setDT(SHED, keep.rownames=TRUE, key=NULL, check.names=FALSE)
SYMP <- setDT(SYMP, keep.rownames=TRUE, key=NULL, check.names=FALSE)
SYMP$sx_day <- substring(SYMP$sx_day, 2)
SYMP$sx_day <- as.numeric(SYMP$sx_day)+1
colnames(SYMP)[2] <- "timepoint"
SYMP$symp_score <- rowSums(SYMP[,7:24])
SYMP[is.na(SYMP)] <- 0
SHED$timepoint <- substring(SHED$timepoint, 2)
SHED$timepoint <- as.numeric(SHED$timepoint)+1
SHED$timepoint[SHED$timepoint==0] <- 1
SYMP$timepoint[SYMP$timepoint==0] <- 1

SYMP <- SYMP %>% group_by(subject_id, timepoint) %>% 
  dplyr::summarize(symp_score = mean(symp_score), pcr_at_discharge = min(pcr_at_discharge),
                   feeling_symp = sum(sx_musclesoreness,sx_fatigue,sx_headache,sx_ear_pain,
                                      sx_throat_discomfort,sx_chest_pain,sx_chills,sx_malaise,
                                      sx_itchy_eyes),
                   showing_symp = sum(sx_fever,sx_stuffy_nose,sx_runny_nose,sx_sneezing,sx_coughing,
                                      sx_soba,sx_hoarseness,sx_diarrhea,sx_wheezy_chest))
shed_score_pcr_perDay <- merge(SYMP, SHED, by=c("subject_id", "timepoint"))

#score_per_person <- SYMP %>% group_by(subject_id) %>%
#  dplyr::summarize(total_symptom_score=sum(symp_score), PCR=min(pcr_at_discharge))
#shed_per_person <- SHED %>% group_by(subject_id) %>%
#  dplyr::summarize(total_shed_score=sum(shedding_value))
#shed_score_pcr <- merge(score_per_person, shed_per_person, by="subject_id")

#criteria for being labelled "sick"
sick.SYMP <- subset(SYMP, pcr_at_discharge == 'Positive')
sickDays <- merge(sick.SYMP, SHED, by= c('subject_id', 'timepoint'))

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

cluster_HRnight2 <- matrix2cluster(HR_night_2,3)
cluster_HRday2 <- matrix2cluster(HR_day_2,3)

#FUNCTIONAL CLUSTERING ON SHEDDING AND SYMPTOM DATA 

#some hacky stuff to get 3d matrix in desired format...
daily_clinic_1 <- shed_score_pcr_perDay[,c('subject_id','timepoint','symp_score')]
daily_clinic_2 <- shed_score_pcr_perDay[,c('subject_id','timepoint','shedding_value')]
daily_clinic_3 <- shed_score_pcr_perDay[,c('subject_id','timepoint','feeling_symp')]
daily_clinic_4 <- shed_score_pcr_perDay[,c('subject_id','timepoint','showing_symp')]
daily_clinic_1 <- dcast(daily_clinic_1, formula = timepoint~subject_id, value.var="symp_score")
daily_clinic_2 <- dcast(daily_clinic_2, formula = timepoint~subject_id, value.var="shedding_value")
daily_clinic_3 <- dcast(daily_clinic_3, formula = timepoint~subject_id, value.var="feeling_symp")
daily_clinic_4 <- dcast(daily_clinic_4, formula = timepoint~subject_id, value.var="showing_symp")
#daily_clinic_1$subject_id <- c("symp_score")
#daily_clinic_2$subject_id <- c("shedding_value")
daily_clinic_1 <- daily_clinic_1[,-1]
daily_clinic_2 <- daily_clinic_2[,-1]
daily_clinic_3 <- daily_clinic_3[,-1]
daily_clinic_4 <- daily_clinic_4[,-1]
row.names(daily_clinic_1) <- unique(sort(shed_score_pcr_perDay$timepoint))
row.names(daily_clinic_2) <-  unique(sort(shed_score_pcr_perDay$timepoint))
row.names(daily_clinic_3) <- unique(sort(shed_score_pcr_perDay$timepoint))
row.names(daily_clinic_4) <-  unique(sort(shed_score_pcr_perDay$timepoint))
daily_clinic_1<-data.matrix(daily_clinic_1)
daily_clinic_2<-data.matrix(daily_clinic_2)
daily_clinic_3<-data.matrix(daily_clinic_3)
daily_clinic_4<-data.matrix(daily_clinic_4)

#daily_clinic_total <- rbind(daily_clinic_1, daily_clinic_2)
clinic_basis_bspline3 <-create.fourier.basis(c(1,12), nbasis=3, period=12)
clinic_basis_bspline5 <-create.fourier.basis(c(1,12), nbasis=5, period=12)#or nbasis 5
#create.bspline.basis(c(1,12), nbasis=12, norder=2)
y_clinic <- setNames(as.numeric(c(1:12)), as.numeric(c(1:12)))
clinic_fd_1B3 <-smooth.basis(y_clinic, daily_clinic_1, clinic_basis_bspline3)$fd
clinic_fd_2B3 <-smooth.basis(y_clinic, daily_clinic_2, clinic_basis_bspline3)$fd
clinic_fd_3B3 <-smooth.basis(y_clinic, daily_clinic_3, clinic_basis_bspline3)$fd
clinic_fd_4B3 <-smooth.basis(y_clinic, daily_clinic_4, clinic_basis_bspline3)$fd

clinic_fd_1B5 <-smooth.basis(y_clinic, daily_clinic_1, clinic_basis_bspline5)$fd
clinic_fd_2B5 <-smooth.basis(y_clinic, daily_clinic_2, clinic_basis_bspline5)$fd
clinic_fd_3B5 <-smooth.basis(y_clinic, daily_clinic_3, clinic_basis_bspline5)$fd
clinic_fd_4B5 <-smooth.basis(y_clinic, daily_clinic_4, clinic_basis_bspline5)$fd


#fdata_clinic_1 <- fdata(daily_clinic_1)
#fdata_clinic_2 <- fdata(daily_clinic_2)
#fdata_clinic_1$data <- as.numeric(fdata_clinic_1$data)
#fdata_clinic_2$data <- as.numeric(fdata_clinic_2$data)
#fdata_list <- list(fdata_clinic_1,fdata_clinic_2)

#fd_list <- list(clinic_fd_1,clinic_fd_2)
fd_list3 <- list(clinic_fd_2B3,clinic_fd_3B3, clinic_fd_4B3)
fd_list5 <- list(clinic_fd_2B5,clinic_fd_3B5, clinic_fd_4B5)

cluster_clinicB3 <- funHDDC(data=fd_list3, K=2:5, 
                            model=c("AkjBkQkDk", "AkjBQkDk","AkBkQkDk","AkBQkDk","ABQkDk"), 
                            init="random",threshold=0.2)
cluster_clinicB5 <- funHDDC(data=fd_list5, K=2:5, 
                            model=c("AkjBkQkDk", "AkjBQkDk","AkBkQkDk","AkBQkDk","ABQkDk"), 
                            init="random",threshold=0.2)


plot(clinic_fd_1B3, col=cluster_clinicB3$class, 
     xlab = "Day", ylab = "Symptom Score")
plot(clinic_fd_1B5, col=cluster_clinicB5$class, 
     xlab = "Day", ylab = "Symptom Score")

subject_class <- data.frame(subject_id = unique(shed_score_pcr_perDay$subject_id),
                            class = paste("Class", as.character(cluster_clinicB3$class)))
#fclust_plot_totalsymp <- ggplot(clinic_fd_1)
#daily_clinic_matrix <- abind(split(daily_clinic_total, daily_clinic_total$subject_id), along=3)
#daily_clinic_matrix <- data.matrix(daily_clinic_matrix)
#daily_clinic_matrix <- daily_clinic_matrix[,-1,]
#clinical_fdata <- fdata(daily_clinic_matrix, fdata2d=TRUE)
#clinical_cluster=kmeans.fd(clinical_fdata, ncl=3, draw=TRUE, par.ini=list(method="exact"))



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