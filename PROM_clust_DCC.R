# DCC functional clustering!


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

dir = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort_Combined/"

HR <- read.csv(paste0(dir, "HR.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)
TEMP <- read.csv(paste0(dir, "TEMP.csv"), header = TRUE, sep = '\t',stringsAsFactors=FALSE)

HR_1 <- setDT(HR_1, keep.rownames=TRUE, key=NULL, check.names=FALSE)
TEMP_1 <- setDT(TEMP_1, keep.rownames=TRUE, key=NULL, check.names=FALSE)
HR_1$ftime <- anytime(HR_1$tod-7200, tz="UTC")
TEMP_1$ftime <- anytime(TEMP_1$tod-7200, tz="UTC")
HR_day0 <- HR_1[,c('subject_id', 'measure', 'ftime')] %>% group_by(subject_id) %>%
  filter(as_date(ftime) == min(as_date(ftime)))  #change to include more days later
TEMP_day0 <- TEMP_1[,c('subject_id', 'measure', 'ftime')] %>% group_by(subject_id) %>%
  filter(as_date(ftime) == min(as_date(ftime)))  #change to include more days later
rm(HR, HR_1, TEMP, TEMP_1)
HR_day0$ftime <-format(HR_day0$ftime, format="%H:%M:%S")
TEMP_day0$ftime <-format(TEMP_day0$ftime, format="%H:%M:%S")

#reshape data to accomodate funFEM package's needs before functional clustering
HR_day0.reshape <- dcast(data = HR_day0, formula = ftime~subject_id, 
                         fun.aggregate = sum,value.var = "measure")
TEMP_day0.reshape <- dcast(data = TEMP_day0, formula = ftime~subject_id, 
                           fun.aggregate = sum, value.var = "measure")

# HR_day0.reshape[HR_day0.reshape == 0] <- NA
# TEMP_day0.reshape[TEMP_day0.reshape == 0] <- NA
# HR_day0.reshape <- setDT(HR_day0.reshape)
# TEMP_day0.reshape <- setDT(TEMP_day0.reshape)
# setkey(HR_day0.reshape, subject_id)
# setkey(TEMP_day0.reshape, subject_id)
# HR_day0.reshape[,measure := ifelse(is.na(measure), median(measure, na.rm=TRUE), measure), 
#                 by=eval(colnames(HR_day0.reshape))]
# TEMP_day0.reshape[,measure := ifelse(is.na(measure), median(measure, na.rm=TRUE), measure), by=subject_id]

ftime_HR <- HR_day0.reshape[-1]
ftime_TEMP <- TEMP_day0.reshape[-1]
row.names(ftime_HR) <- HR_day0.reshape$ftime
row.names(ftime_TEMP) <- TEMP_day0.reshape$ftime

#create basis for functional clustering
#HR_vals <- seq(50, 220, 0.002296149)
#names(HR_vals) <- row.names(ftime_HR)
#TEMP_vals <- seq(50, 220, 0.002296149)
#names(TEMP_vals) <- row.names(ftime_HR)

HR_matrix <- data.matrix(ftime_HR)
TEMP_matrix <- data.matrix(ftime_TEMP)
#TEMP_matrix <- data.matrix(ftime_TEMP)
#HR_matrix <- HR_matrix[12839:74038,]

##uses package funFEM, might not be working/up-to-date, ignore this section
#basis_HR <- create.bspline.basis(rangeval = c(0, dim(HR_matrix)[1]))#, nbasis=4,  norder=4) # norder=4 : cubic spline
#plot(basis_HR)
##fdobj_HR <- smooth.basis(HR_vals, HR_matrix, basis_HR)$fd
#res_HR <- funFEM(fdobj_HR, K=4)

##uses packages funHDDC and funcy
HR_matrix[HR_matrix == 0] <- NA
funcy_HR <- formatFuncy(HR_matrix, format = "Format1")
funcy_HR <- na.omit(funcy_HR)
#ctrl <- new("funcyCtrlMbc", baseType = "splines", dimBase = 3, eps = 0.01)
cluster <- Cluster(funcy_HR)
res_HR <- funcit(data = funcy_HR, k = 4, 
                 methods = c("fitfclust", "distclust", "iterSubspace"),
                 clusters = cls, parallel = TRUE)
                 #funcyCtrl = ctrl)
summary(res_HR)
plot(res_HR, legendPlace = "top")

TEMP_matrix[TEMP_matrix == 0] <- NA
funcy_TEMP <- formatFuncy(TEMO_matrix, format = "Format1")
funcy_TEMP <- na.omit(funcy_TEMP)
#ctrl <- new("funcyCtrlMbc", baseType = "splines", dimBase = 3, eps = 0.01)
cluster <- Cluster(funcy_TEMP)
res_TEMP <- funcit(data = funcy_TEMP, k = 4, 
                 methods = c("fitfclust", "distclust", "iterSubspace"),
                 clusters = cls, parallel = TRUE)
#funcyCtrl = ctrl)
summary(res_TEMP)
plot(res_TEMP, legendPlace = "top")