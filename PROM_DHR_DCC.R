#for DCC: comparing boxplots of sick vs not sick
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
library("readxl")
library("pROC")
library("randomForest")


#load HR + SHED data
HR <- fread(file="/Users/emiliagrzesiak/Cohort_Combined/HR.csv",
            header = TRUE, sep = ",")
SHED <- fread(file="/Users/emiliagrzesiak/Downloads/vw_PR_SHEDDING.csv",
              header = TRUE, sep = "\t")
SYMP_summary <- read_excel("/Users/emiliagrzesiak/Downloads/virus_summary.xls",
                      sheet = "vw_PR_RESPONSE_SUMMARY")
HR <- setDT(HR, keep.rownames=TRUE, key=NULL, check.names=FALSE)
HR$ftime <- anytime(HR$tod-7200, tz="UTC")
SYMP <- fread(file="/Users/emiliagrzesiak/Downloads/vw_PR_SYMPTOM.csv",
              header=TRUE, sep= "\t")
SYMP$sx_day <- substring(SYMP$sx_day, 2)
SYMP$sx_day <- as.numeric(SYMP$sx_day)


#setting up directories
dir = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort_Combined"
# dir1 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort1/"
# dir2 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort2/"
# dir3 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort3/"
# dir4 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort4/"
# dir5 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort5/"
# dir6 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort6/"
# dir7 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort7/"
# dir8 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort8/"
#dir9 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort9/"
#dir10 = "/datacommons/henao/Projects/Prometheus/data/E4/E4_processed/Cohort10/"

#dataframe of personalized RHR (medians) values per person
RHR_Chen <- data.frame("subject_id" = c("PROM001", "PROM002", "PROM003", "PROM004", "PROM005",
                                   "PROM006", "PROM007", "PROM008", "PROM009", "PROM010",
                                   "PROM011", "PROM012", "PROM013", "PROM014", "PROM015",
                                   "PROM016", "PROM017", "PROM018", "PROM019", "PROM020",
                                   "PROM021", "PROM022", "PROM023", "PROM024", "PROM025",
                                   "PROM026", "PROM027", "PROM028", "PROM029", "PROM030",
                                   "PROM031", "PROM032", "PROM033", "PROM034", "PROM035",
                                   "PROM036", "PROM037", "PROM038", "PROM039"),
                  "RHR_median" = c(73.575, 68.95, 66.18, 79.3, 48.4, 66.25, 187.375,
                                   48.93, 76.035, 63.975, 87.08, 57.02, 57.345, 65.08,
                                   131.095, 51.48, 66.015, 50.92, 61.35, 64.675, 117.32,
                                   104.315, 63.8, 70.47, 54.75, 60.45, 99.86, 45.55, 69.195,
                                   57.305, 47.3, 64.785, 71.01, 68.65, 145.87, 58.795,
                                   78.1, 67.36, 58.42))




#Combine HR and DHR values
deltaHR_Chen <- merge(HR[, c("subject_id", "tod", "measure", "ftime")], 
                   RHR_Chen, by=c("subject_id"))
deltaHR_Chen <- deltaHR_Chen %>%
  mutate(deltaHR_Chen = measure - RHR_median)

deltaHR_Chen_summary <- deltaHR_Chen %>% group_by(subject_id, as.POSIXct(as_date(ftime))) %>%
  dplyr::summarize(med_deltaHR = median(deltaHR_Chen))
colnames(deltaHR_Chen_summary) <- c("subject_id", "ftime", "med_deltaHR")
deltaHR_Chen_summary <- setDT(deltaHR_Chen_summary)
deltaHR_Chen_summary[,index:= (order(ftime)-1), by="subject_id"]
deltaHR_Chen_summary$index[deltaHR_Chen_summary$index==0] <- -1

SHED$timepoint <- substring(SHED$timepoint, 2)
SHED$timepoint <- as.numeric(SHED$timepoint)
shedvsDHR_Chen <- merge(SHED, deltaHR_Chen_summary, 
                   by.x=c("timepoint", "subject_id"), by.y=c("index", "subject_id"))

fit_Chen1 <- lm(shedding_value ~ med_deltaHR, 
           data = shedvsDHR_Chen[shedvsDHR_Chen$subject_id %in% c("PROM002", "PROM004", "PROM005",
                                                                  "PROM006", "PROM007", "PROM008",
                                                                  "PROM009", "PROM010", "PROM011",
                                                                  "PROM014", "PROM015", "PROM006",
                                                                  "PROM027", "PROM034")])

#linear regression from Chentian's DHR calc
shedvsDHR_Chen1 <- ggplot(shedvsDHR_Chen[shedvsDHR_Chen$subject_id %in% c("PROM002", "PROM004", "PROM005",
                                                                   "PROM006", "PROM007", "PROM008",
                                                                   "PROM009", "PROM010", "PROM011",
                                                                   "PROM014", "PROM015", "PROM006",
                                                                   "PROM027", "PROM034")],
                         aes(x=med_deltaHR, y=shedding_value, colour=subject_id)) +
  geom_point()+
  stat_smooth(method = 'lm', aes(group = 'linear'), se = FALSE) +
  theme_bw()+
  xlab("Daily Median Delta HR (value - RHR)")+
  ylab("Shedding Value")+
  scale_color_hue(labels= index(unique(shedvsDHR_Chen$subject_id)))+
  #scale_x_discrete(labels=as.factor(index(unique(quartile1_summary$userId))))
  labs(title = paste("R^2 = ",signif(summary(fit_Chen1)$adj.r.squared, 3), ",",
                     " P =",signif(summary(fit_Chen1)$coef[2,4], 4)))
print(shedvsDHR_Chen1)

#boxplots from Chentian's DHR calc (everyone)
shedvsDHR_Chen$shed_sick = (shedvsDHR_Chen$shedding_value > 0)
shedvsDHR_Chen2 <- ggplot(shedvsDHR_Chen[shedvsDHR_Chen$subject_id %in% c("PROM002", "PROM004", "PROM005",
                                                                   "PROM006", "PROM007", "PROM008",
                                                                   "PROM009", "PROM010", "PROM011",
                                                                   "PROM014", "PROM015", "PROM006",
                                                                   "PROM027", "PROM034")],
                        aes(x=shed_sick, y=med_deltaHR, colour=subject_id))+
  geom_boxplot()+
  theme_bw()+
  xlab("Shedding?")+
  ylab("Median Daily Delta HR (value - RHR)")+
  scale_color_hue(labels= index(unique(shedvsDHR_Chen$subject_id)))
print(shedvsDHR_Chen2)

#boxplots from Chentian's DHR calc (average, all ppl + just ppl who got sick)
meanShed <- shedvsDHR_Chen %>% group_by(shed_sick) %>%
  dplyr::summarize(DHRmean=mean(med_deltaHR), DHRsd=sd(med_deltaHR))
##with all ppl...
shedvsDHR_Chen3 <- ggplot(shedvsDHR_Chen, aes(x=shed_sick, y=med_deltaHR))+
  geom_boxplot()+
  theme_bw()+
  xlab("Shedding?")+
  ylab("Median Daily Delta HR (value - RHR)")
print(shedvsDHR_Chen3) #throw plot out!!!!!!

shedvsDHR_Chen4 <- ggplot(shedvsDHR_Chen[shedvsDHR_Chen$subject_id %in% c("PROM002", "PROM004", "PROM005",
                                                                          "PROM006", "PROM007", "PROM008",
                                                                          "PROM009", "PROM010", "PROM011",
                                                                          "PROM014", "PROM015", "PROM006",
                                                                          "PROM027", "PROM034")], 
                          aes(x=shed_sick, y=med_deltaHR))+
  geom_boxplot()+
  theme_bw()+
  xlab("Shedding?")+
  ylab("Median Daily Delta HR (value - RHR)")
print(shedvsDHR_Chen4)

#symptoms and other total score analysis
shedvsDHR_Chen$subject_id <- as.character(shedvsDHR_Chen$subject_id)
colnames(SYMP)[2] <- "timepoint"
allSYMP <- left_join(SYMP, shedvsDHR_Chen[,c("timepoint", 
                                             "subject_id","med_deltaHR", "shedding_value")],
                      by=c("subject_id","timepoint"))
allSYMP[is.na(allSYMP)] <- 0
allSYMP$symp_score <- rowSums(allSYMP[,7:24])
allSYMP.sick <- allSYMP %>% group_by(subject_id) %>% filter(sum(symp_score) > 0)
allSYMP.sick <- allSYMP.sick[, c("subject_id", "timepoint", "med_deltaHR", "symp_score")] %>% 
  group_by(subject_id, timepoint) %>% dplyr::summarize(score= sum(symp_score), 
                                                       med_deltaHR = min(med_deltaHR))

#correlation between DHR and number of symptoms (sick ppl only)
fit <- lm(score ~ med_deltaHR, 
           data = allSYMP.sick)
symp_plot <- ggplot(allSYMP.sick,
                         aes(x=med_deltaHR, y=score, colour=subject_id))+
  geom_point()+
  stat_smooth(method = 'lm', aes(group = 'linear'), se = FALSE) +
  theme_bw()+
  xlab("Daily Avg Delta HR (value - RHR)")+
  ylab("Number of Flu Symptoms")+
  scale_color_hue(labels= index(unique(allSYMP$subject_id)))+
  #scale_x_discrete(labels=as.factor(index(unique(quartile1_summary$userId))))
  labs(title = paste("R^2 = ",signif(summary(fit)$adj.r.squared, 3), ",",
                     " P =",signif(summary(fit)$coef[2,4], 4)))
print(symp_plot)

#comparison of number of sick days per person, looking at all three definitions 
  ## --> then average # of sick days 
sickDef_days <- allSYMP %>% group_by(subject_id, timepoint) %>% 
  dplyr::summarize(symp_score = sum(symp_score), 
                   shedding_value = min(shedding_value), pcr_at_discharge = min(pcr_at_discharge))
sickDef <- sickDef_days %>% group_by(subject_id) %>% 
  dplyr::summarize(days_symp = count((symp_score >0)), days_shed = count((shedding_value >0)),
                   pcr_at_discharge = min(pcr_at_discharge))
sickDef$pcr_at_discharge <- as.factor(sickDef$pcr_at_discharge)
sickDefmelt <- melt(sickDef, id.vars=c("subject_id", "pcr_at_discharge"))

sickDefmelt$subject_id <- as.factor(index(unique(sickDefmelt$subject_id)))
pcr_at_discharge.labs <- c("Negative Discharge PCR", "Positive Discharge PCR")
names(pcr_at_discharge.labs) <- c("Negative", "Positive")
sickDefplot <- ggplot(sickDefmelt, aes(x=subject_id, y=value, fill=variable))+
  geom_bar(stat="identity",position=position_dodge())+
  theme_bw()+
  theme(legend.position = "top")+
  facet_wrap(~pcr_at_discharge, scales = "free_x", 
             labeller = labeller(pcr_at_discharge = pcr_at_discharge.labs))+
  xlab("Subject ID")+
  ylab("Number of Days Experienced")+
  scale_fill_discrete(name = "Infection Definition", labels = c("Symptoms", "Shedding"))
print(sickDefplot)


## SYMP plotting (perceptron-like plot)
SYMP$symp_score <- rowSums(SYMP[,7:24])
SYMP[is.na(SYMP)] <- 0
SYMP <- SYMP %>% group_by(subject_id, timepoint) %>% 
  dplyr::summarize(symp_score = mean(symp_score), pcr_at_discharge = min(pcr_at_discharge))
score_per_person <- SYMP %>% group_by(subject_id) %>%
  dplyr::summarize(total_symptom_score=sum(symp_score), PCR=min(pcr_at_discharge))
shed_per_person <- SHED %>% group_by(subject_id) %>%
  dplyr::summarize(total_shed_score=sum(shedding_value))
shed_score_pcr <- merge(score_per_person, shed_per_person, by="subject_id")

shed_score_pcr_plot <- ggplot(shed_score_pcr, aes(x=total_symptom_score, 
                                                  y=total_shed_score, group=PCR, colour=PCR))+
  geom_point()+
  theme_bw()+
  xlab("Symptom score")+
  ylab("Shedding Score")
print(shed_score_pcr_plot) 

  ##ROC analysis, treating PCR as ground truth
shed_score_pcr$shed_roc = (shed_score_pcr$total_shed_score > 0)
shed_score_pcr$symp_roc = (shed_score_pcr$total_symptom_score > 0)
cols <- sapply(shed_score_pcr, is.logical)
shed_score_pcr[,cols] <- lapply(shed_score_pcr[,cols], as.numeric)
shed_score_pcr$PCR[shed_score_pcr$PCR=='Negative'] <- 0
shed_score_pcr$PCR[shed_score_pcr$PCR=='Positive'] <- 1
shed_score_pcr$normalized_shed <- shed_score_pcr$total_shed_score / max(shed_score_pcr$total_shed_score)
shed_score_pcr$normalized_symp <- shed_score_pcr$total_symptom_score / max(shed_score_pcr$total_symptom_score)
shed_score_pcr$PCR <- as.factor(shed_score_pcr$PCR)
head(shed_score_pcr)

confusion_symp <- confusionMatrix(table(shed_score_pcr$symp_roc, shed_score_pcr$PCR), dnn=c("Symptom", "PCR"))
confusion_shed <- confusionMatrix(table(shed_score_pcr$shed_roc, shed_score_pcr$PCR), dnn=c("Shed", "PCR"))

index <- sample(1:nrow(shed_score_pcr), size = 0.2*nrow(shed_score_pcr))
train <- shed_score_pcr[index,]
test <- shed_score_pcr[-index,]
# build the random forest model and test it
rf_model <- randomForest(PCR ~ ., data = train[,c("total_symptom_score","total_shed_score", "PCR")])
rf_prediction <- predict(rf_model, test[,c("total_symptom_score","total_shed_score", "PCR")], type = "prob")
# build the logistic regression model and test it
lr_model <- glm(PCR ~ ., data = train[,c("total_symptom_score","total_shed_score", "PCR")], family = "binomial")
lr_prediction <- predict(lr_model, test[,c("total_symptom_score","total_shed_score", "PCR")], type = "response")

ROC_rf <- roc(test$PCR, rf_prediction[,2],
              smoothed = TRUE,
              # arguments for ci
              ci=TRUE, ci.alpha=0.95, stratified=FALSE,
              # arguments for plot
              plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
              print.auc=TRUE, show.thres=TRUE)
sens.ci.rf <- ci.se(ROC_rf)
plot(sens.ci.rf, type="shape", col="lightblue")
plot(sens.ci.rf, type="bars")

ROC_lr <- roc(test$PCR, lr_prediction,
              smoothed = TRUE,
              # arguments for ci
              ci=TRUE, ci.alpha=0.95, stratified=FALSE,
              # arguments for plot
              plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
              print.auc=TRUE, show.thres=TRUE)
sens.ci.lr <- ci.se(ROC_lr)
plot(sens.ci.lr, type="shape", col="lightblue")
plot(sens.ci.lr, type="bars")

#ROC_lr <- roc(test$PCR, lr_prediction)
#ROC_rf_auc <- auc(ROC_rf)
#ROC_lr_auc <- auc(ROC_lr)
#plot(ROC_rf, col = "green", main = "ROC For Random Forest (GREEN) vs Logistic Regression (RED)")
#lines(ROC_lr, col = "red")
