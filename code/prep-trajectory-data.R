# Prepare and format data for resilience trajectory study

# Laura E. Meine; laura.meine@uni-mainz.de
# Last updated: 12.11.2021

# 0. load required libraries ----------------------
library(tidyverse) # data formatting, ggplot2
library(magrittr) # piping %<>%
library(rstatix)
source("R_functions/resilience_scores.R") # compute resilient functioning score
source("R_functions/laura_theme.R")

# 1. load and format data -------------------------
data <- read.csv("../data/T0-T5_controls.csv", header=T, sep=",")
str(data)

# as.factor
cols <- c("Cohort", "Code", "Gender", "Group")
data %<>% mutate_at(cols, list(~ as.factor(.)))

# check for duplicate ids
duplicates <- data %>% 
  group_by(Code) %>%
  filter(n() > 1) # 1 duplicate found!

# delete duplicate (participant apparently took part in both cohort 3 and 4)
data <- data %>%
  filter(!data$Code=="IL13SAVI") 

rm(duplicates, cols) # tidy up

# 2. match cohorts --------------------------------
# cohort 3 T2-T5 overlaps with cohort 4 T0-T3
# by extracting these time points from the two cohorts, the data is easily comparable and does not include data collected during the pandemic
# limitation: testing at T0 and T1 may have influenced T2 in cohort 3!

data_cohort3 <- data %>%
  filter(data$Cohort==3) %>%
  select(Code, Cohort,
         BSI_T2_INV, WHO_T2, MIMIS_T2_Frequency, LES_T2_Count, # select variables to be included in resilient functioning score
         BSI_T3_INV, WHO_T3, MIMIS_T3_Frequency, LES_T3_Count,
         BSI_T4_INV, WHO_T4, MIMIS_T4_Frequency, LES_T4_Count,
         BSI_T5_INV, WHO_T5, MIMIS_T5_Frequency, LES_T5_Count)
data_cohort3 <- data_cohort3[complete.cases(data_cohort3), ] #46

# rename variables to match cohort 4
data_cohort3_matched <- data_cohort3 %>% rename(BSI_T0_INV=BSI_T2_INV,
                                                BSI_T1_INV=BSI_T3_INV,
                                                BSI_T2_INV=BSI_T4_INV,
                                                BSI_T3_INV=BSI_T5_INV,
                                                WHO_T0=WHO_T2,
                                                WHO_T1=WHO_T3,
                                                WHO_T2=WHO_T4,
                                                WHO_T3=WHO_T5,
                                                MIMIS_T0_Frequency=MIMIS_T2_Frequency,
                                                MIMIS_T1_Frequency=MIMIS_T3_Frequency,
                                                MIMIS_T2_Frequency=MIMIS_T4_Frequency,
                                                MIMIS_T3_Frequency=MIMIS_T5_Frequency,
                                                LES_T0_Count=LES_T2_Count,
                                                LES_T1_Count=LES_T3_Count,
                                                LES_T2_Count=LES_T4_Count,
                                                LES_T3_Count=LES_T5_Count)

data_cohort4 <- data %>%
  filter(data$Cohort==4) %>%
  select(Code, Cohort,
         BSI_T0_INV, WHO_T0, MIMIS_T0_Frequency, LES_T0_Count,
         BSI_T1_INV, WHO_T1, MIMIS_T1_Frequency, LES_T1_Count,
         BSI_T2_INV, WHO_T2, MIMIS_T2_Frequency, LES_T2_Count,
         BSI_T3_INV, WHO_T3, MIMIS_T3_Frequency, LES_T3_Count)
data_cohort4 <- data_cohort4[complete.cases(data_cohort4), ] #89

#46+89=135

data_matched <- rbind(data_cohort3_matched, data_cohort4)

data_matched$Code <- droplevels(data_matched$Code)
row.names(data_matched) <- NULL # reset row index

# 3. exclude outliers ------------------------------
# exclude participant with extremely high stressor load at T3
MIMIS_outlier <- data_matched %>%
  select(Code, MIMIS_T3_Frequency) %>%
  mutate(MIMIS_T3_Frequency.z = scale(MIMIS_T3_Frequency)) %>%
  filter(abs(MIMIS_T3_Frequency.z)>3)

data_matched <- data_matched %>% filter(! Code=="ND09LESE")

# 4. compute resilient functioning (RF) scores ----
# see van Harmelen et al., 2017, Psychol Med
# requires function resilience_scores (DOI 10.17605/OSF.IO/S7U23, Katja Schueler, 2019)

# keep only participants who reported at least 1 stressful event at T0 (LES/MIMIS)
data_matched <- data_matched %>%
  filter(! (MIMIS_T0_Frequency==0 & LES_T0_Count==0)) # 1 case

# ideally loop at least some of the following code to shorten script

# R Score at T0 ------------------------

# indicators of mental health
mh_itemsT0 <- data_matched %>% select(BSI_T0_INV, WHO_T0)

# indicators of stress
stress_itemsT0 <- data_matched %>% select(MIMIS_T0_Frequency, LES_T0_Count)

resultsT0 <- resilience_scores(mh_itemsT0, stress_itemsT0)

summary(resultsT0$LM) # get results of linear regression mh ~ stress
summary(resultsT0$MH_PCA) # mh component 1 explains 81% variance
summary(resultsT0$Stress_PCA) # stress component 1 explains 62% variance

# add resulting scores to data
data_matched$RF_T0 <- unlist(resultsT0[1])
data_matched$stressPCA_T0 <- unlist(resultsT0[2])
data_matched$mhPCA_T0 <- unlist(resultsT0[3])

# check data
grep("BSI_T0_INV", colnames(data_matched)) # index BSI_GSI inverted: 3
grep("WHO_T0", colnames(data_matched)) # index WHO: 4

index_max_mh <- which.max(data_matched$mhPCA_T0) # id with highest mh component 1 score
data_matched[index_max_mh, c(3,4)] # high WHO and BSI score, so high mh component score correctly indicates high mh
index_min_mh <- which.min(data_matched$mhPCA_T0) #id with lowest mh component 1 score
data_matched[index_min_mh,c(3,4)] # low WHO and BSI score, so low mh component score correctly indicates low mh

grep("MIMIS_T0_Frequency", colnames(data_matched)) # index MIMIS Frequency: 5
grep("LES_T0_Count", colnames(data_matched)) # index LES Count: 6

index_max_stress <- which.max(data_matched$stressPCA_T0) # id with highest stress component 1 score
data_matched[index_max_stress, c(5,6)] # high MIMIS and LES score
index_min_stress <- which.min(data_matched$stressPCA_T0) # id with lowest stress component 1 score
data_matched[index_min_stress,c(5,6)] # low MIMIS and LES score

# linear vs. quadratic?
m1 <- lm(data_matched$mhPCA_T0 ~ data_matched$stressPCA_T0) # linear model
summary.lm(m1)

m2 <- update(m1, .~.+ I(data_matched$stressPCA_T0^2)) # with quadratic term
summary.lm(m2)

# model comparison
anova(m1, m2)
AIC(m1, m2)
BIC (m1, m2)
# linear model is better-fitting

# R Score at T1 ------------------------

# indicators of mental health
mh_itemsT1 <- data_matched %>% select(BSI_T1_INV, WHO_T1)

# indicators of stress 
stress_itemsT1 <- data_matched %>% select(MIMIS_T1_Frequency, LES_T1_Count)

resultsT1 <- resilience_scores(mh_itemsT1, stress_itemsT1)

summary(resultsT1$LM) # get results of linear regression mh ~ stress
summary(resultsT1$MH_PCA) # mh component 1 explains 78% variance
summary(resultsT1$Stress_PCA) # stress component 1 explains 70% variance

# add resulting scores to data
data_matched$RF_T1 <- unlist(resultsT1[1])
data_matched$stressPCA_T1 <- unlist(resultsT1[2])
data_matched$mhPCA_T1 <- unlist(resultsT1[3])

# check data
grep("BSI_T1_INV", colnames(data_matched)) # index BSI_GSI inverted: 7
grep("WHO_T1", colnames(data_matched)) # index WHO: 8

index_max_mh <- which.max(data_matched$mhPCA_T1) # id with highest mh component 1 score
data_matched[index_max_mh, c(7,8)] # high WHO and BSI score, so high mh component score correctly indicates high mh
index_min_mh <- which.min(data_matched$mhPCA_T1) # id with lowest mh component 1 score
data_matched[index_min_mh,c(7,8)] # low WHO and BSI score, so low mh component score correctly indicates low mh

grep("MIMIS_T1_Frequency", colnames(data_matched)) # index MIMIS Frequency: 9
grep("LES_T1_Count", colnames(data_matched)) # index LES Count: 10

index_max_stress <- which.max(data_matched$stressPCA_T1) # id with highest stress component 1 score
data_matched[index_max_stress, c(9,10)] # high MIMIS and LES score
index_min_stress <- which.min(data_matched$stressPCA_T1) # id with lowest stress component 1 score
data_matched[index_min_stress,c(9,10)] # low MIMIS and LES score

# linear vs. quadratic?
m1 <- lm(data_matched$mhPCA_T1 ~ data_matched$stressPCA_T1) # linear model
summary.lm(m1)

m2 <- update(m1, .~.+ I(data_matched$stressPCA_T1^2)) # with quadratic term
summary.lm(m2)

# model comparison
anova(m1, m2)
AIC(m1, m2)
BIC (m1, m2)

# R Score at T2 ------------------------

# indicators of mental health
mh_itemsT2 <- data_matched %>% select(BSI_T2_INV, WHO_T2)

# indicators of stress 
stress_itemsT2 <- data_matched %>% select(MIMIS_T2_Frequency, LES_T2_Count)

resultsT2 <- resilience_scores(mh_itemsT2, stress_itemsT2)

summary(resultsT2$LM) # get results of linear regression mh ~ stress
summary(resultsT2$MH_PCA) # mh component 1 explains 78% variance
summary(resultsT2$Stress_PCA) # stress component 1 explains 75% variance

# add resulting scores to data
data_matched$RF_T2 <- unlist(resultsT2[1])
data_matched$stressPCA_T2 <- unlist(resultsT2[2])
data_matched$mhPCA_T2 <- unlist(resultsT2[3])

# check data
grep("BSI_T2_INV", colnames(data_matched)) # index BSI_GSI inverted: 11
grep("WHO_T2", colnames(data_matched)) # index WHO: 12

index_max_mh <- which.max(data_matched$mhPCA_T2) # id with highest mh component 1 score
data_matched[index_max_mh, c(11,12)] # high WHO and BSI score, so high mh component score correctly indicates high mh
index_min_mh <- which.min(data_matched$mhPCA_T2) # id with lowest mh component 1 score
data_matched[index_min_mh,c(11,12)] # low WHO and BSI score, so low mh component score correctly indicates low mh

grep("MIMIS_T2_Frequency", colnames(data_matched)) # index MIMIS Frequency: 13
grep("LES_T2_Count", colnames(data_matched)) # index LES Count: 14

index_max_stress <- which.max(data_matched$stressPCA_T2) # id with highest stress component 1 score
data_matched[index_max_stress, c(13,14)] # high MIMIS and LES score
index_min_stress <- which.min(data_matched$stressPCA_T2) # id with lowest stress component 1 score
data_matched[index_min_stress,c(13,14)] # low MIMIS and LES score

# linear vs. quadratic?
m1 <- lm(data_matched$mhPCA_T2 ~ data_matched$stressPCA_T2) # linear model
summary.lm(m1)

m2 <- update(m1, .~.+ I(data_matched$stressPCA_T2^2)) # with quadratic term
summary.lm(m2)

# model comparison
anova(m1, m2)
AIC(m1, m2)
BIC (m1, m2)

# R Score at T3 ------------------------

# indicators of mental health
mh_itemsT3 <- data_matched %>% select(BSI_T3_INV, WHO_T3)

# indicators of stress 
stress_itemsT3 <- data_matched %>% select(MIMIS_T3_Frequency, LES_T3_Count)

resultsT3 <- resilience_scores(mh_itemsT3, stress_itemsT3)

summary(resultsT3$LM) # get results of linear regression mh ~ stress
summary(resultsT3$MH_PCA) # mh component 1 explains 75% variance
summary(resultsT3$Stress_PCA) # stress component 1 explains 72% variance

# add resulting scores to data
data_matched$RF_T3 <- unlist(resultsT3[1])
data_matched$stressPCA_T3 <- unlist(resultsT3[2])
data_matched$mhPCA_T3 <- unlist(resultsT3[3])

# check data
grep("BSI_T3_INV", colnames(data_matched)) # index BSI_GSI inverted: 15
grep("WHO_T3", colnames(data_matched)) # index WHO: 16

index_max_mh <- which.max(data_matched$mhPCA_T3) # id with highest mh component 1 score
data_matched[index_max_mh, c(15,16)] # high WHO and BSI score, so high mh component score correctly indicates high mh
index_min_mh <- which.min(data_matched$mhPCA_T3) # id with lowest mh component 1 score
data_matched[index_min_mh,c(15,16)] # low WHO and BSI score, so low mh component score correctly indicates low mh

grep("MIMIS_T3_Frequency", colnames(data_matched)) # index MIMIS Frequency: 17
grep("LES_T3_Count", colnames(data_matched)) # index LES Count: 18

index_max_stress <- which.max(data_matched$stressPCA_T3) # id with highest stress component 1 score
data_matched[index_max_stress, c(17,18)] # high MIMIS and LES score
index_min_stress <- which.min(data_matched$stressPCA_T3) # id with lowest stress component 1 score
data_matched[index_min_stress,c(17,18)] # low MIMIS and LES score

# linear vs. quadratic?
m1 <- lm(data_matched$mhPCA_T3 ~ data_matched$stressPCA_T3) # linear model
summary.lm(m1)

m2 <- update(m1, .~.+ I(data_matched$stressPCA_T3^2)) # with quadratic term
summary.lm(m2)

# model comparison
anova(m1, m2)
AIC(m1, m2)
BIC (m1, m2)

# 5. merge and reshape ----------------------------
# merge data_matched with data
keep.cols <- c("Code","Gender", "Group", "Age", # demographics
               "HamSCQ_T0", "GSE_T0", "FSozU_T0", "LOT_Opt_T0") # resilience factors

data2 <- data %>% 
  select(keep.cols)

data_matched_wide <- list(data_matched, data2) %>% reduce(left_join, by = c("Code"))

# reshape to long format
data_matched_long <- reshape(data_matched_wide, varying = list(
  c("BSI_T0_INV", "BSI_T1_INV", "BSI_T2_INV", "BSI_T3_INV"), # BSI GSI inverted
  c("MIMIS_T0_Frequency", "MIMIS_T1_Frequency", "MIMIS_T2_Frequency", "MIMIS_T3_Frequency"), # MIMIS Frequency
  c("LES_T0_Count", "LES_T1_Count", "LES_T2_Count", "LES_T3_Count"), # LES Count
  c("WHO_T0", "WHO_T1", "WHO_T2", "WHO_T3"), # WHO
  c("RF_T0", "RF_T1", "RF_T2", "RF_T3"), # Resilient Functioning
  c("mhPCA_T0", "mhPCA_T1", "mhPCA_T2", "mhPCA_T3"),
  c("stressPCA_T0", "stressPCA_T1", "stressPCA_T2", "stressPCA_T3")
), 
v.names = c("BSI_GSI_INV", "MIMIS_Frequency", "LES_Count", "WHO", "RF", "mhPCA", "stressPCA"), 
timevar = "timepoint", times = c(0,1,2,3), idvar = c("Code"), direction = "long")

row.names(data_matched_long) <- c() # remove row names

# 6. save data ------------------------------------
write.table(data_matched_wide, "../data/data_matched_wide.csv", sep="\t", col.names = T, row.names = F)
write.table(data_matched_long, "../data/data_matched_long.csv", sep="\t", col.names = T, row.names = F)

# extract RF scores, their underlying components and possible trajectory predictors for Mplus
data_m <- data_matched_wide %>%
  select(matches('Code|Cohort|Age|Gender|RF|mhPCA|stressPCA|LOT_Opt|HamSCQ|FSozU|GSE'))
# sort columns
data_m <- data_m %>% 
  select(sort(current_vars())) %>%
  select(Code, Cohort, Age, Gender, everything())

# recode + prep covariates
data_m$Gender <- recode(data_m$Gender, '1'='0', '2'='1') # 0 = male, 1 = female
data_m$Age <- scale(data_m$Age)
data_m$Cohort <- recode(data_m$Cohort, '3'='0', '4'='1')

write.table(data_m, "../data/data4Mplus.csv", sep=",", col.names = F, row.names = F) # data to be read into Mplus

# covariance matrix
cov(data_m[13:16])

# skewness & kurtosis
psych::describe(data_m[13:16])
