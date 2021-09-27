# Prepare and format data for resilience trajectory study

# Laura E. Meine; laura.meine@uni-mainz.de
# Last updated: 27.09.2021

# 0. load required libraries ----------------------
library(tidyverse) # data formatting, ggplot2
library(magrittr) # piping %<>%
library(corrplot) # correlation matrix
source("R_functions/resilience_scores.R") # compute resilient functioning score

# 1. load and format data -------------------------
data_cohort3 <- read.csv("../data/cohort3_T6_raw_data.csv", header=T, sep=",")
data_cohort4 <- read.csv("../data/cohort4_T4_raw_data.csv", header=T, sep=",")

# rename variables to match cohort 4
names(data_cohort3) <- gsub(x = names(data_cohort3), pattern = "T6", replacement = "T4")  

# merge data sets
data <- rbind(data_cohort3, data_cohort4)

# as.factor
cols <- c("Kohorte", "Code", "Gruppe")
data %<>% mutate_at(cols, funs(as.factor(.)))

# delete experiment group data, keep only controls
data <- data %>%
  filter(!data$Gruppe=="EG") 

# check for duplicate ids
duplicates <- data %>% 
  group_by(Code) %>%
  filter(n() > 1) # 0

rm(duplicates) # tidy up

# 2. compute questionnaire scores -----------------
# 2.1 PSS -----------------------------------------
# Perceived Stress Scale, see Cohen, Karmack, Mermelstein, 1983, J Health Soc Behav
# recode answers for scoring
oldvalues <- c(1,2,3,4,5) 
newvalues <- c(0,1,2,3,4)
newvaluesI <- c(4,3,2,1,0) # positive items (4,5,7,8) need to be inverted
data$PSS_T4_4r <- data$PSS_T4_4; data$PSS_T4_5r <- data$PSS_T4_5; data$PSS_T4_7r <- data$PSS_T4_7; data$PSS_T4_8r <- data$PSS_T4_8; # create new variables for inverting

for(i in names(data[, which(names(data) %in% c("PSS_T4_1", "PSS_T4_2", "PSS_T4_3", "PSS_T4_4", "PSS_T4_5",
                                               "PSS_T4_6", "PSS_T4_7", "PSS_T4_8", "PSS_T4_9", "PSS_T4_10"))])) {
  data[[i]] <- newvalues[match(data[[i]], oldvalues)]}

for(i in names(data[, which(names(data) %in% c("PSS_T4_4r", "PSS_T4_5r", "PSS_T4_7r", "PSS_T4_8r"))])) {
  data[[i]] <- newvaluesI[match(data[[i]], oldvalues)]} # invert positive items

# create sum score (0-13 low stress; 14-26 moderate; 27-40 high)
data$PSS_P_Gesamt <- rowSums(data[, which(names(data) %in% c("PSS_T4_1", "PSS_T4_2", "PSS_T4_3", "PSS_T4_4r", "PSS_T4_5r",
                                                         "PSS_T4_6", "PSS_T4_7r", "PSS_T4_8r", "PSS_T4_9", "PSS_T4_10"))]) # higher score = higher stress level
rm(i, oldvalues, newvalues, newvaluesI) # tidy up

# 2.2 GSE -----------------------------------------
# General Self-Efficacy Scale, see Schwarzer & Jerusalem, 1999

# compute total sum score
data <- data %>%
  mutate(GSE_P_Gesamt = rowSums(across(matches("^GSE"))))
                                             
# scores can range from 10-40
# higher scores indicate greater perceived self-efficacy

# 2.3 WHO -----------------------------------------
# World Health Organisation Well-being Index, see Topp et al., 2015, Psychother and Psychosom, DOI:0.1159/000376585

# recode answers for scoring
oldvalues <- c(1,2,3,4,5,6) 
newvalues <- c(0,1,2,3,4,5)

for(i in names(data[, grep("WHO", colnames(data))])) { # recode WHO values
  data[[i]] <- newvalues[match(data[[i]], oldvalues)]}

rm(i, oldvalues, newvalues) # tidy up

# create score (higher scores reflect better well-being)
data <- data %>%
  mutate(WHO_P_Gesamt = rowSums(across(matches("^WHO")))*4)

# 2.4 BSI -----------------------------------------
# Brief Symptom Inventory, see Derogatis, L. R. (2001). Brief Symptom Inventory (BSI)-18: Administration, scoring and procedures manual. Minneapolis, MN: NCS Pearson.

oldvalues <- c(1,2,3,4,5) # SoSci uses 1-5
newvalues <- c(4,3,2,1,0) # original manual describes 0-4, invert items

for(i in names(data[, grep("BSI", colnames(data))])) { # recode BSI values to match manual description
  data[[i]] <- newvalues[match(data[[i]], oldvalues)]}

# calculate subscale scores 
data$BSI_P_SOMA_INV <- rowSums(data[, which(names(data) %in% c("BSI_T4_1", "BSI_T4_4", "BSI_T4_7", "BSI_T4_10", "BSI_T4_13", "BSI_T4_16"))]) # higher score = more somatization
data$BSI_P_DEPR_INV <- rowSums(data[, which(names(data) %in% c("BSI_T4_2", "BSI_T4_5", "BSI_T4_8", "BSI_T4_11", "BSI_T4_14", "BSI_T4_17"))]) # higher score = more depressive symtoms
data$BSI_P_ANGS_INV <- rowSums(data[, which(names(data) %in% c("BSI_T4_3", "BSI_T4_6", "BSI_T4_9", "BSI_T4_12", "BSI_T4_15", "BSI_T4_18"))]) # higher score = more anxiety

# calculate global severity index
data$BSI_P_GSI_INV = data$BSI_P_SOMA_INV + data$BSI_P_DEPR_INV + data$BSI_P_ANGS_INV

rm(i, oldvalues, newvalues) # tidy up

# 2.5 HSF -----------------------------------------
# Hamburg Self-Care Questionnaire, see Harfst et al., 2009, Die Rehabilitation

# create mean score (higher scores reflect more self-care)
data <- data %>%
  mutate(HSF_P_Gesamt = rowMeans(across(matches("^HSF"))))

# 2.6 DH -----------------------------------------
# Daily Hassles/Microstressors, see Chmitorz et al., 2020, JMIR Ment Health
# (uses indexing because it was too much of a hassle (pun intended) to type it all out...but careful to use correct variables!)

oldvalues <- c(-9, 99999, -1, 1,2,3,4,5,6,7) # missing values;  s. Boosterit Syntax
missingvalues <- c(NA, NA, NA,1,2,3,4,5,6,7)

for(i in names(data[, grep("DHP", colnames(data))])) { # frequency of microstressor encounters
  data[[i]] <- missingvalues[match(data[[i]], oldvalues)]}
rm(i, oldvalues, missingvalues)

oldvalues <- c(-9, 99999, -1, 1,2,3,4,5) # missing values;  s. Boosterit Syntax
missingvalues <- c(NA, NA, NA,0,1,2,3,4) # recode

for(i in names(data[, grep("DHS", colnames(data))])) { # intensity of microstressors
  data[[i]] <- missingvalues[match(data[[i]], oldvalues)]}
rm(i, oldvalues, missingvalues)

grep("DHP_T4_1", colnames(data)) # index first DH item occurrence # 61
grep("DHP_T4_58", colnames(data)) # index last DH item occurrence # 118
grep("DHS_T4_1", colnames(data)) # index first DH item intensity # 119
grep("DHS_T4_58", colnames(data)) # index last DH item intensity # 176

# sum of hassles across all items - one hassle may have occurred on multiple days
data$DH_P_freq <- apply(data[, c(61:118)], 1, function(x) sum(x[x > 0], na.rm=T)) # sum of positive ratings only, exclude -1 (no occurrence)
data$DH_P_intensity <- apply(data[, c(119:176)], 1, function(x) sum(x[x > 0], na.rm=T)) # sum of intensity ratings

rm(i, oldvalues, missingvalues) # tidy up

# 2.7 LES -----------------------------------------
# Life Events Checklist, see Chmitorz et al., 2020, Eur Arch Psychiatry Clin Neurosci

oldvalues <- c(-1,1,2,3,4,5) # missing values;  s. Boosterit Syntax
missingvalues <- c(NA,0,1,2,3,4) # recode

for(i in names(data[, grep("LES", colnames(data))])) {
  data[[i]] <- missingvalues[match(data[[i]], oldvalues)]}
rm(i, oldvalues, missingvalues)

# create score for mean intensity
data <- data %>%
  mutate(LES_P_Severity = rowMeans(across(matches("^LES_T4")), na.rm=T))

# recode answers to count frequency of life events regardless of intensity
oldvalues <- c(NA,0,1,2,3,4) 
newvalues <- c(0,0,1,1,1,1) # count only those stressors that people rated as at least a bit burdensome  (s. also Boosterit syntax)

# this is not good practice! shouldn't overwrite original scores!
for(i in names(data[, grep("LES_T4", colnames(data))])) { # recode LES variables
  data[[i]] <- newvalues[match(data[[i]], oldvalues)]}

# create score for count
data <- data %>%
  mutate(LES_P_Count = rowSums(across(matches("^LES_T4")), na.rm=T))

# 2.8 BSSS -----------------------------------------
# Berlin Social Support Scales, see Schwarzer & Schulz, 2003, Diagnostica

# last item of need for support scale has to be inverted (BSSS_T4_12)
oldvalues <- c(1,2,3,4)
newvaluesI <- c(4,3,2,1)

data$BSSS_T4_12 <- newvaluesI[match(data$BSSS_T4_12, oldvalues)] # item 12

rm(oldvalues, newvaluesI) # tidy up

# compute subscale perceived emotional support
data$BSSS_P_perEmoSupport  <- rowSums(data[, which(names(data) %in% c("BSSS_T4_1", "BSSS_T4_2", "BSSS_T4_3", "BSSS_T4_4"))]) # higher score = more perceived emotional support
# compute subscale perceived instrumental support
data$BSSS_P_perInstSupport <- rowSums(data[, which(names(data) %in% c("BSSS_T4_5", "BSSS_T4_6", "BSSS_T4_7", "BSSS_T4_8"))]) # higher score = more perceived instrumental support
# compute score for need for support scale
data$BSSS_P_needSupport <- rowSums(data[, which(names(data) %in% c("BSSS_T4_9", "BSSS_T4_10", "BSSS_T4_11", "BSSS_T4_12"))]) # higher score = more need for support
# compute score for support seeking scale
data$BSSS_P_seekSupport <- rowSums(data[, which(names(data) %in% c("BSSS_T4_13", "BSSS_T4_14", "BSSS_T4_15", "BSSS_T4_16", "C_BS01_17"))]) # higher score = more support seeking

# 2.9 COR HS -------------------------------------
# Corona Stressors, see Veer et al., 2021, Transl Psychiatry
# COR_HS_T4_1-29

oldvalues <- c(-1,-9,1,2,3,4,5) # missing values;  s. Boosterit Docu
missingvalues <- c(NA,NA,1,2,3,4,5) # recode

for(i in names(data[, grep("COR_HS", colnames(data))])) {
  data[[i]] <- missingvalues[match(data[[i]], oldvalues)]}
rm(i, oldvalues, missingvalues)

# sum of intensity ratings
data <- data %>%
  mutate(COR_HS_P_Intensity = rowSums(across(matches("^COR_HS")), na.rm=T))
# mean of intensity ratings
data <- data %>%
  mutate(COR_HS_P_mean_Intensity = rowMeans(across(matches("^COR_HS_T4")), na.rm=T))

# recode answers to count type of stressors regardless of frequency
oldvalues <- c(1,2,3,4,5) 
newvalues <- c(1,1,1,1,1)

for(i in names(data[, grep("COR_HS_T4", colnames(data))])) { 
  data[[i]] <- newvalues[match(data[[i]], oldvalues)]}

# sum of hassle types encountered
data <- data %>%
  mutate(COR_HS_P_Count = rowSums(across(matches("^COR_HS_T4")), na.rm=T))

# 2.10 LOT-R ------------------------------------
# Life-Orientation-Test, revised version, see Glaesmer et al., 2008, Zeitschrift für Gesundheitspsychologie
# 4 filler items that are not considered

oldvalues <- c(1,2,3,4,5) # SoSci uses 1-5
newvaluesI <- c(5,4,3,2,1) # SoSci error, all items need to be inverted (s. Boosterit Syntax)
newvalues <- c(0,1,2,3,4) # original manual describes to 0-4 

for(i in names(data[, grep("COR_LOT", colnames(data))])) { # invert LOTR values
  data[[i]] <- newvaluesI[match(data[[i]], oldvalues)]}

for(i in names(data[, grep("COR_LOT", colnames(data))])) { # recode LOTR values
  data[[i]] <- newvalues[match(data[[i]], oldvalues)]}

rm(i, oldvalues, newvalues, newvaluesI) # tidy up

# create sub score optimism
data$LOT_P_Optimismus  <- rowSums(data[, which(names(data) %in% c("COR_LOT_T4_1", "COR_LOT_T4_4", "COR_LOT_T4_10"))])
# create sub score pessimism
data$LOT_P_Pessimismus <- rowSums(data[, which(names(data) %in% c("COR_LOT_T4_3", "COR_LOT_T4_7", "COR_LOT_T4_9"))])

# invert some items to calculate total score
oldvalues <- c(4,3,2,1,0)
newvaluesI <- c(0,1,2,3,4)

for(i in names(data[, which(names(data) %in% c("COR_LOT_T4_3", "COR_LOT_T4_7", "COR_LOT_T4_9"))])) {
  data[[i]] <- newvaluesI[match(data[[i]], oldvalues)]} # invert negative items

rm(i, oldvalues, newvaluesI) # tidy up

# create total score
data$LOT_P_Gesamt <- rowSums(data[, which(names(data) %in% c("COR_LOT_T4_1", "COR_LOT_T4_3", "COR_LOT_T4_4",
                                                             "COR_LOT_T4_7", "COR_LOT_T4_9", "COR_LOT_T4_10"))])

# 3. Resilient functioning  score -------------------
# indicators of mental health
mh_items <- data %>% select(BSI_P_GSI_INV, WHO_P_Gesamt)

# indicators of stress (could also use DH diversity instead of frequency, but regression model seems better with frequency and this measure aligns better with LES score)
stress_items <- data %>% select(DH_P_freq, LES_P_Count)

results <- resilience_scores(mh_items, stress_items)

summary(results$LM) # get results of linear regression mh ~ stress
summary(results$MH_PCA) # mh component 1 explains 76% variance
summary(results$Stress_PCA) # stress component 1 explains 67% variance

# join resulting scores to data
data$RF_P <- unlist(results[1])
data$stressPCA_P <- unlist(results[2])
data$mhPCA_P <- unlist(results[3])

# check data
grep("BSI_P_GSI_INV", colnames(data)) # index BSI_GSI inverted: 278
grep("WHO_P_Gesamt", colnames(data)) # index WHO: 274

index_max_mh <- which.max(data$mhPCA_P) #id with highest mh component 1 score
data[index_max_mh, c(278,274)] #high WHO, BSI score, so high mh component score correctly indicates high mh
index_min_mh <- which.min(data$mhPCA_P) #id with lowest mh component 1 score
data[index_min_mh,c(278,274)] #low WHO, BSI score, so low mh component score correctly indicates low mh

grep("DH_P_freq", colnames(data)) # index DH Frequency: 280
grep("LES_P_Count", colnames(data)) # index LES Frequency: 283

index_max_stress <- which.max(data$stressPCA_P) #id with highest stress component 1 score
data[index_max_stress, c(280,283)] #high DH and LES score
index_min_stress <- which.min(data$stressPCA_P) #id with lowest stress component 1 score
data[index_min_stress,c(280,283)] #low DH and LES score

# 4. save data ------------------------------------
# extract relevant columns
data_p <- data %>%
  select(matches('Code|Kohorte|Gender|Age|BRS_P|PSS_P|GSE_P|HSF_P|LOT_P|BSSS_P|WHO_P|BSI_P|HSF_P|BC_P|DH_P|LES_P|CERQ_P|COR_HS_|RF_P|mhPCA_P|stressPCA_P|COR_F_'))
# sort columns
data_p <- data_p %>% 
  select(sort(current_vars())) %>%
  select(Code, Kohorte, Gender, Age, everything())

write.table(data_p, "../data/data_matched_pandemic.csv", sep="\t", col.names = T, row.names = F)
