# Prepare and format data for resilience trajectory study

# Laura E. Meine; laura.meine@uni-mainz.de
# Last updated: 12.11.2021

# 0. load required libraries ----------------------
library(tidyverse) # data formatting, ggplot2
library(magrittr) # piping %<>%

# 1. load and format data -------------------------
data_cohort3 <- read.csv("../data/cohort3_T6_controls_raw.csv", header=T, sep=",")
data_cohort4 <- read.csv("../data/cohort4_T4_controls_raw.csv", header=T, sep=",")

# rename variables to match cohort 4
names(data_cohort3) <- gsub(x = names(data_cohort3), pattern = "T6", replacement = "T4")  

# merge data sets
data <- rbind(data_cohort3, data_cohort4)
str(data)

# as.factor
cols <- c("Cohort", "Code", "Group")
data %<>% mutate_at(cols, list(~ as.factor(.)))

# check for duplicate ids
duplicates <- data %>% 
  group_by(Code) %>%
  filter(n() > 1) # 0

rm(duplicates, cols) # tidy up

# 2. compute questionnaire scores -----------------
# 2.1 PSS -----------------------------------------
# Perceived Stress Scale, see Cohen, Karmack, Mermelstein, 1983, J Health Soc Behav
# recode answers for scoring
oldvalues <- c(1,2,3,4,5) 
newvalues <- c(0,1,2,3,4)

for(i in names(data[, grep("PSS", colnames(data))])) { # recode PSS values
  data[[i]] <- newvalues[match(data[[i]], oldvalues)]}

newvaluesI <- c(4,3,2,1,0) # positive items (4,5,7,8) need to be inverted
data$PSS_T4_4r <- data$PSS_T4_4; data$PSS_T4_5r <- data$PSS_T4_5; data$PSS_T4_7r <- data$PSS_T4_7; data$PSS_T4_8r <- data$PSS_T4_8; # create new variables for inverting

for(i in names(data[, which(names(data) %in% c("PSS_T4_4r", "PSS_T4_5r", "PSS_T4_7r", "PSS_T4_8r"))])) {
  data[[i]] <- newvaluesI[match(data[[i]], newvalues)]} # invert positive items

# create sum score (0-13 low stress; 14-26 moderate; 27-40 high)
data$PSS_P <- rowSums(data[, which(names(data) %in% c("PSS_T4_1", "PSS_T4_2", "PSS_T4_3", "PSS_T4_4r", "PSS_T4_5r",
                                                         "PSS_T4_6", "PSS_T4_7r", "PSS_T4_8r", "PSS_T4_9", "PSS_T4_10"))]) # higher score = higher stress level
rm(i, oldvalues, newvalues, newvaluesI) # tidy up

# 2.2 GSE -----------------------------------------
# General Self-Efficacy Scale, see Schwarzer & Jerusalem, 1999

# compute total sum score
data <- data %>%
  mutate(GSE_P = rowSums(across(matches("^GSE"))))
                                             
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
  mutate(WHO_P = rowSums(across(matches("^WHO")))*4)

# 2.4 BSI -----------------------------------------
# Brief Symptom Inventory, see Derogatis, L. R. (2001). Brief Symptom Inventory (BSI)-18: Administration, scoring and procedures manual. Minneapolis, MN: NCS Pearson.

oldvalues <- c(1,2,3,4,5) # SoSci uses 1-5
newvalues <- c(4,3,2,1,0) # original manual describes 0-4, invert items

for(i in names(data[, grep("BSI", colnames(data))])) { # recode BSI values to match manual description
  data[[i]] <- newvalues[match(data[[i]], oldvalues)]}

# calculate subscale scores 
data$BSI_P_SOM_INV <- rowSums(data[, which(names(data) %in% c("BSI_T4_1", "BSI_T4_4", "BSI_T4_7", "BSI_T4_10", "BSI_T4_13", "BSI_T4_16"))]) 
data$BSI_P_DEP_INV <- rowSums(data[, which(names(data) %in% c("BSI_T4_2", "BSI_T4_5", "BSI_T4_8", "BSI_T4_11", "BSI_T4_14", "BSI_T4_17"))]) 
data$BSI_P_ANX_INV <- rowSums(data[, which(names(data) %in% c("BSI_T4_3", "BSI_T4_6", "BSI_T4_9", "BSI_T4_12", "BSI_T4_15", "BSI_T4_18"))])

# calculate global severity index
data$BSI_P_GSI_INV = data$BSI_P_SOM_INV + data$BSI_P_DEP_INV + data$BSI_P_ANX_INV

rm(i, oldvalues, newvalues) # tidy up

# 2.5 HamSCQ -----------------------------------------
# Hamburg Self-Care Questionnaire, see Harfst et al., 2009, Die Rehabilitation

# create mean score (higher scores reflect more self-care)
data <- data %>%
  mutate(HamSCQ_P = rowMeans(across(matches("^HamSCQ"))))

# 2.6 MIMIS -----------------------------------------
# Daily Hassles/Microstressors, see Chmitorz et al., 2020, JMIR Ment Health
# (uses indexing because it was too much of a hassle (pun intended) to type it all out...but careful to use correct variables!)

oldvalues <- c(-9, 99999, -1, 1,2,3,4,5,6,7) # missing values
missingvalues <- c(NA, NA, NA,1,2,3,4,5,6,7)

for(i in names(data[, grep("MIMIS_F_", colnames(data))])) { # recode MIMIS values
  data[[i]] <- missingvalues[match(data[[i]], oldvalues)]}
rm(i, oldvalues, missingvalues)

data <- data %>%
  mutate(MIMIS_P_freq = rowSums(across(matches("^MIMIS_F_")), na.rm=T)) # microstressor frequency score

# 2.7 LES -----------------------------------------
# Life Events Checklist, see Chmitorz et al., 2020, Eur Arch Psychiatry Clin Neurosci

oldvalues <- c(-1,1,2,3,4,5) # missing values -1
newvalues <- c(NA,0,1,1,1,1) # count only those stressors that people rated as at least a bit burdensome

for(i in names(data[, grep("LES_T4", colnames(data))])) { # recode LES variables
  data[[i]] <- newvalues[match(data[[i]], oldvalues)]}
rm(i, oldvalues, newvalues)

# create score for count
data <- data %>%
  mutate(LES_P_Count = rowSums(across(matches("^LES_T4")), na.rm=T))

# 2.8 BSSS -----------------------------------------
# Berlin Social Support Scales, see Schwarzer & Schulz, 2003, Diagnostica

# compute subscale perceived emotional support
data$BSSS_P_perEmoSupport  <- rowSums(data[, which(names(data) %in% c("BSSS_T4_1", "BSSS_T4_2", "BSSS_T4_3", "BSSS_T4_4"))]) # higher score = more perceived emotional support

# 2.9 COR HS -------------------------------------
# Corona Stressors, see Veer et al., 2021, Transl Psychiatry
# COR_HS_T4_1-29

oldvalues <- c(-1,-9,1,2,3,4,5) # missing values
missingvalues <- c(NA,NA,1,2,3,4,5) # recode

for(i in names(data[, grep("COR_HS", colnames(data))])) {
  data[[i]] <- missingvalues[match(data[[i]], oldvalues)]}
rm(i, oldvalues, missingvalues)

# mean of intensity ratings
data <- data %>%
  mutate(COR_HS_P_Int = rowMeans(across(matches("^COR_HS")), na.rm=T))

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
newvalues <- c(4,3,2,1,0) # original manual describes to 0-4 

for(i in names(data[, grep("COR_LOT", colnames(data))])) { # invert LOTR values
  data[[i]] <- newvaluesI[match(data[[i]], oldvalues)]}

for(i in names(data[, grep("COR_LOT", colnames(data))])) { # recode LOTR values
  data[[i]] <- newvalues[match(data[[i]], newvaluesI)]}

rm(i, oldvalues, newvalues, newvaluesI) # tidy up

# create sub score optimism
data$LOT_Opt_P  <- rowSums(data[, which(names(data) %in% c("COR_LOT_T4_1", "COR_LOT_T4_4", "COR_LOT_T4_10"))])

# 4. save data ------------------------------------
# extract relevant columns
data_p <- data %>%
  select(matches('Code|Cohort|Gender|Age|COR_HS_|COR_F_|PSS_P|GSE_P|HamSCQ_P|LOT_Opt_P|BSSS_P|WHO_P|BSI_P_GSI|MIMIS_P|LES_P'))

# sort columns
data_p <- data_p %>% 
  select(sort(current_vars())) %>%
  select(Code, Cohort, Gender, Age, everything())

write.table(data_p, "../data/data_matched_pandemic.csv", sep="\t", col.names = T, row.names = F)
