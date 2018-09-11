library(tidyverse)
library(readxl)
library(nlme)
library(BayesFactor)

# Import the word level data set
url <- "https://osf.io/4nd8g/download"
destfile <- "osf_s1_dat.xls"

curl::curl_download(url, destfile)
osf_s2_dat <- read_excel(destfile, sheet = 2)

osf_s2_dat$`Stimuli Word German` <- as.factor(osf_s2_dat$`Stimuli Word German`)
osf_s2_dat$Condition <- as.factor(osf_s2_dat$Condition)
osf_s2_dat <- rename(osf_s2_dat, cue_word = "Stimuli Word German")

# Go to long format
## Ratings Data
rating_dat <- osf_s2_dat %>% 
  select(., cue_word, MeanRating_Survival: MeanRating_Vacation) %>% 
  gather(., key = "scenario", value = "rating", -(cue_word))

rating_dat$scenario <- fct_recode(rating_dat$scenario, survival = "MeanRating_Survival", vacation = "MeanRating_Vacation")
rating_dat$scenario <- as.factor(rating_dat$scenario)

## Recall Data
recall_dat <- osf_s2_dat %>% 
  select(., cue_word, Mean_Remembered_Survival:Mean_Remembered_Vacation) %>% 
  gather(., key = "scenario", value = "recall", -(cue_word))

recall_dat$scenario <- fct_recode(recall_dat$scenario, survival = "Mean_Remembered_Survival", vacation = "Mean_Remembered_Vacation")
recall_dat$scenario <- as.factor(recall_dat$scenario)

# Inferential Statistics - Multilevel Models

# note that because the words are being treated as participants scores are nested
# within them and as such thet are at level 2

## Ratings Model
rating_mod <- lme(rating ~ scenario, random = ~ 1|cue_word, data = rating_dat, method = "REML")
summary(rating_mod)

## Recall Model
recall_mod <- lme(recall ~ scenario, random = ~ 1|cue_word, data = recall_dat, method = "REML")
summary(recall_mod)

# Inferential Statistics - Bayes Factors
## Ratings BF
rating_bf <- anovaBF(rating ~ scenario + cue_word, whichRandom = "cue_word", data = rating_dat)
rating_bf

## Recall BF
recall_bf <- anovaBF(recall ~ scenario + cue_word, whichRandom = "cue_word", data = recall_dat)
recall_bf