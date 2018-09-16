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
rating_s2dat <- osf_s2_dat %>% 
  select(., cue_word, MeanRating_Survival: MeanRating_Vacation) %>% 
  gather(., key = "scenario", value = "rating", -(cue_word))

rating_s2dat$scenario <- fct_recode(rating_s2dat$scenario, survival = "MeanRating_Survival", vacation = "MeanRating_Vacation")
rating_s2dat$scenario <- as.factor(rating_s2dat$scenario)

## Recall Data
recall_s2dat <- osf_s2_dat %>% 
  select(., cue_word, Mean_Remembered_Survival:Mean_Remembered_Vacation) %>% 
  gather(., key = "scenario", value = "recall", -(cue_word))

recall_s2dat$scenario <- fct_recode(recall_s2dat$scenario, survival = "Mean_Remembered_Survival", vacation = "Mean_Remembered_Vacation")
recall_s2dat$scenario <- as.factor(recall_s2dat$scenario)

# Inferential Statistics - Multilevel Models

# note that because the words are being treated as participants scores are nested
# within them and as such thet are at level 2

## Ratings Model
rating_s2mod <- lme(rating ~ scenario, random = ~ 1|cue_word, data = rating_s2dat, method = "REML")
summary(rating_s2mod)

## Recall Model
recall_s2mod <- lme(recall ~ scenario, random = ~ 1|cue_word, data = recall_s2dat, method = "REML")
summary(recall_s2mod)

# Inferential Statistics - Bayes Factors

levels(rating_s2dat$cue_word)
rating_s2dat$cue_word <- as.character(rating_s2dat$cue_word)
rating_s2dat$cue_word[1:32] <- as.numeric(1:32)
rating_s2dat$cue_word[33:64] <- as.numeric(1:32)
rating_s2dat$cue_word <- as.factor(rating_s2dat$cue_word)

recall_s2dat$cue_word <- as.character(recall_s2dat$cue_word)
recall_s2dat$cue_word[1:32] <- as.numeric(1:32)
recall_s2dat$cue_word[33:64] <- as.numeric(1:32)
recall_s2dat$cue_word <- as.factor(recall_s2dat$cue_word)

## Ratings BF
rating_s2bf <- anovaBF(rating ~ scenario + cue_word, whichRandom = "cue_word", data = rating_s2dat)
rating_s2bf

## Recall BF
recall_s2bf <- anovaBF(recall ~ scenario + cue_word, whichRandom = "cue_word", data = recall_s2dat)
recall_s2bf
