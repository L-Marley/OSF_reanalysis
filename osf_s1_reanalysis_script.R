library(tidyverse)
library(readxl)
library(nlme)
library(BayesFactor)

# Import the participant level data set

url <- "https://osf.io/4nd8g/download"
destfile <- "osf_s1_dat.xls"
curl::curl_download(url, destfile)
osf_s1_dat <- read_excel(destfile, col_types = c("numeric", "text", "numeric", "numeric", "numeric", 
                                                 "numeric", "numeric", "numeric"))

osf_s1_dat$Subject <- as.factor(osf_s1_dat$Subject)
osf_s1_dat$Condition <- as.factor(osf_s1_dat$Condition)

# Go to long format
## Ratings Data
rating_dat <- osf_s1_dat %>%
  select(., Subject:MeanRating_VacationWords) %>%
  gather(., key = "scenario", value = "rating", -c(Subject, Condition))

rating_dat$scenario <- fct_recode(rating_dat$scenario, survival = "MeanRating_SurvivalWords", vacation = "MeanRating_VacationWords")

## Recall Data
recall_dat <- osf_s1_dat %>%
  select(., Subject, Mean_RememberedWords_Survival:Mean_RememberedWords_Vacation) %>%
  gather(., key = "scenario", value = "recall", -(Subject)) 

recall_dat$scenario <- fct_recode(recall_dat$scenario, survival = "Mean_RememberedWords_Survival", vacation = "Mean_RememberedWords_Vacation")

## Reaction Time Data
RT_dat <- osf_s1_dat %>% 
  select(., Subject, ResponseTime_Survival:ResponseTime_Vacation) %>% 
  gather(., key = "scenario", value = "RT", -(Subject))

RT_dat$scenario <- fct_recode(RT_dat$scenario, survival = "ResponseTime_Survival", vacation = "ResponseTime_Vacation")

# Inferential Statistics - Multilevel Models
## Ratings Model
rating_mod <- lme(rating ~ scenario, random = ~ 1|Subject, data = rating_dat, method = "REML")
summary(rating_mod)

## Recall Model
recall_mod <- lme(recall ~ scenario, random = ~ 1|Subject, data = recall_dat, method = "REML")
summary(recall_mod)

## RT Model
RT_mod <- lme(RT ~ scenario, random =  ~1|Subject, data = RT_dat, method = "REML")
summary(RT_mod)

# Inferential Statistics - Bayes Factors
## Ratings BF
rating_bf <- anovaBF(rating ~ scenario + Subject, whichRandom = "Subject", data = rating_dat)
rating_bf

## Recall BF
recall_bf <- anovaBF(recall ~ scenario + Subject, whichRandom = "Subject", data = recall_dat)
recall_bf

## RT BF
RT_bf <- anovaBF(RT ~ scenario + Subject, whichRandom = "Subject", data = RT_dat)
RT_bf
