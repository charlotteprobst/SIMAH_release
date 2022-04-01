rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(foreign)
library(SASxport)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(sjlabelled)
library(tidyverse)
library(naniar)
library(splitstackshape) 
library(truncnorm)

# CB laptop directory
wd <- "~/Google Drive/SIMAH Sheffield/"
# CB desktop director

data_new <- read.dta("SIMAH_workplace/brfss/processed_data/BRFSS_for_APC_covariates.dta") %>% 
  mutate(marital_status = ifelse(marital_status=="NA",NA,marital_status),
         employment_status = ifelse(employment_status=="NA",NA, employment_status)) %>% 
  drop_na(year, sex, race_eth, age, education, employment_status, marital_status, household_income)

stats_new <- data_new %>% group_by(year, sex) %>% summarise(meangpd = mean(gramsperday),
                                                            meanage = mean(age),
                                                            drinkingprevalence = mean(drinkingstatus)) %>% 
  mutate(datatype="new")

data_old <- read.dta("SIMAH_workplace/brfss/processed_data/BRFSS_for_APC.dta")

stats_old <- data_old %>% group_by(year, sex) %>% summarise(meangpd = mean(gramsperday),
                                                                      meanage = mean(age),
                                                                      drinkingprevalence = mean(drinkingstatus)) %>% 
  mutate(datatype="old")


compare <- rbind(stats_new, stats_old) %>% pivot_wider(names_from=datatype, values_from=c(meangpd, drinkingprevalence, meanage)) %>% 
  mutate(meangpddiff = (abs(meangpd_old - meangpd_new)/meangpd_old)*100,
         prevdiff = (abs(drinkingprevalence_old - drinkingprevalence_new)/drinkingprevalence_old)*100,
         agediff = (abs(meanage_old - meanage_new)/meanage_old)*100)

data_old$age_cat <- relevel(data_old$age_cat, ref="41-50")
data_new$age_cat <- relevel(data_new$age_cat, ref="41-50")


data_old$birth_cohort <- relevel(data_old$birth_cohort, ref="1956-1960")
data_new$birth_cohort <- relevel(data_new$birth_cohort, ref="1956-1960")


m1 <- glm.nb(gramsperday ~ year + age_cat + birth_cohort, data = subset(data_old,sex=="Male"))
exp(coef(m1))

m2 <- glm.nb(gramsperday ~ year + age_cat + birth_cohort, data = subset(data_new,sex=="Male"))
exp(coef(m2))



