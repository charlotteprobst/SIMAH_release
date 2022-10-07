# script for SIMAH project converting between categorical and continuous GPD 
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(tidyverse)
library(MASS)

# CB laptop directory
wd <- "~/Google Drive/SIMAH Sheffield/"
setwd(wd)
# read data
dat <- readRDS("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_1984_2020_final.RDS") %>% filter(YEAR>=2000)

assign_alc_cat <- function(data){
  data <- data %>% mutate(AlcCAT = ifelse(sex_recode=="Male" & gramsperday>0 &
                                   gramsperday<=40, "Low risk",
                                  ifelse(sex_recode=="Female" & gramsperday>0 &
                                           gramsperday<=20, "Low risk",
                                         ifelse(sex_recode=="Male" & gramsperday>40 &
                                                  gramsperday<=60, "Medium risk",
                                                ifelse(sex_recode=="Female" & gramsperday>20 &
                                                         gramsperday<=40, "Medium risk",
                                                       ifelse(sex_recode=="Male" & gramsperday>60,
                                                              "High risk",
                                                              ifelse(sex_recode=="Female" & gramsperday>40,
                                                                     "High risk", NA)))))))
  return(data)
}

lambda <- 0.129 #lambda for transforming consumption taken from M.Strong / D.Moyo script

dat <- dat %>% assign_alc_cat() %>% 
  filter(gramsperday!=0) %>% #remove non drinkers - don't want to model them 
  mutate(transcons = (gramsperday ^ lambda - 1) / lambda) %>% #transform consumption - following M Strong approach
  mutate(sex_recode = factor(sex_recode), education_summary = factor(education_summary),
         employment = factor(employment), race_eth = factor(race_eth), 
         AlcCAT = factor(AlcCAT))
  
# specify model - following M Strong approach with variables available in microsimulation and including race and ethnicity 
m <- lm(transcons ~ age_var + I(age_var^2) + I(age_var^3) + sex_recode + YEAR + 
          education_summary  + AlcCAT + age_var*sex_recode + age_var*education_summary + age_var*AlcCAT +
          sex_recode*education_summary + sex_recode*AlcCAT + education_summary*AlcCAT, data=dat)
# QUESTIONS
# about whether to include frequency in the model - are we assuming that this is static for people over time - 
# seems a strong assumption 
# also I have included year as a predictor - sensible? 

summary(m)
summary(m)$sigma^2
sigma <- summary(m)$sigma

back.tran <- function(x){(lambda * x + 1) ^ (1/lambda)}

# see how the model performs on a given years data

test_model <- function(dat,year){
year <- dat %>% filter(YEAR==year) %>% assign_alc_cat() %>% 
  filter(gramsperday!=0) %>% #remove non drinkers - don't want to model them 
  mutate(transcons = (gramsperday ^ lambda - 1) / lambda) %>% #transform consumption - following M Strong approach
  mutate(sex_recode = factor(sex_recode), education_summary = factor(education_summary),
         employment = factor(employment), race_eth = factor(race_eth), 
         AlcCAT = factor(AlcCAT)) %>% 
  mutate(oldgpd = gramsperday, oldAlcCAT = AlcCAT)

# predict grams per day using model
year$gramsperday <- back.tran(predict(m, year))

# assign alc category based on new grams per day 
year <- assign_alc_cat(year)

# look at the proportion of these correctly assigned - and whether there are systematic differences in
# model performance based on socio demographic groups
proportion <- year %>% mutate(correct = ifelse(oldAlcCAT == AlcCAT, 1,0)) %>% 
  group_by(sex_recode, education_summary, race_eth, correct) %>% tally() %>% ungroup() %>% 
  group_by(sex_recode,education_summary, race_eth) %>% mutate(prop = n/sum(n)) %>% 
  filter(correct==1)
# accuracy with classifying the alcohol categories

cor <- cor.test(year$gramsperday, year$oldgpd)
list <- list(proportion, cor)
# correlation with old and new gpd on training data set 
return(list)
}

proportion <- test_model(dat, 2019)[[1]]
test_model(dat, 2010)[[2]]

# model seems ok for now - save a copy for implementation in the microsimulation 
saveRDS(m, "SIMAH_workplace/microsim/1_input_data/CatContModel1.RDS")

