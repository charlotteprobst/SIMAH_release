
# SES x Lifestyle Differential Vulnerability & Exposure Project
# Descriptive Statistics

# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(gmodels)    # CrossTable command
library(tableone)   # create table one
library(survival)   # surivval analyses
library(survminer)  # surivval analyses
library(timereg)    # additive survival models

# Specify the data and output file locations
data    <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Processed data/"   # Location of data
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/PsychDistress/Output/"      # Location of figures/tables
source("Function - Format Results.R")

# Load data
nhis        <- readRDS (paste0(data, "nhis.rds"))
nhis_male   <- readRDS (paste0(data, "nhis_male.rds"))
nhis_female <- readRDS (paste0(data, "nhis_female.rds"))

CrossTable(nhis$alcohol5v2.factor, nhis$allcause_death.factor, prop.r=FALSE,  prop.t=FALSE, prop.chisq=FALSE)
CrossTable(nhis$k6scale3.factor, nhis$allcause_death.factor, prop.r=FALSE,  prop.t=FALSE, prop.chisq=FALSE)
CrossTable(nhis$edu.factor, nhis$allcause_death.factor, prop.r=FALSE,  prop.t=FALSE, prop.chisq=FALSE)



# PsychDistress
aalen_k6_m  <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(k6scale3.factor) + const(married.factor) + const(edu.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
aalen_10000py(aalen_k6_m, 1); aalen_10000py(aalen_k6_m, 2); # print results of interest

aalen_k6_f  <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(k6scale3.factor) + const(married.factor) + const(edu.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
aalen_10000py(aalen_k6_f, 1); aalen_10000py(aalen_k6_f, 2); # print results of interest



# Alcohol x PsychDistress Interaction
nhis_male <- mutate(nhis_male, alc_k6 = interaction(alcohol5v2.factor, k6scale3.factor))
aalen_alc_k6_m  <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(alcohol5v2.factor)*const(k6scale3.factor) + const(married.factor) + const(edu.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
aalen_alc_k6_m2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(alc_k6) +                                   const(married.factor) + const(edu.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
aalen_10000py(aalen_alc_k6_m, 4); aalen_10000py(aalen_alc_k6_m, 6); aalen_10000py(aalen_alc_k6_m2, 14); aalen_10000py(aalen_alc_k6_m, 34) # print results of interest

nhis_female <- mutate(nhis_female, alc_k6 = interaction(alcohol5v2.factor, k6scale3.factor))
aalen_alc_k6_f  <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(alcohol5v2.factor)*const(k6scale3.factor) + const(married.factor) + const(edu.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
aalen_alc_k6_f2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(alc_k6) +                                   const(married.factor) + const(edu.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
aalen_10000py(aalen_alc_k6_f, 4); aalen_10000py(aalen_alc_k6_f, 6); aalen_10000py(aalen_alc_k6_f2, 14); aalen_10000py(aalen_alc_k6_f, 34) # print results of interest



# Edu x PsychDistress Interaction
nhis_male <- mutate(nhis_male, edu_k6 = interaction(edu.factor, k6scale3.factor))
aalen_edu_k6_m  <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(k6scale3.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
aalen_edu_k6_m2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu_k6) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
aalen_10000py(aalen_edu_k6_m, 1); aalen_10000py(aalen_edu_k6_m, 4); aalen_10000py(aalen_edu_k6_m2, 7); aalen_10000py(aalen_edu_k6_m, 25) # print results of interest

nhis_female <- mutate(nhis_female, edu_k6 = interaction(edu.factor, k6scale3.factor))
aalen_edu_k6_f  <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(k6scale3.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
aalen_edu_k6_f2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu_k6) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
aalen_10000py(aalen_edu_k6_f, 1); aalen_10000py(aalen_edu_k6_f, 4); aalen_10000py(aalen_edu_k6_f2, 7); aalen_10000py(aalen_edu_k6_f, 25) # print results of interest



# Race x PsychDistress Interaction
nhis_male <- mutate(nhis_male, race_k6 = interaction(ethnicity.factor, k6scale3.factor))
aalen_race_k6_m  <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(ethnicity.factor)*const(k6scale3.factor) + const(married.factor) + const(edu.factor) + const(factor(srvy_yr)),  data = nhis_male)
aalen_race_k6_m2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(race_k6) + const(married.factor) + const(edu.factor) + const(factor(srvy_yr)),  data = nhis_male)
aalen_10000py(aalen_race_k6_m, 5); aalen_10000py(aalen_race_k6_m, 1); aalen_10000py(aalen_race_k6_m2, 9); aalen_10000py(aalen_race_k6_m, 29); aalen_10000py(aalen_race_k6_m, 2); aalen_10000py(aalen_race_k6_m2, 10); aalen_10000py(aalen_race_k6_m, 30) 


nhis_female <- mutate(nhis_female, race_k6 = interaction(ethnicity.factor, k6scale3.factor))
aalen_race_k6_f  <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(ethnicity.factor)*const(k6scale3.factor) + const(married.factor) + const(edu.factor) + const(factor(srvy_yr)),  data = nhis_female)
aalen_race_k6_f2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(race_k6) + const(married.factor) + const(edu.factor) + const(factor(srvy_yr)),  data = nhis_female)
aalen_10000py(aalen_race_k6_f, 5); aalen_10000py(aalen_race_k6_f, 1); aalen_10000py(aalen_race_k6_f2, 9); aalen_10000py(aalen_race_k6_f, 29); aalen_10000py(aalen_race_k6_f, 2); aalen_10000py(aalen_race_k6_f2, 10); aalen_10000py(aalen_race_k6_f, 30) 


