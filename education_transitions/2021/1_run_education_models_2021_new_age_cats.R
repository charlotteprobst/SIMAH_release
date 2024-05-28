# script to generate MSM model for education for education transitions paper
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(readxl)
library(doParallel)
library(foreach)
library(parallel)
library(readxl)

# setwd("/home/cbuckley")
setwd("C:/Users/cmp21seb/Documents/SIMAH/")

source("SIMAH_code/education_transitions/2021/functions/0_setup_education_model_2021.R")
source("SIMAH_code/education_transitions/2021/functions/0_setup_education_model_stratified_race_2021.R")

#### SCRIPT CAN BE STARTED FROM HERE IF REWEIGHTED DATA WITH IDS EXISTS ####
data <- read_csv("SIMAH_workplace/education_transitions/2021/data_to_model/new_PSID_weighted_IDs_2021.csv")

# Prep data for models
data$agecat <- ifelse(data$age==18, "18",
                        ifelse(data$age==19, "19",
                               ifelse(data$age==20, "20",
                                      ifelse(data$age>=21, "21",
                                             ifelse(data$age>=22 & data$age<=24, "22-24",
                                                    ifelse(data$age>=25 & data$age<=29, "25-29",
                                                           ifelse(data$age>=30, "30+", NA)))))))
data$agecat <- factor(data$agecat, levels = c("18", "19", "20", "21", "22-24", "25-29", "30+"))

data <- data %>% mutate(sex=factor(sex), sex=ifelse(sex=="female",1,0)) 

saveRDS(data, "SIMAH_workplace/education_transitions/2021/data_to_model/prepped_data_for_markov_2021_new_age_cats.rds")

##### Set-up an individual model for each time period
# NB. Need to prep the data for each different model type using setup_education_model function 
# (re-codes education/race, drops individuals with one year of data, scales age data, drops anyone who transitions backwards etc.)

Q <- rbind( c(0.5, 0.5, 0, 0, 0),
            c(0, 0.5, 0.5, 0, 0),
            c(0, 0, 0.5, 0.5, 0),
            c(0, 0, 0, 0.5, 0.5),
            c(0, 0, 0, 0, 0.5))

# PRE COVID ONLY MODEL: 2013-2019
datat3_2019 <- data %>% filter(year>=2013 & year<=2019)
datat3_2019 <- setup_education_model_2021(datat3_2019)
datat3_2019 <- datat3_2019[order(datat3_2019$newID, datat3_2019$year),]
length(unique(datat3_2019$uniqueID))
length(unique(datat3_2019$newID))
datat3_2019 <- datat3_2019 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 

Q3_2019 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat3_2019)
modelt3_2019 <- msm(educNUM~year, newID, data=datat3_2019, qmatrix=Q3_2019,
                    center=FALSE,
                    covariates=~agecat + sex + racefinal2,
                    control=list(trace=1, fnscale=271181, maxit=200))

# saveRDS(modelt3_2019, "SIMAH_workplace/education_transitions/2021/final_models/covid_modelt3_2019_new_age_cats.RDS")
# did not converge

# COVID MODEL: 2019-2021 
datat4 <- data %>% filter(year>=2019 & year<=2021)
datat4 <- setup_education_model_2021(datat4)
datat4 <- datat4[order(datat4$newID, datat4$year),]
length(unique(datat4$uniqueID))
length(unique(datat4$newID))
datat4 <- datat4 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 

Q4 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat4)
modelt4 <- msm(educNUM~year, newID, data=datat4, qmatrix=Q,
               center=FALSE,
               covariates=~agecat + sex + racefinal2,
               control=list(trace=1, fnscale=271181, maxit=200))

# saveRDS(modelt4, "SIMAH_workplace/education_transitions/2021/final_models/covid_modelt4_new_age_cats.RDS")
# did not converge

##### One combined model with a covariate for time period

# COMBINED MODEL NARROW: 
datat6 <- data %>% filter(year >= 2012)
datat6 <- setup_education_model_2021(datat6)
datat6$timevary <- cut(datat6$year,
                       breaks=c(0,2018, 2021),
                       labels=c("2012-2018", "2019-2021"))
datat6 <- datat6[order(datat6$newID, datat6$year),]
length(unique(datat6$uniqueID)) 
length(unique(datat6$newID)) 
datat6 <- datat6 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
datat6$timevary <- relevel(datat6$timevary, ref = "2012-2018")
Q6 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat6)

modelt6 <- msm(educNUM~year, newID, data=datat6, qmatrix=Q6,
               center=FALSE,
               covariates=~agecat + sex + racefinal2 + timevary,
               control=list(trace=1, fnscale=271181, maxit=200))

# saveRDS(modelt6, "SIMAH_workplace/education_transitions/2021/final_models/covid_modelt6_new_age_cats.RDS")
# did not converge

# COMBINED MODEL BROAD: 
datat7 <- data %>% filter(year>=2006) 
datat7 <- setup_education_model_2021(datat7)
datat7$timevary <- cut(datat7$year,
                            breaks=c(0, 2019, 2021),
                            labels=c("2007-2018", "2019-2021"))
datat7 <- datat7[order(datat7$newID, datat7$year),]
length(unique(datat7$uniqueID))
length(unique(datat7$newID))
datat7 <- datat7 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1)
# Relevel the 'timevary' variable to set "2007-2018" as the reference category
datat7$timevary <- relevel(datat7$timevary, ref = "2007-2018")

Q7 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat7)
modelt7 <- msm(educNUM~year, newID, data=datat7, qmatrix=Q7,
                    center=FALSE,
                    covariates=~agecat + sex + racefinal2 + timevary,
                    control=list(trace=1, fnscale=271181, maxit=1000))
# saveRDS(modelt7, "SIMAH_workplace/education_transitions/2021/final_models/covid_modelt7.RDS")
# did not converge