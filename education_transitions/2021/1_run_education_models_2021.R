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

#### SCRIPT CAN BE STARTED FROM HERE IF REWEIGHTED DATA WITH IDS EXISTS ####
data <- read_csv("SIMAH_workplace/education_transitions/2021/data_to_model/new_PSID_weighted_IDs_2021.csv")

data$agecat <- ifelse(data$age==18, "18",
                        ifelse(data$age==19, "19",
                               ifelse(data$age==20, "20",
                                      ifelse(data$age>=21 & data$age<=25, "21-25","26+"))))

# Convert 'agecat' to a factor with specified levels
data$agecat <- factor(data$agecat, levels = c("18", "19", "20", "21-25", "26+"))

##### Set-up an individual model for each time period
# NB. Need to prep the data for each different model type using setup_education_model function 
# (re-codes education/race, drops individuals with one year of data, scales age data, drops anyone who transitions backwards etc.)

Q <- rbind( c(0.5, 0.5, 0, 0, 0),
            c(0, 0.5, 0.5, 0, 0),
            c(0, 0, 0.5, 0.5, 0),
            c(0, 0, 0, 0.5, 0.5),
            c(0, 0, 0, 0, 0.5))

# MODEL 1: 1999-2005
datat1 <- data %>% filter(year<=2005)
datat1 <- setup_education_model_2021(datat1)
datat1 <- datat1[order(datat1$newID, datat1$year),]
length(unique(datat1$uniqueID)) # 4617
length(unique(datat1$newID)) # 1146668
# remove anyone with only one year of data- this gives an error in MSM 
datat1 <- datat1 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 

Q1 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat1)
modelt1 <- msm(educNUM~year, newID, data=datat1, qmatrix=Q1,
                                   center=FALSE,
                                   covariates=~agecat + sex + racefinal2,
                        control=list(trace=1, fnscale=271181, maxit=200))

# MODEL 2: 2005-2013
datat2 <- data %>% filter(year<=2013 & year>=2005)
datat2 <- setup_education_model_2021(datat2)
datat2 <- datat2[order(datat2$newID, datat2$year),]
length(unique(datat2$uniqueID))
length(unique(datat2$newID))
datat2 <- datat2 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 

Q2 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat2)
modelt2 <- msm(educNUM~year, newID, data=datat2, qmatrix=Q2,
               center=FALSE,
               covariates=~agecat + sex + racefinal2,
               control=list(trace=1, fnscale=271181, maxit=200))

# MODEL 3: 2013-2018
datat3 <- data %>% filter(year>=2013)
datat3 <- setup_education_model_2021(datat3)
datat3 <- datat3[order(datat3$newID, datat3$year),]
length(unique(datat3$uniqueID))
length(unique(datat3$newID))
datat3 <- datat3 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 

Q3 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat3)
modelt3 <- msm(educNUM~year, newID, data=datat3, qmatrix=Q3,
               center=FALSE,
               covariates=~agecat + sex + racefinal2,
               control=list(trace=1, fnscale=271181, maxit=200))

# MODEL 4: 2019-2021
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

##### One combined model with a covariate for time period

# MODEL 5: Covariate for time period, with four time periods
datat5 <- data
datat5 <- setup_education_model_2021(datat5)
datat5$timevary <- cut(datat5$year,
                     breaks=c(0,2005,2011,2018, 2021),
                     labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))
datat5 <- datat5[order(datat5$newID, datat5$year),]
length(unique(datat5$uniqueID))
length(unique(datat5$newID))
datat5 <- datat5 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
# Relevel the 'timevary' variable to set "2012-2018" as the reference category
datat5$timevary <- relevel(datat5$timevary, ref = "2012-2018")

Q5 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat5)
modelt5 <- msm(educNUM~year, newID, data=datat5, qmatrix=Q5,
               center=FALSE,
               covariates=~agecat + sex + racefinal2 + timevary,
               control=list(trace=1, fnscale=271181, maxit=200))

# MODEL 6: Covariate for time period, two time periods (narrow 'pre-covid' time period)
datat6 <- data %>% filter(year >= 2012)
datat6 <- setup_education_model_2021(datat6)
datat6$timevary <- cut(datat6$year,
                       breaks=c(0,2018, 2021),
                       labels=c("2012-2018", "2019-2021"))
datat6 <- datat6[order(datat6$newID, datat6$year),]
length(unique(datat6$uniqueID)) # 6607
length(unique(datat6$newID)) # 1542884
datat6 <- datat6 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
datat6$timevary <- relevel(datat6$timevary, ref = "2012-2018")

Q6 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat6)

modelt6 <- msm(educNUM~year, newID, data=datat6, qmatrix=Q6,
               center=FALSE,
               covariates=~agecat + sex + racefinal2 + timevary,
               control=list(trace=1, fnscale=271181, maxit=200))

# Two time periods (broader 'pre-covid' time period) 
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
# Warning messages for Q7 models:
#   1: In msm.check.model(mf.trans$"(fromstate)", mf.trans$"(tostate)",  :
#                           Absorbing - absorbing transition at observation 15119
#                         2: In msm(educNUM ~ year, newID, data = datat7, qmatrix = Q7, center = FALSE,  :
#                                     Optimisation has probably not converged to the maximum likelihood - Hessian is not positive definite.

# Save all models
saveRDS(modelt1, "SIMAH_workplace/education_transitions/2021/final_models/covid_modelt1.RDS")
saveRDS(modelt2, "SIMAH_workplace/education_transitions/2021/final_models/covid_modelt2.RDS")
saveRDS(modelt3, "SIMAH_workplace/education_transitions/2021/final_models/covid_modelt3.RDS")
saveRDS(modelt4, "SIMAH_workplace/education_transitions/2021/final_models/covid_modelt4.RDS")
saveRDS(modelt5, "SIMAH_workplace/education_transitions/2021/final_models/covid_modelt5.RDS")
saveRDS(modelt6, "SIMAH_workplace/education_transitions/2021/final_models/covid_modelt6.RDS")
saveRDS(modelt7, "SIMAH_workplace/education_transitions/2021/final_models/covid_modelt7.RDS")

# Run a model with an interaction term for race
modelt6_interaction_race <- msm(educNUM~year, newID, data=datat6, qmatrix=Q6,
                                center=FALSE,
                                covariates=~agecat + sex + racefinal2 + timevary + racefinal2*timevary,
                                control=list(trace=1, fnscale=271181, maxit=200))
saveRDS(modelt6_interaction_race, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_interaction_race.RDS")

# Run a model with an interaction term for sex
modelt6_interaction_sex <- msm(educNUM~year, newID, data=datat6, qmatrix=Q6,
                               center=FALSE,
                               covariates=~agecat + sex + racefinal2 + timevary + sex*timevary,
                               control=list(trace=1, fnscale=271181, maxit=200))
saveRDS(modelt6_interaction_sex, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_interaction_sex.RDS")

# Run separate models for each sex group to enable interactions (based on model 6)
sex_data <- data %>% filter(year >= 2012)

# Men
data_men <- sex_data %>% filter(sex=="male")
men <- setup_education_model_2021(data_men)
men$timevary <- cut(men$year,
                       breaks=c(0,2018, 2021),
                       labels=c("2012-2018", "2019-2021"))
men <- men[order(men$newID, men$year),]
length(unique(men$uniqueID)) # 3496
length(unique(men$newID)) # 866201
men <- men %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
men$timevary <- relevel(men$timevary, ref = "2012-2018")

Q_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=men)
modelt6_men <- msm(educNUM~year, newID, data=men, qmatrix=Q_men,
               center=FALSE,
               covariates=~agecat + racefinal2 + timevary,
               control=list(trace=1, fnscale=271181, maxit=200))
# Women
data_women <- sex_data %>% filter(sex=="female")
women <- setup_education_model_2021(data_women)
women$timevary <- cut(women$year,
                    breaks=c(0,2018, 2021),
                    labels=c("2012-2018", "2019-2021"))
women <- women[order(women$newID, women$year),]
length(unique(women$uniqueID)) # 3753
length(unique(women$newID)) # 800071
women <- women %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
women$timevary <- relevel(women$timevary, ref = "2012-2018")

Q_women <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=women)
modelt6_women <- msm(educNUM~year, newID, data=women, qmatrix=Q_women,
                   center=FALSE,
                   covariates=~agecat + racefinal2 + timevary,
                   control=list(trace=1, fnscale=271181, maxit=200))

saveRDS(modelt6_men, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_men.RDS")
saveRDS(modelt6_women, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_women.RDS")

## Run separate models for each race group to enable interactions 
race_data <- data %>% filter(year>=2012)

# White
data_white <- race_data %>% filter(final_race_using_method_hierarchy=="white")
white <- setup_education_model_2021(data_white)
white$timevary <- cut(white$year,
                      breaks=c(0,2018, 2021),
                      labels=c("2012-2018", "2019-2021"))
white <- white[order(white$newID, white$year),]
length(unique(white$uniqueID)) # 
length(unique(white$newID)) # 
white <- white %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
white$timevary <- relevel(white$timevary, ref = "2012-2018")

Q_white <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=white)
modelt6_white <- msm(educNUM~year, newID, data=white, qmatrix=Q_white,
                   center=FALSE,
                   covariates=~agecat + sex + timevary,
                   control=list(trace=1, fnscale=271181, maxit=200))

# Black
data_black <- race_data %>% filter(final_race_using_method_hierarchy=="black")
black <- setup_education_model_2021(data_black) # comment out line 29 of this function when using with models stratified by race
black$timevary <- cut(black$year,
                      breaks=c(0,2018, 2021),
                      labels=c("2012-2018", "2019-2021"))
black <- black[order(black$newID, black$year),]
length(unique(black$uniqueID)) # 
length(unique(black$newID)) # 
black <- black %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
black$timevary <- relevel(black$timevary, ref = "2012-2018")
Q_black <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=black)
modelt6_black <- msm(educNUM~year, newID, data=black, qmatrix=Q_black,
                   center=FALSE,
                   covariates=~agecat + sex + timevary,
                   control=list(trace=1, fnscale=271181, maxit=200))

# Hispanic
data_hispanic <- race_data %>% filter(final_race_using_method_hierarchy=="hispanic")
hispanic <- setup_education_model_2021(data_hispanic) # comment out line 29 of this function when using with models stratified by race
hispanic$timevary <- cut(hispanic$year,
                      breaks=c(0,2018, 2021),
                      labels=c("2012-2018", "2019-2021"))
hispanic <- hispanic[order(hispanic$newID, hispanic$year),]
length(unique(hispanic$uniqueID)) # 519
length(unique(hispanic$newID)) # 181,562
hispanic <- hispanic %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
hispanic$timevary <- relevel(hispanic$timevary, ref = "2012-2018")
Q_hispanic <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=hispanic)
modelt6_hispanic <- msm(educNUM~year, newID, data=hispanic, qmatrix=Q_hispanic,
                     center=FALSE,
                     covariates=~agecat + sex + timevary,
                     control=list(trace=1, fnscale=271181, maxit=200))
# Other
data_other <- race_data %>% filter(final_race_using_method_hierarchy=="other"| 
                                   final_race_using_method_hierarchy=="Asian/PI"|
                                   final_race_using_method_hierarchy=="Native")
other <- setup_education_model_2021(data_other) # comment out line 29 of this function when using with models stratified by race
other$timevary <- cut(other$year,
                      breaks=c(0,2018, 2021),
                      labels=c("2012-2018", "2019-2021"))
other <- other[order(other$newID, other$year),]
length(unique(other$uniqueID)) # 256
length(unique(other$newID)) # 111,513
other <- other %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
other$timevary <- relevel(other$timevary, ref = "2012-2018")
Q_other <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=other)
modelt6_other <- msm(educNUM~year, newID, data=other, qmatrix=Q_other,
                     center=FALSE,
                     covariates=~agecat + sex + timevary,
                     control=list(trace=1, fnscale=271181, maxit=200))

saveRDS(modelt6_white, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_white.RDS")
saveRDS(modelt6_black, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_black.RDS")
saveRDS(modelt6_hispanic, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_hispanic.RDS")
saveRDS(modelt6_other, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_other.RDS")

# Run separate models for each SEX AND RACE group to enable interactions 

# White men
data_white_men <- sex_data %>% filter(final_race_using_method_hierarchy=="white", sex=="male")
white_men <- setup_education_model_2021(data_white_men) # comment out line 29 of this function when using with models stratified by race
white_men$timevary <- cut(white_men$year,
                      breaks=c(0,2018, 2021),
                      labels=c("2012-2018", "2019-2021"))
white_men <- white_men[order(white_men$newID, white_men$year),]
length(unique(white_men$uniqueID)) # 1664
length(unique(white_men$newID)) # 571,481
white_men <- white_men %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
white_men$timevary <- relevel(white_men$timevary, ref = "2012-2018")
Q_white_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=white_men)
modelt6_white_men <- msm(educNUM~year, newID, data=white_men, qmatrix=Q_white_men,
                         center=FALSE,
                         covariates=~agecat + timevary,
                         control=list(trace=1, fnscale=271181, maxit=200))

# Black men
data_black_men <- sex_data %>% filter(final_race_using_method_hierarchy=="black", sex=="male")
black_men <- setup_education_model_2021(data_black_men) # comment out line 29 of this function when using with models stratified by race
black_men$timevary <- cut(black_men$year,
                          breaks=c(0,2018, 2021),
                          labels=c("2012-2018", "2019-2021"))
black_men <- black_men[order(black_men$newID, black_men$year),]
length(unique(black_men$uniqueID)) # 1461
length(unique(black_men$newID)) # 135874
black_men <- black_men %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
black_men$timevary <- relevel(black_men$timevary, ref = "2012-2018")
Q_black_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=black_men)
modelt6_black_men <- msm(educNUM~year, newID, data=black_men, qmatrix=Q_black_men,
                         center=FALSE,
                         covariates=~agecat + timevary,
                         control=list(trace=1, fnscale=271181, maxit=200))

# Hispanic men
data_hispanic_men <- sex_data %>% filter(final_race_using_method_hierarchy=="hispanic", sex=="male")
hispanic_men <- setup_education_model_2021(data_hispanic_men) # comment out line 29 of this function when using with models stratified by race
hispanic_men$timevary <- cut(hispanic_men$year,
                          breaks=c(0,2018, 2021),
                          labels=c("2012-2018", "2019-2021"))
hispanic_men <- hispanic_men[order(hispanic_men$newID, hispanic_men$year),]
length(unique(hispanic_men$uniqueID)) # 
length(unique(hispanic_men$newID)) # 
hispanic_men <- hispanic_men %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
hispanic_men$timevary <- relevel(hispanic_men$timevary, ref = "2012-2018")
Q_hispanic_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=hispanic_men)
modelt6_hispanic_men <- msm(educNUM~year, newID, data=hispanic_men, qmatrix=Q_hispanic_men,
                            center=FALSE,
                            covariates=~agecat + timevary,
                            control=list(trace=1, fnscale=271181, maxit=200))

# Other race men - DID NOT CONVERGE
data_other_men <- sex_data %>% filter((final_race_using_method_hierarchy=="other"| 
                                              final_race_using_method_hierarchy=="Asian/PI"|
                                              final_race_using_method_hierarchy=="Native"), sex=="male")
other_men <- setup_education_model_2021(data_other_men) # comment out line 29 of this function when using with models stratified by race
other_men$timevary <- cut(other_men$year,
                             breaks=c(0,2018, 2021),
                             labels=c("2012-2018", "2019-2021"))
other_men <- other_men[order(other_men$newID, other_men$year),]
length(unique(other_men$uniqueID)) # 
length(unique(other_men$newID)) # 
other_men <- other_men %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
other_men$timevary <- relevel(other_men$timevary, ref = "2012-2018")
Q_other_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=other_men)
modelt6_other_men <- msm(educNUM~year, newID, data=other_men, qmatrix=Q_other_men,
                         center=FALSE,
                         covariates=~agecat + timevary,
                         control=list(trace=1, fnscale=271181, maxit=1000))

# White women
data_white_women <- sex_data %>% filter(final_race_using_method_hierarchy=="white", sex=="female")
white_women <- setup_education_model_2021(data_white_women) # comment out line 29 of this function when using with models stratified by race
white_women$timevary <- cut(white_women$year,
                          breaks=c(0,2018, 2021),
                          labels=c("2012-2018", "2019-2021"))
white_women <- white_women[order(white_women$newID, white_women$year),]
length(unique(white_women$uniqueID)) # 
length(unique(white_women$newID)) # 
white_women <- white_women %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
white_women$timevary <- relevel(white_women$timevary, ref = "2012-2018")
Q_white_women <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=white_women)
modelt6_white_women <- msm(educNUM~year, newID, data=white_women, qmatrix=Q_white_women,
                           center=FALSE,
                           covariates=~agecat + timevary,
                           control=list(trace=1, fnscale=271181, maxit=200))

# Black women
data_black_women <- sex_data %>% filter(final_race_using_method_hierarchy=="black", sex=="female")
black_women <- setup_education_model_2021(data_black_women) # comment out line 29 of this function when using with models stratified by race
black_women$timevary <- cut(black_women$year,
                            breaks=c(0,2018, 2021),
                            labels=c("2012-2018", "2019-2021"))
black_women <- black_women[order(black_women$newID, black_women$year),]
length(unique(black_women$uniqueID)) # 1564
length(unique(black_women$newID)) # 127185
black_women <- black_women %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
black_women$timevary <- relevel(black_women$timevary, ref = "2012-2018")
Q_black_women <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=black_women)
modelt6_black_women <- msm(educNUM~year, newID, data=black_women, qmatrix=Q_black_women,
                           center=FALSE,
                           covariates=~agecat + timevary,
                           control=list(trace=1, fnscale=271181, maxit=200))


# Hispanic women
data_hispanic_women <- sex_data %>% filter(final_race_using_method_hierarchy=="hispanic", sex=="female")
hispanic_women <- setup_education_model_2021(data_hispanic_women) # comment out line 29 of this function when using with models stratified by race
hispanic_women$timevary <- cut(hispanic_women$year,
                            breaks=c(0,2018, 2021),
                            labels=c("2012-2018", "2019-2021"))
hispanic_women <- hispanic_women[order(hispanic_women$newID, hispanic_women$year),]
length(unique(hispanic_women$uniqueID)) # 
length(unique(hispanic_women$newID)) # 
hispanic_women <- hispanic_women %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
hispanic_women$timevary <- relevel(hispanic_women$timevary, ref = "2012-2018")
Q_hispanic_women <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=hispanic_women)
modelt6_hispanic_women <- msm(educNUM~year, newID, data=hispanic_women, qmatrix=Q_hispanic_women,
                              center=FALSE,
                              covariates=~agecat + timevary,
                              control=list(trace=1, fnscale=271181, maxit=200))


# Other race women # DOES NOT CONVERGE
data_other_women <- sex_data %>% filter((final_race_using_method_hierarchy=="other"| 
                                         final_race_using_method_hierarchy=="Asian/PI"|
                                         final_race_using_method_hierarchy=="Native"), sex=="male")
other_women <- setup_education_model_2021(data_other_women) # comment out line 29 of this function when using with models stratified by race
other_women$timevary <- cut(other_women$year,
                          breaks=c(0,2018, 2021),
                          labels=c("2012-2018", "2019-2021"))
other_women <- other_women[order(other_women$newID, other_women$year),]
length(unique(other_women$uniqueID)) # 
length(unique(other_women$newID)) # 
other_women <- other_women %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
other_women$timevary <- relevel(other_women$timevary, ref = "2012-2018")
Q_other_women <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=other_women)
modelt6_other_women <- msm(educNUM~year, newID, data=other_women, qmatrix=Q_other_women,
                           center=FALSE,
                           covariates=~agecat + timevary,
                           control=list(trace=1, fnscale=271181, maxit=1000))

saveRDS(modelt6_white_men, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_white_men.RDS")
saveRDS(modelt6_black_men, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_black_men.RDS")
saveRDS(modelt6_hispanic_men, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_hispanic_men.RDS")
saveRDS(modelt6_other_men, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_other_men.RDS")
saveRDS(modelt6_white_women, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_white_women.RDS")
saveRDS(modelt6_black_women, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_black_women.RDS")
saveRDS(modelt6_hispanic_women, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_hispanic_women.RDS")
saveRDS(modelt6_other_women, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_other_women.RDS")

###### Run models treating age as continuous rather than categorical

# Model 6
modelt6_cont <- msm(educNUM~year, newID, data=datat6, qmatrix=Q6,
               center=FALSE,
               covariates=~ sex + agescaled + agesqscaled + racefinal2 + timevary,
               control=list(trace=1, fnscale=271181, maxit=200))
saveRDS(modelt6_cont, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_cont.RDS")

# Model 6 with an interaction for race
modelt6_interaction_race_age_cont <- msm(educNUM~year, newID, data=datat6, qmatrix=Q6,
                                center=FALSE,
                                covariates=~sex + agescaled + agesqscaled + racefinal2 + timevary + racefinal2*timevary,
                                control=list(trace=1, fnscale=271181, maxit=200))
saveRDS(modelt6_interaction_race_age_cont, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_interaction_race_age_cont.RDS")

