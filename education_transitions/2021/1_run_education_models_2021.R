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
length(unique(datat6$uniqueID)) # 7249
length(unique(datat6$newID)) # 1666272
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
sex_data <- datat6

# Men
men <- sex_data %>% filter(sex==0)
Q_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=men)
modelt6_men <- msm(educNUM~year, newID, data=men, qmatrix=Q_men,
               center=FALSE,
               covariates=~agecat + racefinal2 + timevary,
               control=list(trace=1, fnscale=271181, maxit=200))
# Women
women <- sex_data %>% filter(sex==1)
Q_women <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=women)
modelt6_women <- msm(educNUM~year, newID, data=women, qmatrix=Q_women,
                   center=FALSE,
                   covariates=~agecat + racefinal2 + timevary,
                   control=list(trace=1, fnscale=271181, maxit=200))

saveRDS(modelt6_men, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_men.RDS")
saveRDS(modelt6_women, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_women.RDS")

## Run separate models for each race group to enable interactions 
race_data <- datat6

# White
white <- race_data %>% filter(racefinal2=="white")
Q_white <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=white)
modelt6_white <- msm(educNUM~year, newID, data=white, qmatrix=Q_white,
                   center=FALSE,
                   covariates=~agecat + sex + timevary,
                   control=list(trace=1, fnscale=271181, maxit=200))

# Black
black <- race_data %>% filter(racefinal2=="black")
Q_black <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=black)
modelt6_black <- msm(educNUM~year, newID, data=black, qmatrix=Q_black,
                   center=FALSE,
                   covariates=~agecat + sex + timevary,
                   control=list(trace=1, fnscale=271181, maxit=200))

# Hispanic
hispanic <- race_data %>% filter(racefinal2=="hispanic")
Q_hispanic <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=hispanic)
modelt6_hispanic <- msm(educNUM~year, newID, data=hispanic, qmatrix=Q_hispanic,
                     center=FALSE,
                     covariates=~agecat + sex + timevary,
                     control=list(trace=1, fnscale=271181, maxit=200))
# Other
other <- race_data %>% filter(racefinal2=="other")
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
white_men <- sex_data %>% filter(racefinal2=="white", sex==0)
Q_white_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=white_men)
modelt6_white_men <- msm(educNUM~year, newID, data=white_men, qmatrix=Q_white_men,
                         center=FALSE,
                         covariates=~agecat + timevary,
                         control=list(trace=1, fnscale=271181, maxit=200))

# Black men
black_men <- sex_data %>% filter(racefinal2=="black", sex==0)
Q_black_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=black_men)
modelt6_black_men <- msm(educNUM~year, newID, data=black_men, qmatrix=Q_black_men,
                         center=FALSE,
                         covariates=~agecat + timevary,
                         control=list(trace=1, fnscale=271181, maxit=200))


# Hispanic men
hispanic_men <- sex_data %>% filter(racefinal2=="hispanic", sex==0)
Q_hispanic_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=hispanic_men)
modelt6_hispanic_men <- msm(educNUM~year, newID, data=hispanic_men, qmatrix=Q_hispanic_men,
                            center=FALSE,
                            covariates=~agecat + timevary,
                            control=list(trace=1, fnscale=271181, maxit=200))


# Other race men
other_men <- sex_data %>% filter(racefinal2=="other", sex==0)
Q_other_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=other_men)
modelt6_other_men <- msm(educNUM~year, newID, data=other_men, qmatrix=Q_other_men,
                         center=FALSE,
                         covariates=~agecat + timevary,
                         control=list(trace=1, fnscale=271181, maxit=200))



# White women
white_women <- sex_data %>% filter(racefinal2=="white", sex==1)
Q_white_women <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=white_women)
modelt6_white_women <- msm(educNUM~year, newID, data=white_women, qmatrix=Q_white_women,
                           center=FALSE,
                           covariates=~agecat + timevary,
                           control=list(trace=1, fnscale=271181, maxit=200))


# Black women
black_women <- sex_data %>% filter(racefinal2=="black", sex==1)
Q_black_women <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=black_women)
modelt6_black_women <- msm(educNUM~year, newID, data=black_women, qmatrix=Q_black_women,
                           center=FALSE,
                           covariates=~agecat + timevary,
                           control=list(trace=1, fnscale=271181, maxit=200))


# Hispanic women
hispanic_women <- sex_data %>% filter(racefinal2=="hispanic", sex==1)
Q_hispanic_women <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=hispanic_women)
modelt6_hispanic_women <- msm(educNUM~year, newID, data=hispanic_women, qmatrix=Q_hispanic_women,
                              center=FALSE,
                              covariates=~agecat + timevary,
                              control=list(trace=1, fnscale=271181, maxit=200))


# Other race women
other_women <- sex_data %>% filter(racefinal2=="other", sex==1)
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





# Model 6 with an interaction for race
modelt6_interaction_race_age_cont <- msm(educNUM~year, newID, data=datat6, qmatrix=Q6,
                                center=FALSE,
                                covariates=~sex + agescaled + agesqscaled + racefinal2 + timevary + racefinal2*timevary,
                                control=list(trace=1, fnscale=271181, maxit=200))
saveRDS(modelt6_interaction_race_age_cont, "SIMAH_workplace/education_transitions/2021/final_models/modelt6_interaction_race_age_cont.RDS")

