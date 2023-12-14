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

# Prep data using setup_markov_model_formodel function from the 1_setup_markov_model_2021.R script.
# (re-codes education/race, drops individuals with one year of data, drops anyone who transitions backwards etc.)

data <- setup_education_model_2021(data)

data$agecat <- ifelse(data$age==18, "18",
                        ifelse(data$age==19, "19",
                               ifelse(data$age==20, "20",
                                      ifelse(data$age>=21 & data$age<=25, "21-25","26+"))))

# Convert 'agecat' to a factor with specified levels
data$agecat <- factor(data$agecat, levels = c("18", "19", "20", "21-25", "26+"))

# Save prepped model data
write_rds(data, "SIMAH_workplace/education_transitions/2021/data_to_model/prepped_data_for_markov_2021.rds")

################################################################################################################
# Read in prepped data
data <- read_rds("SIMAH_workplace/education_transitions/2021/data_to_model/prepped_data_for_markov_2021.rds")

# Set-up an individual model for each time period
datat1 <- data %>% filter(year<=2005)
datat1 <- data[order(datat1$newID, datat1$year),]
length(unique(datat1$uniqueID))
length(unique(datat1$newID))

datat2 <- data %>% filter(year<=2013 & year>=2005)
datat2 <- data[order(datat2$newID, datat2$year),]
length(unique(datat2$uniqueID))
length(unique(datat2$newID))

datat3 <- data %>% filter(year>=2013)
datat3 <- data[order(datat3$newID, datat3$year),]
length(unique(datat3$uniqueID))
length(unique(datat3$newID))

datat4 <- data %>% filter(year>=2019 & year<=2021)
datat4 <- data[order(datat4$newID, datat4$year),]
length(unique(datat4$uniqueID))
length(unique(datat4$newID))

datat5 <- data
datat5$timevary <- cut(data$year,
                     breaks=c(0,2005,2011,2018, 2021),
                     labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))

datat6 <- data %>% filter(year >= 2012)
datat6$timevary <- cut(datat6$year,
                       breaks=c(0,2018, 2021),
                       labels=c("2012-2018", "2019-2021"))

# specify models

Q <- rbind( c(0.5, 0.5, 0, 0, 0),
            c(0, 0.5, 0.5, 0, 0),
            c(0, 0, 0.5, 0.5, 0),
            c(0, 0, 0, 0.5, 0.5),
            c(0, 0, 0, 0, 0.5))

Q1 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat1)

modelt1 <- msm(educNUM~year, newID, data=datat1, qmatrix=Q1,
                                   center=FALSE,
                                   covariates=~agecat + sex + racefinal2,
                        control=list(trace=1, fnscale=271181, maxit=200))

Q2 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat2)

modelt2 <- msm(educNUM~year, newID, data=datat2, qmatrix=Q2,
               center=FALSE,
               covariates=~agecat + sex + racefinal2,
               control=list(trace=1, fnscale=271181, maxit=200))

Q3 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat3)

modelt3 <- msm(educNUM~year, newID, data=datat3, qmatrix=Q3,
               center=FALSE,
               covariates=~agecat + sex + racefinal2,
               control=list(trace=1, fnscale=271181, maxit=200))

Q4 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat4)

modelt4 <- msm(educNUM~year, newID, data=datat4, qmatrix=Q,
               center=FALSE,
               covariates=~agecat + sex + racefinal2,
               control=list(trace=1, fnscale=271181, maxit=200))

# One combined model with a covariate for the time period
Q5 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat5)
modelt5 <- msm(educNUM~year, newID, data=datat5, qmatrix=Q5,
               center=FALSE,
               covariates=~agecat + sex + racefinal2 + timevary,
               control=list(trace=1, fnscale=271181, maxit=200))

# One combined model with a covariate for the time period (comparing most recent time period only)
Q6 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat6)
modelt6 <- msm(educNUM~year, newID, data=datat6, qmatrix=Q6,
               center=FALSE,
               covariates=~agecat + sex + racefinal2 + timevary,
               control=list(trace=1, fnscale=271181, maxit=200))

saveRDS(modelt1, "SIMAH_workplace/education_transitions/final_models/formodel_modelt1_newn.RDS")
saveRDS(modelt2, "SIMAH_workplace/education_transitions/final_models/formodel_modelt2_newn.RDS")
saveRDS(modelt3, "SIMAH_workplace/education_transitions/final_models/formodel_modelt3_newn.RDS")
saveRDS(modelt4, "SIMAH_workplace/education_transitions/final_models/formodel_modelt4_newn.RDS")
saveRDS(modelt5, "SIMAH_workplace/education_transitions/final_models/formodel_modelt5_newn.RDS")
saveRDS(modelt6, "SIMAH_workplace/education_transitions/final_models/formodel_modelt6_newn.RDS")

# Run seperate models for each sex group to enable interactions (based on model 5)

sex_data <- data
sex_data$timevary <- cut(sex_data$year,
                 breaks=c(0,2005,2011,2018, 2021),
                 labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))
# Men
men <- sex_data %>% filter(sex==0)
Q_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=men)
modelt5_men <- msm(educNUM~year, newID, data=men, qmatrix=Q_men,
               center=FALSE,
               covariates=~agecat + racefinal2 + timevary,
               control=list(trace=1, fnscale=271181, maxit=200))
# Women
women <- sex_data %>% filter(sex==1)
Q_women <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=women)
modelt5_women <- msm(educNUM~year, newID, data=women, qmatrix=Q_women,
                   center=FALSE,
                   covariates=~agecat + racefinal2 + timevary,
                   control=list(trace=1, fnscale=271181, maxit=200))

saveRDS(modelt5_men, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_men.RDS")
saveRDS(modelt5_women, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_women.RDS")

## Run separate models for each race group to enable interactions (based on model 5)
race_data <- data
race_data$timevary <- cut(data$year,
                              breaks=c(0,2005,2011,2018, 2021),
                              labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))
# White
white <- race_data %>% filter(racefinal2=="white")
Q_white <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=white)
modelt5_white <- msm(educNUM~year, newID, data=white, qmatrix=Q_white,
                   center=FALSE,
                   covariates=~agecat + sex + timevary,
                   control=list(trace=1, fnscale=271181, maxit=200))

# Black
black <- race_data %>% filter(racefinal2=="black")
Q_black <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=black)
modelt5_black <- msm(educNUM~year, newID, data=black, qmatrix=Q_black,
                   center=FALSE,
                   covariates=~agecat + sex + timevary,
                   control=list(trace=1, fnscale=271181, maxit=200))

# Hispanic
hispanic <- race_data %>% filter(racefinal2=="hispanic")
Q_hispanic <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=hispanic)
modelt5_hispanic <- msm(educNUM~year, newID, data=hispanic, qmatrix=Q_hispanic,
                     center=FALSE,
                     covariates=~agecat + sex + timevary,
                     control=list(trace=1, fnscale=271181, maxit=200))
# Other
other <- race_data %>% filter(racefinal2=="other")
Q_other <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=other)
modelt5_other <- msm(educNUM~year, newID, data=other, qmatrix=Q_other,
                     center=FALSE,
                     covariates=~agecat + sex + timevary,
                     control=list(trace=1, fnscale=271181, maxit=200))

saveRDS(modelt5_white, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_white.RDS")
saveRDS(modelt5_black, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_black.RDS")
saveRDS(modelt5_hispanic, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_hispanic.RDS")
saveRDS(modelt5_other, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_other.RDS")

