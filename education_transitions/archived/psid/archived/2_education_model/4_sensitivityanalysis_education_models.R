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

# setwd("/home/cbuckley")
setwd("~/Google Drive/SIMAH Sheffield")

source("SIMAH_code/psid/2_education_model/1_setup_markov_model.R")

#### SCRIPT CAN BE STARTED FROM HERE IF REWEIGHTED DATA WITH IDS EXISTS ####
data <- read_csv("SIMAH_workplace/education_transitions/new_PSID_weighted_IDs.csv")

# do the first analysis on the split time periods 

# # setup the datasets for both time periods
# 
datat1 <- setup_markov_model(data, y=2009)
datat2 <- setup_markov_model(data, y=2011)

datat1 <- datat1 %>% 
  mutate(racefinal2=as.factor(race_new_unique))
datat1$racefinal2 <- relevel(datat1$racefinal2, ref = "white")
datat1 <- datat1 %>% filter(racefinal2!="other")

summary(as.factor(datat1$racefinal2))

datat2 <- datat2 %>% 
  mutate(racefinal2=as.factor(race_new_unique))
datat2$racefinal2 <- relevel(datat2$racefinal2, ref = "white")

Q <- rbind( c(0.5, 0.5, 0, 0, 0,0),
            c(0, 0.5, 0.5, 0, 0,0),
            c(0, 0, 0.5, 0.5, 0,0),
            c(0, 0, 0, 0.5, 0.5,0),
            c(0, 0, 0, 0, 0.5,0.5),
            c(0, 0, 0, 0, 0, 0.5))

Q <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat1)

modelt1_income <- msm(educNUM~year, newID, data=datat1, qmatrix=Q,
                        center=FALSE,
                        covariates=~agescaled + agesqscaled + sex + racefinal2 + incomescaled,
                      hessian=F,
                        control=list(trace=1, fnscale=329428, maxit=200))
modelt1_income


saveRDS(modelt1_income, "SIMAH_workplace/education_transitions/final_models/modelt1_race_sensitivity.RDS")

modelt2_baseline <-   model <- msm(educNUM~year, newID, data=datat2, qmatrix=Q,
                                   center=FALSE,
                                   covariates=~agescaled + agesqscaled + sex + racefinal2,
                                   control=list(trace=1, fnscale=255053, maxit=200))
modelt2_baseline

modelt2_income <-   model <- msm(educNUM~year, newID, data=datat2, qmatrix=Q,
                                   center=FALSE,
                                   covariates=~agescaled + agesqscaled + sex + racefinal2 + incomescaled,
                                   control=list(trace=1,fnscale=255053, maxit=200))
modelt2_income

Q <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat2)

modelt2_income_int <-   model <- msm(educNUM~year, newID, data=datat2, qmatrix=Q,
                                 center=FALSE,
                                 covariates=~agescaled + agesqscaled + sex + racefinal2*incomescaled,
                                 hessian=F,
                                 control=list(trace=1, fnscale=624617, maxit=200))
modelt2_income_int


AIC(modelt2_baseline, modelt2_income, modelt2_income_int)

saveRDS(modelt2_baseline, "SIMAH_workplace/education_transitions/final_models/modelt2_baseline_6cat_new.RDS")
saveRDS(modelt2_income, "SIMAH_workplace/education_transitions/final_models/modelt2_income_6cat_new.RDS")
saveRDS(modelt2_income_int, "SIMAH_workplace/education_transitions/final_models/modelt2_income_int_6cat_16_new.RDS")

