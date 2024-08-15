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
setwd("~/Google Drive/SIMAH Sheffield")

source("SIMAH_code/psid/2_education_model/1_setup_markov_model.R")

#### SCRIPT CAN BE STARTED FROM HERE IF REWEIGHTED DATA WITH IDS EXISTS ####
data <- read_csv("SIMAH_workplace/education_transitions/new_PSID_weighted_IDs.csv")

data <- adjust_income_inflation(data)
# do the first analysis on the split time periods 

# # setup the datasets for both time periods
# 
datat1 <- setup_markov_model(data, y=2009)
datat2 <- setup_markov_model(data, y=2011)

Q <- rbind( c(0.5, 0.5, 0, 0, 0,0),
            c(0, 0.5, 0.5, 0, 0,0),
            c(0, 0, 0.5, 0.5, 0,0),
            c(0, 0, 0, 0.5, 0.5,0),
            c(0, 0, 0, 0, 0.5,0.5),
            c(0, 0, 0, 0, 0, 0.5))

# specify baseline models - just race and ethnicity 
modelt1_baseline <- msm(educNUM~year, newID, data=datat1, qmatrix=Q,
                                   center=FALSE,
                                   covariates=~agescaled + agesqscaled + sex + racefinal2,
                        control=list(trace=1, fnscale=329428, maxit=200))
modelt1_baseline

modelt1_income <- msm(educNUM~year, newID, data=datat1, qmatrix=Q,
                        center=FALSE,
                        covariates=~agescaled + agesqscaled + sex + racefinal2 + incomescaled,
                        control=list(trace=1, fnscale=329428, maxit=200))
modelt1_income

Q <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat1)

modelt1_income_int <- msm(educNUM~year, newID, data=datat1, qmatrix=Q,
                      center=FALSE,
                      covariates=~agescaled + agesqscaled + sex + racefinal2*incomescaled,
                      hessian=F,
                      control=list(trace=1, fnscale=577103, maxit=100))
modelt1_income_int
AIC(modelt1_baseline, modelt1_income, modelt1_income_int)

saveRDS(modelt1_baseline, "SIMAH_workplace/education_transitions/final_models/modelt1_baseline_6cat_new.RDS")
saveRDS(modelt1_income, "SIMAH_workplace/education_transitions/final_models/modelt1_income_6cat_new.RDS")
saveRDS(modelt1_income_int, "SIMAH_workplace/education_transitions/final_models/modelt1_income_int_6cat_16_new.RDS")

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

