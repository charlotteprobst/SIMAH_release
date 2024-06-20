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

# do the first analysis on the split time periods 

# # setup the datasets for both time periods

data <- setup_markov_model_formodel(data)

Q <- rbind( c(0.5, 0.5, 0, 0, 0),
            c(0, 0.5, 0.5, 0, 0),
            c(0, 0, 0.5, 0.5, 0),
            c(0, 0, 0, 0.5, 0.5),
            c(0, 0, 0, 0, 0.5))

# data$timevary <- cut(data$year,
#                      breaks=c(0,2005,2011,2018),
#                      labels=c("1999-2005","2006-2011","2012-2018"))

data$agecat <- ifelse(data$age==18, "18",
                        ifelse(data$age==19, "19",
                               ifelse(data$age==20, "20",
                                      ifelse(data$age>=21 & data$age<=25, "21-25","26+"))))

datat1 <- data %>% filter(year<=2005)
datat1 <- data[order(datat1$newID, datat1$year),]

datat2 <- data %>% filter(year<=2013 & year>=2007)
datat2 <- data[order(datat2$newID, datat2$year),]

datat3 <- data %>% filter(year>=2015)
datat3 <- data[order(datat3$newID, datat3$year),]

# specify baseline models - just race and ethnicity 
modelt1 <- msm(educNUM~year, newID, data=datat1, qmatrix=Q,
                                   center=FALSE,
                                   covariates=~agecat + sex + racefinal2,
                        control=list(trace=1, fnscale=271181, maxit=200))
modelt1

modelt2 <- msm(educNUM~year, newID, data=datat2, qmatrix=Q,
               center=FALSE,
               covariates=~agecat + sex + racefinal2,
               control=list(trace=1, fnscale=271181, maxit=200))
modelt2

modelt3 <- msm(educNUM~year, newID, data=datat3, qmatrix=Q,
               center=FALSE,
               covariates=~agecat + sex + racefinal2,
               control=list(trace=1, fnscale=271181, maxit=200))
modelt3


saveRDS(modelt1, "SIMAH_workplace/education_transitions/final_models/formodel_modelt1.RDS")
saveRDS(modelt2, "SIMAH_workplace/education_transitions/final_models/formodel_modelt2.RDS")
saveRDS(modelt3, "SIMAH_workplace/education_transitions/final_models/formodel_modelt3.RDS")

