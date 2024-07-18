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
data <- read_csv("SIMAH_workplace/education_transitions/new_PSID_weighted_IDs_2021.csv")

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
length(unique(datat1$uniqueID))
length(unique(datat1$newID))

datat2 <- data %>% filter(year<=2013 & year>=2005)
datat2 <- data[order(datat2$newID, datat2$year),]
length(unique(datat2$uniqueID))
length(unique(datat2$newID))

datat3 <- data %>% filter(year>=2013 & year<=2019)
datat3 <- data[order(datat3$newID, datat3$year),]
length(unique(datat3$uniqueID))
length(unique(datat3$newID))
# specify baseline models - just race and ethnicity 
Q <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat1)

modelt1 <- msm(educNUM~year, newID, data=datat1, qmatrix=Q,
                                   center=FALSE,
                                   covariates=~agecat + sex + racefinal2,
                        control=list(trace=1, fnscale=2543177, maxit=200))
modelt1

modelt2 <- msm(educNUM~year, newID, data=datat2, qmatrix=Q,
               center=FALSE,
               covariates=~agecat + sex + racefinal2,
               control=list(trace=1, fnscale=3453485, maxit=200))
modelt2

modelt3 <- msm(educNUM~year, newID, data=datat3, qmatrix=Q,
               center=FALSE,
               covariates=~agecat + sex + racefinal2,
               control=list(trace=1, fnscale=2915927, maxit=200))
modelt3


saveRDS(modelt1, "SIMAH_workplace/education_transitions/final_models/formodel_modelt1_sophie.RDS")
saveRDS(modelt2, "SIMAH_workplace/education_transitions/final_models/formodel_modelt2_sophie.RDS")
saveRDS(modelt3, "SIMAH_workplace/education_transitions/final_models/formodel_modelt3_sophie.RDS")

