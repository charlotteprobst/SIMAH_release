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

source("SIMAH_code/education_transitions/estimate_education_tps/0_setup_markov_model.R")

#### SCRIPT CAN BE STARTED FROM HERE IF REWEIGHTED DATA WITH IDS EXISTS ####
data <- read_csv("SIMAH_workplace/education_transitions/new_PSID_weighted_IDs_2021.csv")

# # setup the datasets for the required time periods (exclude < 2005 due to unreliable data)
data <- data %>% filter(year>=2005 & year<=2019)
data <- data %>% filter(age<=34)

data <- setup_markov_model_formodel(data)

# setup Q matrix - allowed instantaneous transitions

Q <- rbind( c(0.08, 0.08, 0, 0, 0),
            c(0, 0.08, 0.08, 0, 0),
            c(0, 0, 0.08, 0.08, 0),
            c(0, 0, 0, 0.08, 0.08),
            c(0, 0, 0, 0, 0.08))

data$agecat <- ifelse(data$age==18, "18",
                        ifelse(data$age==19, "19",
                               ifelse(data$age==20,  "20",
                                      ifelse(data$age==21, "21",
                                      ifelse(data$age>=22 & data$age<=24, "22-24",
                                             ifelse(data$age>=25 & data$age<=29, "25-29","30+"))))))
data$birthyear <- data$year - data$age
data$cohort <- ifelse(data$birthyear < 1990, "<1990",
                      ifelse(data$birthyear>=1990, ">1990",NA))

data <- data %>% filter(year<=2019)
data <- data[order(data$newID, data$year),]
length(unique(data$uniqueID))
length(unique(data$newID))

# specify baseline models - just race and ethnicity 
Q <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=data)

# set as factor - important for model outputs
data$agecat <- as.factor(data$agecat)
data$racefinal2 <- as.factor(data$racefinal2)
data$sex <- as.factor(data$sex)

model <- msm(educNUM~year, newID, data=data, qmatrix=Q,
                                   center=FALSE,
                                   covariates=~agecat + sex + racefinal2,
                        control=list(trace=1, fnscale=5577458, maxit=200))
model
AIC(model)

saveRDS(model, "SIMAH_workplace/education_transitions/final_models/final_psid_education_model.RDS")
