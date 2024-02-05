# MSM model for education 
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(MASS)

# how many samples to take from the prior? 

source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/functions/msm_functions.R")
source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/functions/msmparsecovariates.R")
source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/functions/Qmatrix_setup.R")
source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/functions/Sample_Probs.R")
source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/functions/extract_for_estimates.R")

# model1 <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_modelt1_sophie.RDS")
# model2 <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_modelt2_sophie.RDS")
# model3 <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_modelt3_sophie.RDS")

model <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_model_alltimes2005_age18-34_agecats.RDS")
# model2 <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_model_alltimes.RDS")

original <- 9064
inflated <- 2057084

Samples1 <- Sample_Probs(model, nsamples, "1999-2019", 30, original,inflated)

# estimates <- rbind(Samples1[[2]], Samples2[[2]], Samples3[[2]])
estimates <- Samples1[[2]]

# probs <- rbind(Samples1[[1]], Samples2[[1]], Samples3[[1]])
probs <- Samples1[[1]]

transitionsList <- list()
for(i in 1:length(unique(estimates$SampleNum))){
  transitionsList[[paste(i)]] <- probs %>% filter(SampleNum==i) %>% 
    mutate(sex = ifelse(sex=="male", "m","f"),
           cat = paste(time,age, sex, race, "STATEFROM", StateFrom, sep="_")) %>% 
    group_by(cat) %>% mutate(cumsum=cumsum(prob)) %>% 
    dplyr::select(cat, StateTo, cumsum)
}

# rm(model1, model2, model3, Samples1, Samples2, Samples3, probs)
