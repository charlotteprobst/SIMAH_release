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

model1 <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_modelt1.RDS")
# model 1 - original sample = 4003, newsample = 88434
model2 <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_modelt2.RDS")
# model 2 - original sample = 4100, newsample = 91300
model3 <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_modelt3.RDS")
# model 3 - original sample = 2704, newsample = 59286

Samples1 <- Sample_Probs(model1, nsamples, "1999-2006", 1, 4003,88434)
Samples2 <- Sample_Probs(model2, nsamples, "2007-2013", 1, 4100,91300)
Samples3 <- Sample_Probs(model3, nsamples, "2014-2019", 1, 2704,59286)

estimates <- rbind(Samples1[[2]], Samples2[[2]], Samples3[[2]])

probs <- rbind(Samples1[[1]], Samples2[[1]], Samples3[[1]])

transitionsList <- list()
for(i in 1:length(unique(estimates$SampleNum))){
  transitionsList[[paste(i)]] <- probs %>% filter(SampleNum==i) %>% 
    mutate(sex = ifelse(sex=="male", "m","f"),
           cat = paste(time,age, sex, race, "STATEFROM", StateFrom, sep="_")) %>% 
    group_by(cat) %>% mutate(cumsum=cumsum(prob)) %>% 
    dplyr::select(cat, StateTo, cumsum)
}

rm(model1, model2, model3, Samples1, Samples2, Samples3, probs)
