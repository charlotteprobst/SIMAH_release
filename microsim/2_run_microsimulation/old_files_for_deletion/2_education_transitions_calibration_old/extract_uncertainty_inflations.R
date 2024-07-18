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
model2 <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_modelt2.RDS")
model3 <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_modelt3.RDS")

# loop through different versions of inflation
from <- 0
to <- 100
by <- round((to-from)/n_inflations)

steps <- seq(from, to, by)
steps[1] <- 1

estimates <- list()
probs <- list()
Samples1 <- list()
Samples2 <- list()
Samples3 <- list()

for(i in 1:length(steps)){
Samples1[[i]] <- Sample_Probs(model1, nsamples, "1999-2006", steps[i])
Samples2[[i]] <- Sample_Probs(model2, nsamples, "2007-2013", steps[i])
Samples3[[i]] <- Sample_Probs(model3, nsamples, "2014-2019", steps[i])

estimates[[i]] <- rbind(Samples1[[i]][[2]], Samples2[[i]][[2]], Samples3[[i]][[2]])

probs[[i]] <- rbind(Samples1[[i]][[1]], Samples2[[i]][[1]], Samples3[[i]][[1]])
}

estimates <- do.call(rbind,estimates)
probs <- do.call(rbind,probs)

estimatesinflations <- estimates %>% dplyr::select(SampleNum, inflation) %>% distinct()

transitionsList <- list()
for(i in 1:nrow(estimatesinflations)){
  sample <- estimatesinflations$SampleNum[i]
  inflationsample <- estimatesinflations$inflation[i]
  transitionsList[[paste(i)]] <- probs %>% filter(SampleNum==sample) %>% 
    filter(inflation==inflationsample) %>% 
    mutate(sex = ifelse(sex=="male", "m","f"),
           cat = paste(time,age, sex, race, "STATEFROM", StateFrom, sep="_")) %>% 
    group_by(cat) %>% mutate(cumsum=cumsum(prob)) %>% 
    dplyr::select(cat, inflation, StateTo, cumsum)
}

rm(model1, model2, model3, Samples1, Samples2, Samples3, probs)
