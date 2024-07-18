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
source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/functions/Sample_Probs_races.R")
source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/functions/extract_for_estimates_races.R")

white <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_model_white.RDS")
# original = 6013, inflated = 1937944
black <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_model_black.RDS")
# original = 4151, inflated = 359774
other <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_model_other.RDS")
# original = 288, inflated = 120738
hispanic <- readRDS("SIMAH_workplace/education_transitions/final_models/formodel_model_hispanic.RDS")
# original = 691, inflated = 232276

Samples1 <- Sample_Probs(white, nsamples, "1999-2019", 1, 6013,1937944)
Samples2 <- Sample_Probs(black, nsamples, "1999-2019", 1, 4151,359774)
Samples3 <- Sample_Probs(other, nsamples, "1999-2019", 1, 288,120738)
Samples4 <- Sample_Probs(other, nsamples, "1999-2019", 1, 691,232276)

Samples1[[1]]$race <- "white"
Samples2[[1]]$race <- "black"
Samples3[[1]]$race <- "other"
Samples4[[1]]$race <- "hispanic"

Samples1[[2]]$race <- "white"
Samples2[[2]]$race <- "black"
Samples3[[2]]$race <- "other"
Samples4[[2]]$race <- "hispanic"


# Samples2 <- Sample_Probs(model2, nsamples, "2007-2013", 1, 2769,915752)
# Samples3 <- Sample_Probs(model3, nsamples, "2014-2019", 1, 2365,772327)

estimates <- rbind(Samples1[[2]], Samples2[[2]], Samples3[[2]], Samples4[[2]])

probs <- rbind(Samples1[[1]], Samples2[[1]], Samples3[[1]], Samples4[[1]])

transitionsList <- list()
for(i in 1:length(unique(estimates$SampleNum))){
  transitionsList[[paste(i)]] <- probs %>% filter(SampleNum==i) %>% 
    mutate(sex = ifelse(sex=="male", "m","f"),
           cat = paste(time,age, sex, race, "STATEFROM", StateFrom, sep="_")) %>% 
    group_by(cat) %>% mutate(cumsum=cumsum(prob)) %>% 
    dplyr::select(cat, StateTo, cumsum)
}

# rm(model1, model2, model3, Samples1, Samples2, Samples3, probs)
