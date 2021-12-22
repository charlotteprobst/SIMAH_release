# MSM model for education 
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(MASS)

# how many samples to take from the prior? 
nsamples <- 25

source("SIMAH_code/microsim/2_run_microsimulation/alcohol_transitions_calibration/functions/msm_functions.R")
source("SIMAH_code/microsim/2_run_microsimulation/alcohol_transitions_calibration/functions/msmparsecovariates.R")
source("SIMAH_code/microsim/2_run_microsimulation/alcohol_transitions_calibration/functions/Qmatrix_setup.R")
source("SIMAH_code/microsim/2_run_microsimulation/alcohol_transitions_calibration/functions/Sample_Probs.R")
source("SIMAH_code/microsim/2_run_microsimulation/alcohol_transitions_calibration/functions/extract_for_estimates.R")

model <- readRDS("SIMAH_workplace/microsim/1_input_data/alc5.msm.RDS")

# pmatrix.msm(model, covariates=list(female_wave1.factorWomen=1))
data <- model$data$mf
# model$call
# unique(data$edu3.factor)
Samples <- Sample_Probs(data, model, nsamples)

estimates <- Samples[[2]]

probs <- Samples[[1]]

transitionsList <- list()
for(i in 1:length(unique(estimates$SampleNum))){
  transitionsList[[paste(i)]] <- probs %>% filter(SampleNum==i) %>% 
    mutate(sex = ifelse(sex=="Men", "m","f"),
           race = recode(race, "Black, non-Hispanic"="BLA",
                         "White, non-Hispanic"="WHI",
                         "Other, non-Hispanic"="OTH",
                         "Hispanic"="SPA"),
           educ = recode(educ, "High"="College",
                         "Med"="SomeC","Low"="LEHS"),
           StateFrom=recode(StateFrom, "1"="Lifetime abstainer",
                            "2"="Former drinker", "3"="Low risk",
                            "4"="Medium risk", "5"="High risk"),
           StateTo = recode(StateTo, "1"="Lifetime abstainer",
                            "2"="Former drinker", "3"="Low risk",
                            "4"="Medium risk", "5"="High risk"),
           
           cat = paste(age, sex, race, educ, "STATEFROM", StateFrom, sep="_")) %>% 
    group_by(cat) %>% mutate(cumsum=cumsum(prob)) %>% 
    dplyr::select(cat, StateTo, cumsum)
}

rm(data, model, Samples, probs)
