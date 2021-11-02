# MSM model for education 
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(MASS)

# how many samples to take from the prior? 
nsamples <- 100

source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/functions/msm_functions.R")
source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/functions/msmparsecovariates.R")
source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/functions/Qmatrix_setup.R")
source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/functions/Sample_Probs.R")
source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/functions/extract_for_estimates.R")


data <- read_csv("SIMAH_workplace/education_transitions/PSID_reweighted_2019_weight.csv")
data$racefinal <- ifelse(data$racefinal=="Asian/PI","other",data$racefinal)
data$racefinal <- ifelse(data$racefinal=="Native","other",data$racefinal)


model1 <- readRDS("SIMAH_workplace/education_transitions/educMSM1_tunnelstates.RDS")
model2 <- readRDS("SIMAH_workplace/education_transitions/educMSM2_tunnelstates.RDS")
model3 <- readRDS("SIMAH_workplace/education_transitions/educMSM3_tunnelstates.RDS")

Samples1 <- Sample_Probs(data, model1, 10, "1999-2005")
Samples2 <- Sample_Probs(data, model2, 10, "2006-2011")
Samples3 <- Sample_Probs(data, model3, 10, "2012-2017")

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

rm(data, model1, model2, model3, Samples1, Samples2, Samples3, probs)
