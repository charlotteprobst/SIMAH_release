# SIMAH project 2024 - script for generating samples from COVID markov model for calibration
library(tidyverse)
library(microsimpackage)
library(calibrationpackage)
library(MASS)
library(msm)

setwd("C:/Users/cmp21seb/Documents/SIMAH")

# Read in data used for model to extract length of the original and replicated samples
data <- readRDS("SIMAH_workplace/education_transitions/2021/data_to_model/prepped_data_for_markov_2021.rds")
originalsample <- 3925
inflatedsample <- 958365

# read in the model of interest
model <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/covid_modelt4.RDS")

nsamples <- 300
 
# first sample from the markov model to get nsamples new estimates
samples <- sample_from_markov(model, nsamples, inflation=1, originalsample, inflatedsample)

# now convert each new sampled estimate into transition probabilities
#first specify the covariates for the model
# every age sex race combination
# note these have to be in exactly the same format of the covariates specified in the model
# if unsure of this run model$covariates to check and e.g. model$data$mf$sex
covariates <- data.frame(expand.grid(agecat=c("18","19","20","21-25","26+"),
                                     sex=c(0,1),
                                     racefinal2=c("white","black","hispanic","other")))
covariates$cat <- paste(covariates$agecat, covariates$sex, covariates$racefinal2, sep="_")

probs <- convert_to_probability(samples, model, covariates)

# format for calibration - label categories and put into list format
probs <- probs %>% 
  pivot_longer(cols=State.1:State.5,
                                                        names_to="StateTo", values_to="prob") %>%
  mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                             endsWith(StateTo,"2") ~ "State 2",
                             endsWith(StateTo,"3") ~ "State 3",
                             endsWith(StateTo,"4") ~ "State 4",
                             endsWith(StateTo,"5") ~ "State 5")) %>%
  separate(cov, into=c("age","sex","race"), sep="_") %>% 
  mutate(sex=ifelse(sex=="0", "m","f"))

transitionsList_covid <- list()
for(i in 1:length(unique(samples$samplenum))){
  transitionsList_covid[[paste(i)]] <- probs %>% filter(samplenum==i) %>%
    mutate(cat = paste(age, sex, race, "STATEFROM", StateFrom, sep="_")) %>%
    group_by(cat) %>% mutate(cumsum=cumsum(prob)) %>%
    dplyr::select(cat, StateTo, cumsum)
}

# save samples - for wave 1 in Output Directory
saveRDS(transitionsList_covid, paste0(OutputDirectory, "/transitionsList-1-COVID",".RDS"))
colnames(samples) <- make.unique(colnames(samples))
write.csv(samples, paste0(OutputDirectory, "/sampled_markov-1-COVID", ".csv"), row.names=F)
