# SIMAH project 2022 - script for setting up sampleseeds file
library(dplyr)
library(readr)
library(microsimpackage)

options(scipen=999)

# set up the number of samples to be run
nsamples <- 1 # indicates samples per same sample seed (for calibration purposes only)
nreps <- 10 # indicates number of different sample seeds
n_uncertainty <- 60 # indicates number of unique combinations of education x alcohol models x own-price elasticities

# generate list of samples to be run with random number seeds
sampleseeds <- expand.grid(samplenum = 1:nsamples, seed=1:nreps)
sampleseeds$seed <- sample(1:3000, nrow(sampleseeds), replace=F)

# set up scenarios and policy settings
sampleseeds <- sampleseeds %>% expand(sampleseeds, policy_setting, scenarios)

# sample policy parameters here based on sampleseeds groups by samplenum, seed, edu/alcmodel
source("~/Desktop/SIMAH_code/microsimpackage/R/sample_policy_parameters.R") # if required
sampleseeds <- sample_policy_parameters(sampleseeds, n_uncertainty)

# add the final education model to be run to the sampleseeds file 
edmodels <- list()
submodel <- list()
for(i in 1:length(unique(sampleseeds$seed))){
  set.seed(unique(sampleseeds$seed)[i])
  
  for(k in 1:ceiling(length(unique(sampleseeds$nunc))/length(education_transitionsList))){
    submodel[[paste(k)]] <- data.frame(seed = unique(sampleseeds$seed)[i],
                                       educationmodel = sample(1:length(education_transitionsList), replace=F))
  }
  
  edmodels[[paste(i)]] <- submodel %>% bind_rows() %>% slice_sample(n = n_uncertainty) %>% mutate(nunc = 1:n_uncertainty)
}

edmodels <- edmodels %>% bind_rows()

sampleseeds <- sampleseeds %>% left_join(., edmodels)

# add the final alcohol model to be run to the sampleseeds file 
alcmodels <- list()
submodel <- list()
for(i in 1:length(unique(sampleseeds$seed))){
  set.seed(unique(sampleseeds$seed)[i])
  
  for(k in 1:ceiling(length(unique(sampleseeds$nunc))/length(alcohol_transitions))){
    submodel[[paste(k)]] <- data.frame(seed = unique(sampleseeds$seed)[i],
                                        alcoholmodel = sample(1:length(alcohol_transitions), replace=F))
  }

  alcmodels[[paste(i)]] <- submodel %>% bind_rows() %>% slice_sample(n = n_uncertainty) %>% mutate(nunc = 1:n_uncertainty)
}

alcmodels <- alcmodels %>% bind_rows()

sampleseeds <- sampleseeds %>% left_join(., alcmodels)

#check whether there are seeds X n_uncertainty unique combinations of elasticities, education, and alcohol model
unique(sampleseeds %>% dplyr::select(c("cons_elasticity", "educationmodel", "alcoholmodel")))

#save sampleseeds file for reproducability 
write.csv(sampleseeds, paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/sampleseeds/output-policy_sampleseeds_", Sys.Date(), ".csv"), row.names=F)
