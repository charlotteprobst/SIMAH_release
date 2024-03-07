# SIMAH project Feb 2024 

# code to run calibration of MSM model parameters to national / state-level education output
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()

library(devtools)
library(roxygen2)
library(gatbxr)
# if having trouble with loading this package - run the below two lines
# install.packages("remotes")
# remotes::install_github("drizztxx/gatbxr")
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(fitdistrplus)
library(lhs)
library(truncnorm)
library(data.table)
library(gridExtra)
library(doParallel)
library(splitstackshape)
library(msm)
options(dplyr.summarise.inform = FALSE)

# WorkingDirectory <- "U:/SIMAH"
# WorkingDirectory <- "C:/Users/laura/Documents/CAMH/SIMAH"
# WorkingDirectory <- "/home/cbuckley"
WorkingDirectory <- "/Users/charlottebuckley/Google Drive/SIMAH Sheffield"

# set wd and install the microsim and calibration packages
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)
install("SIMAH_code/calibrationpackage", dep=T)

library(microsimpackage)
library(calibrationpackage)

ScriptDirectory <- paste0(WorkingDirectory, "/SIMAH_code/microsim/2_run_microsimulation/2_education_transitions_calibration/")

# read in all model settings
source(paste0(ScriptDirectory, "/0_model_settings.R"))

# read in settings for calibration
source(paste0(ScriptDirectory,"0_calibration_settings.R"))

# load all microsim files
source(paste0(ScriptDirectory, "0_load_microsim_files.R"))

# set up samples for calibration for education transitions
source(paste0(ScriptDirectory,"0_generate_calibration_samples.R"))

# parallel loop that runs the calibration process 
# this loops through waves of calibration and runs all sampled settings

while(wave <= num_waves){
  baseorig <- basepop
  Output <- list()
  Output <- foreach(i=1:nrow(sampleseeds), .inorder=TRUE) %do% {
    print(i)
    # set seed and sample number for current iteration
    samplenum <- as.numeric(sampleseeds$samplenum[i])
    seed <- as.numeric(sampleseeds$seed[i])
    # reset the base population to the original pop for each calibration iteration
    basepop <- baseorig
    # change the education transitions for each iteration
    education_transitions <- transitionsList[[samplenum]]
    # execute the simulation with each setting
    run_microsim_alt(seed,samplenum,basepop,brfss,
                     death_counts,
                     updatingeducation, education_transitions,
                     migration_rates,
                     updatingalcohol, alcohol_transitions,
                     catcontmodel, drinkingdistributions,
                     base_counts, diseases, lhs, sesinteraction,
                     policy=0, percentreduction=0.1, year_policy, inflation_factors,
                     age_inflated,
                     update_base_rate,
                     minyear=2000, maxyear=2019, output="demographics")
    }

  Output <- do.call(rbind,Output)
  # save the output in the output directory
  write.csv(Output, paste0(OutputDirectory, "/output-",wave, ".csv"), row.names=F)

  # calculate and save implausibility values
  implausibility <- calculate_implausibility_education(Output, targets)
  write.csv(implausibility, paste0(OutputDirectory, "/implausibility-",wave, ".csv"), row.names=F)
  
  # calculate the difference between the old implausibility and new implausibility 
  new_mean_implausibility <- mean(implausibility$implausibility)
  max_implausibility <- max(implausibility$implausibility)

  if(wave>1){
    # check improvement % and stop if minimal improvement (based on improvement threshold defined in settings)
    improvement <- abs(prev_mean_implausibility - new_mean_implausibility)/prev_mean_implausibility
    if(improvement < improvement_threshold | max_implausibility < 1) {
      break
    }
  }

  # keep top 15% of samples 
  topsamples <- unique(subset(implausibility, percentile<=15)$samplenum)
  # now restrict the markov model samples based on the top fitting models
  newsamples <- samples %>% filter(samplenum %in% topsamples)
  
  # now resample the new parameters for the markov model 
  samples <- newsamples %>% dplyr::select(-c(samplenum)) %>% 
    mutate_all(as.numeric)
  estimates <- colMeans(samples)
  cov <- cov(samples)
  samples <- data.frame(mvrnorm(n=nsamples, estimates, cov))
  
  # now transform this into TPs
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
  
  transitionsList <- list()
  for(i in 1:length(unique(samples$samplenum))){
    transitionsList[[paste(i)]] <- probs %>% filter(samplenum==i) %>%
      mutate(cat = paste(age, sex, race, "STATEFROM", StateFrom, sep="_")) %>%
      group_by(cat) %>% mutate(cumsum=cumsum(prob)) %>%
      dplyr::select(cat, StateTo, cumsum)
  }
  
  prev_mean_implausibility <- new_mean_implausibility
  wave <- wave + 1

  # save the new TPs and the estimates that will be run in the next wave
  saveRDS(transitionsList, paste0(OutputDirectory, "/transitionsList-",wave,".RDS",sep=""))
  write.csv(samples, paste0(OutputDirectory, "/sampled_markov-",wave, ".csv"), row.names=F)
}

