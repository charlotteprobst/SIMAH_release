# SIMAH project July 2024

# code to calibrate continuous alcohol use distributions
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
library(stringr)
library(plotrix)
options(dplyr.summarise.inform = FALSE)

# WorkingDirectory <- "U:/SIMAH"
# WorkingDirectory <- "C:/Users/laura/Documents/CAMH/SIMAH"
WorkingDirectory <- "/home/cbuckley"
# WorkingDirectory <- "/Users/charlottebuckley/Google Drive/SIMAH Sheffield"

# set wd and install the microsim and calibration packages
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)
install("SIMAH_code/calibrationpackage", dep=T)

library(microsimpackage)
library(calibrationpackage)

ScriptDirectory <- paste0(WorkingDirectory, "/SIMAH_code/microsim/2_run_microsimulation/3_alcohol_transitions_calibration/")
OutputDirectory <- paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/continuous_calibration")

# read in all model settings
source(paste0(ScriptDirectory, "/0_model_settings.R"))

contcalibration <- 1
# read in settings for calibration
source(paste0(ScriptDirectory,"0_calibration_settings.R"))

# load all microsim files
source(paste0(ScriptDirectory, "0_load_microsim_files.R"))

# generate latin hypercube for different distributions
source(paste0(ScriptDirectory, "0_generate_calibration_samples_continuous.R"))

# catcontmodel <- read.csv("SIMAH_workplace/microsim/1_input_data/CatContDistr_beta.csv") %>%
#   dplyr::select(group, shape1, shape2, min, max)

catcontmodel <- read.csv("SIMAH_workplace/microsim/2_output_data/alcohol_calibration/continuous_calibration/calibration_continuous_distribution.csv")

# parallel loop that runs the calibration process 
# this loops through waves of calibration and runs all sampled settings
while(wave <= num_waves){
  baseorig <- basepop
  Output <- list()
  Output <- foreach(i=1:nrow(sampleseeds), .inorder=TRUE) %dopar% {
    print(i)
    # set seed and sample number for current iteration
    samplenum <- as.numeric(sampleseeds$samplenum[i])
    seed <- as.numeric(sampleseeds$seed[i])
    # reset the base population to the original pop for each calibration iteration
    basepop <- baseorig 
    # change the alcohol model - based on prior calibrated models 
    alcohol_model_num <- as.numeric(sampleseeds$alcoholmodel[i])
    alcohol_transitions <- alcohol_transitionsList[[alcohol_model_num]]
    # change the education model - based on the prior calibrated models 
    education_model_num <- as.numeric(sampleseeds$educationmodel[i])
    education_transitions <- education_transitionsList[[education_model_num]]
    # change the continuous alcohol consumption distribution 
    catcontmodel <- transitionsList[[samplenum]]
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
                     minyear=2000, maxyear=2019, output="alcoholcont")
  }
  
  Output <- do.call(rbind,Output)
  # save the output in the output directory
  write.csv(Output, paste0(OutputDirectory, "/output-",wave, ".csv"), row.names=F)

  # # # calculate and save implausibility values
  implausibility <- calculate_implausibility_alcohol_mean(Output,brfss)
  write.csv(implausibility, paste0(OutputDirectory, "/implausibility-",wave, ".csv"), row.names=F)
  # 
  # # calculate the difference between the old implausibility and new implausibility 
  new_mean_implausibility <- mean(implausibility$max, na.rm=T)
  max_implausibility <- max(implausibility$max)
  # 
  if(wave>1){
    # check improvement % and stop if minimal improvement (based on improvement threshold defined in settings)
    improvement <- abs(prev_mean_implausibility - new_mean_implausibility)/prev_mean_implausibility
    if(improvement < improvement_threshold | max_implausibility < 1) {
      break
    }
  }
  # 
  # # keep top 15% of samples 
  implausibility <- implausibility %>% ungroup() %>% 
    mutate(percentile = ntile(max, 100))
  topsamples <- unique(subset(implausibility, percentile<=15)$samplenum)

  lhs <- resample_ordinal_model(transitionsList, topsamples, nsamples)
  
  transitionsList <- list()
  for(i in unique(lhs$sample)){
    transitionsList[[paste(i)]] <- lhs %>% filter(sample==i) %>%
      ungroup() %>%
      dplyr::select(-sample)
}
  prev_mean_implausibility <- new_mean_implausibility
  wave <- wave + 1
  
  # save new samples from the regression
  write.csv(lhs, paste0(OutputDirectory, "/lhs_regression-", wave,".csv"))
}

