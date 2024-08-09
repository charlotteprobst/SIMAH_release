# SIMAH project Feb 2024 

# code to run calibration of MSM model parameters to national / state-level alcohol output
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
OutputDirectory <- paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/categorical_validation")

# read in all model settings
source(paste0(ScriptDirectory, "/0_model_settings.R"))

# read in settings for calibration
contcalibration <- 1
source(paste0(ScriptDirectory,"0_calibration_settings.R"))

# load all microsim files
source(paste0(ScriptDirectory, "0_load_microsim_files.R"))

catcontmodel <- NULL

baseorig <- basepop
Output <- list()
Output <- foreach(i=1:nrow(sampleseeds), .inorder=TRUE) %dopar% {
  print(i)
  # set seed and sample number for current iteration
  samplenum <- as.numeric(sampleseeds$samplenum[i])
  seed <- as.numeric(sampleseeds$seed[i])
  # reset the base population to the original pop for each calibration iteration
  basepop <- baseorig 
  # change the alcohol model being run 
  alcohol_transitions <- alcohol_transitionsList[[samplenum]]
  # alcohol_transitions <- lhs %>% filter(sample==samplenum)
  # change the education model - based on the prior calibrated models 
  education_model_num <- as.numeric(sampleseeds$educationmodel[i])
  education_transitions <- education_transitionsList[[education_model_num]]
  # execute the simulation with each setting
  run_microsim_alt(seed,samplenum,basepop,brfss,
                   death_counts,
                   updatingeducation, education_transitions,
                   COVID_specific_tps=0,
                   migration_rates,
                   updatingalcohol, alcohol_transitions,
                   catcontmodel, drinkingdistributions,
                   base_counts, diseases, lhs, sesinteraction,
                   policy=0, percentreduction=0.1, year_policy, inflation_factors,
                   age_inflated,
                   update_base_rate,
                   minyear=2000, maxyear=2019, output="alcoholcat")
}

Output <- do.call(rbind,Output)
# save the output in the output directory
write.csv(Output, paste0(OutputDirectory, "/validation_output-",wave, ".csv"), row.names=F)

# # # calculate and save implausibility values
implausibility <- calculate_implausibility_alcohol(Output)
write.csv(implausibility, paste0(OutputDirectory, "/validation_implausibility-",wave, ".csv"), row.names=F)