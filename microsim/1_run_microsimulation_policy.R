#####SIMAH project - script for running SIMAH microsimulation model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(devtools)
library(roxygen2)
library(gatbxr)
library(faux)
# if having trouble with loading this package - run the below two lines
# install.packages("remotes")
# remotes::install_github("drizztxx/gatbxr")
library(dplyr)
library(tidyverse)
library(fitdistrplus)
library(lhs)
library(truncnorm)
library(data.table)
library(gridExtra)
library(doParallel)
options(dplyr.summarise.inform = FALSE)
registerDoParallel(1)

library(beepr)

# set working directory to the main "SIMAH" folder in your directory 
# WorkingDirectory <- "/Users/carolinkilian/Desktop/"
WorkingDirectory <- "/Users/julialemp/Desktop/"
setwd(paste(WorkingDirectory))

# set up data input and output directories 
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")
OutputDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/", Sys.Date())
dir.create(OutputDirectory)

# load in microsim R package - IMPORTANT: required to update functions for price policy version
install("SIMAH_code/microsimpackage", dep=T)
install("SIMAH_code/calibrationpackage", dep=T)

library(microsimpackage)
library(calibrationpackage)

# Double-check that those functions are loaded - does not always work for some reason!
source("SIMAH_code/microsimpackage/R/sample_policy_parameters.R")
source("SIMAH_code/microsimpackage/R/apply_tax_policy.R")
source("SIMAH_code/microsimpackage/R/prob_alcohol_transition.R")
source("SIMAH_code/microsimpackage/R/run_microsim_alt.R")
source("SIMAH_code/microsimpackage/R/fix_initial_education.R")


# load model settings 
# file.remove("/Users/carolinkilian/Desktop/SIMAH_workplace/microsim/2_output_data/testing")
source("SIMAH_code/microsim/0_model_settings.R")
source("SIMAH_code/microsim/0_policy_settings.R")

# load microsim files
source("SIMAH_code/microsim/0_load_microsim_files.R")


# read in sampleseeds file or source 0_generate_sampleseeds.R
source("SIMAH_code/microsim/0_generate_sampleseeds.R") 
# sampleseeds <- read.csv("SIMAH_workplace/microsim/2_output_data/sampleseeds/price_policy/output-policy_sampleseeds_2025-01-20.csv")

# generate copy of basepop to loop through sampleseeds iterations
baseorig <- basepop

# loop by outcome_type
foreach(k=1:length(output_type)) %do% {

  output <- output_type[k]
  print(output)
  
  # microsimulation loop 
  Output <- list()
  # Output <- foreach(i=1:nrow(sampleseeds), .inorder=TRUE) %do% {
  Output <- foreach(i=1:2, .inorder=TRUE) %do% {
    print(i)
    # set seed and nunc for current iteration
    seed <- as.numeric(sampleseeds$seed[i])
    nunc <- as.numeric(sampleseeds$nunc[i])
    samplenum <- as.numeric(sampleseeds$samplenum[i])
    # set up policy parameters
    policymodel <- sampleseeds$policymodel[i]
    scenario <- as.numeric(unlist(strsplit(sampleseeds$scenario[i], ",")))
    setting <- sampleseeds$setting[i]
    participation <- as.numeric(sampleseeds$participation[i])
    cons_elasticity <- as.numeric(unlist(strsplit(sampleseeds$cons_elasticity[i], ",")))
    cons_elasticity_se <- as.numeric(unlist(strsplit(sampleseeds$cons_elasticity_se[i], ",")))
    part_elasticity <- as.numeric(sampleseeds$part_elasticity[i])
    r_sim_obs <- as.numeric(sampleseeds$r_sim_obs[i])
    # reset the base population to the original pop for each sampleseed iteration
    basepop <- baseorig 
    # change the alcohol model - based on prior calibrated models 
    alcohol_model_num <- as.numeric(sampleseeds$alcoholmodel[i])
    alcohol_transitions <- alcohol_transitionsList[[alcohol_model_num]]
    # change the education model - based on the prior calibrated models 
    education_model_num <- as.numeric(sampleseeds$educationmodel[i])
    education_transitions <- education_transitionsList[[education_model_num]]
    
    run_microsim_alt(seed,samplenum,basepop,brfss,
                     death_counts,
                     updatingeducation, education_transitions,
                     COVID_specific_tps=0,
                     migration_rates,
                     updatingalcohol, alcohol_transitions,
                     catcontmodel, drinkingdistributions,
                     base_counts, diseases, mortality_parameters, sesinteraction,
                     policy, policy_int, policymodel, year_policy, scenario, 
                     participation, part_elasticity, cons_elasticity, cons_elasticity_se, r_sim_obs,
                     inflation_factors,
                     age_inflated,
                     update_base_rate,
                     minyear=minyear, maxyear=maxyear, output=output)
  }

  Output <- do.call(rbind,Output)
  # save the output in the output directory
  write.csv(Output, paste0(OutputDirectory, "/output-policy_", output, "_", Sys.Date(), ".csv"), row.names=F)
  
}

