#####SIMAH project 2022 - script for running SIMAH microsimulation model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(devtools)
library(roxygen2)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(fitdistrplus)
library(lhs)
library(truncnorm)
library(data.table)
library(gridExtra)
library(foreach)
library(doParallel)
options(dplyr.summarise.inform = FALSE)

###set working directory to the main "SIMAH" folder in your directory 
# WorkingDirectory <- "U:/SIMAH/"
# WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
WorkingDirectory <- "/home/cbuckley/"


DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")

# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")

# alcohol_transitions <- read.csv("SIMAH_workplace/microsim/1_input_data/alcohol_transitions_new.csv")
alcohol_transitions <- readRDS(paste0(DataDirectory, "final_alc_transitionsUSA.RDS"))

output_type <- "mortality"

# set lhs to the first element of the lhs list- for testing 
# set lhs to the best calibrated settings
lhs <- read.csv("SIMAH_workplace/microsim/2_output_data/SIMAH_calibration/best_lhs.csv")

registerDoParallel(6)

seeds <- 1:10

samplestorun <- expand.grid(percentreduction=percentreductions,
                            replication = seeds)

Output <- foreach(i=1:length(percentreductions), .inorder=TRUE, .combine=rbind) %dopar% {
  percentreduction <- samplestorun$percentreduction[i]
  seed <- samplestorun$replication[i]
  year_policy <- 2015
  run_microsim(seed,i,basepop,brfss,
               death_counts,
               updatingeducation, education_setup,
               migration_counts,
               updatingalcohol, alcohol_transitions,
               base_counts, diseases, lhs, liverinteraction,
               policy, percentreduction, year_policy, inflation_factor,
               update_base_rate,
               2000, 2019, output_type)
}

saveRDS(Output, "SIMAH_workplace/microsim/2_output_data/policy_experiments/Alcohol_output_policyexperiments.RDS")
