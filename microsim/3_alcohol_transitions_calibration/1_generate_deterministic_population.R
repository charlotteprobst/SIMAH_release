# SIMAH project Feb 2024 

# code to generate "deterministic" population for the US level alcohol use 
# this is then used to fit a model of alcohol transitions 
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
WorkingDirectory <- "/Users/charlottebuckley/Google Drive/SIMAH Sheffield"

# set wd and install the microsim and calibration packages
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)
install("SIMAH_code/calibrationpackage", dep=T)

library(microsimpackage)
library(calibrationpackage)

ScriptDirectory <- paste0(WorkingDirectory, "/SIMAH_code/microsim/3_alcohol_transitions_calibration/")

# read in all model settings
source(paste0(ScriptDirectory, "0_model_settings.R"))
OutputDirectory <- paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/")
dir.create(OutputDirectory)

# for deterministic population - reduce population size to 10,000
PopulationSize <- 10000
# what proportion of the population does this represent
WholePopSize <- read.csv(paste0(DataDirectory,"fullpopcounts.csv")) %>% 
  filter(STATE==SelectedState)

proportion <- PopulationSize/WholePopSize$total
proportion <- ifelse(proportion>1,1,proportion)

# read in settings for calibration
contcalibration <- 0
source(paste0(ScriptDirectory,"0_calibration_settings.R"))

# load all microsim files
source(paste0(ScriptDirectory, "0_load_microsim_files.R"))

set.seed <- 10
Output <- run_microsim_determ(1,1,basepop,brfss,
                 death_counts,
                 updatingeducation,education_transitionsList[[1]],
                 migration_rates,
                 updatingalcohol, alcohol_transitions,
                 catcontmodel, drinkingdistributions,
                 base_counts, diseases, lhs, sesinteraction,
                 policy=0, percentreduction=0.1, year_policy, inflation_factors,
                 age_inflated,
                 update_base_rate,
                 minyear=2000, maxyear=2010, output="alcohol")

# save the deterministic population
write.csv(Output, paste0(OutputDirectory, "/full_pop_deterministic10000_withID.csv"))



