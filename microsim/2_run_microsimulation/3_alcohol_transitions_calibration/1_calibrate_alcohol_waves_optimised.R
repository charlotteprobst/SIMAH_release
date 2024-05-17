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
library(DEoptim)
options(dplyr.summarise.inform = FALSE)
options(dplyr.show_progress = FALSE)

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

# read in all model settings
source(paste0(ScriptDirectory, "/0_model_settings.R"))

# read in settings for calibration
source(paste0(ScriptDirectory,"0_calibration_settings.R"))

# load all microsim files
source(paste0(ScriptDirectory, "0_load_microsim_files.R"))

# read in original NESARC Markov model 
model <- read_rds("SIMAH_workplace/nesarc/Models/msm3a_relevel.RDS")

# extract the estimates to be used in the optimisation equation
estimates <- model$estimates

targets <- generate_targets_alcohol(brfss)

# read in variance from NESARC calibration before 
variance <- read_csv("SIMAH_workplace/microsim/2_output_data/alcohol_calibration/NESARC_compare/variance.csv")

nsamples <- 540
max <- 3
min <- -3

# define the upper and lower boundaries of the function
lower <- rep(min, length(estimates))
upper <- rep(max, length(estimates))

# provide the original model as a starting point
lhs <- randomLHS(nsamples, length(model$estimates))
scaled_lhs <- t(apply(lhs, 1, function(x) min + (max - min) * x))
starting <- scaled_lhs
# 
# starting <- sample_from_markov(model, 520, 20, 34165,2043174)
# starting$samplenum <- NULL
starting <- as.matrix(starting)

registerDoParallel(15)

threshold <- 5

maxyear <- 2014

optimised_output <- DEoptim(optimisation_wrapper, lower, upper, 
                            DEoptim.control(NP=nsamples, itermax=200, 
                                            parallelType = "foreach",
                                            initialpop=starting),
                            model, targets, variance,maxyear,threshold)

saveRDS(optimised_output, paste0(OutputDirectory, "/optimised_output", ".RDS"))

