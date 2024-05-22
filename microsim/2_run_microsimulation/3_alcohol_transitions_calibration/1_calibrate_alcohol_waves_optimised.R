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

estimates <- vector <- c(-2.773619, -3.872718, -3.704144, -0.677565, 3.436232, 2.831864, -0.071953, 0.511260, 1.668939, -6.542272, 2.546827, -3.608328, 0.128767, -4.217254, -3.797963, 0.157761, -0.294253, -3.532982, -1.027474, -1.583659, 2.145717, 1.803017, 2.500121, -2.753740, -0.249049, 1.402317, 2.628795, 1.819124, -5.198994, -1.280129, 0.019194, 4.055601, -5.923229, 3.929180, 0.869474, -3.134149, -2.714466, -2.937343, 1.052735, -1.679412, 3.303745, -2.192480, 1.242604, 1.257872, 0.832802, -3.565072, -0.403924, 6.406877, 0.163469, -1.769091, -1.459316, 1.412919, -0.476330, -0.758632)

# extract the estimates to be used in the optimisation equation
estimatesold <- model$estimates

targets <- generate_targets_alcohol(brfss)

# read in variance from NESARC calibration before 
variance <- read_csv("SIMAH_workplace/microsim/2_output_data/alcohol_calibration/NESARC_compare/variance.csv")

nsamples <- 540
max <- 8
min <- -8

optimised <- read_rds(paste0(OutputDirectory, "/optimised_output.RDS"))

bestpop <- optimised$member$pop
estimates <- colMeans(bestpop)
cov <- cov(bestpop)*2
starting <- mvrnorm(n=nrow(bestpop), estimates, cov)

# # define the upper and lower boundaries of the function
lower <- rep(min, length(estimates))
upper <- rep(max, length(estimates))
# 
# # # provide the original model as a starting point
# lhs <- randomLHS(nsamples, length(model$estimates))
# scaled_lhs <- t(apply(lhs, 1, function(x) min + (max - min) * x))
# starting <- scaled_lhs
# 
# starting <- sample_from_markov(model, 520, 20, 34165,2043174)
# starting$samplenum <- NULL
starting <- as.matrix(starting)

registerDoParallel(18)

threshold <- 5

maxyear <- 2014

optimised_output <- DEoptim(optimisation_wrapper, lower, upper, 
                            DEoptim.control(NP=nsamples, itermax=200, 
                                            strategy=2,
                                            CR=0.9, F=0.8,
                                            parallelType = "foreach",
                                            initialpop=starting),
                            model, targets, variance,maxyear,threshold)

saveRDS(optimised_output, paste0(OutputDirectory, "/optimised_output2", ".RDS"))
