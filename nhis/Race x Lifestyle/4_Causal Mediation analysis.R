
# Race x Lifestyle Differential Vulnerability & Exposure Project
# Causal Mediation File 


# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(timereg)    # additive survival models
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions
library(foreach)    # to bootstrap
library(parallel)   # for parallel processing
library(doParallel) # for parallel processing
library(tictoc)     # To track time
memory.limit(size=1e+13)

# Personal computer; specify locations 
data   <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Processed data/"            # Location of data
output <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Race x Lifestyle/CausMed/"  # Location of model output
source("Function - CausalMed.R")


# SCC; ; specify locations 
# setwd("/external/mgmt3/imaging/scratch/Imhpr/kpuka/nhis/")
# data    <- "Data/"
# output  <- "Output/"
# source("Function - CausalMed.R")


# Load data
nhis        <- readRDS (paste0(data, "nhis18_85.rds"))
nhis_male   <- filter(nhis, female==0)
nhis_female <- filter(nhis, female==1)


set.seed(1235)

# WOMEN: Bootstrap Causal Mediation -----------------------------------------------------------------------------------------
tic() # start timer

# Specify number of cores to use 
cl <- makeCluster(4, outfile = "log.txt")
registerDoParallel(cl) 

    # Check
    detectCores() # Identify # of cores available
    foreach::getDoParWorkers() # Identify # of cores that will be used


# Run analysis using bootstrap
CMed_boot_w <- bootstrap_CMed(nhis_female, reps=32, prop=0.01)
stopCluster(cl) # To stop the parallel processing (windows)
saveRDS(CMed_boot_w, file.path(output, "CMed_boot_w.rds")) # Save bootstrap results

    
# load bootstrap results
CMed_boot_w <- readRDS(file.path(output, "CMed_boot_w.rds")) 

# Compute CI and format results 
CMed_women <- format_CMed(CMed_boot_w)
view(CMed_women) # view results
write.csv(CMed_women, file=paste0(output, "CMed_results_women.csv")) # save results



# MEN: Bootstrap Causal Mediation -----------------------------------------------------------------------------------------

# Specify number of cores to use 
parallel::detectCores() # Identify # of cores available
cl <- parallel::makeCluster(4, outfile = "log.txt")   # Windows
# cl <- parallel::makeForkCluster(2) # Linux
doParallel::registerDoParallel(cl) 

foreach::getDoParWorkers() # Identify # of cores that will be used


# Run analysis using bootstrap
CMed_boot_m <- bootstrap_CMed(nhis_male, reps=12, prop=0.01)

stopCluster(cl) # To stop the parallel processing (windows)
saveRDS(CMed_boot_m, file.path(output, "CMed_boot_m.rds")) # Save bootstrap results

    
    
# load bootstrap results
CMed_boot_m <- readRDS(file.path(output, "CMed_boot_m.rds")) 

# Compute CI and format results 
CMed_men <- format_CMed(CMed_boot_m)
view(CMed_men) # view results
write.csv(CMed_men, file=paste0(output, "CMed_results_men.csv")) # save results

