
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
#library(doMC)       # parallel processing in Linux

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



# Set up parallel processing -----------------------------------------------------------------------------------------

foreach::getDoParWorkers()                # Identify # of cores that will be used
# registerDoMC(5)                         # Linux: Specify number of cores to use  
cl <- makeCluster(4, outfile = "log.txt") # Windows: Specify number of cores to use  
registerDoParallel(cl)                    # Windows: Specify number of cores to use  
foreach::getDoParWorkers()  # Identify # of cores that will be used




# WOMEN: Bootstrap Causal Mediation -----------------------------------------------------------------------------------------

set.seed(1235)

# Analysis
# CMed_boot_w <- bootstrap_CMed(nhis_female, reps=1000, prop=0.20)  # Run analysis using bootstrap
# saveRDS(CMed_boot_w, file.path(output, "CMed_boot_w.rds"))        # Save bootstrap results
CMed_boot_w <- readRDS(file.path(output, "CMed_boot_w.rds"))        # load bootstrap results

# Results 
boot_data <- as.data.frame(do.call(cbind, CMed_boot_w))
CMed_women <- format_CMed(boot_data)                                 # Compute CI and format results 
CMed_women                                                           # print results 
write.csv(CMed_women, file=paste0(output, "CMed_results_women.csv")) # save results




# MEN: Bootstrap Causal Mediation -----------------------------------------------------------------------------------------

set.seed(1235)

# Analysis
# CMed_boot_m <- bootstrap_CMed(nhis_male, reps=1000, prop=0.20)  # Run analysis using bootstrap
# saveRDS(CMed_boot_m, file.path(output, "CMed_boot_m.rds"))      # Save bootstrap results
CMed_boot_m <- readRDS(file.path(output, "CMed_boot_m.rds"))    # load bootstrap results

# Results 
boot_data <- as.data.frame(do.call(cbind, CMed_boot_m))
CMed_men <- format_CMed(boot_data)                                 # Compute CI and format results 
CMed_men                                                           # print results 
write.csv(CMed_men, file=paste0(output, "CMed_results_men.csv")) # save results

