
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
model  <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Race x Lifestyle/CausMed/"  # Location of model output
output <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/Race x Lifestyle/Output/"


# SCC; ; specify locations 
# setwd("/external/mgmt3/imaging/scratch/Imhpr/kpuka/nhis/")
# data    <- "Data/"
# model  <- "Model/"


# Load data
nhis        <- readRDS (paste0(data, "nhis18_85.rds"))
nhis_male   <- filter(nhis, female==0)
nhis_female <- filter(nhis, female==1)
source("_Extra_Function - CausalMed - remove 'Others'.R")


# Causal Mediation *****************************************************************************************

# Analyis 
CMed_women <- CMed_analysis(nhis_female) %>% saveRDS(paste0(model,"CMed_women_test.RDS"))
CMed_men   <- CMed_analysis(nhis_male)   %>% saveRDS(paste0(model,"CMed_men.RDS"))


# Format results
CMed_black_w    <- format_CMed(CMed_women, "Black Women", c(1,3,5,7,9,31,35,39,43))
CMed_hispanic_w <- format_CMed(CMed_women, "Hispanic Women", c(2,4,6,8,10,34,38,42,46))
CMed_black_m    <- format_CMed(CMed_men, "Black Men", c(1,3,5,7,9,31,35,39,43))
CMed_hispanic_m <- format_CMed(CMed_men, "Hispanic Men", c(2,4,6,8,10,34,38,42,46))


CMed_results <- cbind(CMed_black_m, CMed_black_w, CMed_hispanic_m, CMed_hispanic_w)[,c(2:4,7,8, 11,12, 15,16)]
view(CMed_results)

CMed_results_Noprop <- cbind(CMed_black_m, CMed_black_w, CMed_hispanic_m, CMed_hispanic_w)[,c(2,3,7,11,15)]
view(CMed_results_Noprop)










