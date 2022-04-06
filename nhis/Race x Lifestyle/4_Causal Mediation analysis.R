
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
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))

set.seed(1235)

# WOMEN: Bootstrap Causal Mediation -----------------------------------------------------------------------------------------
tic()
# Specify number of cores to use 
detectCores() # Identify # of cores available
cl <- makeCluster(4, outfile = "log.txt")   # Windows
# cl <- makeForkCluster(2) # Linux
registerDoParallel(cl) 

foreach::getDoParWorkers() # Identify # of cores that will be used


# Run analysis using bootstrap
CMed_boot_w <- bootstrap_CMed(nhis_female, reps=32, prop=0.01)
    
stopCluster(cl) # To stop the parallel processing (windows)
saveRDS(CMed_boot_w, file.path(output, "CMed_boot_w.rds")) # Save bootstrap results
toc()
    
# load bootstrap results
CMed_boot_w <- readRDS(file.path(output, "CMed_boot_w.rds")) 

# Compute CI and format results 
mean <- rowMeans(CMed_boot_w) # get mean estimate
ci <- apply(CMed_boot_w, 1, quantile, probs=c(0.025, 0.975)) %>% t() #get 95% CI
CMed_women <- cbind(mean, ci) %>% as.data.frame() %>% 
  
  # add labels
  mutate (term = rep(c( "Total effect of race (ref=White)", 
                    "Direct effect of raca (ref=White)", 
                    "Indirect effect of race (ref=White)", 
                    "     Alcohol use: differential exposure", 
                    "     Alcohol use: differential vulnerability ", 
                    "     Smoking: differential exposure", 
                    "     Smoking: differential vulnerability ", 
                    "     BMI: differential exposure", 
                    "     BMI: differential vulnerability ", 
                    "     Physical activity: differential exposure", 
                    "     Physical activity: differential vulnerability"), 6),
          race = rep(c("Black", "Hispanic", "Other"), each=22),
          type = rep(c("deaths", "prop"), 3, each=11)) %>% 
  
  # Separate the 'additional deaths' and 'proportion' estimates 
  pivot_wider (names_from="type", values_from=c("mean", "2.5%", "97.5%")) %>%
  
  # reformat 
  mutate (deaths = round(mean_deaths*10000,1),
    deaths_lower = round(`2.5%_deaths`*10000,1),
    deaths_upper = round(`97.5%_deaths`*10000,1),
    prop = round(mean_prop*100,0),
    prop_lower = round(`2.5%_prop`*100,0),
    prop_upper = round(`97.5%_prop`*100,0),
    deaths_10000py_ci = paste0(deaths, " (", deaths_lower, ", ", deaths_upper, ")"),
    prop_ci = paste0(prop, " (", prop_lower, ", ", prop_upper, ")"))%>%
  dplyr::select(race, term, deaths_10000py_ci, prop_ci)

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
mean <- rowMeans(CMed_boot_m) # get mean estimate
ci <- apply(CMed_boot_m, 1, quantile, probs=c(0.025, 0.975)) %>% t() #get 95% CI
CMed_men <- cbind(mean, ci) %>% as.data.frame() %>% 
  
  # add labels
  mutate (term = rep(c( "Total effect of race (ref=White)", 
    "Direct effect of raca (ref=White)", 
    "Indirect effect of race (ref=White)", 
    "     Alcohol use: differential exposure", 
    "     Alcohol use: differential vulnerability ", 
    "     Smoking: differential exposure", 
    "     Smoking: differential vulnerability ", 
    "     BMI: differential exposure", 
    "     BMI: differential vulnerability ", 
    "     Physical activity: differential exposure", 
    "     Physical activity: differential vulnerability"), 6),
    race = rep(c("Black", "Hispanic", "Other"), each=22),
    type = rep(c("deaths", "prop"), 3, each=11)) %>% 
  
  # Separate the 'additional deaths' and 'proportion' estimates 
  pivot_wider (names_from="type", values_from=c("mean", "2.5%", "97.5%")) %>%
  
  # reformat 
  mutate (deaths = round(mean_deaths*10000,1),
    deaths_lower = round(`2.5%_deaths`*10000,1),
    deaths_upper = round(`97.5%_deaths`*10000,1),
    prop = round(mean_prop*100,0),
    prop_lower = round(`2.5%_prop`*100,0),
    prop_upper = round(`97.5%_prop`*100,0),
    deaths_10000py_ci = paste0(deaths, " (", deaths_lower, ", ", deaths_upper, ")"),
    prop_ci = paste0(prop, " (", prop_lower, ", ", prop_upper, ")"))%>%
  dplyr::select(race, term, deaths_10000py_ci, prop_ci) 

view(CMed_men) # view results
write.csv(CMed_men, file=paste0(output, "CMed_results_men.csv")) # save results

