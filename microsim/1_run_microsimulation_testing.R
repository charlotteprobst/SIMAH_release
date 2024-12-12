#####SIMAH project 2024 - script for running SIMAH microsimulation model
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

###set working directory to the main "SIMAH" folder in your directory 
# WorkingDirectory <- "U:/SIMAH/"
# WorkingDirectory <- "C:/Users/laura/Documents/CAMH/SIMAH/"
# WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "C:/Users/marie/Dropbox/NIH2020/"
WorkingDirectory <- "/Users/carolinkilian/Desktop/"
# WorkingDirectory <- "C:/Users/cmp21seb/Documents/SIMAH/"

DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")

# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)
install("SIMAH_code/calibrationpackage", dep=T)

library(microsimpackage)
library(calibrationpackage)

# load model settings 
# file.remove("/Users/carolinkilian/Desktop/SIMAH_workplace/microsim/2_output_data/testing")
source("SIMAH_code/microsim/0_model_settings.R")
source("SIMAH_code/microsim/0_policy_settings.R")

# load microsim files
source("SIMAH_code/microsim/0_load_microsim_files.R")

# set up the number of samples to be run
nsamples <- 1 # indicates samples per same sample seed (for calibration purposes only)
nreps <- 1 # indicates number of different sample seeds
n_uncertainty <- 100

# generate list of samples to be run with random number seeds
sampleseeds <- expand.grid(samplenum = 1:nsamples, seed=1:nreps)
sampleseeds$seed <- sample(1:3000, nrow(sampleseeds), replace=F)

# read in calibrated education transitions
education_transitionsList <- read_rds(paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration", "/transitionsList-10",".RDS"))
for(i in 1:length(education_transitionsList)){
  education_transitionsList[[i]]$cat <- gsub("1999-2019+_","",education_transitionsList[[i]]$cat)
}

# add the final education model to be run to the sampleseeds file 
edmodels <- list()
for(i in 1:ceiling(nrow(sampleseeds)/length(education_transitionsList))){
  edmodels[[paste(i)]] <- data.frame(education_model = sample(1:length(education_transitionsList), replace=F))
}
edmodels <- edmodels %>% bind_rows()

sampleseeds$educationmodel <- edmodels$education_model[1:nrow(sampleseeds)]

# read in calibrated alcohol transitions 
alcohol_transitions <- read_csv(paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/alcohol_calibration/lhs_regression-4.csv"), show_col_types = FALSE)
alcohol_transitionsList <- list()
for(i in 1:max(alcohol_transitions$sample)){
  alcohol_transitionsList[[i]] <- alcohol_transitions %>% filter(sample==i)}

# add the final alcohol model to be run to the sampleseeds file 
alcmodels <- list()
for(i in 1:ceiling(nrow(sampleseeds)/length(alcohol_transitions))){
  alcmodels[[paste(i)]] <- data.frame(alcohol_model = sample(1:length(alcohol_transitions), replace=F))
}
alcmodels <- alcmodels %>% bind_rows()

sampleseeds$alcoholmodel <- alcmodels$alcohol_model[1:nrow(sampleseeds)]

# set up scenarios and policy settings
sampleseeds <- sampleseeds %>% expand(sampleseeds, policy_setting, scenarios)

# sample policy parameters here based on sampleseeds groups by samplenum, seed, edu/alcmodel
sampleseeds <- sample_policy_parameters(sampleseeds, n_uncertainty)

# get counterfactual scenario
counterfactual <- sampleseeds %>% 
  group_by(samplenum, seed, educationmodel, alcoholmodel) %>% 
  slice(1) %>% mutate(policymodel = 0, scenario = "0,0,0", setting = "counterfactual")
sampleseeds <- rbind(sampleseeds, counterfactual)

# FOR NOW: limit to standard model and only one policy model
sampleseeds <- sampleseeds %>% filter(setting == "standard") %>%
  filter(policymodel == 0 | policymodel == 4)

# pick a random education / alcohol model to use (for testing purposes)
# this picks a random model from the calibrated education / alcohol models
# samplenum <- sample(1:300, 1, replace=F)

# education_transitions <- education_transitions[[samplenum]]
# alcohol_transitions <- alcohol_transitions %>% filter(sample==samplenum)

# read in the categorical to continuous distributions
catcontmodel <- read.csv("SIMAH_workplace/microsim/2_output_data/alcohol_calibration/calibration_continuous_distribution.csv")

output_type <- "percentreduction"

# set minyear and maxyear 
minyear <- 2000
maxyear <- 2001
year_policy <- 2001

diseases <- NULL

#sampleseeds <- read.csv(paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/2024-10-05/output-policy_sampleseeds_2024-10-05.csv"))

Output <- list()
baseorig <- basepop
Output <- foreach(i=1:nrow(sampleseeds), .inorder=TRUE) %do% {
  print(i)
  # set seed and sample number for current iteration
  samplenum <- as.numeric(sampleseeds$samplenum[i])
  seed <- as.numeric(sampleseeds$seed[i])
  nunc <- as.numeric(sampleseeds$nunc[i])
  #set up policy parameters
  policymodel <- sampleseeds$policymodel[i]
  scenario <- as.numeric(unlist(strsplit(sampleseeds$scenario[i], ",")))
  setting <- sampleseeds$setting[i]
  participation <- as.numeric(sampleseeds$participation[i])
  cons_elasticity <- as.numeric(unlist(strsplit(sampleseeds$cons_elasticity[i], ",")))
  cons_elasticity_se <- as.numeric(unlist(strsplit(sampleseeds$cons_elasticity_se[i], ",")))
  #cons_elasticity <- sample_policy_parameters(sampleseeds[i,], n_uncertainty)
  part_elasticity <- as.numeric(sampleseeds$part_elasticity[i])
  r_sim_obs <- as.numeric(sampleseeds$r_sim_obs[i])
  # reset the base population to the original pop for each calibration iteration
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
                   minyear=minyear, maxyear=maxyear, output=output_type)
}
beep()

Sys.Date <- "2024-11-14"
Output <- do.call(rbind,Output) %>% 
  group_by(nunc) %>% sample_frac(0.001)
# save the output in the output directory
write.csv(Output, paste0(OutputDirectory, "/output-policy_alcoholcontcat_", Sys.Date, ".csv"), row.names=F)
write.csv(sampleseeds, paste0(OutputDirectory, "/output-policy_sampleseeds_", Sys.Date, ".csv"), row.names=F)

plot <- summarise_alcohol_policy(Output, SelectedState = "USA", version = "standard")
write.csv(plot[[1]], paste0(OutputDirectory, "/tab-policy_alccat_", Sys.Date, ".csv"))
write.csv(plot[[2]], paste0(OutputDirectory, "/tab-policy_alccat_", Sys.Date, ".csv"))
ggsave(paste0(OutputDirectory, "/plot-policy_gpd_", Sys.Date, ".png"), plot[[3]], width = 14, height = 8)
ggsave(paste0(OutputDirectory, "/plot-policy_diffgpd_", Sys.Date, ".png"), plot[[4]], width = 14, height = 18)
ggsave(paste0(OutputDirectory, "/plot-policy_percgpd_", Sys.Date, ".png"), plot[[5]], width = 14, height = 18)
