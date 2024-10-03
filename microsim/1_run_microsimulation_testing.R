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
# WorkingDirectory <- "C:/Users/cmp21seb/Documents/SIMAH/"
WorkingDirectory <- "/Users/carolinkilian/Desktop/"
# WorkingDirectory <- "/imaging/home/Imhpr/ckilian/"

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
nreps <- 10 # indicates number of different sample seeds

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
sampleseeds <- sampleseeds %>% expand(sampleseeds, policy_setting)
counterfactual <- sampleseeds %>% 
  group_by(samplenum, seed, educationmodel, alcoholmodel) %>% 
  slice(1) %>% mutate(policymodel = 0, scenario = "0,0,0", setting = "counterfactual")
sampleseeds <- rbind(sampleseeds, counterfactual)

sampleseeds <- sampleseeds %>% filter(setting == "standard" | setting == "counterfactual") 

# pick a random education / alcohol model to use (for testing purposes)
# this picks a random model from the calibrated education / alcohol models
# samplenum <- sample(1:300, 1, replace=F)

# education_transitions <- education_transitions[[samplenum]]
# alcohol_transitions <- alcohol_transitions %>% filter(sample==samplenum)

# read in the categorical to continuous distributions
catcontmodel <- read.csv("SIMAH_workplace/microsim/2_output_data/alcohol_calibration/calibration_continuous_distribution.csv")

output_type <- "alcoholcontcat"

# set minyear and maxyear 
minyear <- 2000
maxyear <- 2019
year_policy <- 2015

diseases <- NULL

#sampleseeds <- read.csv(paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/2024-09-12/output-policy_sampleseeds_2024-09-12.csv"))
#sampleseeds <- sampleseeds %>% filter(setting == "mup")

Output <- list()
baseorig <- basepop
Output <- foreach(i=1:nrow(sampleseeds), .inorder=TRUE) %do% {
  print(i)
  # set seed and sample number for current iteration
  samplenum <- as.numeric(sampleseeds$samplenum[i])
  seed <- as.numeric(sampleseeds$seed[i])
  #set up policy parameters
  policymodel <- sampleseeds$policymodel[i]
  scenario <- as.numeric(unlist(strsplit(sampleseeds$scenario[i], ",")))
  setting <- sampleseeds$setting[i]
  participation <- as.numeric(sampleseeds$participation[i])
  cons_elasticity <- as.numeric(unlist(strsplit(sampleseeds$cons_elasticity[i], ",")))
  cons_elasticity_se <- as.numeric(unlist(strsplit(sampleseeds$cons_elasticity_se[i], ",")))
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

Output <- do.call(rbind,Output)
# save the output in the output directory
write.csv(Output, paste0(OutputDirectory, "/output-policy_alcoholcontcat_", Sys.Date(), ".csv"), row.names=F)
write.csv(sampleseeds, paste0(OutputDirectory, "/output-policy_sampleseeds_", Sys.Date(), ".csv"), row.names=F)

plot <- summarise_alcohol_policy(Output, SelectedState = "USA", out = "main")
ggsave(paste0(OutputDirectory, "/plot-policy_meangpd_", Sys.Date(), ".png"), plot[[3]], width = 14, height = 8)
ggsave(paste0(OutputDirectory, "/plot-policy_diffgpd_", Sys.Date(), ".png"), plot[[4]], width = 14, height = 8)
ggsave(paste0(OutputDirectory, "/plot-policy_percgpd_", Sys.Date(), ".png"), plot[[5]], width = 14, height = 8)
