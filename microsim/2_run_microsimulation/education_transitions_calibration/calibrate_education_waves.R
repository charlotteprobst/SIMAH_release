# SIMAH project November 2021 

# code for calibration of MSM model parameters to state-level education outputs 

# first set up for microsimulation 
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

options(dplyr.summarise.inform = FALSE)
# set seed for reproducibility - IMPORTANT - DO NOT CHANGE
# note - this also needs to be ran straight after R has been opened
set.seed(42)

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
# WorkingDirectory <- "U:/SIMAH/"
# WorkingDirectory <- "C:/Users/laura/Documents/CAMH/SIMAH/"
# WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
WorkingDirectory <- "/home/cbuckley/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")

# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)

####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee
States <- c("California","Colorado","Florida","Indiana",
            "Louisiana","Massachusetts","Michigan","Minnesota",
            "Missouri","New York", "Oregon", "Pennsylvania",
            "Tennessee","Texas","USA")

# for(s in States){
SelectedState <- "USA"

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")
lhs <- lhs[[1]]

# now sample parameters for the education transitions
nsamples <- 300
source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/extract_uncertainty.R")
rm(model)

# save samples 
saveRDS(transitionsList, paste("SIMAH_workplace/microsim/2_output_data/education_calibration/transitionsList", SelectedState,".RDS",sep=""))
saveRDS(estimates, paste("SIMAH_workplace/microsim/2_output_data/education_calibration/sampled_markov", SelectedState, ".RDS"))

# set to 1 if running on local machine 
registerDoParallel(10)
# registerDoSNOW(c1)
# plan(multicore, workers=24)
options(future.rng.onMisuse="ignore")
options(future.globals.maxSize = 10000 * 1024^3)
options(future.fork.multithreading.enable = FALSE)
Output <- list()

sampleseeds <- expand.grid(samplenum = 1:length(transitionsList), seeds=1:2)
# sampleseeds <- sampleseeds %>% filter(samplenum<=2)

rm(education_transitions)
baseorig <- basepop
updatingalcohol <- 0
Output <- foreach(i=1:nrow(sampleseeds), .inorder=TRUE, .combine=rbind) %dopar% {
  print(i)
  samplenum <- as.numeric(sampleseeds$samplenum[i])
  seed <- as.numeric(sampleseeds$seed[i])
  basepop <- baseorig
  education_transitions <- transitionsList[[samplenum]]
  run_microsim_alt(seed=seed,samplenum=samplenum,basepop,brfss,
                   death_counts,
                   updatingeducation, education_transitions,
                   migration_rates,
                   updatingalcohol=0, alcohol_transitions,
                   catcontmodel, Hep, drinkingdistributions,
                   base_counts, diseases, lhs, liverinteraction,
                   policy=0, percentreduction=0.1, year_policy, inflation_factors,
                   age_inflated,
                   update_base_rate,
                   minyear=2000, maxyear=2019, output="demographics")
}

# save the output 
Output <- do.call(rbind,Output)
write.csv(Output, "SIMAH_workplace/microsim/2_output_data/education_calibration/prior_range_inflated_allyears.csv", row.names=F)

