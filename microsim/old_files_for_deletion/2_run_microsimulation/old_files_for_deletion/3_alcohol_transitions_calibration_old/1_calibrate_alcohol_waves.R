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
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")
OutputDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/alcohol_calibration/firstattempt")
dir.create(OutputDirectory)

# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)
# install("SIMAH_code/calibratemicrosimpackage", dep=T)

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
nsamples <- 10
source("SIMAH_code/microsim/2_run_microsimulation/alcohol_transitions_calibration/extract_uncertainty.R")
# rm(model)

# save samples 
saveRDS(transitionsList, paste0(OutputDirectory, "/transitionsList-1",".RDS"))
write.csv(estimates, paste0(OutputDirectory, "/sampled_markov-1", ".csv"), row.names=F)

# set to 1 if running on local machine 
registerDoParallel(10)
# registerDoSNOW(c1)
# plan(multicore, workers=24)
options(future.rng.onMisuse="ignore")
options(future.globals.maxSize = 10000 * 1024^3)
options(future.fork.multithreading.enable = FALSE)

sampleseeds <- expand.grid(samplenum = 1:length(transitionsList), seed=1:2)
sampleseeds$seed <- sample(1:nrow(sampleseeds), nrow(sampleseeds), replace=T)
# sampleseeds <- sampleseeds %>% filter(samplenum<=2)

num_waves <- 15

improvement_threshold <- 0.005

source("SIMAH_code/microsim/2_run_microsimulation/2_postprocessing_scripts/postprocess_alcohol.R")


targets <- read.csv("SIMAH_workplace/microsim/2_output_data/education_calibration/education_targets.csv") %>% 
  group_by(YEAR, AGECAT, RACE, SEX) %>% 
  mutate(target=TPop/sum(TPop),
         SE=sqrt(target*(1-target)/sum(OrigSample)),
         variance = (SE^2) * OrigSample) %>% 
  dplyr::select(-c(TPop:OrigSample))

prev_mean_implausibility <- 100

source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/calculate_implausibility_education.R")
wave <- 1
rm(education_transitions)

while(wave <= num_waves){
  baseorig <- basepop
  updatingalcohol <- 0
  Output <- list()
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
                     minyear=2000, maxyear=2014, output="demographics")}
  
  Output <- do.call(rbind,Output)
  write.csv(Output, paste0(OutputDirectory, "/output-",wave, ".csv"), row.names=F)
  
  # calculate and save implausibility values 
  implausibility <- calculate_implausibility_education(Output, targets)
  write.csv(implausibility, paste0(OutputDirectory, "/implausibility-",wave, ".csv"), row.names=F)
  
  new_mean_implausibility <- mean(implausibility$implausibility)
  max_implausibility <- max(implausibility$implausibility)
  
  if(wave>1){
    # check improvement % and stop if minimal improvement
    improvement <- abs(prev_mean_implausibility - new_mean_implausibility)/prev_mean_implausibility
    if(improvement < improvement_threshold | max_implausibility < 1) {
      break
    }
  }
  
  topsamples <- unique(subset(implausibility, percentile<=15)$samplenum)
  # now subset the model estimates based on those
  newestimates <- estimates %>% filter(SampleNum %in% topsamples)
  
  source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/resample_markov.R")

  prev_mean_implausibility <- new_mean_implausibility
  wave <- wave + 1
  
  # save the new TPs and the estimates
  saveRDS(transitionsList, paste0(OutputDirectory, "/transitionsList-",wave,".RDS",sep=""))
  write.csv(estimates, paste0(OutputDirectory, "/sampled_markov-",wave, ".csv"), row.names=F)
  
  
}

