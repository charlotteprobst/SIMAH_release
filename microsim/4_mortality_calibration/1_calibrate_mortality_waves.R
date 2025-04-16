# SIMAH project 

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
library(stringr)
library(plotrix)
library(ggplot2)
options(dplyr.summarise.inform = FALSE)

# WorkingDirectory <- "U:/SIMAH"
# WorkingDirectory <- "C:/Users/laura/Documents/CAMH/SIMAH"
WorkingDirectory <- "/home/cbuckley/"
WorkingDirectory <- "/Users/charlottebuckley/Google Drive/SIMAH Sheffield"

# set wd and install the microsim and calibration packages
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)
install("SIMAH_code/calibrationpackage", dep=T)

library(microsimpackage)
library(calibrationpackage)

ScriptDirectory <- paste0(WorkingDirectory, "/SIMAH_code/microsim/4_mortality_calibration/")
OutputDirectory <- paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/mortality_calibration/random_number_sampling")

# read in all model settings
source(paste0(ScriptDirectory, "0_model_settings.R"))

# read in settings for calibration
source(paste0(ScriptDirectory,"0_calibration_settings.R"))

# load all microsim files
source(paste0(ScriptDirectory, "0_load_microsim_files.R"))

# set up samples for calibration for education transitions
source(paste0(ScriptDirectory,"0_generate_calibration_samples_mortality.R"))

# save a copy of the original base population 
baseorig <- basepop
# parallel loop that runs the calibration process 
# this loops through waves of calibration and runs all sampled settings
while(wave <= num_waves){
  Output <- list()
  Output <- foreach(i=1:nrow(sampleseeds), .inorder=TRUE) %dopar% {
    print(i)
    # set seed and sample number for current iteration
    samplenum <- as.numeric(sampleseeds$samplenum[i])
    seed <- as.numeric(sampleseeds$seed[i])
    # reset the base population to the original pop for each calibration iteration
    basepop <- baseorig 
    # change the alcohol model - based on prior calibrated models 
    alcohol_model_num <- as.numeric(sampleseeds$alcoholmodel[i])
    alcohol_transitions <- alcohol_transitionsList[[alcohol_model_num]]
    # change the education model - based on the prior calibrated models 
    education_model_num <- as.numeric(sampleseeds$educationmodel[i])
    education_transitions <- education_transitionsList[[education_model_num]]
    # change the mortality parameters being run
    mortality_parameters <- transitionsList[[samplenum]]
    # execute the simulation with each setting
    run_microsim_alt(seed,samplenum,basepop,brfss,
                     death_counts,
                     updatingeducation, education_transitions,
                     COVID_specific_tps=0,
                     migration_rates,
                     updatingalcohol, alcohol_transitions,
                     catcontmodel, drinkingdistributions,
                     base_counts, diseases, mortality_parameters, sesinteraction,
                     policy=0, percentreduction=0.1, year_policy, inflation_factors,
                     age_inflated,
                     update_base_rate,
                     minyear=2000, maxyear=2014, output="mortality")
    }

  Output <- do.call(rbind,Output)
  # save the raw output in the output directory
  write.csv(Output, paste0(OutputDirectory, "/output-",wave, ".csv"), row.names=F)

  # # # calculate and save implausibility values - age standardised but can be switched
  # at the moment this calculates uncertainty based on 5% tolerance
  # this needs to be updated based on GBD uncertainty! 
  summary <- calculate_implausibility_mortality(Output,agest=1, agestyear=2010, model_error=0.05)
  implausibility <- summary[[1]]
  write.csv(implausibility, paste0(OutputDirectory, "/implausibility-",wave, ".csv"), row.names=F)
  # 
  # output and save processed data too 
  processed_data <- summary[[2]]
  write.csv(processed_data, paste0(OutputDirectory, "/processed_data-",wave, ".csv"), row.names=F)
  
  processed_data$education <- factor(processed_data$education,
                                     levels=c("LEHS","SomeC","College"))
  # draw a plot of all the samples 
  ggplot(processed_data, aes(x=year, y=agest_simulated_mortality_rate,
                             colour=as.factor(samplenum))) + 
    geom_line() +
    geom_line(aes(x=year, y=agest_observed_mortality_rate), colour="black") + 
    facet_grid(cols=vars(sex,education), rows=vars(cause), scales="free") + 
    theme_bw() + 
    theme(legend.position="none")
  ggsave(paste0(OutputDirectory, "/plot-",wave, ".png"), dpi=300,
         width=33, height=19, units="cm")

  # # calculate the difference between the old implausibility and new implausibility 
  new_mean_implausibility <- mean(implausibility$max, na.rm=T)
  max_implausibility <- max(implausibility$max)
  # 
  if(wave>1){
    # check improvement % and stop if minimal improvement (based on improvement threshold defined in settings)
    improvement <- abs(prev_mean_implausibility - new_mean_implausibility)/prev_mean_implausibility
    if(improvement < improvement_threshold | max_implausibility < 1) {
      break
    }
  }
  # 
  # # keep top 15% of samples 
  implausibility <- implausibility %>% ungroup() %>% 
    mutate(percentile = ntile(max, 100))
  # select top 15% of samples to keep
  topsamples <- unique(subset(implausibility, percentile<=15)$samplenum)
# sample new settings for mortality parameters based on best samples
  mortality_parameters <- resample_mortality_lhs(transitionsList, topsamples, nsamples)
  
  transitionsList <- list()
  for(i in unique(lhs$sample)){
    transitionsList[[paste(i)]] <- lhs %>% filter(sample==i) %>%
      ungroup() %>%
      dplyr::select(-sample)
}
  prev_mean_implausibility <- new_mean_implausibility
  wave <- wave + 1
  
  # save new mortality parameters for next wave 
  write.csv(mortality_parameters, paste0(OutputDirectory, "/lhs_mortality-", wave,".csv"))
}

