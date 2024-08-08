# SIMAH project August 2024 

# code for validation of calibrated model outputs

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
OutputDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration/")
# dir.create(OutputDirectory)

# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)
install("SIMAH_code/calibrationpackage", dep=T)

library(microsimpackage)
library(calibrationpackage)


####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee
States <- c("California","Colorado","Florida","Indiana",
            "Louisiana","Massachusetts","Michigan","Minnesota",
            "Missouri","New York", "Oregon", "Pennsylvania",
            "Tennessee","Texas","USA")

# for(s in States){
SelectedState <- "USA"

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")
lhs <- lhs[[1]]

# read in the education transitions from the final wave of calibration
transitionsList <- read_rds(paste0(OutputDirectory, "/transitionsList-10",".RDS"))

# set to 1 if running on local machine 
registerDoParallel(15)
# registerDoSNOW(c1)
# plan(multicore, workers=24)
options(future.rng.onMisuse="ignore")
options(future.globals.maxSize = 10000 * 1024^3)
options(future.fork.multithreading.enable = FALSE)

sampleseeds <- expand.grid(samplenum = 1:length(transitionsList), seeds=1:2)
sampleseeds$seed <- sample(1:nrow(sampleseeds), nrow(sampleseeds), replace=T)
# sampleseeds <- sampleseeds %>% filter(samplenum<=2)

targets <- read.csv("SIMAH_workplace/microsim/2_output_data/education_calibration/education_targets.csv") %>% 
  group_by(YEAR, AGECAT, RACE, SEX) %>% 
  mutate(target=TPop/sum(TPop),
         SE=sqrt(target*(1-target)/sum(OrigSample))) %>% 
  dplyr::select(-c(TPop:OrigSample))

# source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/calculate_implausibility_education.R")
rm(education_transitions)

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
                     minyear=2000, maxyear=2019, output="demographics")}
  
Output <- do.call(rbind,Output)
write.csv(Output, paste0(OutputDirectory, "/validation_output", ".csv"), row.names=F)
  
# calculate and save implausibility values 
implausibility <- calculate_implausibility_education(Output, targets)
write.csv(implausibility, paste0(OutputDirectory, "/validation_implausibility", ".csv"), row.names=F)