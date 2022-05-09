#####Wrapper code for dynamic microsimulation
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
suppressPackageStartupMessages(library("dplyr"))
library(dplyr)
library(knitr)
library(ipfp)
library(tidyr)
library(janitor)
library(stringr)
library(reshape2)
library(pbapply)
library(ggplot2)
library(gridExtra)
library(readr)
library(readxl)
library(parallel)
library(foreach)
library(faux)
library(splitstackshape)
library(fitdistrplus)
library(lhs)
library(doParallel)
options(scipen=999)
# set seed for reproducibility - IMPORTANT - DO NOT CHANGE
# note - this also needs to be ran straight after R has been opened
set.seed(42)

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
# WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee
SelectedState <- "USA"

####Size of population 
PopulationSize <- 200000

# run model for CASCADE (1984 start) or SIMAH (2000 start)?
model <- "CASCADE"

# what proportion of the population does this represent - change to ifelse with all pop sizes when other states added 
if(model=="SIMAH"){
  WholePopSize <- read.csv("SIMAH_workplace/microsim/1_input_data/fullpopcounts.csv") %>% 
    filter(STATE==SelectedState)
}else if(model=="CASCADE"){
  WholePopSize <- read.csv("SIMAH_workplace/microsim/1_generating_population/constraintsUSA.csv") %>% 
    dplyr::select(marriedF:unmarriedM) %>% mutate(total=marriedF+unmarriedF+marriedM+unmarriedM)
}

proportion <- PopulationSize/WholePopSize$total
proportion <- ifelse(proportion>1,1,proportion)

# switch to 1 when adjusting migration scripts
adjusting <- 1

# switching between mortality and morbidity models
mortality <- 1

#####first read in and process all the necessary data files 
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/CASCADE_load_files.R")
# load in the education transitions data
# source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/education_transitions.R")

# load all functions for running the microsimulation - death rates, migration, transition education
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/apply_death_rates.R")
if(model=="CASCADE"){
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/CASCADE_apply_death_rates.R")
}
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/outward_migration.R")
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/inward_migration.R")
if(model=="CASCADE"){
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/CASCADE_inward_migration.R")
}
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/education_setup.R")
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/transition_ed.R")

# load the function for running the simulation
if(model=="SIMAH"){
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/simulation.R")
}else if(model=="CASCADE"){
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/CASCADE_simulation.R")  
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/HistoryFunction.R")
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/formerdrinkers_history.R")
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/cirrhosis_functions.R")
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/assign_hepatitis.R")
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/updating_alcohol.R")
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/updating_BMI.R")

}

# switch on and off migration and deaths
migrationdeaths <- 1

# switch on and off education updates
updatingeducation <- 0
# switch on and off alcohol updates
updatingalcohol <- 0

Rates <- readRDS(paste("SIMAH_workplace/microsim/1_input_data/migration_rates/CASCADEfinal_rates",SelectedState,".RDS",sep=""))
Rates$agecat <- as.character(Rates$agecat)
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/projecting_migration_and_deaths.R")

agest <- 1
N_SAMPLES <- 100
PE <- 0
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/sample_parameters_top.R")
N_REPS <- 2
if(PE==1){
  N_SAMPLES <- 1
}
# run top 5% versions of the age specific and age standardized version
# top <- read.csv("SIMAH_workplace/microsim/2_output_data/calibration_output_agest/implausibility_wave15.csv") %>% 
#   mutate(percentile=ntile(maximplausibility,500)) %>% 
#   filter(percentile<=1)
# lhs <- read.csv("SIMAH_workplace/microsim/2_output_data/calibration_output_agest/lhsSamples_wave15.csv") %>% 
#   filter(SampleNum %in% top$samplenum)
# 
# lhs <- read.csv("SIMAH_workplace/microsim/2_output_data/calibration_output_agest/lhsSamples_wave15.csv") %>% 
#   pivot_longer(BETA_MALE_MORTALITY:DECAY_SPEED) %>% 
#   group_by(name) %>% 
#   summarise(value=mean(value)) %>% 
#   pivot_wider(names_from=name, values_from=value)

# lhsSample <- list()
# for(i in 1:nrow(lhs)){
#   lhsSample[[paste(i)]] <- lhs[i,]
# }
# 
# N_SAMPLES <- 1
sampleseeds <- expand.grid(seed=1:N_REPS, SampleNum=1:N_SAMPLES)
sampleseeds$seed <- sample(1:nrow(sampleseeds), nrow(sampleseeds), replace=F)

baseorig <- basepop

# adjust parallel settings
registerDoParallel(18)
# registerDoSNOW(c1)
# plan(multicore, workers=24)
options(future.rng.onMisuse="ignore")
options(future.globals.maxSize = 10000 * 1024^3)
options(future.fork.multithreading.enable = FALSE)

Cirrhosis <- foreach(i=1:nrow(sampleseeds), .inorder=FALSE,
                     .packages=c("dplyr","tidyr","foreach")) %dopar% {
                       samplenum <- as.numeric(sampleseeds$SampleNum[i])
                       seed <- as.numeric(sampleseeds$seed[i])
                       print(i)
                       set.seed(as.numeric(Sys.time()))
                       basepop <- baseorig
                       selectedlhs <- lhsSample[[samplenum]]
                       history <- HistoryFunction(basepop, ages, selectedlhs)
                       basepop <- left_join(basepop, history)
                       basepop <- formerdrinkers_history(basepop,selectedlhs)
                       basepop <- basepop %>% 
                         mutate(Cirrhosis_risk = ifelse(formerdrinker==0 & microsim.init.sex=="m" & 
                                                          grams_10years>= as.numeric(lhsSample[[samplenum]]["THRESHOLD"]), 1,
                                                        ifelse(formerdrinker==0 & microsim.init.sex=="f" & 
                                                                 grams_10years>=as.numeric(lhsSample[[samplenum]]["THRESHOLD"])*
                                                                 as.numeric(lhsSample[[samplenum]]["THRESHOLD_MODIFIER"]), 1, 
                                                               ifelse(formerdrinker==1, Cirrhosis_risk, 0))),
                                grams_10years = ifelse(formerdrinker==1, former_history,
                                                       grams_10years)) %>% dplyr::select(-former_history)
                       run_microsim(seed,samplenum,lhsSample[[samplenum]], basepop, deathrates, apply_death_rates,
                                    outward_migration, inward_migration, mortality,
                                    AssignAcuteHep, AssignChronicHep, CirrhosisHeavyUse, CirrhosisHepatitis, 
                                    MetabolicPathway,
                                    brfss,Rates, 1984, 2016)
                     }

saveRDS(Cirrhosis, "SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agest_test.RDS")
