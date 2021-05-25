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
options(scipen=999)
# set seed for reproducibility - IMPORTANT - DO NOT CHANGE
# note - this also needs to be ran straight after R has been opened
set.seed(42)

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/Microsimulation/Microsim_code/Microsimulation"
# WorkingDirectory <- "/home/cbuckley/SIMAH/Microsimulation"
setwd(paste(WorkingDirectory))

####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee
SelectedState <- "Colorado"

####Size of population 
PopulationSize <- 3500000

# what proportion of the population does this represent - change to ifelse with all pop sizes when other states added 
WholePopSize <- ifelse(SelectedState=="USA", 199659972,
                       ifelse(SelectedState=="California", 24496093,
                              ifelse(SelectedState=="Colorado", 3186525,
                              ifelse(SelectedState=="Florida",12218475,
                                     ifelse(SelectedState=="Indiana",4475625,
                                            ifelse(SelectedState=="Kentucky",3028260,
                                                   ifelse(SelectedState=="Louisiana",3227122,
                                                          ifelse(SelectedState=="Massachusetts",4812276,
         ifelse(SelectedState=="Michigan", 7293411,
                ifelse(SelectedState=="Minnesota",3605476,
                       ifelse(SelectedState=="Missouri",4135266,
                              ifelse(SelectedState=="New York", 14191275,
                                     ifelse(SelectedState=="Oregon", 2556185,
                                            ifelse(SelectedState=="Pennsylvania", 9270291,
                                                   ifelse(SelectedState=="Tennessee", 4263045,
                                                          ifelse(SelectedState=="Texas", 14889072))))))))
                                                          ))))))))

proportion <- PopulationSize/WholePopSize
proportion <- ifelse(proportion>1,1,proportion)

#####first read in and process all the necessary data files 
source("1_preprocessing_scripts/load_files.R")
# load in the education transitions data
source("1_preprocessing_scripts/education_transitions.R")

# load all functions for running the microsimulation - death rates, migration, transition education
source("1_functions/apply_death_rates.R")
source("1_functions/outward_migration.R")
source("1_functions/education_setup.R")
source("1_functions/transition_ed.R")

# load the function for running the simulation
source("1_functions/simulation.R")

# switch on and off migration and deaths
migrationdeaths <- 1

# switch on and off education updates
updatingeducation <- 1

# # function for running simulation - one run
Output <- list()
Output <- run_microsim(basepop, migrants, deathrates, apply_death_rates,
                       updatingeducation, education_setup, transitionroles,
                       transitions, PopPerYear, 2000, 2018)

PopPerYear <- Output[[1]]
DeathSummary <- Output[[2]]

saveRDS(Output, "3_output_data/PopSummaryKentucky.RDS")


# select number of samples and set up parameter settings for calibration
nsamples <- 300
source("1_preprocessing_scripts/sampling_parameters.R")

# function for running simulation - loop through different TP values
# Summary <- list()
# Output <- list()
# for(i in 1:length(TPList)){
#   print(paste("Sample = ", i))
#   transitions <- TPList[[i]]
#   Output[[paste(i)]] <- run_microsim(basepop, migrants, deathrates, apply_death_rates,
#                                     updatingeducation, education_setup, transitionroles, 
#                                     transitions, DeathSummary, Summary, PopPerYear, 2000, 2018)
#   Output[[paste(i)]]$Sample <- i
# }

# save the output data from the simulation
