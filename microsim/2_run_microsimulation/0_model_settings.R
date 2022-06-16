# SIMAH project 2022 - script for setting up microsimulation model settings
library(devtools)
library(roxygen2)
library(tidyverse)
# load in R package

# press enter when prompted
install("SIMAH_code/microsimpackage", dep=T)
library(microsimpackage)

set.seed(42)

options(scipen=999)

######################EDIT ONLY BELOW HERE ##################################################

####which geography -  needs to be written as USA or full state name 
SelectedState <- "USA"

####Size of population 
PopulationSize <- 1000000

# switch on and off migration and deaths
migrationdeaths <- 1

# switch on and off education updates
updatingeducation <- 1

# switch on and off alcohol updates
updatingalcohol <- 1

####################EDIT ONLY ABOVE HERE ##################################################

# what proportion of the population does this represent
WholePopSize <- read.csv("SIMAH_workplace/microsim/1_input_data/fullpopcounts.csv") %>% 
  filter(STATE==SelectedState)

proportion <- PopulationSize/WholePopSize$total
proportion <- ifelse(proportion>1,1,proportion)

model <- "SIMAH"

#####first read in and process all the necessary data files 
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/load_files.R")

# read in migration in and out rates 
Rates <- readRDS(paste("SIMAH_workplace/microsim/1_input_data/migration_rates/final_rates",SelectedState,".RDS",sep=""))
Rates$agecat <- as.character(Rates$agecat)
# project those migration rates forwards - to get rates for 2020 onwards 
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/projecting_migration_and_deaths.R")

# load in the education transitions data
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/education_transitions.R")

# load in the alcohol transitions data 
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/alcohol_transitions.R")
