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

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/SIMAH/Microsimulation"
setwd(paste(WorkingDirectory))

####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee
SelectedState <- "USA"

####Size of population 
PopulationSize <- 1000000

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

#####first read in and process all the necessary data files 
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/load_files.R")
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
}

# do baseline rates need to be calculated? 1 IF this is the first time a state is being ran
baselinerates <- 1

# switch on and off migration and deaths
migrationdeaths <- 1

# switch on and off education updates
updatingeducation <- 0

# calculating baseline rates
# this runs the microsimulation once and extracts the migration in and out rates
if(baselinerates == 1){
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/simulation_baserates.R")
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/CASCADE_simulation_baserates.R")
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/calculate_migration_rates.R")
Baseline_Rates <- list()
Baseline_Rates <- run_microsim_baserates(1,basepop, outwardmigrants, inwardmigrants, deathrates, apply_death_rates,
                               updatingeducation, education_setup, transitionroles,
                               calculate_migration_rates, outward_migration, inward_migration, 
                               brfss,
                               transitions, PopPerYear, 1984, 2016)
Baseline_Rates <- do.call(rbind,Baseline_Rates)
saveRDS(Baseline_Rates, paste("SIMAH_workplace/microsim/1_input_data/migration_rates/CASCADEbaseline_rates", SelectedState, ".RDS", sep=""))
}

Rates <- readRDS(paste("SIMAH_workplace/microsim/1_input_data/migration_rates/CASCADEbaseline_rates",SelectedState,".RDS",sep=""))

source("SIMAH_code/microsim/2_run_microsimulation/1_functions/CASCADE_simulation.R")

# now run using these rates to get population totals by category in each year of the simulation
Output <- list()
Output <- run_microsim(1,1,basepop, outwardmigrants, inwardmigrants, deathrates, apply_death_rates,
                       updatingeducation, education_setup, transitionroles,
                       calculate_migration_rates, outward_migration, inward_migration, 
                       brfss,Rates,transitions,
                       transitions, PopPerYear, 1984, 2016)

source("SIMAH_code/microsim/2_run_microsimulation/1_functions/compare_output_target.R")

for(i in 1984:2016){
  # now join the error up with the rates 
  compare <- compare_output_target(Output)
  
  Rates <- left_join(Rates, compare)
  
  print(i)
  
  # now adjust the rates to the observed population counts from each year
  Rates <- Rates %>% mutate(MigrationInNADJ = ifelse(Year==i, MigrationInN + diffscaled, MigrationInN),
                            MigrationOutNADJ = ifelse(Year==i & MigrationInNADJ<0, MigrationOutN + abs(MigrationInNADJ),
                                                      MigrationOutN),
                            MigrationInNADJ = ifelse(MigrationInNADJ<=0,0,MigrationInNADJ)) %>% 
    dplyr::select(Year, agecat, microsim.init.sex, microsim.init.race, MigrationInNADJ, MigrationOutNADJ) %>% 
    rename(MigrationInN = MigrationInNADJ, MigrationOutN = MigrationOutNADJ)
  
  # now re-run the simulation to get population totals
  Output <- list()
  if(i<2016){
  Output <- run_microsim(1,1,basepop, outwardmigrants, inwardmigrants, deathrates, apply_death_rates,
                         updatingeducation, education_setup, transitionroles,
                         calculate_migration_rates, outward_migration, inward_migration, 
                         brfss,Rates,transitions,
                         transitions, PopPerYear, 1984, i+1)
  }else if(i==2016){
  Output <- run_microsim(1,1,basepop, outwardmigrants, inwardmigrants, deathrates, apply_death_rates,
                         updatingeducation, education_setup, transitionroles,
                         calculate_migration_rates, outward_migration, inward_migration, 
                         brfss,Rates,transitions,
                         transitions, PopPerYear, 1984, 2016)
  }
  if(i==2016){
    saveRDS(Rates, paste("SIMAH_workplace/microsim/1_input_data/migration_rates/CASCADEfinal_rates",SelectedState,".RDS",sep=""))
  }
}
