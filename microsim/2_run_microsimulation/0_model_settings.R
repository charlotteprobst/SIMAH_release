# SIMAH project 2022 - script for setting up microsimulation model settings
library(devtools)
library(roxygen2)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)

set.seed(42)

options(scipen=999)

# press enter if prompted
install("SIMAH_code/microsimpackage")
library(microsimpackage)

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

# switch between modelling mortality and morbidity (mortality = 1)
mortality <- 1

# switch on or off liver cirrhosis modelling
cirrhosis <- 0

####################EDIT ONLY ABOVE HERE ##################################################

# what proportion of the population does this represent
WholePopSize <- read.csv("SIMAH_workplace/microsim/1_input_data/fullpopcounts.csv") %>% 
  filter(STATE==SelectedState)

proportion <- PopulationSize/WholePopSize$total
proportion <- ifelse(proportion>1,1,proportion)

model <- "SIMAH"

#####first read in and process all the necessary data files 

# read in base population
if(model=="SIMAH"){
  basepop <- read_csv(paste("SIMAH_workplace/microsim/1_input_data/agent_files/", SelectedState, "basepop", PopulationSize, ".csv", sep=""),
                      show_col_types = FALSE)
}else if(model=="CASCADE"){
  basepop <- read_csv(paste("SIMAH_workplace/microsim/1_input_data/agent_files/", SelectedState, "basepopCASCADE", PopulationSize, ".csv", sep=""))
}

# read in BRFSS data for migrants and 18-year-olds entering the model
brfss <- load_brfss(model,SelectedState)

# read in death rates data
death_rates <- load_death_rates(model, SelectedState)

# read in migration in and out rates and optionally project forwards to 2025
migration_rates <- load_migration_rates(SelectedState)

# load in the education transition rates
list <- load_education_transitions(SelectedState, basepop, brfss)
education_transitions <- list[[1]]
basepop <- list[[2]]
brfss <- list[[3]]
rm(list)
# load in alcohol transition rates
list <- load_alcohol_transitions(basepop, brfss)
alcohol_transitions <- list[[1]]
basepop <- list[[2]]
brfss <- list[[3]]
rm(list)

# save a copy of original population files
baseorig <- basepop

# set microsim individuals IDs 
microsim.init.id <- 1:nrow(basepop)
basepop <- cbind(microsim.init.id, basepop)
