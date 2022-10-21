# SIMAH project 2022 - script for setting up microsimulation model settings
library(dplyr)
library(readr)
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

# switch between modelling mortality and morbidity (mortality = 1)
mortality <- 1

# switch between mortality causes
disease <- "cirrhosis"

# switch between CASCADE and SIMAH models 
model <- "SIMAH"

# output (which version of the output is required) options are "education" "alcohol" or "mortality"
output_type <- "alcohol"

# do you want policy effects switched on? at the moment this is binary but 
# as the simulation develops there will be more options for policy scenarios
# default value is 0
policy <- 0
# percentage to reduce alcohol consumption by -> this is overall for the population
# as the simulation develops this will take a more complex parameter indicating changes in consumption in different groups
percentreduction <- 0.1

####################EDIT ONLY ABOVE HERE ##################################################

# what proportion of the population does this represent
WholePopSize <- read.csv(paste0(DataDirectory,"fullpopcounts.csv")) %>% 
  filter(STATE==SelectedState)

proportion <- PopulationSize/WholePopSize$total
proportion <- ifelse(proportion>1,1,proportion)


#####first read in and process all the necessary data files 

# read in base population
if(model=="SIMAH"){
  basepop <- read_csv(paste0(DataDirectory, "agent_files/", SelectedState, "basepop", PopulationSize, ".csv"),
                      show_col_types = FALSE)
}else if(model=="CASCADE"){
  basepop <- read_csv(paste0(WorkingDirectory, SelectedState, "basepopCASCADE", PopulationSize, ".csv"))
}

# save a copy of original population files
baseorig <- basepop

# set microsim individuals IDs 
microsim.init.id <- 1:nrow(basepop)
basepop <- cbind(microsim.init.id, basepop)

# read in BRFSS data for migrants and 18-year-olds entering the model
brfss <- load_brfss(model,SelectedState, DataDirectory)

# read in death rates data
death_rates <- load_death_rates(model, proportion, SelectedState, DataDirectory)

# read in migration in and out rates and project rates forwards to 2025 (in case needed)
migration_rates <- load_migration_rates(SelectedState, DataDirectory)

# load in the education transition rates
list <- load_education_transitions(SelectedState, basepop, brfss, DataDirectory)
education_transitions <- list[[1]]
basepop <- list[[2]]
brfss <- list[[3]]
rm(list)
# load in alcohol transition rates
list <- load_alcohol_transitions(SelectedState, basepop, brfss, DataDirectory)
alcohol_transitions <- list[[1]]
basepop <- list[[2]]
brfss <- list[[3]]
rm(list)

# load in categorical to continuous model 
catcontmodel <- read_rds(paste0(DataDirectory, "SSCatContModel.RDS"))

