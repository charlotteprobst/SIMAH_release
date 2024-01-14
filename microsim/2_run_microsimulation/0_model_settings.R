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
# write in the same format as the death rates file 
# "LVDC"  "HLVDC" "DM"    "IHD"   "ISTR"  "HYPHD"
# "AUD"   "UIJ"   "MVACC" "IJ"

#  insert causes to model here - this can be a vector so multiple causes can be modelled
# if interaction is switch on we can only model the available interactions for: LVDC, AUD, IHD
diseases <- c("LVDC","AUD","IHD","DM", "MVACC", "IJ")

# switch between CASCADE and SIMAH models 
model <- "SIMAH"

# output (which version of the output is required) options are "education" "alcohol" or "mortality"
output_type <- "mortality"

# whether we want SES interaction effects   
sesinteraction <- 1

# do you want policy effects switched on? at the moment this is binary but 
# as the simulation develops there will be more options for policy scenarios
# this is also a temporary variable for model testing
# default value is 0
policy <- 0

# year to introduce policy
# depends on policy to be implemented
# 2014, 2015, 2016 (no policy change happened in SIMAH states)
year_policy <- 2015
# percentage to reduce alcohol consumption by -> this is overall for the population
# as the simulation develops this will take a more complex parameter indicating changes in consumption in different groups
# upper and lower and PE for policy estimate 
# Kilian et al. 2023: Alcohol control policy review	
# Relative change in alcohol use for 100% tax increase: 
# -0.108 (95% CI: -0.145, -0.071; 95% PI: -0.185, -0.012)
percentreductions <- c(0, 0.108, 0.145, 0.071, 0.185, 0.012)
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

# read in death counts data
death_counts <- load_death_counts(model, proportion, SelectedState, DataDirectory)

# read in migration in and out counts and project rates forwards to 2025 (in case needed)
# migration_counts <- load_migration_counts(SelectedState, DataDirectory)
migration_rates <- read.csv("SIMAH_workplace/microsim/1_input_data/birth_migration_rates_USA.csv")

# load in the education transition rates
list <- load_education_transitions(SelectedState, basepop, brfss, DataDirectory)
education_transitions <- list[[1]]
basepop <- list[[2]]
brfss <- list[[3]]
rm(list)
# load in alcohol transition rates
#### bring alcohol TPs out as an adjustable parameter - with name of the alcohol transitions file?
list <- load_alcohol_transitions(SelectedState, basepop, brfss, DataDirectory)
alcohol_transitions <- list[[1]]
basepop <- list[[2]]
brfss <- list[[3]]
rm(list)

# load in model parameters - using latin hypercube sampling 
# number of settings required 

n_samples <- 10

# whether to just use the point estimate - for now this is set to 1
PE <- 1
if(sesinteraction==1){
  lhs <- sensitivity_sample_lhs(n_samples, PE)
}else if(sesinteraction==0){
  lhs <- sample_lhs(n_samples, PE)
}

samples <- do.call(rbind,lhs)

for(i in 1:length(lhs)){
  lhs[[i]]$samplenum <- i
}

write.csv(do.call(rbind,lhs), "SIMAH_workplace/microsim/2_output_data/lhsSamples.csv")

update_base_rate <- 1

# if modelling mortality from specific causes - set up base mortality rates for the causes modelled
# set inflation factor 
# define inflation for different categories - i.e. 1 for those not being used and 50 for those inflated

inflation_factors <- c(50, 10)

# note age categories should be in 10 year categories - except 75-79
age_inflated <- list(
    c("18-24","25-34","35-44","45-54","55-64"), 
    c("65-74", "75-79"))

if(length(diseases)>=1){
  base_counts <- setup_base_counts(death_counts,diseases, inflation_factors, age_inflated)
}
