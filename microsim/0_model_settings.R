# SIMAH project - script for setting up microsimulation model settings
options(scipen=999)

# set minyear and maxyear - the microsimulation must start in year 2000 (minyear)
minyear <- 2000
maxyear <- 2019

# select geography -  needs to be written as USA or full state name 
SelectedState <- "USA"

# list of states - for calibration looping through states
States <- c("California","Colorado","Florida","Indiana",
            "Louisiana","Massachusetts","Michigan","Minnesota",
            "Missouri","New York", "Oregon", "Pennsylvania",
            "Tennessee","Texas","USA")

# specify size of synthetic population 
PopulationSize <- 1000000

# switch on and off migration and deaths
migrationdeaths <- 1

# switch on and off education updates
updatingeducation <- 1

# switch on and off alcohol updates
updatingalcohol <- 1

# insert causes to model here - not used for this publication
diseases <- NULL

# set output_type
output_type <- c("alcoholcat", "alcoholcont", "alcoholcontcat")

# switch for SES interaction effects for mortality modelling - not used for this publication
sesinteraction <- 0

####################EDIT ONLY ABOVE HERE ##################################################

# calculate what proportion the synthetic population represents of the target population
WholePopSize <- read.csv(paste0(DataDirectory,"fullpopcounts.csv")) %>% 
  filter(STATE==SelectedState)

proportion <- PopulationSize/WholePopSize$total
proportion <- ifelse(proportion>1,1,proportion)

# set parameters for mortality modelling - not used for this publication
PE <- 1
update_base_rate <- 1
inflation_factors <- c(50, 10)

# note age categories should be in 10 year categories - except 75-79
age_inflated <- list(
    c("18-24","25-34","35-44","45-54","55-64"), 
    c("65-74", "75-79"))
