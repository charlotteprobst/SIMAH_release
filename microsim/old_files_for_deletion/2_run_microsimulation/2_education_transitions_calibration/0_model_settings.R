# SIMAH project 2024 - script for setting up microsimulation model settings

# set up working directories and data output directories 
###set working directory to the main "Microsimulation" folder in your directory
DataDirectory <- paste0(WorkingDirectory, "/SIMAH_workplace/microsim/1_input_data/")
OutputDirectory <- paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/education_calibration/version1")

dir.create(OutputDirectory)

seed <- 42 

options(scipen=999)

####which geography -  needs to be written as USA or full state name 
SelectedState <- "USA"

# list of states - for calibration looping through states
States <- c("California","Colorado","Florida","Indiana",
            "Louisiana","Massachusetts","Michigan","Minnesota",
            "Missouri","New York", "Oregon", "Pennsylvania",
            "Tennessee","Texas","USA")

####Size of population 
PopulationSize <- 1000000

# switch on and off migration and deaths
migrationdeaths <- 1

# switch on and off education updates
updatingeducation <- 0

# switch on and off alcohol updates
updatingalcohol <- 0

# switch between modelling mortality and morbidity (mortality = 1)
mortality <- 1

# switch between mortality causes
# write in the same format as the death rates file 
# "LVDC"  "HLVDC" "DM"    "IHD"   "ISTR"  "HYPHD"
# "AUD"   "UIJ"   "MVACC" "IJ"

#  insert causes to model here - this can be a vector so multiple causes can be modelled
diseases <- NULL

# switch between CASCADE and SIMAH models 
model <- "SIMAH"

# output (which version of the output is required) options are "education" "alcohol" or "mortality"
output_type <- "mortality"

# whether we want SES interaction effects for liver cirrhosis 
# note this is a temporary variable and may change to a more general SES interaction flag 
liverinteraction <- 0

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

# parameter settings for calibration
n_samples <- 1

# whether to just use the point estimate - set this to 1 for education and alcohol transitions 
PE <- 1

update_base_rate <- 1

# if modelling mortality from specific causes - set up base mortality rates for the causes modelled
# set inflation factor 
# define inflation for different categories - i.e. 1 for those not being used and 50 for those inflated
inflation_factors <- c(50, 10)

# note age categories should be in 10 year categories - except 75-79
age_inflated <- list(
    c("18-24","25-34","35-44","45-54","55-64"), 
    c("65-74", "75-79"))
