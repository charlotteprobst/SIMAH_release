###processing data for dynamic microsimulation

# READ IN MICROSIM AND MIGRANT FILES
basepop <- read.csv(paste("SIMAH_workplace/microsim/1_input_data/agent_files/", SelectedState, "basepop", PopulationSize, ".csv", sep=""))
# migrants <- read.csv(paste("1_input_data/agent_files/", SelectedState, "migrants", PopulationSize, ".csv", sep=""))

source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/load_brfss.R")

# READ IN INWARD MIGRATION DATA - redundant in final version
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/inward_migration.R")

####READ IN OUTWARD MIGRATION DATA - redundant in final version
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/outward_migration.R")

# READ IN DEATH RATES
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/death_rates.R")

####PREP MICROSIM OUTPUTS
PopPerYear <- list()
DeathSummary <- list()

# save a copy of original population files
baseorig <- basepop
# migorig <- migrants
microsim.init.id <- 1:nrow(basepop)
basepop <- cbind(microsim.init.id, basepop)
# migrants$microsim.init.id <- nrow(basepop)+1:nrow(migrants)

