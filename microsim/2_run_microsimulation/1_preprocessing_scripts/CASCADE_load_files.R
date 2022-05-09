###processing data for dynamic microsimulation

# READ IN MICROSIM AND MIGRANT FILES
if(model=="SIMAH"){
basepop <- read.csv(paste("SIMAH_workplace/microsim/1_input_data/agent_files/", SelectedState, "basepop", PopulationSize, ".csv", sep=""))
}else if(model=="CASCADE"){
  basepop <- read.csv(paste("SIMAH_workplace/microsim/1_input_data/agent_files/", SelectedState, "basepopCASCADE", PopulationSize, ".csv", sep="")) %>% 
    mutate(microsim.init.BMI = ifelse(microsim.init.BMI<15, 15, ifelse(microsim.init.BMI>50, 50, 
                                                                       microsim.init.BMI)))
}
# migrants <- read.csv(paste("1_input_data/agent_files/", SelectedState, "migrants", PopulationSize, ".csv", sep=""))

source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/load_brfss.R")

# READ IN INWARD MIGRATION DATA - redundant in final version
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/inward_migration.R")

####READ IN OUTWARD MIGRATION DATA - redundant in final version
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/outward_migration.R")

cirrhosis <- 1

# READ IN DEATH RATES
if(model=="SIMAH"){
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/death_rates.R")
}else if(model=="CASCADE"){
  
  # first load cirrhosis mortality data
  source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/process_cirrhosis_1984_2016.R")
  
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/CASCADE_deathrates.R")
}


####PREP MICROSIM OUTPUTS
PopPerYear <- list()
DeathSummary <- list()

# save a copy of original population files
# migorig <- migrants
microsim.init.id <- 1:nrow(basepop)
basepop <- cbind(microsim.init.id, basepop)
baseorig <- basepop

# migrants$microsim.init.id <- nrow(basepop)+1:nrow(migrants)

# # # # allocate hepatitis at baseline 
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/baseline_hepatitis.R")

# read in ages for drinking history imputation
ages <- read_csv("SIMAH_workplace/microsim/1_input_data/agesforhistory.csv")
