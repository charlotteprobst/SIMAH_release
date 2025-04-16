# SIMAH project - script for reading in and processing all the necessary data files 

# read in base population
basepop <- read_csv(paste0(DataDirectory, "agent_files/", SelectedState, "basepop", PopulationSize, ".csv"),
                      show_col_types = FALSE)

# set microsim individuals IDs 
ID <- 1:nrow(basepop)
basepop <- cbind(ID, basepop)

# read in BRFSS data for migrants and 18-year-olds entering the model
brfss <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_2000_2022_final.RDS")
brfss <- process_brfss(brfss,SelectedState)

# read in death counts data
death_counts <- load_death_counts(proportion, SelectedState, DataDirectory)

# read in migration in and out counts
# these can be generated in the folder 1_migration_calibration if needed 
migration_rates <- read.csv(paste0("SIMAH_workplace/microsim/1_input_data/birth_migration_rates_", SelectedState, ".csv"))
# load in the education transition rates
# fix educational attainment at baseline - correct by years of age 
basepop$YEAR <- 2000
basepop <- fix_initial_education(basepop)
brfss <- fix_initial_education(brfss)

# fix educational attainment at baseline - specific years of some college 
basepop <- fix_years_somecollege(basepop)
brfss <- fix_years_somecollege(brfss)
basepop$YEAR <- NULL

# code alcohol categories 
basepop <- code_alcohol_categories(basepop)
brfss <- code_alcohol_categories(brfss)

# read in calibrated education transitions 
education_transitionsList <- read_rds(paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/education_calibration", "/transitionsList-10",".RDS"))
for(i in 1:length(education_transitionsList)){
  education_transitionsList[[i]]$cat <- gsub("1999-2019+_","",education_transitionsList[[i]]$cat)
}

# read in calibrated alcohol transitions
alcohol_transitions <- read_csv(paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/alcohol_calibration/lhs_regression-4.csv"), show_col_types = FALSE)
alcohol_transitionsList <- list()
for(i in 1:max(alcohol_transitions$sample)){
  alcohol_transitionsList[[i]] <- alcohol_transitions %>% filter(sample==i)
}

# read in the categorical to continuous distributions 
catcontmodel <- read.csv("SIMAH_workplace/microsim/2_output_data/alcohol_calibration/calibration_continuous_distribution.csv")

# set up latin hypercube for mortality parameters - PE only (sampling is elsewhere)
mortality_parameters <- sample_lhs(n_samples, 1)
mortality_parameters <- mortality_parameters[[1]]

if(length(diseases)>=1){
  base_counts <- setup_base_counts(death_counts,diseases, inflation_factors, age_inflated)
}
