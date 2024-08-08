#####first read in and process all the necessary data files 

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
migration_rates <- read.csv(paste0("SIMAH_workplace/microsim/1_input_data/birth_migration_rates", SelectedState, ".csv")) %>% 
  rename(race=microsim.init.race, sex=microsim.init.sex) %>% 
  mutate(race = case_when(race=="WHI" ~ "White",
                          race=="BLA" ~ "Black",
                          race=="SPA" ~ "Hispanic",
                          race=="OTH" ~ "Others"))

# load in the education transition rates
# fix educational attainment at baseline - correct by years of age 
basepop$YEAR <- 2000
basepop <- fix_initial_education(basepop)
brfss <- fix_initial_education(brfss)

# fix educational attainment at baseline - specific years of some college 
basepop <- fix_years_somecollege(basepop)
brfss <- fix_years_somecollege(brfss)
basepop$YEAR <- NULL

# no longer read in education transitions here -> this is generated in a separate script
education_transitions <- NULL

# load in alcohol transition rates
#### bring alcohol TPs out as an adjustable parameter - with name of the alcohol transitions file?
# code alcohol categories 
basepop <- code_alcohol_categories(basepop)
brfss <- code_alcohol_categories(brfss)

# no longer read in alcohol transitions here -> this is generated in a separate script 
alcohol_transitions <- NULL

# set up latin hypercube for mortality parameters - PE only (sampling is elsewhere)
mortality_parameters <- sample_lhs(n_samples, 1)
mortality_parameters <- mortality_parameters[[1]]

if(length(diseases)>=1){
  base_counts <- setup_base_counts(death_counts,diseases, inflation_factors, age_inflated)
}
