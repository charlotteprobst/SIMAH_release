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

# set up latin hypercube for mortality parameters 
lhs <- sample_lhs(n_samples, PE)

samples <- do.call(rbind,lhs)

for(i in 1:length(lhs)){
  lhs[[i]]$samplenum <- i
}

lhs <- ifelse(PE==1, lhs[[1]], lhs)