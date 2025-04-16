# SIMAH project - script for setting up sampleseeds file

# set seed to replicate selection of transition models and policy parameters 
set.seed(123)

# set up the number of samples to be run
nsamples <- 1 # indicates samples per same sample seed (for calibration purposes only)
nreps <- 10 # indicates number of different sample seeds
n_uncertainty <- 60 # indicates number of unique combinations of education x alcohol models x own-price elasticities

# generate list of samples to be run with random number seeds
sampleseeds <- expand.grid(samplenum = 1:nsamples, seed=1:nreps)
sampleseeds$seed <- sample(1:3000, nrow(sampleseeds), replace=F)

# set up scenarios and policy settings
sampleseeds <- sampleseeds %>% expand(sampleseeds, policy_setting, scenarios)

# sample policy parameters
sampleseeds <- sample_policy_parameters(sampleseeds, n_uncertainty)

# sample education transition models
education_assignments <- data.frame(
  nunc = 1:n_uncertainty,
  educationmodel = sample(1:length(education_transitionsList), n_uncertainty, replace = TRUE)
)

# sample alcohol transition models
alcohol_assignments <- data.frame(
  nunc = 1:n_uncertainty,
  alcoholmodel = sample(1:length(alcohol_transitionsList), n_uncertainty, replace = TRUE)
)

# Join sample seeds and policy parameters with transition models
sampleseeds <- sampleseeds %>%
  left_join(education_assignments, by = "nunc") %>%
  left_join(alcohol_assignments, by = "nunc")

# check whether there are seeds X n_uncertainty unique combinations of elasticities, education, and alcohol model
unique_combinations <- unique(sampleseeds %>% dplyr::select(c("cons_elasticity", "educationmodel", "alcoholmodel")))

# save sampleseeds file for reproducibility 
write.csv(sampleseeds, paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/sampleseeds/output-policy_sampleseeds_", Sys.Date(), "_lhs.csv"), row.names=F)
