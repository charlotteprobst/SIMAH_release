# SIMAH project 2024 - script for setting up settings for calibration

# setting up parallel settings for calibration
# note that this is for use on HPC across multiple cores 
# if running locally (which is not suggested) set to 1
registerDoParallel(16)
options(future.rng.onMisuse="ignore")
options(future.globals.maxSize = 10000 * 1024^3)
options(future.fork.multithreading.enable = FALSE)

# set up the number of samples to be run
nsamples <- 1
nreps <- 30

# generate list of samples to be run with random number seeds
sampleseeds <- expand.grid(samplenum = 1:nsamples, seed=1:nreps, scenario=c(0,1))
sampleseeds$seed <- sample(1:3000, nrow(sampleseeds), replace=F)

# maximum number of potential calibration waves
num_waves <- 1

# improvement threshold to stop simulation - set at 0.5% 
# this means the calibration will stop when implausibility does not improve by more than 0.5%
improvement_threshold <- 0.005

#set up starting point for implausibility - this is what the first comparison will be made against 
prev_mean_implausibility <- 100

# set wave to 1 initially
wave <- 1

# read in the education models for alcohol model calibration 
education_transitionsList <- read_rds(paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/education_calibration", "/transitionsList-10",".RDS"))

for(i in 1:length(education_transitionsList)){
  education_transitionsList[[i]]$cat <- gsub("1999-2019+_","",education_transitionsList[[i]]$cat)
}


# add the education model to be run to the sampleseeds file 
edmodels <- list()
for(i in 1:ceiling(nrow(sampleseeds)/length(education_transitionsList))){
edmodels[[paste(i)]] <- data.frame(education_model = sample(1:length(education_transitionsList), replace=F))
}
edmodels <- edmodels %>% bind_rows()

sampleseeds$educationmodel <- edmodels$education_model[1:nrow(sampleseeds)]

# add the final alcohol TPs 
alcohol_transitions <- read_csv(paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/lhs_regression-4.csv"))
alcohol_transitions$...1 <- NULL

alcohol_transitionsList <- list()
for(i in 1:max(alcohol_transitions$sample)){
  alcohol_transitionsList[[i]] <- alcohol_transitions %>% filter(sample==i)}

alcmodels <- list()
for(i in 1:ceiling(nrow(sampleseeds)/length(alcohol_transitionsList))){
  alcmodels[[paste(i)]] <- data.frame(alcohol_model = sample(1:length(alcohol_transitionsList), replace=F))
}
alcmodels <- alcmodels %>% bind_rows()

sampleseeds$alcoholmodel <- alcmodels$alcohol_model[1:nrow(sampleseeds)]

catcontmodel <- read.csv("SIMAH_workplace/microsim/2_output_data/alcohol_calibration/calibration_continuous_distribution.csv")
