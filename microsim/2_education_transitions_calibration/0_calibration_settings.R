# SIMAH project - script for setting up settings for calibration

# setting up parallel settings for calibration
# note that this is for use on HPC across multiple cores 
# if running locally (which is not suggested) set to 1
registerDoParallel(10)
options(future.rng.onMisuse="ignore")
options(future.globals.maxSize = 10000 * 1024^3)
options(future.fork.multithreading.enable = FALSE)

# set up the number of samples to be run
nsamples <- 2

# generate list of samples to be run with random number seeds
sampleseeds <- expand.grid(samplenum = 1:nsamples, seed=1:2)
sampleseeds$seed <- sample(1:nrow(sampleseeds), nrow(sampleseeds), replace=T)

# maximum number of potential calibration waves
num_waves <- 15

# improvement threshold to stop simulation - set at 0.5% 
# this means the calibration will stop when implausibility does not improve by more than 0.5%
improvement_threshold <- 0.005

#set up starting point for implausibility - this is what the first comparison will be made against 
prev_mean_implausibility <- 100

# set wave to 1 initially
wave <- 1

# read in the targets
targets <- read.csv("SIMAH_workplace/microsim/education_calibration/education_targets.csv") %>%
  group_by(YEAR, AGECAT, RACE, SEX) %>%
  mutate(target=TPop/sum(TPop),
         SE=sqrt(target*(1-target)/sum(OrigSample)),
         variance = (SE^2) * OrigSample) %>%
  dplyr::select(-c(TPop:OrigSample))
