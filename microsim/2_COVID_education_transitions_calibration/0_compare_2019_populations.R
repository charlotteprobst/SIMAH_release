# SIMAH project Feb 2024 

# This script compares the proportion of people in each demographic group in the year 2019 
# when using the final post-calibration versions of the education TPs for 2000-2019 (i.e. wave 10)

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()

library(devtools)
library(roxygen2)
library(gatbxr)
# if having trouble with loading this package - run the below two lines
# install.packages("remotes")
# remotes::install_github("drizztxx/gatbxr")
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(fitdistrplus)
library(lhs)
library(truncnorm)
library(data.table)
library(gridExtra)
library(doParallel)
library(splitstackshape)
library(msm)
options(dplyr.summarise.inform = FALSE)

# WorkingDirectory <- "U:/SIMAH"
# WorkingDirectory <- "C:/Users/laura/Documents/CAMH/SIMAH"
# WorkingDirectory <- "/home/cbuckley"
# WorkingDirectory <- "/Users/charlottebuckley/Google Drive/SIMAH Sheffield"
WorkingDirectory <- "C:/Users/cmp21seb/Documents/SIMAH/"

# set wd and install the microsim and calibration packages
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)
install("SIMAH_code/calibrationpackage", dep=T)

library(microsimpackage)
library(calibrationpackage)

ScriptDirectory <- paste0(WorkingDirectory, "/SIMAH_code/microsim/2_run_microsimulation/2_COVID_education_transitions_calibration/")

# read in all model settings (with n_samples set to the number of populations you want to compare)
source(paste0(ScriptDirectory, "/0_model_settings.R"))

# read in settings for calibration (with nsamples set to 300, reps set to 1)
source(paste0(ScriptDirectory,"0_calibration_settings.R"))

# load all microsim files
source(paste0(ScriptDirectory, "0_load_microsim_files.R"))

# Run the simulation for each of the different sets of TP parameters saved from wave 10 of calibration
  baseorig <- basepop
  Output <- list()
  Output <- foreach(i=1:nrow(sampleseeds), .inorder=TRUE) %do% {
    print(i)
    # set seed and sample number for current iteration
    samplenum <- as.numeric(sampleseeds$samplenum[i])
    seed <- as.numeric(sampleseeds$seed[i])
    # reset the base population to the original pop for each calibration iteration
    basepop <- baseorig
    # change the alcohol model being run 
    # alcohol_transitions <- transitionsList[[samplenum]]
    # change the education model - based on the prior calibrated models 
    education_model_num <- as.numeric(sampleseeds$educationmodel[i])
    education_transitions <- education_transitionsList[[education_model_num]]
    # execute the simulation with each setting
    run_microsim_alt(seed,samplenum,basepop,brfss,
                     death_counts,
                     updatingeducation, education_transitions,
                     migration_rates,
                     updatingalcohol, alcohol_transitions,
                     catcontmodel, drinkingdistributions,
                     base_counts, diseases, lhs, sesinteraction,
                     policy=0, percentreduction=0.1, year_policy, inflation_factors,
                     age_inflated,
                     update_base_rate,
                     minyear=2000, maxyear=2019, output="population")
    }

# Add identifier for each list, reflecting the different samples used to generate the population
temp <- lapply(seq_along(Output), function(i) {
    df <- Output[[i]]
    df$sample_ID <- i
    return(df)
  })
  
# Combine the data frames into one table
Combined_Output <- do.call(rbind, temp)
  
# Compare the difference, between samplhttp://127.0.0.1:43777/graphics/plot_zoom_png?width=1015&height=518es, of the % of people in each demographic group
Combinations <- Combined_Output %>% 
    group_by(microsim.init.sex,agecat,microsim.init.race,microsimnewED) %>% 
    mutate(max_difference = max(percentage)-min(percentage))
  
# Keep only the unique combinations and one max_difference value
unique_combinations <- Combinations %>%
    distinct(microsim.init.sex, agecat, microsim.init.race, microsimnewED, .keep_all = TRUE) %>%
    dplyr::select(microsim.init.sex, agecat, microsim.init.race, microsimnewED, max_difference)
  
# save the results
write.csv(unique_combinations, "SIMAH_workplace/education_transitions/2021/comparisson_of_populations_2019_300_samples.csv", row.names=F) 

# read in the results
unique_combinations <- read.csv("SIMAH_workplace/education_transitions/2021/comparisson_of_populations_2019_300_samples.csv")
 
# Plot
unique_combinations %>%
  ggplot(aes(x = microsimnewED , y = max_difference, color = microsim.init.race)) +
  geom_point() +
  facet_grid(microsim.init.sex ~ agecat) +
  labs(x = "Race", y = "Max Difference (%)", color = "Education", title = "Max difference across 300 populations") +
  theme_minimal()