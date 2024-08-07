#####SIMAH project 2022 - script for running SIMAH microsimulation model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(devtools)
library(roxygen2)
library(gatbxr)
# if having trouble with loading this package - run the below two lines
# install.packages("remotes")
# remotes::install_github("drizztxx/gatbxr")
library(dplyr)
library(tidyverse)
library(fitdistrplus)
library(lhs)
library(truncnorm)
library(data.table)
library(gridExtra)
options(dplyr.summarise.inform = FALSE)

###set working directory to the main "SIMAH" folder in your directory 
# WorkingDirectory <- "U:/SIMAH/"
WorkingDirectory <- "C:/Users/laura/Documents/CAMH/SIMAH/"
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "C:/Users/marie/Dropbox/NIH2020/"

DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")

# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)
install("SIMAH_code/calibrationpackage", dep=T)

library(microsimpackage)
library(calibrationpackage)

ScriptDirectory <- paste0(WorkingDirectory, "/SIMAH_code/microsim/1_migration_calibration/")

# read in all model settings
source(paste0(ScriptDirectory, "0_model_settings.R"))

# load all microsim files
source(paste0(ScriptDirectory, "0_load_microsim_files.R"))

# random number seed - sample random number 
seed <- as.numeric(sample(1:100, 1))

# sample number - set to 1 when just running 1 simulation 
samplenum <- 1

lhs <- NULL

migration_counts <- read.csv(paste0("SIMAH_workplace/microsim/1_input_data/migration_in_calibrated_", SelectedState, ".csv"))

# checking how the original migration counts fit the population data
Output <- list()
Output <- run_microsim_alt_estimaterate(seed=1,samplenum=1,basepop,brfss,
                       death_counts,
                       updatingeducation, education_transitions,
                       migration_counts,
                       updatingalcohol, alcohol_transitions,
                       catcontmodel, Hep, drinkingdistributions,
                       base_counts, diseases, lhs, liverinteraction,
                       policy=0, percentreduction=0.1, year_policy, inflation_factors,
                       age_inflated,
                       update_base_rate,
                       minyear=2000, maxyear=2022, output="demographics")
Output

birth_rates <- Output[[2]] %>% dplyr::select(year,agecat,race,sex,rate) %>% 
  rename(birthrate=rate) %>% mutate(migrationinrate=NA)
library(scales)
scaleFUN <- function(x) sprintf("%.2f", x)
ggplot(data=birth_rates, aes(x=year, y=birthrate, colour=sex)) + 
  geom_line() + 
  facet_grid(rows=vars(race)) + 
  ylim(0,0.05) + 
  scale_y_continuous(labels=scaleFUN, limits=c(0,0.05))
ggsave("SIMAH_workplace/microsim/2_output_data/migration/birth_rates.png", dpi=300, width=33, height=19, units="cm")
  
migration_rates <- Output[[3]]%>% dplyr::select(year,agecat,race,sex,rate_in, rate_out)
ggplot(data=migration_rates, aes(x=year, y=rate_in, colour=sex)) + 
  geom_line() +
  facet_grid(cols=vars(agecat), rows=vars(race)) + 
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA))
ggsave("SIMAH_workplace/microsim/2_output_data/migration/migration_rates.png", dpi=300, width=33, height=19, units="cm")

migration_rates <- migration_rates %>% 
  rename(migrationinrate=rate_in,
          migrationoutrate=rate_out) %>% 
  mutate(birthrate=NA)
birth_rates$migrationoutrate <- NA

migration_rates <- rbind(birth_rates, migration_rates)

# save the migration rates for this geography
write.csv(migration_rates, paste0("SIMAH_workplace/microsim/1_input_data/birth_migration_rates_", SelectedState, ".csv"),row.names=F)
