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
# WorkingDirectory <- "C:/Users/laura/Documents/CAMH/SIMAH/"
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

# set lhs to the first element of the lhs list- for testing 
lhs <- lhs[[1]]

migration_counts <- read.csv("SIMAH_workplace/microsim/population_data/migration_in_USA.csv") %>% 
  mutate(BirthsInN = BirthsInN*proportion,
         MigrationInN = MigrationInN*proportion,
         MigrationOutN = MigrationOutN*proportion)

population_counts <- read.csv("SIMAH_workplace/microsim/census_data/ACS_population_constraints.csv") %>% 
  mutate(TotalPop=round(TotalPop*proportion),
         Year=Year-1)

basepop$brfssID <- NULL
# checking how the original migration counts fit the population data
Output <- list()
Output <- run_microsim_alt_adjustmigration(seed=1,samplenum=1,basepop,brfss,
                                           death_counts,
                                           updatingeducation, education_transitions,
                                           migration_counts, population_counts,
                                           updatingalcohol, alcohol_transitions,
                                           catcontmodel, Hep, drinkingdistributions,
                                           base_counts, diseases, lhs, liverinteraction,
                                           policy=0, percentreduction=0.1, year_policy, inflation_factors,
                                           age_inflated,
                                           update_base_rate,
                                           minyear=2000, maxyear=2022, output="demographics")
Output

migration_counts_new <- Output[[2]]

write.csv(migration_counts_new, paste0("SIMAH_workplace/microsim/1_input_data/migration_in_calibrated_", SelectedState, ".csv"),
          row.names=F)

popsummary <- Output[[1]] %>% 
  rename(microsim=n)

population_counts <- read.csv("SIMAH_workplace/microsim/census_data/ACS_population_constraints.csv") %>% 
  mutate(TotalPop=round(TotalPop*proportion)) %>% 
  rename(year=Year, ACS=TotalPop)
popsummary <- left_join(popsummary, population_counts) %>% 
  pivot_longer(microsim:ACS)

ggplot(data=subset(popsummary,microsim.init.sex=="f"), aes(x=year, y=value, colour=name, linetype=name)) + 
  geom_line(alpha=1) + facet_grid(cols=vars(agecat), rows=vars(microsim.init.race))

ggplot(data=subset(popsummary,microsim.init.sex=="m"), aes(x=year, y=value, colour=name, linetype=name)) + 
  geom_line(alpha=1) + facet_grid(cols=vars(agecat), rows=vars(microsim.init.race))

