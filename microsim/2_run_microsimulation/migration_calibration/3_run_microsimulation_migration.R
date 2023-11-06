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

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")

# alcohol_transitions <- read.csv("SIMAH_workplace/microsim/1_input_data/alcohol_transitions_new.csv")
alcohol_transitions <- readRDS(paste0(DataDirectory, "final_alc_transitionsUSA.RDS"))

# output_type <- "mortality"

# random number seed - sample random number 
seed <- as.numeric(sample(1:100, 1))

# sample number - set to 1 when just running 1 simulation 
samplenum <- 1

# set lhs to the first element of the lhs list- for testing 
lhs <- lhs[[1]]

migration_rates <- read.csv("SIMAH_workplace/microsim/1_input_data/birth_migration_rates_raw.csv")

# checking how the original migration counts fit the population data
Output <- list()
Output <- run_microsim_alt(seed=1,samplenum=1,basepop,brfss,
                       death_counts,
                       updatingeducation, education_transitions,
                       migration_rates,
                       updatingalcohol, alcohol_transitions,
                       catcontmodel, Hep, drinkingdistributions,
                       base_counts, diseases, lhs, liverinteraction,
                       policy=0, percentreduction=0.1, year_policy, inflation_factors,
                       age_inflated,
                       update_base_rate,
                       minyear=2000, maxyear=2019, output="demographics")
Output

PopSummary <- Output[[1]] %>% mutate(type="microsim") %>% dplyr::select(-c(samplenum,seed)) %>% 
  rename(TotalPop=n)

Population <- read.csv("SIMAH_workplace/microsim/census_data/ACS_population_constraints.csv") %>% 
  mutate(TotalPop=TotalPop*proportion, type="ACS") %>% rename(year=Year)

compare <- rbind(PopSummary, Population)

ggplot(subset(compare,microsim.init.sex=="m"), aes(x=year, y=TotalPop, colour=type)) + geom_line() + 
  facet_grid(cols=vars(agecat), rows=vars(microsim.init.race))
  
