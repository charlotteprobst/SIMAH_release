#Wrapper code for generating population weights for base population for all states
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()

######packages######
library(dplyr)
library(knitr)
library(ipfp)
library(tidyr)
library(janitor)
library(stringr)
library(readr)
library(sjmisc)
library(readxl)
library(foreign)
library(splitstackshape)

library(microsimpackage)

options(scipen=999)

###set working directory to the main folder
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)

source("SIMAH_code/microsim/1_generate_baseline_population/0_IPF_functions.R")

# install microsim package for generating population functions

# which states do we want to generate weights for - all SIMAH states and whole USA
# this can be edited to get other state populations 
selected_states <- c("California", "Colorado", "Florida", "Indiana", "Kentucky", 
                     "Louisiana", "Massachusetts", "Michigan", "Minnesota", 
                     "Missouri", "New York", "Oregon", "Pennsylvania", 
                     "Tennessee", "Texas", "USA")

selected_states <- "USA"

# read in constraints file - generated in the previous step (1) 
cons <- read.csv("SIMAH_workplace/microsim/1_generating_population/constraints_IPF_2023.csv") %>% 
  filter(state %in% selected_states)
  
# read in the individual-level data 
brfss <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_2000_2020_final.RDS")

# process the individual-level data into the correct format and sort out any missing data for categories
# do this for each state 

# which years do you want to generate the baseline population for? 
# default to 2000-2003 (can add or remove years as necessary but be aware there may be missing data for some states with less years)
# this is also to match with the alcohol targets at baseline that need to be pooled across years
years <- c(2001)

# subset the data for these years to minimise computation needed
brfss <- brfss %>% filter(YEAR %in% years)

# which variables do you want
variables <- c("State","region","sex","race","age_var","agecat","education","household_income","BMI",
               "drinkingstatus","drinkingstatus_detailed", "formerdrinker","gramsperday",
               "frequency","quantity_per_occasion")

# process brfss data for IPF - including which variables to keep for the baseline population
# making sure there are people in each category in each state - looping through states
# this also "borrows" people from the same region to be in that state population if they are missing
processed_data <- list()
for(i in unique(selected_states)){
  processed_data[[paste(i)]] <- process_for_IPF(brfss, i, variables)
}
processed_data <- processed_data %>% bind_rows()

# do IPF to get weights for each state 
weights_file <- list()
for(i in unique(selected_states)){
  weights_file[[paste(i)]] <- generate_weights_IPF(processed_data, i, cons)
}

# this weights file can now be used to generate any sized population for any state without needing to re-run the above
# the above only needs to be re-run when the years / variables etc. need to change
weights_file <- weights_file %>% bind_rows()

# join back up with the processed data file 
processed_data <- left_join(processed_data, weights_file)

# now save a copy of the processed data and associated weights 
# this means this script does not need to be re-run
write.csv(processed_data, "SIMAH_workplace/microsim/base_population/brfss_with_weights_USA.csv", row.names=F)
