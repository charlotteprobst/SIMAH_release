# SIMAH project 2022 - script for setting up microsimulation model settings
library(dplyr)
library(readr)
library(microsimpackage)

set.seed(42)

options(scipen=999)

######################EDIT ONLY BELOW HERE ##################################################

# general policy settings
policy <- 1
year_policy <- 2015
policy_int <- "price"
#scenarios <- c(0.05, 0.1, 0.2)

######################DO NOT EDIT BELOW HERE ##################################################

policy_setting <- read.csv(paste0(DataDirectory,"input_policy_sim.csv")) %>% 
  filter(policy %in% policy_int)

