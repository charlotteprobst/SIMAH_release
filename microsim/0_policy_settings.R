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
policy_model <- "tax_generic"
# policy_model <- c("tax_generic_main", "tax_generic_min", "tax_generic_max")  
scenarios <- c(0,1)

######################DO NOT EDIT BELOW HERE ##################################################

policy_setting <- read.csv(paste0(DataDirectory,"input_policy_sim.csv")) %>% 
  filter(model %in% policy_model)

