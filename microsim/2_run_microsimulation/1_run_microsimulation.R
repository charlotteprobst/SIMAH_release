#####SIMAH project 2022 - script for running SIMAH microsimulation model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(devtools)
library(roxygen2)
library(dplyr)
library(tidyverse)
library(fitdistrplus)
options(dplyr.summarise.inform = FALSE)

# load in microsim R package
setwd("~/Google Drive/SIMAH Sheffield/SIMAH_code")
install("microsimpackage", dep=T)

###set working directory to the main "SIMAH" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
DataDirectory <- "~/Google Drive/SIMAH Sheffield/RSA/Microsim_workshop/workshop_data/"

setwd(paste(WorkingDirectory))

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")

Output <- list()
Output <- run_microsim(1,1,basepop,brfss,
                       death_rates,
                       updatingeducation, education_setup,
                       migration_rates,
                       updatingalcohol, alcohol_transitions,
                       policy, percentreduction,
                       2000, 2005, output="alcohol")

if(output_type=="demographics"){
summary <- summarise_education_output(Output, SelectedState, DataDirectory)
}else if(output_type=="alcohol"){
summary <- summarise_alcohol_output(Output, SelectedState, DataDirectory)
}else if(output_type=="mortality"){
summary <- summarise_mortality_output(Output, SelectedState, DataDirectory)
}

summary
summary[[1]]

