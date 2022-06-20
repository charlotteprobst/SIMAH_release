#####SIMAH project 2022 - script for running SIMAH microsimulation model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(devtools)
library(roxygen2)
library(dplyr)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)

###set working directory to the main "SIMAH" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(paste(WorkingDirectory))

# load in microsim R package
setwd("SIMAH_code")
install("microsimpackage", dep=T)

setwd(paste(WorkingDirectory))

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")

Output <- list()
Output <- run_microsim(1,1,basepop,brfss,
                       death_rates,
                       updatingeducation, education_setup,
                       migration_rates,
                       updatingalcohol, alcohol_transitions,
                       2000, 2019, output="mortality")

if(output_type=="demographics"){
summary <- summarise_education_output(Output, SelectedState, WorkingDirectory)
}else if(output_type=="alcohol"){
summary <- summarise_alcohol_output(Output, SelectedState, WorkingDirectory)
}