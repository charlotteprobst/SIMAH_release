#####SIMAH project 2022 - script for running SIMAH microsimulation model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(devtools)
library(roxygen2)
library(dplyr)
library(tidyverse)
library(fitdistrplus)
library(lhs)
library(truncnorm)
options(dplyr.summarise.inform = FALSE)

# load in microsim R package
setwd("~/Google Drive/SIMAH Sheffield/SIMAH_code")
install("microsimpackage", dep=T)

###set working directory to the main "SIMAH" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
DataDirectory <- "~/Google Drive/SIMAH Sheffield/SIMAH_workplace/microsim/1_input_data/"

setwd(paste(WorkingDirectory))

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")

output_type <- "mortality"

Output <- list()
Output <- run_microsim(1,1,basepop,brfss,
                       death_rates,
                       updatingeducation, education_setup,
                       migration_rates,
                       updatingalcohol, alcohol_transitions,
                       catcontmodel, Hep, drinkingdistributions,
                       base_rates, diseases, lhs[[1]],
                       policy, percentreduction, year_policy, inflation_factor,
                       2000, 2019, output_type)

alcohol_type <- "continuous"

if(output_type=="demographics"){
summary <- summarise_education_output(Output, SelectedState, DataDirectory)
}else if(output_type=="alcohol"){
  if(alcohol_type=="categorical"){
summary <- summarise_alcohol_output(Output[[1]], SelectedState, DataDirectory)
}else if(alcohol_type=="continuous"){
summary <- summarise_alcohol_output_continuous(Output[[2]], SelectedState, DataDirectory)
}
}else if(output_type=="mortality"){
summary <- summarise_mortality_output(Output, SelectedState, DataDirectory)
}else if(output_type=="hepatitis"){
summary <- summarise_hepatitis_output(Output)  
}

summary[[1]]
summary[[2]]
summary
ggsave("SIMAH_workplace/microsim/2_output_data/rates_compare_HLVDC.png", dpi=300,
       width=33, height=19, units="cm")
