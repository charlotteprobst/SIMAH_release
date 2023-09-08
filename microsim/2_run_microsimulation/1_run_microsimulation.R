#####SIMAH project 2022 - script for running SIMAH microsimulation model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(devtools)
library(roxygen2)
library(dplyr)
library(tidyverse)
library(fitdistrplus)
library(lhs)
library(truncnorm)
library(data.table)
options(dplyr.summarise.inform = FALSE)


###set working directory to the main "SIMAH" folder in your directory 
# WorkingDirectory <- "U:/SIMAH/"
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")


# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")

alcohol_transitions <- readRDS(paste0(DataDirectory, "final_alc_transitionsUSA.RDS"))
output_type <- "mortality"
Output <- list()
Output <- run_microsim(1,1,basepop,brfss,
                       death_counts,
                       updatingeducation, education_setup,
                       migration_counts,
                       updatingalcohol, alcohol_transitions,
                       catcontmodel, Hep, drinkingdistributions,
                       base_counts, diseases, lhs[[1]], liverinteraction,
                       policy, percentreduction, year_policy, inflation_factor,
                       2000, 2019, output_type)

alcohol_type <- "categorical"

if(output_type=="demographics"){
summary <- summarise_education_output(Output, SelectedState, DataDirectory)
}else if(output_type=="alcohol"){
  if(alcohol_type=="categorical"){
summary <- summarise_alcohol_output(Output, SelectedState, DataDirectory)
}else if(alcohol_type=="continuous"){
summary <- summarise_alcohol_output_continuous(Output[[2]], SelectedState, DataDirectory)
}
}else if(output_type=="mortality"){
summary <- summarise_mortality_output(Output, SelectedState, DataDirectory, inflation_factor)
}
summary[[2]]
write.csv(summary[[1]], "SIMAH_workplace/microsim/2_output_data/AlcCats_newTP-calibratedmean.csv")

nointeraction <- summary[[1]] %>% filter(year==2010) %>% 
  filter(name=="simulated")
write.csv(with_policy, "SIMAH_workplace/microsim/2_output_data/2015_with_policy.csv", row.names=F)
summary[[2]]
summary
ggsave("SIMAH_workplace/microsim/2_output_data/LVDC_interaction.png", dpi=300,
# summary 1 - table containing summary stats - observed to simulated 
summary[[2]]

# summary 2 - plot comparing mortality rates (age standardised)
summary[[2]]

# save a copy of the plot
ggsave("SIMAH_workplace/microsim/2_output_data/Women_newTP_calibrated_mean.png", dpi=300,
       width=33, height=19, units="cm")
