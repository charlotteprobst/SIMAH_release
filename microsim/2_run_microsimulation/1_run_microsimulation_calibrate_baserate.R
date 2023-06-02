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
library(gridExtra)
library(foreach)
library(doParallel)
options(dplyr.summarise.inform = FALSE)

###set working directory to the main "SIMAH" folder in your directory 
# WorkingDirectory <- "U:/SIMAH/"
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")

# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")

# alcohol_transitions <- read.csv("SIMAH_workplace/microsim/1_input_data/alcohol_transitions_new.csv")
alcohol_transitions <- readRDS(paste0(DataDirectory, "final_alc_transitionsUSA.RDS"))

output_type <- "mortality"

# random number seed - sample random number 
numsamples <- 5
sampleseeds <- expand.grid(sampleum=1:numsamples)
sampleseeds$seed <- sample(1:nrow(sampleseeds), nrow(sampleseeds), replace=T)

registerDoParallel(15)
# explore different base rates

baseorig <- basepop

lhs <- lhs[1:5]

Output <- foreach(i=1:nrow(sampleseeds), .inorder=TRUE) %do% {
print(i)
samplenum <- as.numeric(sampleseeds$sampleum[i])
seed <- as.numeric(sampleseeds$seed[i])
selectedlhs <- lhs[[samplenum]]
basepop <- baseorig
run_microsim(seed,samplenum,basepop,brfss,
                       death_counts,
                       updatingeducation, education_setup,
                       migration_counts,
                       updatingalcohol, alcohol_transitions,
                       base_counts, diseases, selectedlhs, liverinteraction,
                       policy, percentreduction, year_policy, inflation_factor,
                       2000, 2002, output_type)
}

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
summary <- summarise_mortality_output_calibration(Output, SelectedState, DataDirectory, inflation_factor, diseases)
}
summary[[2]]

# save a copy of the plot
ggsave("SIMAH_workplace/microsim/2_output_data/mortality_summary_multiple.png", dpi=300,
       width=33, height=19, units="cm")
