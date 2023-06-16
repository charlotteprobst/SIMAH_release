#####SIMAH project 2022 - script for running SIMAH microsimulation model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(devtools)
library(roxygen2)
library(dplyr)
library(tidyr)
library(purrr)
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
# WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
WorkingDirectory <- "/home/cbuckley/"

DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")

# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")

# alcohol_transitions <- read.csv("SIMAH_workplace/microsim/1_input_data/alcohol_transitions_new.csv")
alcohol_transitions <- readRDS(paste0(DataDirectory, "final_alc_transitionsUSA.RDS"))

output_type <- "mortality"

# random number seed - sample random number 
sampleseeds <- expand.grid(samplenum=1:numsamples)
sampleseeds$seed <- sample(1:nrow(sampleseeds), nrow(sampleseeds), replace=T)

registerDoParallel(12)
# explore different base rates

baseorig <- basepop

Output <- foreach(i=1:nrow(sampleseeds), .inorder=TRUE, .combine=rbind) %dopar% {
print(i)
samplenum <- as.numeric(sampleseeds$samplenum[i])
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
                       update_base_rate,
                       2000, 2015, output_type)
}

write.csv(Output, "SIMAH_workplace/microsim/2_output_data/Output_basecalibration.csv", row.names=F)
