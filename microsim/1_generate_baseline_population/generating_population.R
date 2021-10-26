#Wrapper code for generating microsimulation populations from BRFSS data
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

options(scipen=999)

####wrapper code for generating base population 
set.seed(42)

####EDIT ONLY BELOW HERE ### 

###set working directory to the main folder
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)

####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee, Washington

SelectedState <- "Texas"

PopulationSize <- 1000000

####EDIT ONLY ABOVE HERE ##

tokeep <- c("SelectedState", "PopulationSize", "WorkingDirectory", "microsim", "tokeep",
            "wholepopsize", "percentpop", "adjusting")

###generate a base population - IPF from NSDUH data to PSID / Census constraints 

source("SIMAH_code/microsim/1_generate_baseline_population/scripts/basepop_IPF.R")

source("SIMAH_code/microsim/1_generate_baseline_population/scripts/process_for_microsim.R")
write.csv(microsim, paste("SIMAH_workplace/microsim/1_input_data/agent_files/",State, "basepop", sep="", PopulationSize, ".csv"), row.names=FALSE)
