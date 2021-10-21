# SIMAH October 2021 - code to take the processed BRFSS data file and up-shift to per-capita consumption on a state by state basis
# this code also adjusts the BRFSS monthly alcohol consumption to annual alcohol consumption using data from the NAS 

library(foreign)
library(SASxport)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(sjlabelled)

# CB laptop directory
wd <- "~/Google Drive/SIMAH Sheffield/"
# CB desktop directory
wd <- "G:/My Drive/SIMAH Sheffield/"
setwd(wd)

####read in the joined up data files 
dataFiles <- readRDS("SIMAH_workplace/brfss/processed_data/brfss_full_selected.RDS")
gc()

# first exploring the data for our selected states and for the US current prevalence and quantity (monthly)
summary <- lapply(dataFiles, summariseprevalence)
summary <- do.call(rbind,summary)
