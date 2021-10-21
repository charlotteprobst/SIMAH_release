####SIMAH OCT 2021 brfss processing - processing the raw data files 
# BRFSS data 1999- 2020
library(foreign)
library(SASxport)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(sjlabelled)

wd <- "~/Google Drive/SIMAH Sheffield/"
setwd(wd)

####read in the joined up data files 
dataFiles <- readRDS("SIMAH_workplace/brfss/raw_data/data/brfss_full.RDS")
gc()

years <- 1999:2020
for(i in 1:length(dataFiles)){
  dataFiles[[i]]$YEAR <- years[i]
}

dataFiles <- lapply(dataFiles, remove_all_labels)

# recode state names
dataFiles <- lapply(dataFiles, recode_state)

# recode educational attainment 
dataFiles <- lapply(dataFiles, recode_education)

# recode race / ethnicity 
dataFiles <- lapply(dataFiles, recode_race)

for(i in years){
  print(i)
  # print(summary(as.factor(dataFiles[[paste(i)]]$hispanic_comb)))
  print(summary(as.factor(dataFiles[[paste(i)]]$race_comb)))
}


