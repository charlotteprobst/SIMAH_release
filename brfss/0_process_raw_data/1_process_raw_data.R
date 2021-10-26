####SIMAH OCT 2021 brfss processing - processing the raw data files 
# BRFSS data 1999- 2020
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
# wd <- "G:/My Drive/SIMAH Sheffield/"
setwd(wd)

####read in the joined up data files 
dataFiles <- readRDS("SIMAH_workplace/brfss/raw_data/data/brfss_full.RDS")
gc()

source("SIMAH_code/brfss/0_process_raw_data/1_processing_functions.R")

years <- 1999:2020
for(i in 1:length(dataFiles)){
  dataFiles[[i]]$YEAR <- years[i]
}

dataFiles <- lapply(dataFiles, remove_all_labels)
gc()
options(memory.limit=10000000)

# recode state names
dataFiles <- lapply(dataFiles, recode_state)

# recode age
dataFiles <- lapply(dataFiles, recode_age)

# recode sex 
dataFiles <- lapply(dataFiles, recode_sex)

# recode race / ethnicity 
dataFiles <- lapply(dataFiles, recode_race)

gc()
# recode educational attainment 
dataFiles <- lapply(dataFiles, recode_education)

# recode employment status 
dataFiles <- lapply(dataFiles, recode_employment)

# recode income 
dataFiles <- lapply(dataFiles, recode_income)

# recode BMI
dataFiles <- lapply(dataFiles, recode_weight)

dataFiles <- lapply(dataFiles, recode_height)

dataFiles <- lapply(dataFiles, recode_BMI)

#### recode alcohol variables

# prevalence - note not available in all years but can be derived from quant and freq vars
dataFiles <- lapply(dataFiles, recode_alc_prevalence)

# frequency - drinking days per month
dataFiles <- lapply(dataFiles, recode_alc_frequency)

# quantity- drinks per occasion and grams per day 
dataFiles <- lapply(dataFiles, recode_alc_quantity)

# hed - number of days with 4+ 5+
dataFiles <- lapply(dataFiles, recode_hed)

# recode the sample weights 
dataFiles <- lapply(dataFiles, recode_sample_weights)

# select the variables needed and save the output - adjust this in the function "subset_data"
dataFilesSubset <- lapply(dataFiles, subset_data)

saveRDS(dataFilesSubset, "SIMAH_workplace/brfss/processed_data/brfss_full_selected.RDS")
