####SIMAH OCT 2021 brfss processing - processing the raw data files 
# BRFSS data 1984- 2020
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

# read in R script with the functions
source("SIMAH_code/brfss/0_process_raw_data/1_processing_functions.R")

years <- 1984:2020 # Extend this, if 2021 data is added 
for(i in 1:length(dataFiles)){
  dataFiles[[i]]$YEAR <- years[i]
}

# remove the labels from the BRFSS raw data files (stata format etc.)
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

# recode marital status
dataFiles <- lapply(dataFiles, recode_marital)

# recode income 
dataFiles <- lapply(dataFiles, recode_income)

# recode BMI - weight, then height, then BMI
dataFiles <- lapply(dataFiles, recode_weight)

dataFiles <- lapply(dataFiles, recode_height)

dataFiles <- lapply(dataFiles, recode_BMI)

# impute missing BMI data
# dataFiles <- lapply(dataFiles, impute_missing_BMI)

dataFiles <- lapply(dataFiles, recode_derived_BMI)

# Summary <- list()
# ### recode alcohol variables
# for(i in 1984:2020){
#   Summary[[paste(i)]] <- 
#     dataFiles[[paste(i)]] %>% filter(BMI_final>60 | BMI_final<13) %>% dplyr::select(YEAR,State,race_eth,
#                                                               age_var, sex_recode,
#                                                               height_cm, weight_kg,
#                                                               BMI, BMI_derived, BMI_final)
# }

# frequency - drinking days per month
dataFiles <- lapply(dataFiles, recode_alc_frequency)

# prevalence - note not available in all years but can be derived from quant / freq vars
dataFiles <- lapply(dataFiles, recode_alc_prevalence)

# quantity- drinks per occasion and grams per day 
dataFiles <- lapply(dataFiles, recode_alc_quantity)

# hed - number of days with 4+ 5+
dataFiles <- lapply(dataFiles, recode_hed)

# recode mental health days variable - number of days in past 30 struggle with mental health
dataFiles <- lapply(dataFiles, recode_menthealth)

# recode the sample weights 
dataFiles <- lapply(dataFiles, recode_sample_weights)

# extract survey date 
dataFiles <- lapply(dataFiles, extract_date)

# select the variables needed and save the output - 
# to select new / different variables adjust this in the function "subset_data" in functions script 
dataFilesSubset <- lapply(dataFiles, subset_data)

# for(i in 1:length(dataFilesSubset)){
#   print(unique(dataFilesSubset[[i]]$YEAR))
#   print(summary(as.factor(dataFilesSubset[[i]]$surveyyear)))
# }

# save an RDS of the processed data
saveRDS(dataFilesSubset, "SIMAH_workplace/brfss/processed_data/brfss_full_selected.RDS")
