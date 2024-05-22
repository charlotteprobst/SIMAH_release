
# SIMAH - NESARC Alcohol Transitions
# Regression model estimation

# Load packages and data
library(tidyverse)  # data management

# Specify the data and output file locations
data    <- "~/Google Drive/SIMAH Sheffield/SIMAH_workplace/nesarc/Processed data/"  # Location of data
models  <- "~/Google Drive/SIMAH Sheffield/SIMAH_workplace/nesarc/Models/"          # Location of saved MSM models

# Load data / functions
nesarc_expanded <- readRDS(paste0(data, "nesarc_clean_expanded_new.rds")) 

# select the variables that are needed 
nesarc_selected <- nesarc_expanded %>% 
  dplyr::select(idnum, wave, years, age,
                female.factor, race.factor,
                edu3, alc_daily_g, alc5.factor) %>% 
  mutate(formerdrinker = ifelse(alc5.factor=="Former", 1,0)) %>% 
  pivot_wider(names_from=wave, 
              values_from=c(years,age,female.factor,race.factor, edu3,
                            alc_daily_g, alc5.factor,formerdrinker))


