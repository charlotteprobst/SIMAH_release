# Script to replicate individuals using their cross-sectional weights

library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(splitstackshape)
library(data.table)

# If running on local computer
setwd("C:/Users/cmp21seb/Documents/SIMAH/")
data <- read.csv("SIMAH_workplace/PSID/cleaned data/psid_data_1999_2021_050424.csv")

# If running on the discomachine
# serverwd <- "/home/sophie/"
# setwd(serverwd)
# data <- read.csv("inputs/psid_data_1999_2021.csv")

# Filter only survey respondents or people with TAS alc data & year >= 2005 
data <- data %>% filter((relationship=="head"|!is.na(AlcCAT_TAS)) & year>=2005) 

# Drop people with no alc data
data <- data %>% drop_na(gpd)

# Generate a final alcohol category based on TAS and main survey data
data <- data %>% mutate(final_alc_cat=if_else(is.na(AlcCAT_TAS), 
                                              AlcCAT, 
                                              AlcCAT_TAS))

# Select variables of interest
data <- data %>% 
  filter(age>=18) %>%
  drop_na(sex, education, age, final_race_using_method_hierarchy) %>%
  dplyr::select(uniqueID, year, individualweight_cross.sectional,
                sex, age, education, final_alc_cat, gpd, AlcCAT_TAS,
                final_race_using_method_hierarchy) %>%
  distinct()

# Unify sample weights (model can't cope with different weights per year)
data <- data %>% group_by(uniqueID) %>% mutate(sampleweight = round(mean(individualweight_cross.sectional))) %>% 
  filter(sampleweight!=0)

# Identify smallest sample weight
min(data$sampleweight) # 47

# Divide by smallest sample weight (47) to get smaller (but still representative sample)
data$sample_weight <- data$sampleweight/47

replicated_df <- data %>%
  mutate(replicates = round(sampleweight)) %>%
  uncount(replicates)

replicated_df$newID <- 1:nrow(replicated_df)
replicated_df$newID <- as.numeric(replicated_df$newID)

# check that there are no duplicate newIDs for different original IDs
#test <- replicated_df %>% group_by(newID,year) %>% tally() # Too long to run

install.packages("feather")
library(feather)
write_feather(replicated_df, "SIMAH_workplace/alcohol_transitions_psid/psid_data_2005_2021_replicated_for_alc_transitions.feather")

