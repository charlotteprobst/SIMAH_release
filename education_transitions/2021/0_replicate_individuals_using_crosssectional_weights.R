# Script to replicate individuals using their cross-sectional weights

library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(splitstackshape)

# If running on local computer
# wd <- "C:/Users/cmp21seb/Documents/SIMAH"
# data <- read.csv("SIMAH_workplace/PSID/cleaned data/psid_data_1999_2021.csv")

# If running on the discomachine
serverwd <- "/home/sophie/"
setwd(serverwd)
data <- read.csv("inputs/psid_data_1999_2021.csv")

# Select variables of interest
data_1 <- data %>% 
  filter(age>=16 & age<=34) %>% 
  drop_na(sex, education, age, final_race_using_method_hierarchy, total_fam_income) %>% 
  dplyr::select(uniqueID, year, individualweight_cross.sectional,individualweight_longitudinal,
                sex, age, education, final_race_using_method_hierarchy, total_fam_income) %>% 
  distinct()

# Unify sample weights (model can't cope with different weights per year)
data_2 <- data_1 %>% group_by(uniqueID) %>% mutate(sampleweight = round(mean(individualweight_cross.sectional))) %>% 
  filter(sampleweight!=0)

# Divide by smallest sample weight (45) to get smaller (but still representative sample)
data_2$sample_weight <- data_2$sampleweight/45

# Replicate individuals according to their sample weight
individuals <- data_2 %>% dplyr::select(uniqueID, sampleweight) %>% distinct()
individuals <- expandRows(individuals, "sampleweight")
# Allocate new IDs for each individual
individuals$newID <- 1:nrow(individuals)
individuals$newID <- as.numeric(individuals$newID)

alldata <- merge(data_2, individuals)

# check that there are no duplicate newIDs for different original IDs
test <- alldata %>% group_by(newID,year) %>% tally()

write.csv(alldata, "outputs/new_PSID_weighted_IDs_2021.csv", row.names=F)

