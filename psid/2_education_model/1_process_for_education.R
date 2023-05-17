
library(splitstackshape)
library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
setwd("~/Google Drive/SIMAH Sheffield")

alldata <- read_csv("SIMAH_workplace/PSID/alldata_new_1999_2019.csv") %>% 
  filter(age>=16 & age<=34) %>% 
  drop_na(sex, education, age, race_new_unique, total_fam_income) %>% 
  dplyr::select(uniqueID, year, weight, sex, age, education, race_new_unique, total_fam_income) %>% 
  distinct()

# unify sample weights (model can't cope with different weights per year)
alldata <- alldata %>% group_by(uniqueID) %>% mutate(sampleweight = round(mean(weight))) %>% filter(sampleweight!=0)

alldata <- alldata %>% distinct()

# newIDS lookup for after expansion
# extract all unique individual IDS and number of replications they will have
individuals <- alldata %>% dplyr::select(uniqueID, sampleweight) %>% distinct()
individuals <- expandRows(individuals, "sampleweight")
individuals$newID <- 1:nrow(individuals)

# alldata <- expandRows(alldata, "sampleweight")

alldata <- merge(alldata, individuals)

write.csv(alldata, "SIMAH_workplace/education_transitions/new_PSID_processed_weighted.csv")
alldata <- read.csv("SIMAH_workplace/education_transitions/new_PSID_processed_weighted.csv")
# #
alldata$newID <- as.numeric(alldata$newID)
# # # # # # # # check that there are no duplicate newIDs for different original IDs
test <- alldata %>% group_by(newID,year) %>% tally()
# #
write.csv(alldata, "SIMAH_workplace/education_transitions/new_PSID_weighted_IDs.csv", row.names=F)
