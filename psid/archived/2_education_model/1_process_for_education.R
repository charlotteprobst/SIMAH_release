
library(splitstackshape)
library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
setwd("~/Google Drive/SIMAH Sheffield")

alldata <- read_csv("SIMAH_workplace/PSID/psid_data_1999_2021.csv") %>% 
  filter(age>=18 & age<=34) %>% 
  drop_na(sex, education, age, final_race_using_priority_order, `individualweight_cross-sectional`) %>% 
  dplyr::select(uniqueID, year, `individualweight_cross-sectional`, individualweight_longitudinal, sex, age, education, 
                final_race_using_priority_order, final_race_using_method_hierarchy) %>% 
  distinct()

# unify sample weights (model can't cope with different weights per year)
alldata <- alldata %>% group_by(uniqueID) %>% mutate(sampleweight = round(mean(`individualweight_cross-sectional`))) %>% filter(sampleweight!=0)

alldata <- alldata %>% distinct()

# newIDS lookup for after expansion
# extract all unique individual IDS and number of replications they will have
individuals <- alldata %>% dplyr::select(uniqueID, sampleweight) %>% distinct()
summary(alldata$sampleweight)
individuals$sampleweight <- individuals$sampleweight/45
individuals <- expandRows(individuals, "sampleweight")
individuals$newID <- 1:nrow(individuals)

# alldata <- expandRows(alldata, "sampleweight")

alldata <- merge(alldata, individuals)

alldata$newID <- as.numeric(alldata$newID)
# # # # # # # # check that there are no duplicate newIDs for different original IDs
# test <- alldata %>% group_by(newID,year) %>% tally()
# #
write.csv(alldata, "SIMAH_workplace/education_transitions/new_PSID_weighted_IDs_2021.csv", row.names=F)



