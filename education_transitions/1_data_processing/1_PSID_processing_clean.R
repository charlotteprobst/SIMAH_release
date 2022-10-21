# new clean script to process PSID data

library(foreign)
library(dplyr)
library(tidyr)
setwd("~/Google Drive/SIMAH Sheffield")

# read in the data 
data <- read.dbf("SIMAH_workplace/education_transitions/J312239/J312239.dbf")
source("SIMAH_code/education_transitions/1_data_processing/PSID_processing_functions.R")

data$familyID <- data$ER30001
data$ID <- data$ER30002
data$uniqueID <- (data$familyID*1000) + data$ID
data$IDmother = ifelse(data$ER32010==0, NA, 
                       ifelse(data$ER32010==999, NA,
                  (data$ER32010*1000) + data$ER30002))
data$IDfather = ifelse(data$ER32017==0, NA,
                       ifelse(data$ER32017==999, NA,
                  (data$ER32017*1000) + data$ER30002))
data$sex <- data$ER32000
data$sex <- recode(as.factor(data$sex), "1"="male", "2"="female")

# process educational attainment data
education <- process_education(data)

# process age data 
age <- process_age(data)

# process relationship to householder data
relationship <- process_relationship(data)

# process sampling weights 
sampleweights <- process_sample_weights(data)

# process race and ethnicity 
race <- process_race(data)

# recode race and ethnicity - based on householder / wife  and mothers / fathers race and ethnicity
race <- left_join(race, relationship)

race <- recode_race(race, T)

race <- individual_race(race, T)

race <- code_race_parents(race)

race <- recode_race(race, F)

summary(as.factor(race$individualrace))

race <- individual_race(race, F)

summary(as.factor(race$individualrace))

# process parents educational attainment
parented <- process_parent_ed(data)

# recode parents educational attainment - based on what is available 
# first join together parent education and relation to householder 
parented <- left_join(parented, relationship)

parented <- left_join(parented, education)

parented <- code_education_parent(parented)

# now combine all the data together and look at missingness
alldata <- expand.grid(uniqueID = unique(data$uniqueID), year=unique(race$year))

education <- education %>% dplyr::select(uniqueID, year, education, education_cat, education_cat_detailed)

race <- race %>% dplyr::select(uniqueID, year, sex, relationship, individualrace)

alldata <- left_join(alldata, education)

alldata <- left_join(alldata, race)

alldata <- left_join(alldata, parented)

alldata <- left_join(alldata, sampleweights)

alldata <- left_join(alldata, age)

# now subset by the population that we need for the transition probabilities
# aged 34 and under and containing sample weights and after year 1999

alldata <- alldata %>% filter(age <= 34) %>% filter(year>=1999) %>% drop_na(sampleweight)

# remove nonresponders 
nonresponse <- read.dbf("SIMAH_workplace/education_transitions/J312243/J312243.dbf") %>% 
  mutate(familyID = ER30001,
         ID = ER30002,
         uniqueID = familyID*1000 + ID,
         year_nonresponse = ER32007)
  
alldata <- left_join(alldata, nonresponse) %>% 
  mutate(toremove = ifelse(year_nonresponse==year, 1,0)) %>% filter(toremove==0) %>% 
  filter(sampleweight!=0)

library(naniar)

missing_summary <- alldata %>% group_by(year) %>% 
  miss_var_summary() 

alldata <- alldata %>% drop_na()

library(splitstackshape)
alldata <- alldata %>% group_by(uniqueID) %>% mutate(sampleweight = mean(sampleweight))

alldata <- expandRows(alldata, "sampleweight")

write.csv(alldata, "SIMAH_workplace/education_transitions/new_PSID_processed_weighted.csv")


