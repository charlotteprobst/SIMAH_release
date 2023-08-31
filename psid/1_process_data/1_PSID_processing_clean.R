# clean script to process PSID data
library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
setwd("C:/Users/cmp21seb/Documents/SIMAH/")

# read in the data 
data <- read_excel("SIMAH_workplace/PSID/Full_2021/extract_v2/J323498.xlsx")

# Source existing PSID processing functions
source("SIMAH_code/PSID/1_process_data/PSID_processing_functions.R")

# Recode static variables
data$familyID <- data$ER30001
data$ID <- data$ER30002
data$uniqueID <- (data$familyID*1000) + data$ID

data$IDmother = ifelse(data$ER32010==0, NA, 
                       ifelse(data$ER32010>=800 & data$ER32010<=999, NA,
                  (data$ER32010*1000) + data$ER30002))

data$IDfather = ifelse(data$ER32017==0, NA,
                       ifelse(data$ER32017>=800 & data$ER32017<=999, NA,
                  (data$ER32017*1000) + data$ER30002))

# Process sex data
data$sex <- data$ER32000
data$sex <- recode(as.factor(data$sex), "1"="male", "2"="female")

# Process educational attainment data
education <- process_education(data)

# process age data 
age <- process_age(data)

# process relationship to householder data
relationship <- process_relationship(data)

# weightsdata <- read.dbf("SIMAH_workplace/education_transitions/J312968/J312968.dbf") ??

# process sampling weights 
sampleweights <- process_sample_weights(data)

# process race and ethnicity 
race <- process_race(data)

# remove individuals with same uniqueID for person and mother - 0 people
race <- race %>% mutate(match=ifelse(uniqueID==IDmother,1,0),
                         match = ifelse(is.na(match),0,match)) %>% 
  filter(match==0)

# recode race and ethnicity - based on householder / wife  and mothers / fathers race and ethnicity
race <- left_join(race, relationship)

# recode race based on householder - race is reported for head and "wife" of householder  
race <- recode_race(race, T)   ###  what does this T mean?

race <- individual_race(race, T)

race <- code_race_parents(race)

race <- recode_race(race, F)

summary(as.factor(race$individualrace))

race <- individual_race(race, F)

summary(as.factor(race$individualrace))

race <- race %>% dplyr::select(uniqueID, year, sex, individualrace) %>% 
  group_by(uniqueID) %>% 
  fill(individualrace, .direction=c("downup"))

summary(as.factor(race$individualrace))

# now get a definitive race for each individual - see if there are any discrepancies
tally <- race %>% dplyr::select(uniqueID, sex, individualrace) %>% 
  distinct() %>% 
  ungroup() %>% group_by(uniqueID) %>% tally() %>% 
  mutate(flag=ifelse(n>1,1,0))

IDS <- unique(subset(tally, flag==1)$uniqueID)

# just take each individuals first observation of race and ethnicity?
firsteth <- race %>% 
  mutate(firstyear = ifelse(year==min(year),1,0)) %>% 
  filter(firstyear==1) %>% dplyr::select(-c(firstyear, year))
  
# process parents educational attainment
parented <- process_parent_ed(data)

# recode parents educational attainment - based on what is available 
# first join together parent education and relation to householder 
parented <- left_join(parented, relationship)

parented <- left_join(parented, education)

parented <- process_parent_ed_data(parented)

parented <- code_education_parent(parented)

# kessler score 
kessler <- process_kessler(data)

# alcohol data
alcohol <- process_alcohol(data)

# employment status
employment <- process_employment(data)

# process income 
income <- process_income(data)

# process home ownership 
homeowner <- process_homeowner(data)

# now combine all the data together and look at missingness
alldata <- left_join(education, firsteth) %>% left_join(., age) %>% 
  left_join(., relationship) %>% left_join(.,alcohol) %>% 
  left_join(., kessler) %>% left_join(., sampleweights) %>% 
  left_join(., employment) %>% left_join(., income) %>% 
  left_join(.,homeowner) %>% left_join(., parented)

# recode alcohol and kessler values 
alldata <- recode_alcohol(alldata)

##Creating categorized variables based on the Kessler Scale##
##Classification based on paper by Prochaska et al. 2012-Validity study of the K6 scale as ameasure of moderate mental distressbased on mental health treatment needand utilization
alldata$distress_severe <- ifelse(alldata$kessler_score>=13, "Yes",
                                 ifelse(alldata$kessler_score<13, "No", NA))
alldata$distress_class <- ifelse(alldata$kessler_score<5, "Low or none",
                                ifelse(alldata$kessler_score>=5 & alldata$kessler_score<13, "Moderate",
                                       ifelse(alldata$kessler_score>=13, "Severe", NA)))  
summary(as.factor(alldata$distress_severe))
summary(as.factor(alldata$distress_class))

alldata$age <- alldata$year - alldata$birthyear

alldata <- alldata %>% 
  select(uniqueID, year, relationship, sex, age, education, education_cat, weight,
         employment_stat,total_fam_income, homeowner,
         mothers_ed_final, fathers_ed_final,
         individualrace, kessler_score, distress_severe, distress_class,
         frequency, drinkingstatus, quantity, AlcCAT, gpd, bingedrinkdays) %>% 
  filter(year>=1999) %>% 
  group_by(uniqueID) %>% 
  fill(weight, .direction=c("downup")) %>% 
  fill(education_cat, .direction=c("downup")) %>% mutate(weight=mean(weight, na.rm=T))

# remove nonresponders ?
# nonresponse <- read.dbf("SIMAH_workplace/education_transitions/J312243/J312243.dbf") %>%
#   mutate(familyID = ER30001,
#          ID = ER30002,
#          uniqueID = familyID*1000 + ID,
#          year_nonresponse = ER32007) %>%
#   dplyr::select(uniqueID, familyID, year_nonresponse)
# 
# alldata <- left_join(alldata, nonresponse) %>%
#   mutate(toremove = ifelse(year_nonresponse<=year, 1,0))

write.csv(alldata, "SIMAH_workplace/PSID/alldata_new_1999_2021.csv", row.names=F)
