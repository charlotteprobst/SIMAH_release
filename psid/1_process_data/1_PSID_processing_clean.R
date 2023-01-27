# new clean script to process PSID data

library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
setwd("~/Google Drive/SIMAH Sheffield")

# read in the data 
data <- read_excel("SIMAH_workplace/PSID/J315522/J315522.xlsx")
source("SIMAH_code/PSID/1_process_data/PSID_processing_functions.R")

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

# weightsdata <- read.dbf("SIMAH_workplace/education_transitions/J312968/J312968.dbf")

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
alldata <- left_join(age, race) %>% left_join(., education) %>% 
  left_join(., relationship) %>% left_join(.,alcohol) %>% 
  left_join(., kessler) %>% left_join(., sampleweights) %>% 
  left_join(., employment) %>% left_join(., income) %>% 
  left_join(.,homeowner)

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

alldata <- alldata %>% 
  select(uniqueID, year, relationship, sex, age, education_cat, weight,
         employment_stat,total_fam_income, homeowner,
         individualrace, kessler_score, distress_severe, distress_class,
         frequency, drinkingstatus, quantity, AlcCAT, gpd, bingedrinkdays) %>% 
  filter(year>=1999) %>% 
  group_by(uniqueID) %>% 
  fill(weight, .direction=c("downup")) %>% 
  fill(education_cat, .direction=c("downup")) %>% mutate(weight=mean(weight, na.rm=T))

# remove nonresponders 
nonresponse <- read.dbf("SIMAH_workplace/education_transitions/J312243/J312243.dbf") %>% 
  mutate(familyID = ER30001,
         ID = ER30002,
         uniqueID = familyID*1000 + ID,
         year_nonresponse = ER32007) %>% 
  dplyr::select(uniqueID, familyID, year_nonresponse)

alldata <- left_join(alldata, nonresponse) %>%
  mutate(toremove = ifelse(year_nonresponse<=year, 1,0))

write.csv(alldata, "SIMAH_workplace/PSID/alldata_new_1999_2019.csv", row.names=F)



library(naniar)

# alldata <- alldata %>% filter(year>=1999) %>% 
#   filter(age>=18 & age<=34) %>% filter(sampleweight!=0)

length(unique(alldata$uniqueID))

missinged <- alldata[is.na(alldata$education),]
length(unique(missinged$uniqueID))

missingrace <- alldata[is.na(alldata$individualrace),]
length(unique(missingrace$uniqueID))

missingparent <- alldata[is.na(alldata$onecollegeplus),]
length(unique(missingparent$uniqueID))


missing_summary <- alldata %>% group_by(year) %>% 
  miss_var_summary() 

length(unique(alldata$uniqueID))

alldata <- alldata %>% drop_na(individualrace, education_cat, onecollegeplus)

length(unique(alldata$uniqueID))

library(splitstackshape)
alldata <- alldata %>% group_by(uniqueID) %>% mutate(sampleweight = mean(weight)) %>% filter(sampleweight!=0)

alldata <- alldata %>% filter(age>=18 & age<=34)

length(unique(alldata$uniqueID))

alldata$timeperiod <- ifelse(alldata$year<=2009, 1,
                             ifelse(alldata$year>=2009, 0, NA))

alldata %>% group_by(timeperiod) %>% summarise(n=length(unique(uniqueID)))

alldata %>% group_by(timeperiod) %>% summarise(meanage = mean(age))

alldata %>% group_by(timeperiod, year, individualrace) %>% tally() %>% ungroup() %>% 
  group_by(timeperiod, year) %>% 
  mutate(n/sum(n))

alldata <- expandRows(alldata, "sampleweight")

write.csv(alldata, "SIMAH_workplace/education_transitions/new_PSID_processed_weighted.csv")

length(unique(alldata$uniqueID))
