# clean script to process PSID data
library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
setwd("C:/Users/cmp21seb/Documents/SIMAH/")

# read in the data 
data <- read_excel("SIMAH_workplace/PSID/Full_2021/extract_v3/J324498.xlsx")

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
race <- process_race(data) #1.

# remove individuals with the same uniqueID for person and mother - 0 people
race <- race %>% mutate(match=ifelse(uniqueID==IDmother,1,0),
                         match = ifelse(is.na(match),0,match)) %>% 
  filter(match==0)

# Summarise number of individuals missing race and ethnicity data...
# See other script

race <- left_join(race, relationship)

# recode race based on the race of the head and/or wife, generating 2 variables:
# racefamily_both_known <- when both the head and wife have data
# racefamily_one_known <- when either the head or wife have data
race <- family_race_head_wife(race)

# Review number of individuals with racefamily data
summary(as.factor(race$racefamily_both_known))# Total NA = 152,115 out of 200,592
summary(as.factor(race$racefamily_one_known)) # Total NA = 95,306

race <- individual_race_head(race) #3. assign individuals a race based on racefamily_head

# recode race based on parents race if race of head or wife unknown
race <- code_race_parents(race) #4.
race <- family_race_parents(race) #5. generate variable racefamily_parents
race <- individual_race_parents(race) #6. assign individuals a race based on racefamily_parents

summary(as.factor(race$individualrace))

race <- race %>% dplyr::select(uniqueID, year, sex, individualrace) %>% 
  group_by(uniqueID) %>% 
  fill(individualrace, .direction=c("downup"))

summary(as.factor(race$individualrace))
# Total NA now 23,635 out of 200,592

# now get a definitive race for each individual - see if there are any discrepancies
tally <- race %>% dplyr::select(uniqueID, sex, individualrace) %>% 
  distinct() %>% 
  ungroup() %>% group_by(uniqueID) %>% tally() %>% 
  mutate(flag=ifelse(n>1,1,0))

IDS <- unique(subset(tally, flag==1)$uniqueID) 

# just take each individuals first observation of race and ethnicity
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
maindata <- left_join(education, firsteth) %>% left_join(., age) %>% 
  left_join(., relationship) %>% left_join(.,alcohol) %>% 
  left_join(., kessler) %>% left_join(., sampleweights) %>% 
  left_join(., employment) %>% left_join(., income) %>% 
  left_join(.,homeowner) %>% left_join(., parented)

# recode alcohol and kessler values 
maindata <- recode_alcohol(maindata)

##Creating categorized variables based on the Kessler Scale##
##Classification based on paper by Prochaska et al. 2012-Validity study of the K6 scale as ameasure of moderate mental distressbased on mental health treatment needand utilization
maindata$distress_severe <- ifelse(maindata$kessler_score>=13, "Yes",
                                 ifelse(maindata$kessler_score<13, "No", NA))
maindata$distress_class <- ifelse(maindata$kessler_score<5, "Low or none",
                                ifelse(maindata$kessler_score>=5 & maindata$kessler_score<13, "Moderate",
                                       ifelse(maindata$kessler_score>=13, "Severe", NA)))  
summary(as.factor(maindata$distress_severe))
summary(as.factor(maindata$distress_class))

maindata$age <- maindata$year - maindata$birthyear

maindata <- maindata %>% 
  # select(uniqueID, year, relationship, sex, age, education, education_cat, weight,
  #        employment_stat,total_fam_income, homeowner,
  #        mothers_ed_final, fathers_ed_final,
  #        individualrace, kessler_score, distress_severe, distress_class,
  #        frequency, drinkingstatus, quantity, AlcCAT, gpd, bingedrinkdays) %>% 
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
# maindata <- left_join(maindata, nonresponse) %>%
#   mutate(toremove = ifelse(year_nonresponse<=year, 1,0))

write.csv(maindata, "SIMAH_workplace/PSID/maindata_1999_2021.csv", row.names=F)

### Clean the TAS data

# Process TAS race data
race <- process_TAS_race(data)

# Process TAS education data
education <- process_TAS_education(data) %>% distinct()

# Generate clean TAS datafile, merging the processed race and education data files
TAS <- merge(education, race, by=c("uniqueID"))
TAS$year <- as.integer(TAS$year)
TAS$uniqueID <- as.integer(TAS$uniqueID)

# save the TAS 
write.csv(TAS, "SIMAH_workplace/PSID/TAS_2011_2019.csv", row.names=F)

# Select all unique individuals from the TAS and their race
TAS <- TAS %>% dplyr::select(uniqueID, TAS_race) %>% distinct()

# join these with the main sample data 
alldata <- left_join(maindata, TAS)

# Fill individuals race data for each year (based on the year their race was recorded)
alldata <- alldata %>% 
  group_by(uniqueID) %>% 
  fill(individualrace, .direction=c("downup")) %>% 
  fill(TAS_race, .direction=c("downup")) %>% 
  mutate(race_new = ifelse(is.na(TAS_race), individualrace, TAS_race))

# Check the face validity by viewing the data for one individual only
test <- alldata %>% filter(uniqueID==53042)

# ensure that each person has a unique value for race and ethnicity over time
race_eth_function <- function(data){
  races <- unique(data$race_new)
  # recode according to hierarchy - black, hispanic, native american, asian/pi, other
  newrace <- ifelse("black" %in% races, "black",
                    ifelse("hispanic" %in% races, "hispanic", 
                           ifelse("Native" %in% races, "Native",
                                  ifelse("Asian/PI" %in% races, "Asian/PI",
                                         ifelse("other" %in% races, "other",
                                                ifelse("white" %in% races, "white", NA))))))
  data$race_new_unique <- newrace
  return(data)
}

uniquerace <- alldata %>% dplyr::select(uniqueID, race_new) %>% 
  distinct() %>% 
  group_by(uniqueID) %>% 
  do(race_eth_function(.))

uniquerace <- uniquerace %>% dplyr::select(uniqueID, race_new_unique) %>% 
  distinct()

alldata <- left_join(alldata, uniquerace)

# write new version of df containing TAS race and education 
write.csv(alldata, "SIMAH_workplace/PSID/alldata_1999_2019.csv", row.names=F)








