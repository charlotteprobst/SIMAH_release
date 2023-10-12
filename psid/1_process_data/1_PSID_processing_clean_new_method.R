# clean script to process PSID data
library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
setwd("C:/Users/cmp21seb/Documents/SIMAH/")

# read in the data 
data <- read_excel("SIMAH_workplace/PSID/Raw_data/Full_2021/J324498.xlsx")

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
education <- process_education(data) # main survey
TAS_education <- process_TAS_education(data) %>% distinct() # Transition to Adulthood Supplement (TAS)

# process age data 
age <- process_age(data)

# process relationship to householder data
relationship <- process_relationship(data)
# review relationship data
relationship %>% group_by(relationship) %>% count()

# process sampling weights 
# weightsdata <- read.dbf("SIMAH_workplace/education_transitions/J312968/J312968.dbf") ??
sampleweights <- process_sample_weights(data)

### Process and assign race and ethnicity
 
## 1. process race and ethnicity data
race <- process_race(data)

# remove individuals with the same uniqueID for person and mother - 0 people
race <- race %>% mutate(match=ifelse(uniqueID==IDmother,1,0),
                         match = ifelse(is.na(match),0,match)) %>% 
  filter(match==0)
race <- left_join(race, relationship)

## 2. Generate family race variable based on the race of the head and/or wife
race <- generate_family_race(race)

# Review number of individuals with family race data
summary(as.factor(race$racefamily_both_known))# Total NA = 152,115 out of 200,592
summary(as.factor(race$racefamily_best_guess)) # Total NA = 46,185

## 3. Assign individuals their family race
race <- assign_individual_family_race(race)

## 4. Process the race and ethnicity data of an individual's parents (based on record linkage)
race <- process_race_parents(race)

## 5. Generate an overarching race variable for the parents
race <- generate_race_parents(race)

## 6. Assign any individuals with missing 'individual race' data, the parents race
race <- assign_individual_race_parents(race) 

## 7. Assign the method for imputation of race
race <- assign_race_method(race)
summary_race_methods <- race %>% group_by(relationship, race_method) %>% count()
write.csv(summary_race_methods, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/PSID/Results/Demographics/summary of race methods.csv")

# condense the race dataset
# race <- race %>% dplyr::select(uniqueID, year, sex, individualrace) 

# Review missing race data in the main survey
summary(as.factor(race$individualrace)) # 23,142 NA observations
missing <- race %>% filter(is.na(individualrace)) %>% group_by(uniqueID) %>% tally()  # 551 individuals
missing_IDS <- unique(missing$uniqueID)

# Explore discrepancies in race data
tally <- race %>% dplyr::select(uniqueID, sex, individualrace) %>% 
  distinct() %>% 
  ungroup() %>% group_by(uniqueID) %>% tally() %>% 
  mutate(flag=ifelse(n>1,1,0))

inconsistant_IDS <- unique(subset(tally, flag==1)$uniqueID) 
inconsistant_race <- race %>% filter(uniqueID%in%inconsistant_IDS)
inconsistant_race_summary <- inconsistant_race %>% group_by(uniqueID) %>% count(race_method) 
# 392 individuals with discrepancies

has_self_report <- inconsistant_race_summary %>% filter(race_method=="self reported") 
self_report_IDS <- unique(has_self_report$uniqueID) # 392 individuals with discrepancies
# 303 of those with inconsistant data have self reported data available

temp <- race %>% filter(uniqueID%in%self_report_IDS) %>% filter(race_method=="self reported") %>% 
  group_by(uniqueID) %>% count(individualrace) 
consistant_self_report <- temp %>% group_by(uniqueID) %>% count() %>% filter(n==1) # 289
inconsistant_self_report <- temp %>% group_by(uniqueID) %>% count() %>% filter(n>1) # 14
# 14 of those with self reported data, have inconsistencies within their self-reported data

no_self_report <- inconsistant_race_summary %>% filter(!(uniqueID%in%self_report_IDS)) 
temp <- no_self_report %>% count(uniqueID)
no_self_report_IDS <- unique(no_self_report$uniqueID)
# All 89 others either change between NA (i.e. imputed from another year or unavailable) or 
# switch from one method to another (e.g. imputed based on nearest family member and then reported by head)

## 8. Process Transition to Adulthood Supplement race data
TAS_race <- process_TAS_race(data)

# Explore missing race data in the TAS
summary(as.factor(TAS_race$TAS_race)) # Nil NA in TAS

# join the TAS race with the main race data 
TAS_race$year <- as.numeric(TAS_race$year)
all_race <- left_join(race, TAS_race)

###############################################################################
# 38,208 observations of race from the TAS

# Assign individuals their TAS self-reported race, if this is available, creating a variable called race_new
all_race <- all_race %>% 
  mutate(race_new = ifelse(is.na(TAS_race), individualrace, TAS_race))
all_race %>% group_by(race_new) %>% count()

# Compare TAS race & individual race to see how many inconsistencies there are:
all_race <- all_race %>% 
  mutate(inconsistancies_TAS_main = ifelse(is.na(TAS_race), NA,
                                           ifelse(TAS_race==individualrace, 0, 1)))

all_race %>% ungroup() %>% group_by(inconsistancies_TAS_main) %>% count() 
# consistant in 25,176 cases
# inconsistant in 8,624 cases # 34%
# note total does not add to 38,208 because some individuals in TAS are not within the all_race dataset??

# Fill individuals race data for each year if not already filled
all_race <- all_race %>%
  group_by(uniqueID) %>%
  fill(individualrace, .direction=c("downup")) %>%
  fill(TAS_race, .direction=c("downup"))

# Re-compare TAS_race & individual race to see how many inconsistencies there are:
all_race <- all_race %>% 
  mutate(inconsistancies_TAS_main = ifelse(is.na(TAS_race), NA,
                                           ifelse(TAS_race==individualrace, 0, 1)))
all_race %>% ungroup() %>% group_by(inconsistancies_TAS_main) %>% count() 
# consistant in 141965 cases
# inconsistant in 35485 cases # 25%

# Need to look into when we are doing filling so that can be sure to compare like for like?# 
############################################################################################

### Get a definitive race for each individual, consistent across all years and then compare consistency again

# 1. By taking each individuals first observation of race and ethnicity
firsteth <- race %>% 
  mutate(firstyear = ifelse(year==min(year),1,0)) %>% 
  filter(firstyear==1) %>% dplyr::select(-c(firstyear, year))

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
# maindata <- left_join(education, firsteth) %>% left_join(., age) %>% 
#   left_join(., relationship) %>% left_join(.,alcohol) %>% 
#   left_join(., kessler) %>% left_join(., sampleweights) %>% 
#   left_join(., employment) %>% left_join(., income) %>% 
#   left_join(.,homeowner)

maindata <- left_join(education, all_race) %>% left_join(., age) %>% 
  left_join(., relationship) %>% left_join(.,alcohol) %>% 
  left_join(., kessler) %>% left_join(., sampleweights) %>% 
  left_join(., employment) %>% left_join(., income) %>% 
  left_join(.,homeowner)



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



################################# race processing cont.

# Fill individuals race data for each year (based on the year their race was recorded)
alldata <- alldata %>% 
  group_by(uniqueID) %>% 
  fill(individualrace, .direction=c("downup")) %>% 
  fill(TAS_race, .direction=c("downup")) %>% 
  mutate(race_new = ifelse(is.na(TAS_race), individualrace, TAS_race))

# Check the face validity by viewing the data for one individual only
test <- alldata %>% filter(uniqueID==53042)

##### 8. Allocate final race
# ensure that each person has a unique value for race and ethnicity over time
uniquerace <- alldata %>% dplyr::select(uniqueID, race_new) %>% 
  distinct() %>% 
  group_by(uniqueID) %>% 
  do(allocate_final_race(.))

uniquerace <- uniquerace %>% dplyr::select(uniqueID, race_new_unique) %>% 
  distinct()

alldata <- left_join(alldata, uniquerace)

write.csv(alldata, "SIMAH_workplace/PSID/alldata_1999_2021.csv", row.names=F)





