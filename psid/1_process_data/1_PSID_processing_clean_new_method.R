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

# Label the method used to imputate each persons race
race <- assign_race_method(race)
summary_race_methods <- race %>% group_by(race_method) %>% count()

## 7. Fill individual race data by individual
race <- race %>% group_by(uniqueID) %>% fill(individualrace, race_method, .direction = c("downup"))

# Review missing race data in the main survey
summary(as.factor(race$individualrace)) 
missing <- race %>% filter(is.na(individualrace)) %>% group_by(uniqueID) %>% tally()  
missing_IDS <- unique(missing$uniqueID)

# Explore discrepancies in race data
tally <- race %>% dplyr::select(uniqueID, individualrace) %>% 
  distinct() %>% 
  ungroup() %>% group_by(uniqueID) %>% tally() %>% 
  mutate(flag=ifelse(n>1,1,0))

# Inconsistent
inconsistent_IDS <- unique(subset(tally, flag==1)$uniqueID) 

# IDs of individuals who have some self-reported race data: 
self_report <- race %>% filter(uniqueID%in%inconsistent_IDS) %>%
  filter(race_method=="self reported") 
self_report_IDS <- unique(self_report$uniqueID) 

temp <- race %>% filter(uniqueID%in%self_report_IDS) %>% filter(race_method=="self reported") %>% 
  group_by(uniqueID) %>% count(individualrace) 
consistent_self_report <- temp %>% group_by(uniqueID) %>% count() %>% filter(n==1)
consistent_self_report_IDS <- consistent_self_report$uniqueID
inconsistent_self_report <- temp %>% group_by(uniqueID) %>% count() %>% filter(n>1) 
inconsistent_self_report_IDS <- inconsistent_self_report$uniqueID
  
# IDs of individuals who have some self-reported race data: 
no_self_report <- tally %>% filter(flag==1) %>% filter(!(uniqueID%in%self_report_IDS)) 
no_self_report_IDS <- unique(no_self_report$uniqueID) 

# Add a column to each individual noting if there data is consistant or not
race <- race %>% mutate(consistency = case_when((!(uniqueID%in%inconsistent_IDS)) ~ "consistent throughout",
                                                uniqueID%in%consistent_self_report_IDS ~ "inconsistent, but self-reports consistent",
                                                uniqueID%in%inconsistent_self_report_IDS | uniqueID%in%no_self_report_IDS ~ "inconsistent"))

###############################################################################
### Assign one single race for each individual, across all years, based on the main data.

# Option A.  final_race_first_obs_MAIN
# 1. If individuals have self-reported data, assign their final main race to be that
# 2. Otherwise, assign them their first observation of individualrace

# Continue from here....

race <- race %>% 
  mutate(final_race_first_obs_MAIN = ifelse(!(uniqueID%in%inconsistent_IDS), individualrace,
    ifelse(uniqueID%in%consistent_self_report_IDS & race_method=="self reported",  individualrace, NA)))
                                            
                                  
#                                             
#                                             individualrace, TAS_race))
# all_race %>% group_by(final_race_first_obs_MAIN) %>% count()

# firsteth <- race %>% 
#   mutate(firstyear = ifelse(year==min(year),1,0)) %>% 
#   filter(firstyear==1) %>% dplyr::select(-c(firstyear, year))

# Option B  final_race_priority_MAIN
# 1. If individuals have self-reported data, assign their final main race to be that
# 2. Otherwise, assign them their highest 'priority' race
 
### TBC



## 8. Process Transition to Adulthood Supplement race data
TAS_race <- process_TAS_race(data)

# nb. No individuals missing race data across all years in the TAS

### Assign one single race for each individual, across all years, in the TAS data
# nb. all TAS race data is self-reported therefore step 1 not needed

# Option A: final_race_first_obs_TAS 
# Assign them their first observation of race 




# Option B: final_race_priority_TAS
# Assign them their highest 'priority' race



###############################################################################

# Join the TAS race with the main race data 
TAS_race$year <- as.numeric(TAS_race$year)
all_race <- left_join(race, TAS_race)

# assign 'self reported' as the race method for those assigned their TAS race variable
all_race <- assign_race_method_TAS(all_race)
### Compare the TAS data and the main sample (in some cases imputed) race data

# Compare TAS race & individual race to see how many inconsistencies there are:
all_race <- all_race %>% 
  mutate(inconsistancies_TAS_main = ifelse(is.na(TAS_race), NA,
                                           ifelse(TAS_race==individualrace, 0, 1)))
all_race %>% ungroup() %>% group_by(inconsistancies_TAS_main) %>% count() 

# Assign individuals their TAS self-reported race, if this is available, creating a variable called race_new
all_race <- all_race %>% 
  mutate(race_new = ifelse(is.na(TAS_race), individualrace, TAS_race))
all_race %>% group_by(race_new) %>% count()
# 
# # Explore discrepancies in the race_new variable
# tally_race_new <- all_race %>% dplyr::select(uniqueID, race_new) %>% 
#   distinct() %>% 
#   ungroup() %>% group_by(uniqueID) %>% tally() %>% 
#   mutate(flag=ifelse(n>1,1,0))
# 
# inconsistent_race_new_IDS <- unique(subset(tally_race_new, flag==1)$uniqueID)
# inconsistent_race_new <- all_race %>% filter(uniqueID%in%inconsistent_race_new_IDS)
# inconsistent_race_new_summary <- inconsistent_race_new %>% group_by(uniqueID) %>% count(race_method) 
# # 3,370 individuals with discrepancies
# 
# has_self_report_race_new <- inconsistent_race_summary_TAS %>% filter(race_method=="self reported") 
# self_report_IDS_race_new <- unique(has_self_report_race_new$uniqueID) 
#  
# temp <- all_race %>% filter(uniqueID%in%self_report_IDS_race_new) %>% filter(race_method=="self reported") %>% 
#    group_by(uniqueID) %>% count(race_new) 
# consistant_self_report_race_new <- temp %>% group_by(uniqueID) %>% count() %>% filter(n==1) 
# inconsistent_self_report_race_new <- temp %>% group_by(uniqueID) %>% count() %>% filter(n>1) 
# 
# no_self_report_race_new <- inconsistent_race_new_summary %>% filter(!(uniqueID%in%self_report_IDS_race_new)) 
# temp <- no_self_report_race_new %>% count(uniqueID)
# no_self_report_IDS_race_new <- unique(no_self_report_race_new$uniqueID)

### Check that have just one definitive race in each column for each individual, consistent across all years


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
# uniquerace <- alldata %>% dplyr::select(uniqueID, race_new) %>% 
#   distinct() %>% 
#   group_by(uniqueID) %>% 
#   do(allocate_final_race(.))
# 
# uniquerace <- uniquerace %>% dplyr::select(uniqueID, race_new_unique) %>% 
#   distinct()
# 
# alldata <- left_join(alldata, uniquerace)
# 
# write.csv(alldata, "SIMAH_workplace/PSID/alldata_1999_2021.csv", row.names=F)
# 




