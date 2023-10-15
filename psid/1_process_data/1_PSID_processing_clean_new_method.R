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

# Count number of individuals in dataset
data %>% group_by(uniqueID) %>% count() # 4,776

# Process educational attainment data
education <- process_education(data) # main survey
TAS_education <- process_TAS_education(data) %>% distinct() # Transition to Adulthood Supplement (TAS)

# process age data 
age <- process_age(data) # nb only returns birthyear.  Sample only seems to have people born after 1982

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

# Add a column to each individual noting if there data is consistent or not
race <- race %>% mutate(consistency = case_when((!(uniqueID%in%inconsistent_IDS)) ~ "consistent throughout",
                                                uniqueID%in%consistent_self_report_IDS ~ "inconsistent, but self-reports consistent",
                                                uniqueID%in%inconsistent_self_report_IDS | uniqueID%in%no_self_report_IDS ~ "inconsistent"))

###############################################################################
### Assign one single race for each individual, across all years, based on the main data.

## Option A.  final_race_first_year

# Add an indicator of the first year a person has race data
race <- race %>% group_by(uniqueID) %>%
  mutate(firstyear = ifelse(year==min(year),1,0))

# If individuals have consistent data assign them that
race <- race %>% 
  mutate(final_race_first_year = ifelse(consistency=="consistent throughout", individualrace, 
# If individual has inconsistent data, but consistent self report, assign them that        
ifelse(consistency=="inconsistent, but self-reports consistent" & race_method=="self reported",  individualrace,  
# Otherwise, assign them their first observation of individualrace                   
ifelse(consistency=="inconsistent" & firstyear==1, individualrace, NA)))) %>%
group_by(uniqueID) %>% fill(final_race_first_year, .direction="downup")

first_year_summary_main <- race %>% group_by(uniqueID) %>% 
  distinct(final_race_first_year) %>% ungroup() %>% 
  count(final_race_first_year)

## Option B  final_race_priority_MAIN

# Add an indicator of the highest priority race of an individual
race <- race %>% ungroup() %>%  mutate(
  priority_rank = case_when(individualrace=="hispanic" ~ 1,
                         individualrace=="black" ~ 2,
                         individualrace=="Native" ~ 3,
                         individualrace=="Asian/PI" ~ 4,
                         individualrace=="other" ~ 5,
                         individualrace=="white" ~ 6))

race <- race %>% group_by(uniqueID) %>%
  mutate(highest_priority_race = ifelse(priority_rank==min(priority_rank),1,0))

# If individuals have consistent data assign them that
race <- race %>% 
  mutate(final_race_highest_priority = ifelse(consistency=="consistent throughout", individualrace, 
# If individual has inconsistent data, but consistent self report, assign them that        
ifelse(consistency=="inconsistent, but self-reports consistent" & race_method=="self reported",  individualrace,  
# Otherwise, assign them their highest priority race                  
ifelse(consistency=="inconsistent" & highest_priority_race==1, individualrace, NA)))) %>%
group_by(uniqueID) %>% fill(final_race_highest_priority, .direction="downup")

highest_priority_summary_main <- race %>% group_by(uniqueID) %>% 
  distinct(final_race_highest_priority) %>% ungroup() %>% 
  count(final_race_highest_priority)

## 8. Process Transition to Adulthood Supplement race data
TAS_race <- process_TAS_race(data)

# number of individuals in the TAS data:
TAS_race %>% group_by(uniqueID) %>% count() # 4766 (no individuals with zero race data)

# Explore discrepancies in TAS race data
TAS_tally <- TAS_race %>% drop_na() %>% dplyr::select(uniqueID, TAS_race) %>% 
  distinct() %>% 
  ungroup() %>% group_by(uniqueID) %>% tally() %>% 
  mutate(flag=ifelse(n>1,1,0))

TAS_race <- left_join(TAS_race, TAS_tally)

# Inconsistent
inconsistent_IDS_TAS <- unique(subset(TAS_tally, flag==1)$uniqueID) 

### Assign one single race for each individual, across all years, in the TAS data
# nb. all TAS race data is self-reported therefore step 1 not needed

# Option A: final_race_first_year_TAS 
# Assign them their first observation of race 

TAS_first_year <- TAS_race %>% drop_na() %>% group_by(uniqueID) %>%
  mutate(firstyear_TAS = ifelse(year==min(year),1,0))

TAS_race <- left_join(TAS_race, TAS_first_year)

# If individuals have consistent data assign them that
TAS_race <- TAS_race %>% 
   mutate(final_race_first_year_TAS = ifelse(flag==0, TAS_race, 
# If individual has inconsistent data,assign them their first observation of TAS_race                   
          ifelse(flag==1 & firstyear_TAS==1, TAS_race, NA))) %>%
  fill(final_race_first_year_TAS, .direction="downup")

first_year_summary_TAS <- TAS_race %>% group_by(uniqueID) %>% 
   distinct(final_race_first_year_TAS) %>% ungroup() %>% 
   count(final_race_first_year_TAS)

# Option B: final_race_priority_TAS
# Assign them their highest 'priority' race

# Add an indicator of the highest priority race of an individual
TAS_race <- TAS_race %>% ungroup() %>%  mutate(
  priority_rank = case_when(TAS_race=="hispanic" ~ 1,
                            TAS_race=="black" ~ 2,
                            TAS_race=="Native" ~ 3,
                            TAS_race=="Asian/PI" ~ 4,
                            TAS_race=="other" ~ 5,
                            TAS_race=="white" ~ 6))

TAS_race <- TAS_race %>% group_by(uniqueID) %>%
  mutate(highest_priority_race = ifelse(priority_rank==min(priority_rank, na.rm=TRUE),1,0))

# If individuals have consistent data assign them that
TAS_race <- TAS_race %>% 
  mutate(final_race_highest_priority_TAS = ifelse(flag==0, TAS_race, 
# Otherwise, assign them their highest priority race                  
ifelse(flag==1 & highest_priority_race==1, TAS_race, NA))) %>%
  fill(final_race_highest_priority_TAS, .direction="downup")

highest_priority_summary_TAS <- TAS_race %>% group_by(uniqueID) %>% 
  distinct(final_race_highest_priority_TAS) %>% ungroup() %>% 
  count(final_race_highest_priority_TAS)

# Keep only important variables from TAS_Race
TAS_race <- TAS_race %>% dplyr::select(uniqueID, year, TAS_race, final_race_first_year_TAS, final_race_highest_priority_TAS)

###############################################################################

# Join the TAS race with the main race data 
TAS_race$year <- as.numeric(TAS_race$year)
all_race <- left_join(race, TAS_race)

# Fill final TAS race variables for all years
all_race <- all_race %>% fill(final_race_first_year_TAS, .direction="downup")
all_race <- all_race %>% fill(final_race_highest_priority_TAS, .direction="downup")

# assign 'self reported' as the race method for those assigned their TAS race variable
all_race <- assign_race_method_TAS(all_race)

### Compare the TAS data and the main sample (in some cases imputed) race data to see how many inconsistencies there are:

# Comparing estimates based on first year
all_race <- all_race %>% 
  mutate(inconsistancies_first_year = ifelse(final_race_first_year==final_race_first_year_TAS, 0, 1))
all_race %>% distinct(uniqueID, .keep_all = TRUE) %>% group_by(inconsistancies_first_year) %>% count() 
# 1                          0  3448 - consistent
# 2                          1   778 - inconsistent
# 3                         NA   550 - no race in main data

# Comparing estimates based on highest priority
all_race <- all_race %>% 
  mutate(inconsistancies_highest_priority = ifelse(final_race_highest_priority==final_race_highest_priority_TAS, 0, 1))
all_race %>% distinct(uniqueID, .keep_all = TRUE) %>% group_by(inconsistancies_highest_priority) %>% count() 
# 1                                0  1612 - consistent
# 2                                1  2614 - inconsistent
# 3                               NA   550 - no race in main data

# Comparing estimates based on highest priority

# Assign individuals their TAS self-reported race, if this is available, creating a variable called race_new
all_race <- all_race %>% 
  mutate(race_new = ifelse(is.na(TAS_race), individualrace, TAS_race))
all_race %>% group_by(race_new) %>% count()
 


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




