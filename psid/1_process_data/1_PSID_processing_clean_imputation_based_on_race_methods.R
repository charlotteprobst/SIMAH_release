# clean script to process PSID data
library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
library(haven)
library(naniar)
setwd("C:/Users/cmp21seb/Documents/SIMAH/")

# read in the data 
main_data <- read_excel("SIMAH_workplace/PSID/Raw_data/J325713/J325713.xlsx")
tas_data <- read_excel("SIMAH_workplace/PSID/Raw_data/Full_2021_new/TAS sample/J325254.xlsx")

# Source existing PSID processing functions
source("SIMAH_code/PSID/1_process_data/PSID_processing_functions.R")

# Recode static variables
main_data$familyID <- main_data$ER30001
tas_data$familyID <- tas_data$ER30001

main_data$ID <- main_data$ER30002
tas_data$ID <- tas_data$ER30002

main_data$uniqueID <- (main_data$familyID*1000) + main_data$ID
tas_data$uniqueID <- (tas_data$familyID*1000) + tas_data$ID

main_data$IDmother = ifelse(main_data$ER32010==0, NA, 
                       ifelse(main_data$ER32010>=800 & main_data$ER32010<=999, NA,
                  (main_data$ER32010*1000) + main_data$ER30002))

main_data$IDfather = ifelse(main_data$ER32017==0, NA,
                       ifelse(main_data$ER32017>=800 & main_data$ER32017<=999, NA,
                  (main_data$ER32017*1000) + main_data$ER30002))

main_data$sex <- main_data$ER32000
main_data <- main_data %>%dplyr::mutate(sex = dplyr::na_if(sex, 9))
main_data$sex <- recode(as.factor(main_data$sex), "1"="male", "2"="female")

# Count number of unique individuals and families in each dataset
n_distinct(main_data$uniqueID) # 84,121
n_distinct(main_data$familyID) #8,102
n_distinct(tas_data$uniqueID) # 4,776

# Process survey year
survey_year <- process_survey_year(main_data)
survey_year$year <- as.numeric(survey_year$year)

# Process family interview ID
family_interview_ID <- process_family_interview_ID(main_data)
family_interview_ID$year <- as.numeric(family_interview_ID$year)

# Join info and flag non-responses
responses <- merge(survey_year, family_interview_ID, all=TRUE)
responses <- responses %>% mutate(flag_non_response = ifelse(is.na(family_interview_ID), 1, 0)) 

# Process educational attainment data
education <- process_education(main_data) 
TAS_education <- process_TAS_education(tas_data) %>% distinct() # Transition to Adulthood Supplement (TAS)
# Join the TAS education with the main education data 
TAS_education$year <- as.numeric(TAS_education$year)
all_education <- left_join(education, TAS_education)

# process age data 
age <- process_age(main_data) 

# process relationship to householder data
relationship <- process_relationship(main_data)
# review relationship data
relationship_summary <- relationship %>% group_by(relationship) %>% summarise(distinct_individuals = n_distinct(uniqueID))
# nb. counts sum to more than the total population as individuals may hold multiple roles

# process family sampling weights 
sampleweights <- process_sample_weights(main_data)
sum(is.na(sampleweights$weight)) # 608,203 out of 1,009,452 (60%)

# kessler score 
kessler <- process_kessler(main_data)

#Create as a categorized variable based on paper by Prochaska et al. 2012-Validity study of the K6 scale as ameasure of moderate mental distressbased on mental health treatment needand utilization
kessler$distress_severe <- ifelse(kessler$kessler_score>=13, "Yes",
                                   ifelse(kessler$kessler_score<13, "No", NA))
kessler$distress_class <- ifelse(kessler$kessler_score<5, "Low or none",
                                  ifelse(kessler$kessler_score>=5 & kessler$kessler_score<13, "Moderate",
                                         ifelse(kessler$kessler_score>=13, "Severe", NA)))  
summary(as.factor(kessler$distress_severe))
summary(as.factor(kessler$distress_class))

# alcohol data
alcohol <- process_alcohol(main_data)

# employment status
employment <- process_employment(main_data)

# process income 
income <- process_income(main_data)

# process home ownership 
homeowner <- process_homeowner(main_data)

# Combine all subsets of data
all_data <- left_join(responses, age) %>% 
  left_join(., relationship) %>% left_join(.,alcohol) %>% 
  left_join(., kessler) %>% left_join(., sampleweights) %>% 
  left_join(., employment) %>% left_join(., income) %>% 
  left_join(.,homeowner) %>% 
  left_join(.,all_education) # n 3,533,082
n_distinct(all_data$familyID) # 8,102 families 
n_distinct(all_data$uniqueID) # 84,121 individuals remaining 

# Drop rows of data where non-responses (no family interview ID)
all_data_responses_only <- all_data %>% filter(flag_non_response==0) # 2,036,453
n_distinct(all_data_responses_only$familyID) # 8102 families remaining
n_distinct(all_data_responses_only$uniqueID) # 84,121 individuals remaining

# Drop rows of data where individuals are not yet born (age <1)
all_data_responses_only$age <- all_data_responses_only$year - all_data_responses_only$birthyear
all_data_responses_and_born_only <- all_data_responses_only %>% filter(age >= 1) # 1,308,153
n_distinct(all_data_responses_and_born_only$familyID) # 8102 families remaining
n_distinct(all_data_responses_and_born_only$uniqueID) # 83,974 individuals remaining

########################################################################################################################

### Process and assign race and ethnicity

## 1. process race and ethnicity data
race <- process_race(main_data) # 3,533,082

# remove individuals with the same uniqueID for person and mother
race <- race %>% mutate(match=ifelse(uniqueID==IDmother,1,0),
                         match = ifelse(is.na(match),0,match)) %>%
  filter(match==0) # 3,532,914

# Join race data with all other data (to exclude non-respondents)
race <- left_join(all_data_responses_and_born_only, race) # 1,308,153

## 2. Generate family race variable based on the race of the head and/or wife
race <- generate_family_race(race)

## 3. Assign individuals their individual race (race of the head if direct relative to the head, race of partner if direct relative of the partner, or race of family if grandchild)
race <- assign_individual_family_race(race)

## 4. Process the race and ethnicity data of an individual's parents (based on record linkage)
race <- process_race_parents(race)

## 5. Generate an overarching race variable for the parents
race <- generate_race_parents(race)

## 6. Assign any individuals with missing 'individual race' data, their parents race (if known)
race <- assign_individual_race_parents(race)

# Label the method used to generate each persons race
race <- assign_race_method(race)
# 
# ## 7. Fill race data by individual (only filling within race_method)
# race <- race %>% group_by(uniqueID, race_method) %>% fill(individualrace, race_method, .direction = c("downup")) %>% ungroup()

# Review how many people have inconsistent race data over time
tally <- race %>% dplyr::select(uniqueID, individualrace) %>%
  distinct() %>% 
  group_by(uniqueID) %>% tally(!(is.na(individualrace))) %>%
  mutate(flag=ifelse(n>1,1,0))
inconsistent_IDS <- unique(subset(tally, flag==1)$uniqueID)
inconsistent_data_main <- race %>% filter(uniqueID%in%inconsistent_IDS)
n_distinct(inconsistent_data_main$uniqueID) # 1,317

# Count how many of those with inconsistent data have more than one race method
inconsistent_data_main %>% dplyr::select(uniqueID, race_method) %>%
  distinct() %>%
  ungroup() %>% group_by(uniqueID) %>% tally(!(is.na(race_method))) %>%
 filter(n>1) %>% count() # 588

# Flag for each inconsistent individual, whether they are consistent within race method(s)
flag_within <- inconsistent_data_main %>% dplyr::select(uniqueID, individualrace, race_method) %>%
  distinct() %>%
  ungroup() %>% group_by(uniqueID, race_method) %>% tally() %>%
  mutate(flag_consistency_within_race_method=ifelse(n==1,1,0))
race <- left_join(race, flag_within) 

# Note which individuals have NA throughout their race category
some_race_data <- race %>% filter(!is.na(individualrace))
some_race_data_IDS <- unique(some_race_data$uniqueID)
no_race_data <- race %>% filter(!(uniqueID%in%some_race_data_IDS))
no_race_data_IDS <- unique(no_race_data$uniqueID)

# Note which inconsistent individuals have consistent data, by method
temp <- race %>% filter(flag_consistency_within_race_method==1) %>% dplyr::select(uniqueID, individualrace, race_method, flag_consistency_within_race_method)
temp_method <- temp %>% group_by(race_method) %>% summarise(count = n_distinct(uniqueID))

temp_self_report <- temp %>% filter(race_method=="self reported")
consistent_self_report_IDS <- unique(temp_self_report$uniqueID) # 421

temp_head_report <- temp %>% filter(race_method=="reported by head")
consistent_head_report_IDS <- unique(temp_head_report$uniqueID) # 169

temp_imputed_family <- temp %>% filter(race_method=="imputed based on nearest family member")
consistent_imputed_family_IDS <- unique(temp_imputed_family$uniqueID) # 372

temp_imputed_parents <- temp %>% filter(race_method=="imputed based on parents")
consistent_imputed_parents_IDS <- unique(temp_imputed_parents$uniqueID) # 141

temp_2 <- race %>% filter(flag_consistency_within_race_method==0) 

temp_inconsistent_within_all_methods <- temp_2 %>% filter(!(uniqueID%in%consistent_self_report_IDS) & 
                                                   !(uniqueID%in%consistent_head_report_IDS) & 
                                                   !(uniqueID%in%consistent_imputed_family_IDS) &
                                                   !(uniqueID%in%consistent_imputed_parents_IDS))

inconsistent_within_all_methods_IDS <- unique(temp_inconsistent_within_all_methods$uniqueID) # 735

# Add a column to each individual noting their best available consistent data
race <- race %>% mutate(consistency = case_when(uniqueID%in%no_race_data_IDS ~ "no race data",
                                                (!(uniqueID%in%inconsistent_IDS)) ~ "consistent throughout",
                                                uniqueID%in%consistent_self_report_IDS ~ "consistent self report",
                                                uniqueID%in%consistent_head_report_IDS ~ "consistent head report",
                                                uniqueID%in%consistent_imputed_family_IDS ~ "consistent imputation family",
                                                uniqueID%in%consistent_imputed_parents_IDS ~ "consistent imputation parents",
                                                uniqueID%in%inconsistent_within_all_methods_IDS ~ "inconsistent within all methods"))

race %>% group_by(consistency) %>% summarise(count=n_distinct(uniqueID))

### Assign one single race for each individual, across all years, based on the main data.

## Option A.  final_race_first_year

# Add an indicator of the first year a person has race data
race <- race %>% ungroup() %>% group_by(uniqueID) %>%
  mutate(firstyear = ifelse(year == min(year[!is.na(individualrace)], na.rm = TRUE), 1, 0))

# If individuals have consistent self-reported data assign them that, otherwise assign them their first year of available race data
race <- race %>% ungroup() %>%
  mutate(final_race_first_year = 
           case_when(consistency=="consistent throughout" ~ individualrace,
                     consistency=="consistent self report" & race_method=="self reported" ~  individualrace,
                     ((consistency=="consistent head report"|
                       consistency=="consistent imputation family"|
                       consistency=="consistent imputation parents"|
                       consistency=="inconsistent within all methods"|
                       consistency=="no race data") & firstyear==1 ~ individualrace)))
race <- race %>%
group_by(uniqueID) %>% fill(final_race_first_year, .direction="downup")

final_race_first_year_main_summary <- race %>% group_by(final_race_first_year) %>%
  summarise(distinct_individuals = n_distinct(uniqueID))

## Option B  final_race_highest_priority

# Add an indicator of the highest priority race of an individual
race <- race %>% ungroup() %>%  mutate(
  priority_rank = case_when(individualrace=="hispanic" ~ 1,
                         individualrace=="black" ~ 2,
                         individualrace=="Native" ~ 3,
                         individualrace=="Asian/PI" ~ 4,
                         individualrace=="other" ~ 5,
                         individualrace=="white" ~ 6))

race <- race %>% ungroup() %>% group_by(uniqueID) %>%
  mutate(highest_priority_race = ifelse(priority_rank == min(priority_rank[!is.na(individualrace)], na.rm = TRUE), 1, 0))

# If individuals have consistent data assign them that
race <- race %>%
  mutate(final_race_highest_priority = ifelse(consistency=="consistent throughout", individualrace,
# If individual has inconsistent data, but consistent self report, assign them that
ifelse(consistency=="consistent self report" & race_method=="self reported",  individualrace,
# Otherwise, assign them their highest priority race
ifelse((consistency=="consistent head report"|consistency=="consistent imputation family"|consistency=="consistent imputation parents"|consistency=="inconsistent within all methods"|consistency=="no race data") 
       & highest_priority_race==1, individualrace, NA)))) %>%
group_by(uniqueID) %>% fill(final_race_highest_priority, .direction="downup")

final_race_hierarchy_main_summary <- race %>% group_by(final_race_highest_priority) %>%
  summarise(distinct_individuals = n_distinct(uniqueID))

## Option C  final_race_best_method

# Assign race based on hierarchy of methods (self report > reported by head > imputed) if consistent within that method, 
# Otherwise, follow the racial hierarchy 
 
race <- race %>%
mutate(final_race_best_method = case_when(
  consistency=="consistent throughout" ~ individualrace,
  consistency=="consistent self report" & race_method=="self reported" ~ individualrace,
  consistency=="consistent head report" & race_method=="reported by head" ~ individualrace,
  consistency=="consistent imputation family" & race_method=="imputed based on nearest family member" ~ individualrace,
  consistency=="consistent imputation parents" & race_method=="imputed based on parents" ~ individualrace,       
  consistency=="inconsistent within all methods" & highest_priority_race==1 ~ individualrace,
  consistency=="no race data" ~ NA)) %>%
 group_by(uniqueID) %>% fill(final_race_best_method, .direction="downup")

final_race_best_method_main_summary <- race %>% group_by(final_race_best_method) %>%
  summarise(distinct_individuals = n_distinct(uniqueID))

## 8. Process Transition to Adulthood Supplement race data
TAS_race <- process_TAS_race(tas_data)

# number of individuals in the TAS data:
TAS_race %>% group_by(uniqueID) %>% count() # 4,766 

# Drop TAS years with race data NA so as to not create new rows when merging back with main data
TAS_no_na <- TAS_race %>% drop_na(TAS_race)

# Explore discrepancies in TAS race data
TAS_tally <- TAS_no_na %>% dplyr::select(uniqueID, TAS_race) %>%
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

TAS_first_year <- TAS_race %>% group_by(uniqueID) %>% mutate(
  firstyear_TAS = ifelse(year==min(year[!is.na(TAS_race)], na.rm = TRUE), 1, 0))

TAS_race <- left_join(TAS_race, TAS_first_year)

# If individuals have consistent data assign them that, otherwise assign them their first observation of TAS_race
TAS_race <- TAS_race %>%
   mutate(final_race_first_year_TAS = ifelse(flag==0, TAS_race,
          ifelse(flag==1 & firstyear_TAS==1, TAS_race, NA))) %>% group_by(uniqueID) %>%
  fill(final_race_first_year_TAS, .direction="downup")

final_race_first_year_TAS_summary <- TAS_race %>% group_by(final_race_first_year_TAS) %>%
  summarise(distinct_individuals = n_distinct(uniqueID))

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
  mutate(highest_priority_race = ifelse(priority_rank == min(priority_rank[!is.na(TAS_race)], na.rm = TRUE), 1, 0))

# If individuals have consistent data assign them that
TAS_race <- TAS_race %>%
  mutate(final_race_highest_priority_TAS = ifelse(flag==0, TAS_race,
# Otherwise, assign them their highest priority race
ifelse(flag==1 & highest_priority_race==1, TAS_race, NA))) %>%
  group_by(uniqueID) %>%
  fill(final_race_highest_priority_TAS, .direction="downup")

final_race_hierarchy_TAS_summary <- TAS_race %>% group_by(final_race_highest_priority_TAS) %>%
  summarise(distinct_individuals = n_distinct(uniqueID))

# Keep only important variables from TAS_Race
TAS_race <- TAS_race %>% dplyr::select(uniqueID, year, TAS_race, final_race_first_year_TAS, final_race_highest_priority_TAS)

###############################################################################

# Join the TAS race with the main race data
TAS_race$year <- as.numeric(TAS_race$year)
all_race <- left_join(race, TAS_race)

# Fill final TAS race variables for all years
all_race <- all_race %>% group_by(uniqueID) %>% fill(final_race_first_year_TAS, .direction="downup")
all_race <- all_race %>% group_by(uniqueID) %>% fill(final_race_highest_priority_TAS, .direction="downup")

# assign 'self reported' as the race method for those assigned their TAS race variable
all_race <- assign_race_method_TAS(all_race)

### Compare the TAS data and the main sample (in some cases imputed) race data to see how many inconsistencies there are:

# Comparing estimates based on first year
all_race <- all_race %>%
  mutate(inconsistancies_first_year = ifelse(final_race_first_year==final_race_first_year_TAS, 0, 1))
all_race %>% distinct(uniqueID, .keep_all = TRUE) %>% group_by(inconsistancies_first_year) %>% count()
#                           0  3911
#                           1   300
#                          NA 79763

# Comparing estimates based on highest priority
all_race <- all_race %>%
  mutate(inconsistancies_highest_priority = ifelse(final_race_highest_priority==final_race_highest_priority_TAS, 0, 1))
all_race %>% distinct(uniqueID, .keep_all = TRUE) %>% group_by(inconsistancies_highest_priority) %>% count()
#                               0  3,920 - consistent
#                               1  291 - inconsistent
#                               NA  79,763 - don't have data to compare

# Comparing estimates based on highest priority

# Assign individuals their TAS self-reported race, if this is available, creating a variable called race_new

## A - If TAS not available taking final_race_highest_priority from main
all_race <- all_race %>%
  mutate(race_new_highest_priority = ifelse(is.na(final_race_highest_priority_TAS), final_race_highest_priority, final_race_highest_priority_TAS)) %>%
  group_by(uniqueID) %>% fill(race_new_highest_priority, .direction="downup")

temp <- all_race %>% filter(!(is.na(race_new_highest_priority)))
n_distinct(temp$uniqueID) # n allocated final race 34,102
temp <- all_race %>% filter(is.na(race_new_highest_priority))
n_distinct(temp$uniqueID) # n not allocated final race 49,872

# Check that only one race per person
all_race %>% group_by(uniqueID) %>%
  summarise(n_races=n_distinct(race_new_highest_priority)) %>%
  filter(n_races>1) %>% count()

## B - If TAS not available taking final_race_best_method from main
all_race <- all_race %>%
  mutate(race_new_best_method = ifelse(is.na(final_race_highest_priority_TAS), final_race_best_method, final_race_highest_priority_TAS)) %>%
  group_by(uniqueID) %>% fill(race_new_best_method, .direction="downup")

temp <- all_race %>% filter(!(is.na(race_new_best_method)))
n_distinct(temp$uniqueID) # n allocated final race 34,102
temp <- all_race %>% filter(is.na(race_new_best_method))
n_distinct(temp$uniqueID) # n not allocated final race 49,872

# Check that only one race per person
all_race %>% group_by(uniqueID) %>%
  summarise(n_races=n_distinct(race_new_best_method)) %>%
  filter(n_races>1) %>% count()

### NB. Regardless of the race 
# Note the best available race method used to allocate the final_race
all_race <- all_race %>% group_by(uniqueID) %>%
  mutate(best_available_race_method = ifelse(race_method_rank==min(race_method_rank, na.rm=TRUE),race_method,NA)) %>%
  fill(best_available_race_method, .direction="downup")

# Check that only one race method per person
all_race %>% group_by(uniqueID) %>%
  summarise(n_race_methods=n_distinct(best_available_race_method)) %>%
  filter(n_race_methods>1) %>% count()

# Summarise final race demographics and the method used to generate them:
summary_final_method <- all_race %>% group_by(best_available_race_method) %>% summarise(distinct_individuals = n_distinct(uniqueID))

summary_final_race_best_method <- all_race %>% group_by(race_new_best_method, best_available_race_method) %>% summarise(distinct_individuals = n_distinct(uniqueID))
summary_final_race_best_method <- summary_final_race_best_method %>% ungroup() %>% mutate(percent_of_full_sample = distinct_individuals/sum(distinct_individuals)*100)
summary_final_race_best_method <- summary_final_race_best_method %>% group_by(race_new_best_method) %>% mutate(percent_of_race_subgroup = distinct_individuals/sum(distinct_individuals)*100)
write.csv(summary_final_race_best_method, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/PSID/Results/Demographics/summary_final_race_new_method_excluding_non_responders.csv")

summary_final_race_highest_priority <- all_race %>% group_by(race_new_highest_priority, best_available_race_method) %>% summarise(distinct_individuals = n_distinct(uniqueID))
summary_final_race_highest_priority <- summary_final_race_highest_priority %>% ungroup() %>% mutate(percent_of_full_sample = distinct_individuals/sum(distinct_individuals)*100)
summary_final_race_highest_priority <- summary_final_race_highest_priority %>% group_by(race_new_highest_priority) %>% mutate(percent_of_race_subgroup = distinct_individuals/sum(distinct_individuals)*100)
write.csv(summary_final_race_highest_priority, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/PSID/Results/Demographics/summary_final_race_highest_priority_excluding_non_responders.csv")

# Final steps of cleaning:

# recode alcohol
all_data_incl_race <- recode_alcohol(all_race)

# Fill education and weight data
all_data_filled <- all_data_incl_race %>%
  filter(year>=1999) %>%
  group_by(uniqueID) %>%
  fill(weight, .direction=c("downup")) %>%
  fill(education_cat, .direction=c("downup")) %>% mutate(weight=mean(weight, na.rm=T))

# Filter to remove variables that are now redundent

# nb. woulld be usful to rename final_race_frist_year to race_first year and then 'new' to FINAL

# all_data_filled <- all_data_filled %>% dplyr::select(-c(""race_method_rank", "firstyear", "final_race_first_year", "priority_rank"                       "highest_priority_race"              
# [49] "final_race_highest_priority"         "final_race_best_method"              "TAS_race"                           
# [52] "final_race_first_year_TAS"           "final_race_highest_priority_TAS"     "inconsistancies_first_year"         
# [55] "inconsistancies_highest_priority"    "race_new_highest_priority"           "race_new_best_method"               
# [58] "best_available_race_method"          "drinkingstatus"                      "quantity"                           
# [61] "frequency"                           "gpd"                                 "bingedrinkdays"                     
# [64] "AlcCAT"        

write.csv(all_data, "SIMAH_workplace/PSID/cleaned data/all_data_1999_2021_excl_non_responders.csv", row.names=F)
# write.csv(all_data, "SIMAH_workplace/PSID/cleaned data/all_data_all_years_highest_priority_race.csv", row.names=F)




