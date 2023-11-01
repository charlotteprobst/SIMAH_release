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

# Counts within in each dataset
n_distinct(main_data$uniqueID) # 84,121 individuals
n_distinct(main_data$familyID) # 8,102 families
n_distinct(tas_data$uniqueID) # 4,776 individuals

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

# process alcohol data
alcohol <- process_alcohol(main_data)

# process employment status
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

### Process and assign race and ethnicity

race <- process_race(main_data) # 3,533,082

# Remove individuals with the same uniqueID for person and mother
race <- race %>% mutate(match=ifelse(uniqueID==IDmother,1,0),
                         match = ifelse(is.na(match),0,match)) %>%
  filter(match==0) # 3,532,914

# Join race data with all other data (to exclude non-respondents)
race <- left_join(all_data_responses_and_born_only, race) # 1,308,153

# Generate family race variable based on the race of the head and/or wife
race <- generate_family_race(race)

# Assign individuals a race based on their nearest relative's information
# (race of the head if direct relative to the head, race of partner if direct relative of the partner, or race of family if grandchild)
race <- assign_individual_family_race(race)

# Process the race and ethnicity data of an individual's parents (based on record linkage)
race <- process_race_parents(race)
race <- generate_race_parents(race)

# Assign any individuals with missing 'individual race' data, their parents race (if known)
race <- assign_individual_race_parents(race)

# Record the method used to generate 'individualrace' for each person
race <- assign_race_method(race)

# Identify individuals who (following imputation based on family/parents) have some race data, and those who still have no race data
some_race_data <- race %>% filter(!is.na(individualrace))
some_race_data_IDS <- unique(some_race_data$uniqueID)
no_race_data <- race %>% filter(!(uniqueID%in%some_race_data_IDS))
no_race_data_IDS <- unique(no_race_data$uniqueID)

# Identify individuals who have inconsistent race data over time
inconsistent_race_tally <- race %>% dplyr::select(uniqueID, individualrace) %>%
  distinct() %>% 
  group_by(uniqueID) %>% tally(!(is.na(individualrace))) %>%
  mutate(flag=ifelse(n>1,1,0))
inconsistent_IDS <- unique(subset(inconsistent_race_tally, flag==1)$uniqueID)
inconsistent_data_main <- race %>% filter(uniqueID%in%inconsistent_IDS)
n_distinct(inconsistent_data_main$uniqueID) # 1,317

# For each inconsistent individual, flag whether they are consistent within method(s)
temp <- inconsistent_data_main %>% dplyr::select(uniqueID, individualrace, race_method) %>%
  distinct() %>%
  ungroup() %>% group_by(uniqueID, race_method) %>% tally() %>%
  mutate(flag_consistency_within_race_method=ifelse(n==1,1,0))
race <- left_join(race, temp) 
temp <- race %>% filter(flag_consistency_within_race_method==1) # %>% dplyr::select(uniqueID, individualrace, race_method, flag_consistency_within_race_method)

# Consistent self-reports
temp_self_report <- temp %>% filter(race_method=="self reported")
consistent_self_report_IDS <- unique(temp_self_report$uniqueID) # 421

# Consistent reports by the head
temp_head_report <- temp %>% filter(race_method=="reported by head")
consistent_head_report_IDS <- unique(temp_head_report$uniqueID) # 169

# Consistent imputation from nearest family member
temp_imputed_family <- temp %>% filter(race_method=="imputed based on nearest family member")
consistent_imputed_family_IDS <- unique(temp_imputed_family$uniqueID) # 372

# Consistent imputation from parental record linkage
temp_imputed_parents <- temp %>% filter(race_method=="imputed based on parents")
consistent_imputed_parents_IDS <- unique(temp_imputed_parents$uniqueID) # 141

# Inconsistent within all methods
temp <- race %>% filter(flag_consistency_within_race_method==0) 
temp_inconsistent_within_all_methods <- temp %>% filter(!(uniqueID%in%consistent_self_report_IDS) & 
                                                   !(uniqueID%in%consistent_head_report_IDS) & 
                                                   !(uniqueID%in%consistent_imputed_family_IDS) &
                                                   !(uniqueID%in%consistent_imputed_parents_IDS))
inconsistent_within_all_methods_IDS <- unique(temp_inconsistent_within_all_methods$uniqueID) # 735

# Add a column to each individual noting their best available consistent data
race <- race %>% mutate(race_consistency_main = case_when(uniqueID%in%no_race_data_IDS ~ "no race data",
                                                (!(uniqueID%in%inconsistent_IDS)) ~ "consistent throughout",
                                                uniqueID%in%consistent_self_report_IDS ~ "consistent self report",
                                                uniqueID%in%consistent_head_report_IDS ~ "consistent head report",
                                                uniqueID%in%consistent_imputed_family_IDS ~ "consistent imputation family",
                                                uniqueID%in%consistent_imputed_parents_IDS ~ "consistent imputation parents",
                                                uniqueID%in%inconsistent_within_all_methods_IDS ~ "inconsistent within all methods"))

best_available_consistent_data_summary <- race %>% group_by(race_consistency_main) %>% summarise(count=n_distinct(uniqueID))

# Assign one single race for each individual, across all years, based on the main data (3 options)

# Option A.  race_using_first_year

# Add an indicator of the first year a person has race data
race <- race %>% ungroup() %>% group_by(uniqueID) %>%
  mutate(firstyear_flag = ifelse(year == min(year[!is.na(individualrace)], na.rm = TRUE), 1, 0))

# If individuals have consistent self-reported data assign them that, otherwise assign them their first year of available race data
race <- race %>% ungroup() %>%
  mutate(race_using_first_year = 
           case_when(race_consistency_main=="consistent throughout" ~ individualrace,
                     race_consistency_main=="consistent self report" & race_method=="self reported" ~  individualrace,
                     ((race_consistency_main=="consistent head report"|
                       race_consistency_main=="consistent imputation family"|
                       race_consistency_main=="consistent imputation parents"|
                       race_consistency_main=="inconsistent within all methods"|
                       race_consistency_main=="no race data") & firstyear_flag==1 ~ individualrace)))
race <- race %>% group_by(uniqueID) %>% fill(race_using_first_year, .direction="downup")

race_using_first_year_main_summary <- race %>% group_by(race_using_first_year) %>%
  summarise(distinct_individuals = n_distinct(uniqueID))

# Option B.  race_using_priority_order

# Add an indicator of the highest priority race of an individual
race <- race %>% ungroup() %>%  mutate(
  priorityorder_flag = case_when(individualrace=="hispanic" ~ 1,
                         individualrace=="black" ~ 2,
                         individualrace=="Native" ~ 3,
                         individualrace=="Asian/PI" ~ 4,
                         individualrace=="other" ~ 5,
                         individualrace=="white" ~ 6))

race <- race %>% ungroup() %>% group_by(uniqueID) %>%
  mutate(highest_priority_race = ifelse(priorityorder_flag == min(priorityorder_flag[!is.na(individualrace)], na.rm = TRUE), 1, 0))

# If individuals have consistent data assign them that
race <- race %>%
  mutate(race_using_priority_order = ifelse(race_consistency_main=="consistent throughout", individualrace,
# If individual has inconsistent data, but consistent self report, assign them that
ifelse(race_consistency_main=="consistent self report" & race_method=="self reported",  individualrace,
# Otherwise, assign them their 'highest priority' race
ifelse((race_consistency_main=="consistent head report"|race_consistency_main=="consistent imputation family"|race_consistency_main=="consistent imputation parents"|race_consistency_main=="inconsistent within all methods"|race_consistency_main=="no race data") 
       & highest_priority_race==1, individualrace, NA)))) %>%
group_by(uniqueID) %>% fill(race_using_priority_order, .direction="downup")

race_using_priority_order_main_summary <- race %>% group_by(race_using_priority_order) %>%
  summarise(distinct_individuals = n_distinct(uniqueID))

# Option C.  race_using_method_hierarchy

# Assign race based on hierarchy of methods (self report > reported by head > imputed) if consistent within that method. 
race <- race %>%
mutate(race_using_method_hierarchy = case_when(
  race_consistency_main=="consistent throughout" ~ individualrace,
  race_consistency_main=="consistent self report" & race_method=="self reported" ~ individualrace,
  race_consistency_main=="consistent head report" & race_method=="reported by head" ~ individualrace,
  race_consistency_main=="consistent imputation family" & race_method=="imputed based on nearest family member" ~ individualrace,
  race_consistency_main=="consistent imputation parents" & race_method=="imputed based on parents" ~ individualrace,  
# Otherwise, follow the race priority order
  race_consistency_main=="inconsistent within all methods" & highest_priority_race==1 ~ individualrace,
  race_consistency_main=="no race data" ~ NA)) %>%
 group_by(uniqueID) %>% fill(race_using_method_hierarchy, .direction="downup")

race_using_method_hierarchy_main_summary <- race %>% group_by(race_using_method_hierarchy) %>%
  summarise(distinct_individuals = n_distinct(uniqueID))

# Process race data from within the Transition to Adulthood Supplement (TAS)
TAS_race <- process_TAS_race(tas_data)

# Drop TAS years with no race data
TAS_no_na <- TAS_race %>% drop_na(individualrace_TAS)

# Identify individuals who have inconsistent race data over time
inconsistent_race_tally_TAS <- TAS_no_na %>% dplyr::select(uniqueID, individualrace_TAS) %>%
  distinct() %>%
  ungroup() %>% group_by(uniqueID) %>% tally() %>%
  mutate(race_consistency_TAS=ifelse(n>1,"inconsistent within TAS","consistent within TAS"))
TAS_race <- left_join(TAS_race, inconsistent_race_tally_TAS)
inconsistent_IDS_TAS <- unique(subset(inconsistent_race_tally_TAS, race_consistency_TAS=="inconsistent within TAS")$uniqueID)

# Assign one single race for each individual, across all years, based on the TAS data (2 options)
# nb. There are 2 options only as all TAS race data is self-reported

# Option A. race_using_first_year_TAS 

# Add an indicator of the first year a person has race data
TAS_first_year <- TAS_race %>% group_by(uniqueID) %>% mutate(
  firstyear_flag_TAS = ifelse(year==min(year[!is.na(individualrace_TAS)], na.rm = TRUE), 1, 0))
TAS_race <- left_join(TAS_race, TAS_first_year)

# If individuals have consistent data assign them that, otherwise assign them their first observation of individualrace_TAS
TAS_race <- TAS_race %>%
   mutate(race_using_first_year_TAS = ifelse(race_consistency_TAS=="consistent within TAS", individualrace_TAS,
          ifelse(race_consistency_TAS=="inconsistent within TAS" & firstyear_flag_TAS==1, individualrace_TAS, NA))) %>% group_by(uniqueID) %>%
  fill(race_using_first_year_TAS, .direction="downup")

race_using_first_year_TAS_summary <- TAS_race %>% group_by(race_using_first_year_TAS) %>%
  summarise(distinct_individuals = n_distinct(uniqueID))

# Option B. final_race_priority_TAS

# Add an indicator of the highest priority race of an individual
TAS_race <- TAS_race %>% ungroup() %>%  mutate(
  priorityorder_flag = case_when(individualrace_TAS=="hispanic" ~ 1,
                            individualrace_TAS=="black" ~ 2,
                            individualrace_TAS=="Native" ~ 3,
                            individualrace_TAS=="Asian/PI" ~ 4,
                            individualrace_TAS=="other" ~ 5,
                            individualrace_TAS=="white" ~ 6))

TAS_race <- TAS_race %>% group_by(uniqueID) %>%
  mutate(highest_priority_race = ifelse(priorityorder_flag == min(priorityorder_flag[!is.na(individualrace_TAS)], na.rm = TRUE), 1, 0))

# If individuals have consistent data assign them that
TAS_race <- TAS_race %>%
  mutate(race_using_priority_order_TAS = ifelse(race_consistency_TAS=="consistent within TAS", individualrace_TAS,
# Otherwise, assign them their highest priority race
ifelse(race_consistency_TAS=="inconsistent within TAS" & highest_priority_race==1, individualrace_TAS, NA))) %>%
  group_by(uniqueID) %>%
  fill(race_using_priority_order_TAS, .direction="downup")

race_using_priority_order_TAS_summary <- TAS_race %>% group_by(race_using_priority_order_TAS) %>%
  summarise(distinct_individuals = n_distinct(uniqueID))

# Keep only important variables from TAS_Race
TAS_race <- TAS_race %>% dplyr::select(uniqueID, year, individualrace_TAS, race_using_first_year_TAS, race_using_priority_order_TAS, race_consistency_TAS)

# Join the TAS race with the main race data
TAS_race$year <- as.numeric(TAS_race$year)
all_race <- left_join(race, TAS_race)

# Fill TAS race variables for all years
all_race <- all_race %>% group_by(uniqueID) %>% fill(race_using_first_year_TAS, .direction="downup")
all_race <- all_race %>% group_by(uniqueID) %>% fill(race_using_priority_order_TAS, .direction="downup")

# Assign 'self reported' as the race method for those assigned their TAS race variable
all_race <- assign_race_method_TAS(all_race)

# Compare the race estimated based on the main sample data to the race estimated based on TAS data:

# Comparing estimates based on first year
all_race <- all_race %>%
  mutate(inconsistent_race_MAIN_vs_TAS_using_first_year = ifelse(race_using_first_year==race_using_first_year_TAS, 0, 1))
all_race %>% distinct(uniqueID, .keep_all = TRUE) %>% group_by(inconsistent_race_MAIN_vs_TAS_using_first_year) %>% count()

# Comparing estimates based on highest priority
all_race <- all_race %>%
  mutate(inconsistent_race_MAIN_vs_TAS_using_priority_order = ifelse(race_using_priority_order==race_using_priority_order_TAS, 0, 1))
all_race %>% distinct(uniqueID, .keep_all = TRUE) %>% group_by(inconsistent_race_MAIN_vs_TAS_using_priority_order) %>% count()

# Resolve conflicts between the main and TAS estimates of race (2 options)

# Option A. Assign individuals their TAS self-reported race, if available, otherwise assign race_using_priority_order from main
all_race <- all_race %>%
  mutate(final_race_using_priority_order = ifelse(is.na(race_using_priority_order_TAS), race_using_priority_order, race_using_priority_order_TAS)) %>%
  group_by(uniqueID) %>% fill(final_race_using_priority_order, .direction="downup")

temp <- all_race %>% filter(!(is.na(final_race_using_priority_order)))
n_distinct(temp$uniqueID) # n allocated final race 34,102
temp <- all_race %>% filter(is.na(final_race_using_priority_order))
n_distinct(temp$uniqueID) # n not allocated final race 49,872

# Check that only one race per person
all_race %>% group_by(uniqueID) %>%
  summarise(n_races=n_distinct(final_race_using_priority_order)) %>%
  filter(n_races>1) %>% count()

# Option B. Assign individuals their TAS self-reported race, if available, otherwise assign race_using_method_hierarchy from main
all_race <- all_race %>%
  mutate(final_race_using_method_hierarchy = ifelse(is.na(race_using_priority_order_TAS), race_using_method_hierarchy, race_using_priority_order_TAS)) %>%
  group_by(uniqueID) %>% fill(final_race_using_method_hierarchy, .direction="downup")

temp <- all_race %>% filter(!(is.na(final_race_using_method_hierarchy)))
n_distinct(temp$uniqueID) # n allocated final race 34,102
temp <- all_race %>% filter(is.na(final_race_using_method_hierarchy))
n_distinct(temp$uniqueID) # n not allocated final race 49,872

# Check that only one race per person
all_race %>% group_by(uniqueID) %>%
  summarise(n_races=n_distinct(final_race_using_method_hierarchy)) %>%
  filter(n_races>1) %>% count()

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

temp <- all_race %>% group_by(final_race_using_method_hierarchy, best_available_race_method) %>% summarise(distinct_individuals = n_distinct(uniqueID))
temp <- temp %>% ungroup() %>% mutate(percent_of_full_sample = distinct_individuals/sum(distinct_individuals)*100)
summary_race_using_method_hierarchy <- temp %>% group_by(final_race_using_method_hierarchy) %>% mutate(percent_of_race_subgroup = distinct_individuals/sum(distinct_individuals)*100)
write.csv(summary_race_using_method_hierarchy, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/PSID/Results/Demographics/summary_race_using_method_hierarchy.csv")

temp <- all_race %>% group_by(final_race_using_priority_order, best_available_race_method) %>% summarise(distinct_individuals = n_distinct(uniqueID))
temp <- temp %>% ungroup() %>% mutate(percent_of_full_sample = distinct_individuals/sum(distinct_individuals)*100)
summary_race_using_priority_order <- temp %>% group_by(final_race_using_priority_order) %>% mutate(percent_of_race_subgroup = distinct_individuals/sum(distinct_individuals)*100)
write.csv(summary_race_using_priority_order, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/PSID/Results/Demographics/summary_race_using_priority_order.csv")

# Final steps of cleaning:

# recode alcohol
all_data_incl_race <- recode_alcohol(all_race)

# Fill education and weight data
all_data_filled <- all_data_incl_race %>%
  filter(year>=1999) %>%
  group_by(uniqueID) %>%
  fill(weight, .direction=c("downup")) %>%
  fill(education_cat, .direction=c("downup")) %>% mutate(weight=mean(weight, na.rm=T))

# Filter to select only final variables
PSID_data_cleaned <- all_data_filled %>% 
  dplyr::select(c(
    # Interview information
    "uniqueID","familyID","relationship","IDmother", "IDfather","family_interview_ID","year","survey_year", 
    # Age and sex
    "birthyear","age", "sex", 
    # Psychological distress
    "kessler_score","distress_severe","distress_class", 
    # Socioeconomic status
    "employment_stat","total_fam_income","homeowner", 
    "education", "education_cat", "education_cat_detailed", "TAS_education","TAS_education_cat", 
    # Race and ethnicity
    "raceethhead","raceethwife","racefamily_best_guess","mothersrace","fathersrace", 
    "individualrace", "race_consistency_main",
    "individualrace_TAS","race_using_first_year_TAS", "race_consistency_TAS",
    "final_race_using_priority_order", "final_race_using_method_hierarchy","best_available_race_method",
    # Alcohol consumption
     "drinkingstatus","quantity","frequency","gpd","bingedrinkdays","AlcCAT"))    

write.csv(PSID_data_cleaned, "SIMAH_workplace/PSID/cleaned data/all_data_1999_2021_excl_non_responders.csv", row.names=F)




