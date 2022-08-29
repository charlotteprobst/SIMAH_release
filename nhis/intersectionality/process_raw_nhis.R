"This script cleans the raw nhis data from 2000 onwards and subsets the required variables for the interesectional analysis
NB. Survey is cross-sectional rather than longitudinal therefore health information can be trended for demographic groups,
and the country as a whole but not for indiviudals or families."

## Read in necessary R packages
library(readr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(ipumsr)     # load in data extracted from IPUMS website
library(labelled)
library(haven)
library(naniar)

## Clear environment
rm(list = ls())

## Set working directory
wd <- setwd("U:/SIMAH/")
data_path <- "SIMAH_workplace/nhis/intersectionality/"
code_path <- "SIMAH_code/nhis/intersectionality/"

##  Source required functions:
source(paste0(code_path,"functions/convert_missing_data.R"))
source(paste0(code_path,"functions/assign_grams_alcohol.R"))
source(paste0(code_path,"functions/remove_na.R"))
source(paste0(code_path,"functions/generate_ALCSTAT.R"))
source(paste0(code_path,"functions/recode_alc_cats.R"))


## Extract NHIS data obtained from nhis.ipums.org (Lynn A. Blewett, Julia A. Rivera Drew, Miriam L. King, Kari C.W. Williams, Natalie Del Ponte and Pat Convey. IPUMS Health Surveys: National Health Interview Survey, Version 7.1 [dataset]. Minneapolis, MN: IPUMS, 2021).
# Avilable at: https://doi.org/10.18128/D070.V7.1
# Years requested: 2000-2020
# Variables requested:
# TYPE    VARIABLE  LABEL
# H	      YEAR	Survey year
# H	      SERIAL	Sequential Serial Number, Household Record
# H	      STRATA	Stratum for variance estimation
# H	      PSU	Primary sampling unit (PSU) for variance estimation
# H	      NHISHID	NHIS Unique identifier, household
# H	      HHWEIGHT	Household weight, final annual
# H	      REGION	Region of residence
# P	      PERNUM	Person number within family/household (from reformatting)
# P	      NHISPID	NHIS Unique Identifier, person
# P	      HHX	Household number (from NHIS)
# P	      FMX	Family number (from NHIS)
# P	      PX	Person number of respondent (from NHIS).
# P	      PERWEIGHT	Final basic annual weight
# P	      SAMPWEIGHT	Sample Person Weight
# P	      LONGWEIGHT	Sample adult weight, longitudinal sample
# P	      PARTWEIGHT	Sample adult weight, partial sample
# P       FWEIGHT	Final annual family weight
# P	      INTERVWMO	Month of NHIS interview
# P	      INTERVWYR	Year of NHIS interview
# P	      ASTATFLG	Sample adult flag
# P	      CSTATFLG	Sample child flag
# P	      AGE	Age
# P	      SEX	Sex
# P	      SEXORIEN	Sexual orientation
# P	      MARSTCOHAB	Marital status, including living with partner
# P	      BIRTHMO	Month of birth
# P	      BIRTHYR	Year of birth
# P	      NCHILD	Number of own children (from programming)
# P	      RACENEW	Self-reported Race (Post-1997 OMB standards)
# P	      HISPETH	Hispanic ethnicity
# P	      YRSINUSG	Number of years spent in the U.S. (grouped year estimate)
# P	      USBORN	Born in the United States
# P	      CITIZEN	U.S. citizenship
# P	      RACEBR	Race Bridge Variable (Pre-1997 OMB standards to Post-1997 OMB standards)
# P	      RACEIMPUTE	Race imputation flag
# P	      REGIONBR	Global region of birth
# P	      HISPTYPEFLAG	Type of Hispanic origin imputation flag
# P	      RACETHFLAG	Ethnicity/race imputation flag
# P	      INTERVLANG	Language of interview
# P	      LANGSPEAK	Language generally speak
# P	      NOWAF	Currently in armed forces
# P	      EDUCREC2	Educational attainment recode, intervalled
# P	      EDUC	Educational attainment
# P	      USUALFT	Usually work full time
# P     	EMPFT	Usually work 35+ hours per week
# P	      EMPSTATWKYR	Work status: Last week, past 12 months
# P	      POORYN	Above or below poverty threshold
# P	      INCFAM97ON2	Total combined family income (1997+ w. 2007 categories)
# P	      INCFAM07ON	Total combined family income (2007+)
# P	      FAMTOTINC	Total family income, last year (top coded)
# P	      WELFMO	Months received welfare income, previous calendar year
# P	      FSSTAT	Family-level food security status for 30-day food security
# P	      HEALTH	Health status
# P	      HEIGHT	Height in inches without shoes
# P	      WEIGHT	Weight in pounds without shoes
# P	      BMI	Body mass index
# P	      BMICAT	Categorical body mass index
# P	      NBHDTRUST	How much do you agree that people in this neighborhood can be trusted
# P	      NBHDHELP	How much do you agree that people in this neighborhood help each other out
# P	      HEALTHMENT	Self-assessed mental health
# P	      PHYSACTABLE	Ability to carry out everyday physical activities
# P	      QUALOFLIFE	Self-assessed quality of life
# P	      USUALPL	Has usual place for medical care
# P	      DELAYCOST	Medical care delayed due to cost, past 12 months
# P	      YBARCARE	Needed but couldn't afford medical care, past 12 months
# P	      CLTRLIMPT	How important is it for providers to understand/share culture
# P	      RSPCTFREQ	How often treated w/respect by providers
# P	      EZINFOFREQ	How often providers give easy to understand information
# P	      HINOTCOVE	Health Insurance coverage status
# P	      ALCDRINKEV	Ever drink alcohol
# P	      ALCANYNOE	Frequency drank alcohol in past year: Edited number of units
# P	      ALCANYTPE	Frequency drank alcohol in past year: Edited time period
# P	      ALC5UPEVYR	Ever had 5+ drinks in a day, past year
# P	      ALCEV30D	Ever had 1+ drinks, past 30 days
# P	      ALC5UPOCC30D	Occasions had 5+ drinks in a row, past 30 days
# P	      ALCDRKHVY12M	Ever had 4+/5+ drinks on an occasion, past year
# P	      ALC1YR	Ever had 12+ drinks in any one year
# P	      ALCLIFE	Had 12+ drinks in entire life
# P     	ALC5UPYR	Days had 5+ drinks, past year
# P	      ALCAMT	Average number of drinks on days drank
# P	      ALCSTAT1	Alcohol drinking status: Recode
# P	      ALCSTAT2	Current alcohol drinking status: Recode
# P	      ALCANYNO	Frequency drank alcohol in past year: Number of units
# P	      ALCANYTP	Frequency drank alcohol in past year: Time period
# P	      ALCDAYSMO	Frequency drank alcohol in past year: Days per month
# P	      ALCDAYSWK	Frequency drank alcohol in past year: Days per week
# P	      ALCDAYSYR	Frequency drank alcohol in past year: Days in past year
# P	      ALC5UPNO	Days had 5+ drinks, past year: Number of units
# P	      ALC5UPTP	Days had 5+ drinks, past year: Time period
# P	      SMOKEV	Ever smoked 100 cigarettes in life
# P	      SMOKFREQNOW	Smoke every day, some days, or not at all
# P	      ATTLACOM	Attitudes of other people limit/prevent community activities
# P	      ATTLAHOM	Attitudes of other people limit/prevent home activities
# P	      ATTLASCH	Attitudes of other people limit/prevent schooling
# P	      ATTLAWRK	Attitudes of other people limit/prevent work
# P	      OFTLACOM	How often barriers limit/prevent community activities
# P	      OFTLAHOM	How often barriers limit/prevent home activities
# P	      OFTLASCH	How often barriers limit/prevent schooling
# P	      OFTLAWRK	How often barriers limit/prevent work
# P	      POLLACOM	Policies limit/prevent community activities
# P	      POLLAHOM	Policies limit/prevent home activities
# P	      POLLASCH	Policies limit/prevent schooling
# P	      POLLAWRK	Policies limit/prevent work
# P	      TRALACOM	Transportation barriers limit/prevent community activities
# P	      TRALAHOM	Transportation barriers limit/prevent home activities
# P	      TRALASCH	Transportation barriers limit/prevent schooling
# P	      TRALAWRK	Transportation barriers limit/prevent work
# P	      AEFFORT	Felt everything an effort, past 30 days (adults)
# P	      AFEELINT1MO	Feelings interfered w. life, past 30 days (adults)
# P	      AHOPELESS	How often felt hopeless, past 30 days (adults)
# P	      ANERVOUS	How often felt nervous, past 30 days (adults)
# P	      ARESTLESS	How often felt restless, past 30 days (adults)
# P	      ASAD	How often felt sad, past 30 days (adults)
# P	      AWORTHLESS	How often felt worthless, past 30 days (adults)
# P	      MORTELIG	Eligibility status for mortality follow-up
# P	      MORTSTAT	Final mortality status
# P	      MORTDODQ	Quarter of death
# P	      MORTDODY	Year of death
# P	      MORTUCOD	Underlying cause of death (ICD-10)
# P	      MORTUCODLD	Leading underlying cause of death (ICD-10)
# P	      MORTWT	Weight adjusted for ineligible respondents in mortality analysis
# P	      MORTNDI	Mortality match with National Death Index
# P	      MORTWTSA	Sample adult weight adjusted for ineligible respondents in mortality analysis
# P	      EDUC_MOM	Educational attainment [of mother]
# P	      EDUC_MOM2	Educational attainment [of same sex mother]
# P	      EDUC_POP	Educational attainment [of father]
# P	      EDUC_POP2	Educational attainment [of same sex father]

# ## Read in data extracted from IPUMS:
# ddi <- read_ipums_ddi(paste0(data_path,"raw_data/nhis_00002.xml"))
# data <- read_ipums_micro(ddi)

## Save extracted data as .RDS file
# saveRDS(data, paste0(data_path,"cleaned_data/nhis_full_extract.RDS"))

# ## Read in extracted data:
# nhis_full_extract <- readRDS(paste0(data_path,"cleaned_data/nhis_full_extract.RDS"))
# # 
# # ## Create subset of data for primary variables of interest for intersectional analysis 1
# # # Dependent variables: alcohol consumption AND mortality
# # # intersections: age,sex,race/ethnicity/SES(education and income)/migrant status
# # # controlling for: ?? smoking status
#  
# nhis_subset_raw <- nhis_full_extract %>% select(YEAR, NHISPID, AGE, SEX, SEXORIEN,
#                                              EDUCREC2, RACENEW, USBORN, CITIZEN,
#                                              INCFAM97ON2,
#                                              starts_with("ALC"), starts_with("MORT"),
#                                              SMOKFREQNOW)
# 
# ## Save raw subset of data as .RDS file
# saveRDS(nhis_subset_raw, paste0(data_path,"cleaned_data/nhis_subset_raw.RDS"))
 
## Read back in raw subset1 data (1,841,207 individuals)
nhis_subset1_raw <- readRDS(paste0(data_path,"cleaned_data/nhis_subset_raw.RDS"))

## create data dictionary ----
dictionary <- labelled::generate_dictionary(nhis_subset1_raw)

## Zap labels from the dataframe
nhis_subset1_zapped <- nhis_subset1_raw %>% zap_formats() %>% zap_labels()

# convert 'refused', 'not ascertained', 'don't know', 'inconsistent' and 'NIU' to NA
nhis_subset1_NA <- convert_missing_data_NA(nhis_subset1_zapped)

# Drop individuals missing key demographic data 
variables <- c("YEAR", "AGE", "SEXORIEN","RACENEW", "EDUCREC2", "INCFAM97ON2")  # list the variables to drop missing data from
nhis_subset1_no_missing_demo <- remove_na(nhis_subset1_NA, variables)
nhis_subset1_no_missing_demo %>% count(NHISPID)  # 494,489 individuals

# Generate broad drinking status category (lifetime abstainer, former drinker, current drinker)
nhis_subset1_alcstat <- generate_ALCSTAT(nhis_subset1_no_missing_demo)

# Drop individuals with no alcohol consumption data:
variables <- c("ALCSTAT")  # list the variables to drop
nhis_subset1_no_missing_alc <-  remove_na(nhis_subset1_alcstat, variables)
nhis_subset1_no_missing_alc %>% count(NHISPID)  # 169,614 individuals


### CONTINUE FROM HERE (CHECK ASSIGNMENT MAKES SENSE TO ME - REWORD/ CHANGE ORDER AS NEEDED.  ALSO CHECK ASSUMPTION ABOUT BEING OK TO GROUP ABSTAINERS AND MINIMAL DRINKERS)
# Assign average grams of alcohol use per person (based on average # days drinking, average drinks per occasion, and number of heavy drinking days):
nhis_subset1_grams <- assign_grams_alcohol(nhis_subset1_no_missing_alc)

# Check face validity of alcohol grams data:
grams_summary <- nhis_subset1_grams %>% select(ALCSTAT, ALCDAYSYR, ALCAMT, ALC5UPYR, alc_daily_g_crude, alc_daily_g_heavy, alc_daily_g)
view(grams_summary)

# Create more detailed sub-categories of alcohol use based on average alcohol consumption (grams):
nhis_subset1 <- recode_alc_cats(nhis_subset1)

## Recategorise race/ethnicity into groupings needed for intersectional analysis

## Recategorise wealth into groupings needed for intersectional analysis

## Recategorise education into groupings needed for intersectional analysis

