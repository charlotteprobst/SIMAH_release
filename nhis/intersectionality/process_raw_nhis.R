"This script cleans the raw nhis data from 2000 onwards and subsets the required variables for the interesectional analysis
NB. Survey is cross-sectional rather than longitudinal therefore health information can be trended for demographic groups,
and the country as a whole but not for indiviudals or families."

## Read in necessary R packages
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(ipumsr)     # load in data extracted from IPUMS website
library(labelled)
library(haven)

## Clear environment
rm(list = ls())

## Set working directory
wd <- setwd("U:/SIMAH/")
data_path <- "SIMAH_workplace/nhis/intersectionality/"
code_path <- "SIMAH_code/nhis/intersectionality/"
 
##  Source required functions:
source(paste0(code_path,"functions/convert_missing_data.R"))
source(paste0(code_path,"functions/remove_na.R"))
source(paste0(code_path,"functions/generate_ALCSTAT.R"))
source(paste0(code_path,"functions/assign_grams_alcohol.R"))
source(paste0(code_path,"functions/recode_alc_cats.R"))
source(paste0(code_path,"functions/recode_race.R"))
source(paste0(code_path,"functions/recode_race_1_percent.R"))
source(paste0(code_path,"functions/recode_income.R"))
source(paste0(code_path,"functions/recode_education.R"))

## Extract NHIS data obtained from nhis.ipums.org (Lynn A. Blewett, Julia A. Rivera Drew, Miriam L. King, Kari C.W. Williams, Natalie Del Ponte and Pat Convey. IPUMS Health Surveys: National Health Interview Survey, Version 7.1 [dataset]. Minneapolis, MN: IPUMS, 2021).
# Available at: https://doi.org/10.18128/D070.V7.1
# Years requested: 2000-2020

# Variables requested:
# VARIABLE  LABEL
# YEAR	Survey year
# SERIAL	Sequential Serial Number, Household Record
# STRATA	Stratum for variance estimation
# PSU	Primary sampling unit (PSU) for variance estimation
# NHISHID	NHIS Unique identifier, household
# HHWEIGHT	Household weight, final annual
# REGION	Region of residence
# PERNUM	Person number within family/household (from reformatting)
# NHISPID	NHIS Unique Identifier, person
# HHX	Household number (from NHIS)
# FMX	Family number (from NHIS)
# PX	Person number of respondent (from NHIS).
# PERWEIGHT	Final basic annual weight
# SAMPWEIGHT	Sample Person Weight
# LONGWEIGHT	Sample adult weight, longitudinal sample
# PARTWEIGHT	Sample adult weight, partial sample
# FWEIGHT	Final annual family weight
# INTERVWMO	Month of NHIS interview
# INTERVWYR	Year of NHIS interview
# ASTATFLG	Sample adult flag
# CSTATFLG	Sample child flag
# AGE	Age
# SEX	Sex
# SEXORIEN	Sexual orientation
# MARSTCOHAB	Marital status, including living with partner
# BIRTHMO	Month of birth
# BIRTHYR	Year of birth
# NCHILD	Number of own children (from programming)
# RACENEW	Self-reported Race (Post-1997 OMB standards)
# HISPETH	Hispanic ethnicity
# YRSINUSG	Number of years spent in the U.S. (grouped year estimate)
# USBORN	Born in the United States
# CITIZEN	U.S. citizenship
# RACEBR	Race Bridge Variable (Pre-1997 OMB standards to Post-1997 OMB standards)
# RACEIMPUTE	Race imputation flag
# REGIONBR	Global region of birth
# HISPTYPEFLAG	Type of Hispanic origin imputation flag
# RACETHFLAG	Ethnicity/race imputation flag
# INTERVLANG	Language of interview
# LANGSPEAK	Language generally speak
# NOWAF	Currently in armed forces
# EDUCREC2	Educational attainment recode, intervalled
# EDUC	Educational attainment
# USUALFT	Usually work full time
# EMPFT	Usually work 35+ hours per week
# EMPSTATWKYR	Work status: Last week, past 12 months
# POORYN	Above or below poverty threshold
# INCFAM97ON2	Total combined family income (1997+ w. 2007 categories)
# INCFAM07ON	Total combined family income (2007+)
# FAMTOTINC	Total family income, last year (top coded)
# WELFMO	Months received welfare income, previous calendar year
# FSSTAT	Family-level food security status for 30-day food security
# HEALTH	Health status
# HEIGHT	Height in inches without shoes
# WEIGHT	Weight in pounds without shoes
# BMI	Body mass index
# BMICAT	Categorical body mass index
# NBHDTRUST	How much do you agree that people in this neighborhood can be trusted
# NBHDHELP	How much do you agree that people in this neighborhood help each other out
# HEALTHMENT	Self-assessed mental health
# PHYSACTABLE	Ability to carry out everyday physical activities
# QUALOFLIFE	Self-assessed quality of life
# USUALPL	Has usual place for medical care
# DELAYCOST	Medical care delayed due to cost, past 12 months
# YBARCARE	Needed but couldn't afford medical care, past 12 months
# CLTRLIMPT	How important is it for providers to understand/share culture
# RSPCTFREQ	How often treated w/respect by providers
# EZINFOFREQ	How often providers give easy to understand information
# HINOTCOVE	Health Insurance coverage status
# ALCDRINKEV	Ever drink alcohol
# ALCANYNOE	Frequency drank alcohol in past year: Edited number of units
# ALCANYTPE	Frequency drank alcohol in past year: Edited time period
# ALC5UPEVYR	Ever had 5+ drinks in a day, past year
# ALCEV30D	Ever had 1+ drinks, past 30 days
# ALC5UPOCC30D	Occasions had 5+ drinks in a row, past 30 days
# ALCDRKHVY12M	Ever had 4+/5+ drinks on an occasion, past year
# ALC1YR	Ever had 12+ drinks in any one year
# ALCLIFE	Had 12+ drinks in entire life
# ALC5UPYR	Days had 5+ drinks, past year
# ALCAMT	Average number of drinks on days drank
# ALCSTAT1	Alcohol drinking status: Recode
# ALCSTAT2	Current alcohol drinking status: Recode
# ALCANYNO	Frequency drank alcohol in past year: Number of units
# ALCANYTP	Frequency drank alcohol in past year: Time period
# ALCDAYSMO	Frequency drank alcohol in past year: Days per month
# ALCDAYSWK	Frequency drank alcohol in past year: Days per week
# ALCDAYSYR	Frequency drank alcohol in past year: Days in past year
# ALC5UPNO	Days had 5+ drinks, past year: Number of units
# ALC5UPTP	Days had 5+ drinks, past year: Time period
# SMOKEV	Ever smoked 100 cigarettes in life
# SMOKFREQNOW	Smoke every day, some days, or not at all
# ATTLACOM	Attitudes of other people limit/prevent community activities
# ATTLAHOM	Attitudes of other people limit/prevent home activities
# ATTLASCH	Attitudes of other people limit/prevent schooling
# ATTLAWRK	Attitudes of other people limit/prevent work
# OFTLACOM	How often barriers limit/prevent community activities
# OFTLAHOM	How often barriers limit/prevent home activities
# OFTLASCH	How often barriers limit/prevent schooling
# OFTLAWRK	How often barriers limit/prevent work
# POLLACOM	Policies limit/prevent community activities
# POLLAHOM	Policies limit/prevent home activities
# POLLASCH	Policies limit/prevent schooling
# POLLAWRK	Policies limit/prevent work
# TRALACOM	Transportation barriers limit/prevent community activities
# TRALAHOM	Transportation barriers limit/prevent home activities
# TRALASCH	Transportation barriers limit/prevent schooling
# TRALAWRK	Transportation barriers limit/prevent work
# AEFFORT	  Felt everything an effort, past 30 days (adults)
# AFEELINT1MO	Feelings interfered w. life, past 30 days (adults)
# AHOPELESS	How often felt hopeless, past 30 days (adults)
# ANERVOUS	How often felt nervous, past 30 days (adults)
# ARESTLESS	How often felt restless, past 30 days (adults)
# ASAD	How often felt sad, past 30 days (adults)
# AWORTHLESS	How often felt worthless, past 30 days (adults)
# MORTELIG	Eligibility status for mortality follow-up
# MORTSTAT	Final mortality status
# MORTDODQ	Quarter of death
# MORTDODY	Year of death
# MORTUCOD	Underlying cause of death (ICD-10)
# MORTUCODLD	Leading underlying cause of death (ICD-10)
# MORTWT	Weight adjusted for ineligible respondents in mortality analysis
# MORTNDI	Mortality match with National Death Index
# MORTWTSA	Sample adult weight adjusted for ineligible respondents in mortality analysis
# EDUC_MOM	Educational attainment [of mother]
# EDUC_MOM2	Educational attainment [of same sex mother]
# EDUC_POP	Educational attainment [of father]
# EDUC_POP2	Educational attainment [of same sex father]

## Read in data extracted from IPUMS:
# ddi <- read_ipums_ddi(paste0(data_path,"raw_data/nhis_00002.xml"))
# data <- read_ipums_micro(ddi)

## Save extracted data as .RDS file
# saveRDS(data, paste0(data_path,"cleaned_data/nhis_full_extract.RDS"))
 
## Read in extracted data:
nhis_full_extract <- readRDS(paste0(data_path,"cleaned_data/nhis_full_extract.RDS"))

## Create subset of data for primary variables of interest for intersectional analysis 1
## Dependent variables: alcohol consumption AND mortality
## intersections: age,sex,race/ethnicity/SES(education and income)/migrant status
## controlling for: smoking status ?

nhis_subset_raw <- nhis_full_extract %>% 
  select(YEAR, NHISPID, AGE, SEX, SEXORIEN,
  EDUCREC2, RACENEW, RACEBR, HISPETH, USBORN, 
  CITIZEN, INCFAM97ON2, starts_with("ALC"),
  starts_with("MORT"), SMOKFREQNOW)

## Save raw subset of data as .RDS file
saveRDS(nhis_subset_raw, paste0(data_path,"cleaned_data/nhis_subset_raw.RDS"))

## Read in raw subset of data
nhis_subset_raw <- readRDS(paste0(data_path,"cleaned_data/nhis_subset_raw.RDS")) # 1,841,207 individuals

## create data dictionary
dictionary <- labelled::generate_dictionary(nhis_subset_raw)

## Zap labels from the data
nhis_subset_zapped <- nhis_subset_raw %>% zap_formats() %>% zap_labels()

# convert 'refused', 'not ascertained', 'don't know', 'inconsistent' and 'NIU' to NA
nhis_subset_converted_NA <- convert_missing_data_NA(nhis_subset_zapped)

# # Drop individuals missing key demographic data
variables <- c("YEAR", "AGE", "SEXORIEN","RACENEW","EDUCREC2", "INCFAM97ON2", "HISPETH")
nhis_subset_dropped_NA_demogr <- remove_na(nhis_subset_converted_NA, variables)
# 494,489 individuals remaining.  Lose years pre-2013 as sexual-orientation not available pre-2013.

## Review available categorisations for ALCSTAT 1 & 2:
nhis_subset_raw %>% count(ALCSTAT1)
nhis_subset_raw %>% count(ALCSTAT2)

## Generate broad drinking status ("ALCSTAT", lifetime abstainer/former drinker/current drinker)
## by combining info from ALCSTAT1 & ALCSTAT2:
nhis_subset_alcstat <- generate_ALCSTAT(nhis_subset_dropped_NA_demogr)
nhis_subset_alcstat %>% count(ALCSTAT, ALCSTAT1, ALCSTAT2) # Check face validity of output

## Drop individuals with no info on drinking status:
variables <- c("ALCSTAT")  # variable to drop
nhis_subset_dropped_na_alcstat <- remove_na(nhis_subset_alcstat, variables) # 169,614 individuals remaining

## Estimate average grams of alcohol use per person and assign as a new variable ("alc_daily_g")
##(Expanded Quantity/Frequency (QF) Approach & assuming standard drink size)
nhis_subset_grams_alc <- assign_grams_alcohol(nhis_subset_dropped_na_alcstat)

## Create more detailed sub-categories of alcohol use based on average alcohol consumption (grams):
nhis_subset_alc_cats <- recode_alc_cats(nhis_subset_grams_alc)

## Check face validity of new alcohol variables:
alc_summary <- nhis_subset_alc_cats %>%
group_by(ALCSTAT, ALCDAYSYR, ALCAMT, ALC5UPYR, alc_daily_g, alc_4_cats, alc_5_cats, alc_6_cats) %>%
count()
view(alc_summary)

## Save alcohol_processed subset of data as .RDS file
saveRDS(nhis_subset_alc_cats, paste0(data_path,"cleaned_data/nhis_subset_processed_alc.RDS"))

## Read in subset of data with alcohol data processed
nhis_subset_alc_processed <- readRDS(paste0(data_path,"cleaned_data/nhis_subset_processed_alc.RDS"))

## Recode race/ethnicity:

## Create a binary variable for hispanic ethnicity
nhis_subset_hisp <- nhis_subset_alc_processed %>% mutate(hisp = ifelse(HISPETH != 10, 1, 0))

## Recategorise based on race and (hispanic) ethnicity
nhis_subset_race <- recode_race(nhis_subset_hisp) 

## View resulting group numbers/ proportions
169614*0.01
# [1] 1696.14 (1%)
nhis_subset_race %>% group_by(race) %>% count()  
## 1 110315
## 2  20846
## 3   1319  <1%
## 4   8952
## 5    339  <1%
## 6   2950  
## 7  22243
## 8    978  <1%
## 9    560  <1%
## 10   454  <1%
## 11    74  <1%
## 12   584  <1%

## Re-categorise so that no group <1% (i.e. combine groups 3,4 & 8-12 to 'other')
nhis_subset_race_1_percent <- recode_race_1_percent(nhis_subset_race)

## Creates groups:
## race == 1, Non-hispanic, White
## race == 2, Non-hispanic, Black/African American
## race == 3, Non-hispanic, Asian
## race == 4, Non-hispanic, race group not releasable 
## race == 5, Hispanic,  White
## race == 6, Other


## Recode income:
nhis_subset_income <- recode_income(nhis_subset_race_1_percent)

## Creates groups:
## Income == 1, $0 - $34,999 
## Income == 2, $35,000-$74,999
## Income == 3, $75,000-$99,999
## Income == 4, $100,000 and over


## Recategorise education (into 3, 4 and 5 categories):
nhis_subset_education <- recode_education(nhis_subset_income)

