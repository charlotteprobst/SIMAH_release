"This script cleans the raw nhis data from 2000 onwards and subsets the required variables for the interesectional analysis
NB. Survey is cross-sectional rather than longitudinal therefore health information can be trended for demographic groups,
and the country as a whole but not for indiviudals or families."

## Read in necessary R packages
library(readr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(janitor)    # clean variable names
library(skimr)      # descriptive statistics
library(ipumsr)     # load in data extracted from IPUMS website

## Clear environment
rm(list = ls())

## Set working directory
wd <- setwd("U:/SIMAH/")
data_path <- "SIMAH_workplace/nhis/intersectionality/"
code_path <- "SIMAH_code/nhis/intersectionality/"

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

## Read in data extracted from IPUMS:
ddi <- read_ipums_ddi(paste0(data_path,"raw_data/nhis_00002.xml"))
data <- read_ipums_micro(ddi)



## Rename variables of interest
# nhis_raw_all_years <- mutate(survey_year == SRVY_YR, etc....


## Use functions to clean the data...






# Save data outputs in a clean data file