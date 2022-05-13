
# SIMAH Restricted-access Data
# Data Management File

library(haven)      # Read SAS file
library(tidyverse)  # data management
library(janitor)    # clean variable names
library(skimr)      # descriptive statistics
library(survey)     # to accomodate survey weights
library(srvyr)      # adds dplyr like syntax to the survey package
library(dplyr)

# Specify the data file location
# Klajdi's 
data_orig <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Original data/"
data_new  <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Restricted access data/Data/"

# Yachen's 
data_orig <- "C:/Users/yzhu/Desktop/SIMAH project/SIMAH/SIMAH_workplace/nhis/Original data/"
data_new  <- "C:/Users/yzhu/Desktop/SIMAH project/SIMAH/SIMAH_workplace/nhis/Restricted access data/Data/"


# NOTE: the variable names have been structured such that those ending in a number are factors, where the number indicates the number of categories

# Load data and recode/create variables -------------------------------------------------------------------------
nhis <- read_sas (paste0(data_orig, "rdcp2058dataset_temp_mort.sas7bdat")) %>% 
  zap_formats() %>% zap_label() %>%  # removes labels/formats from SAS

  # Recode and create variables
  mutate( 
    
    # PRIMARY EXPOSURE *****************************************************************************************
    # **********************************************************************************************************
    
    ## EDUCATION
      # Coded as EDUC1 (2004-2018) or EDUC (1997-2003)
      edu = case_when(EDUC1 <= 14 | EDUC <= 14 ~ 1,                            
                      EDUC1 %in% c(15,16,17) | EDUC %in% c(15,16,17) ~ 2,       
                      EDUC1 %in% c(18,19,20,21) | EDUC %in% c(18,19,20,21) ~ 3, 
                      TRUE ~ NA_real_),
                
      edu3 = factor(edu, levels=c(3,1,2), labels=c("Bachelors", "Highschool", `2`="Some college")), # the first listed category is the reference
    
    
    # LIFESTYLE FACTORS **************************************************************************************
    # *********************************************************************************************************
    
    ## ALCOHOL USE - 1997-2018
      # Data is consistent from 1997-2018, except that heavy drinking has a different variable name starting in 2014; make data consistent*/
      ALC5UPYR = if_else(SRVY_YR >= 2014, ALC5UPY1, ALC5UPYR), 
      
      # Remove 'refused', 'not ascertained' and 'don't know'
      ALC12MYR = recode (ALC12MYR, `997`=NA_real_, `998`=NA_real_, `999`=NA_real_),
      ALCAMT = recode (ALCAMT, `97`=NA_real_, `98`=NA_real_, `99`=NA_real_),
      ALC5UPYR = recode (ALC5UPYR, `997`=NA_real_, `998`=NA_real_, `999`=NA_real_),
    
      # Input missing data - e.g. # days drank only asked to those who have had >12 drinks in any one year or their lifetime
      ALC12MYR = if_else(is.na(ALC12MYR) & (ALC1YR == 2 | ALCLIFE == 2), 0, ALC12MYR),

      # Create new variables - non-drinkers and heavy-drinkers
      none_drinker = 1,
      none_drinker = if_else(ALC1YR == 1 | ALCLIFE == 1, 0, none_drinker),
      none_drinker = if_else(ALC1YR %in% c(7,8,9), NA_real_, none_drinker),
      
      heavy_drinker = NA_real_,
      heavy_drinker = if_else(ALC5UPYR >= 12, 1, heavy_drinker),
      heavy_drinker = if_else(ALC5UPYR < 12, 0, heavy_drinker),
      heavy_drinker = if_else(none_drinker == 1, 0, heavy_drinker),
      heavy_drinker = if_else(ALCAMT >= 5, 1, heavy_drinker),

      drink_hist = NA_real_,
      drink_hist = if_else(ALC12MYR == 0 & ALC1YR == 2, 0, drink_hist),    # Lifetime abstainers
      drink_hist = if_else(ALC12MYR == 0 & ALC1YR == 1, 1, drink_hist),    # Former drinkers
      drink_hist = if_else(ALC12MYR > 0, 2, drink_hist),                   # Current drinkers
      drink_hist = if_else(is.na(drink_hist) & ALCAMT > 0, 2, drink_hist), # Current Drinkers, though don't know how many drank days in past year
      drink_hist = if_else(is.na(drink_hist) & ALC12MYR == 0 & ALCLIFE == 2, 0, drink_hist),   # Lifetime abstainers
      drink_hist = if_else(is.na(drink_hist) & ALC12MYR == 0 & ALCLIFE == 1, 1 , drink_hist),  # Former drinkers
      
      # Calculate daily grams of alcohol; assuming 14 grams per drink
    	    alc_daily_g_crude = ALC12MYR / 365 * ALCAMT * 14,    # (days drank in past year) / 365 * (freq drinks per drinking day) * (14 grams per drink)
          
    			# input alcohol_grams for those who didn't drink in past year	
          alc_daily_g_crude = if_else(ALC12MYR == 0, 0, alc_daily_g_crude),
    					
    			# input alcohol_grams if avg # drinks per drinking day is missing but # drinks a year is very small
          alc_daily_g_crude = if_else(ALC12MYR>0 & ALC12MYR<12 & is.na(ALCAMT) & (is.na(ALC5UPYR) | ALC5UPYR==0 | ALC5UPYR==1), 1, alc_daily_g_crude),
     
    			# Add grams alcohol from heavy drinking days, among those whose avg drinks per day was <5 drinks; assume 5 drinks on heavy drinking days
    			alc_daily_g_heavy = ALC5UPYR / 365 * 5 * 14,  # (heavy drinking days in past year) / 365 * (assume 5 drinks per day) * (14 grams per drink) 
    					
    			# Missing alc_grams_heavy set to 0 so that future calculations can still be calculated
          alc_daily_g_heavy = if_else(is.na(alc_daily_g_heavy), 0, alc_daily_g_heavy),
 
    			# If avg drinks per drinking data <5, then add heavy drinking grams to overall grams
          alc_daily_g = if_else(ALCAMT < 5, alc_daily_g_crude + alc_daily_g_heavy, alc_daily_g_crude),
          alc_daily_g = if_else(is.na(alc_daily_g), alc_daily_g_crude, alc_daily_g),

    
      # Create the Alcohol Use Categorical Variable
  		alcohol6 = case_when(	
  		                  # Females
                        SEX==2 & alc_daily_g == 0 & drink_hist == 0 ~ 1, # Lifetime abstainer
                        SEX==2 & alc_daily_g == 0 & drink_hist == 1 ~ 2, # Former drinker
                        SEX==2 & alc_daily_g >0 & alc_daily_g <= 20 ~ 3, # Category I
                        SEX==2 & alc_daily_g >20 & alc_daily_g <=40 ~ 4, # Cateogry II
                        SEX==2 & alc_daily_g >40 & alc_daily_g <=60 ~ 5, # Category III
                        SEX==2 & alc_daily_g >60                            ~ 6, # Category IV
                    
                        # Males
                        SEX==1 & alc_daily_g == 0 & drink_hist == 0  ~ 1, # Lifetime abstainer
                        SEX==1 & alc_daily_g == 0 & drink_hist == 1  ~ 2, # Former drinker
                        SEX==1 & alc_daily_g >0 & alc_daily_g <= 40  ~ 3, # Category I
                        SEX==1 & alc_daily_g >40 & alc_daily_g <=60  ~ 4, # Cateogry II
                        SEX==1 & alc_daily_g >60 & alc_daily_g <=100 ~ 5, # Category III
                        SEX==1 & alc_daily_g >100                            ~6, # Category IV
  		                  TRUE ~ NA_real_), 
      
      alcohol5 = recode(alcohol6, `1`=1, `2`=2, `3`=3, `4`=4, `5`=5, `6`=5),  # merge category III and IV
      alcohol4 = recode(alcohol6, `1`=1, `2`=1, `3`=2, `4`=3, `5`=4, `6`=4),  # merge category III and IV and abstainers/former drinkers
      
      alc6 = factor(alcohol6, levels=c(3,1,2,4,5,6), labels=c("Category I", "Lifetime abstainer", "Former drinker", "Category II", "Category III", "Category IV")), # the first listed category is the reference
      alc5 = factor(alcohol5, levels=c(3,1,2,4,5), labels=c("Category I", "Lifetime abstainer", "Former drinker", "Category II", "Category III")),
      alc4 = factor(alcohol4, levels=c(2,1,2,4), labels=c("Category I", "Non-drinker", "Category II", "Category III")),
    
    
    
      # Calculate frequency of heavy episodic drinking 
      hed = case_when(
                      ALC5UPYR >= 52 ~ 4,                                                 # HED once a week or more
                      ALC5UPYR >= 12 & ALC5UPYR < 52 ~ 3,                                 # HED more than once a month but less than once a week
                      ALC5UPYR >= 1 & ALC5UPYR < 12 ~ 2,                                  # HED less than once a month
                      ALC5UPYR == 0 | (ALC1YR == 2) | ALCLIFE == 2 | ALC12MYR == 0 ~ 1),  # No HED
                      
          # Fill in missing values
          hed = ifelse(is.na(hed) & (ALCAMT < 5 & !is.na(ALCAMT)) & (ALC12MYR == 1 & !is.na(ALC12MYR)), 1, hed),
          hed = ifelse(is.na(hed) & (is.na(ALCAMT)) & (ALC12MYR == 1 & !is.na(ALC12MYR)), 1, hed),
          hed = ifelse((is.na(hed) | (ALC5UPYR == 0 & !is.na(ALC5UPYR))) & (ALCAMT>=5 & !is.na(ALCAMT)) & (ALC12MYR >=1 & ALC12MYR <12 & !is.na(ALC12MYR)), 2, hed),
          hed = ifelse((is.na(hed) | (ALC5UPYR == 0 & !is.na(ALC5UPYR))) & (ALCAMT>=5 & !is.na(ALCAMT)) & (ALC12MYR >=12 & ALC12MYR <52 & !is.na(ALC12MYR)), 3, hed),
          hed = ifelse((is.na(hed) | (ALC5UPYR == 0 & !is.na(ALC5UPYR))) & (ALCAMT>=5 & !is.na(ALCAMT)) & (ALC12MYR >= 52 & !is.na(ALC12MYR)), 4, hed),
          
      hed4 = factor(hed, levels=c(1,2,3,4), labels=c("No HED", "HED <1/month", "HED >1/month, <1/week", "HED >=1/week")), 

    
    ## SMOKING - 1997-2018 
    smk = recode (SMKSTAT2, `1`=4, `2`=3, `3`=2, `4`=1, `5`=NA_real_, `9`=NA_real_),
  
    smk4 = factor(smk, levels=c(1,2,3,4), labels=c("Current everyday smoker", "Current someday smoker", "Former smoker ", "Never smoker")), 
    
    
    ## BMI - 1997-2018
    BMI = if_else(BMI==99.99, NA_real_, BMI), # remove 'unknown' category
    bmi = case_when(BMI < 18.5 ~ 1,           
                    BMI >=18.5 & BMI < 25 ~ 2, 
                    BMI >=25 & BMI < 30 ~ 3,   
                    BMI >=30 ~ 4,
                    is.na(BMI) ~ NA_real_),        
    
    bmi4 = factor(bmi, levels=c(2,1,3,4), labels=c("Healthy weight", "Underweight", "Overweight", "Obese")), 
    

    ## PHYSICAL ACTIVITY 1997-2018
    
        # Remove 'refused', 'not ascertained' and 'don't know'; 'unable to' or 'never' coded as 0 
        VIGFREQW = recode(VIGFREQW, `95`=0, `96`=0, `97`=NA_real_, `98`=NA_real_, `99`=NA_real_),
        VIGMIN = recode(VIGMIN, `997`=NA_real_, `998`=NA_real_, `999`=NA_real_),
        VIGMIN = if_else(is.na(VIGMIN) & VIGFREQW %in% c(0, 95, 96), 0, VIGMIN),
        
        MODFREQW = recode(MODFREQW, `95`=0, `96`=0, `97`=NA_real_, `98`=NA_real_, `99`=NA_real_),
        MODMIN = recode(MODMIN, `997`=NA_real_, `998`=NA_real_, `999`=NA_real_),
        MODMIN = if_else(is.na(MODMIN) & MODFREQW %in% c(0, 95, 96), 0, MODMIN),
        
        # Calculate weekly minutes spent on physical activity
        min_wkly_vig_act = VIGFREQW * VIGMIN,
        min_wkly_mod_act = MODFREQW * MODMIN,
        
        min_wkly_mod_combined = (min_wkly_vig_act * 2) + min_wkly_mod_act, 
        
        # Create categorical variable for physical activity
        phy = case_when( min_wkly_mod_combined == 0 ~ 1,                               
                         min_wkly_mod_combined > 0 & min_wkly_mod_combined < 150 ~ 2,  
                         min_wkly_mod_combined >= 150 ~ 3,
                         is.na(min_wkly_vig_act) & min_wkly_mod_act >= 150 ~ 3, 
                         is.na(min_wkly_mod_act) & min_wkly_vig_act >= 75 ~ 3),
    
        phy3 = factor(phy, levels=c(3,1,2), labels=c("Active", "Sedentary", "Somewhat active")), 
          
          
    # COVARIATES **********************************************************************************************
    # *********************************************************************************************************

    ## SEX - 1997-2018
    female = recode(SEX, `1`=0, `2`=1),
    female2 = factor(female, levels=c(0,1), labels=c("Men", "Women")), 
    
    
    # #MARITAL STATUS - 1997-2018
    married = recode(R_MARITL, `1`=1, `2`=1, `3`=1, `4`=0, `5`=0, `6`=0, `7`=0, `8`=1, `9`=NA_real_), 
    married2 = factor(married, levels=c(0,1), labels=c("Not married/cohabitating", "Married/cohabitating")), 
    
  
    ## RACE/ETHNICITY
    race = case_when(
      HISCODI3 == 2        | HISCODI2 == 2 | HISCOD_I == 2 | HISPCODR == 2 | HISPCODE == 2 | (ORIGIN == 2 & RACEREC == 1) ~ 1, # White
      HISCODI3 == 3        | HISCODI2 == 3 | HISCOD_I == 3 | HISPCODR == 3 | HISPCODE == 3 | (ORIGIN == 2 & RACEREC == 2) ~ 2, # black
      HISCODI3 == 1        | HISCODI2 == 1 | HISCOD_I == 1 | HISPCODR == 1 | HISPCODE == 1 |  ORIGIN == 1                 ~ 3, # hispanic
      HISCODI3 %in% c(4,5) | HISCODI2 == 4 | HISCOD_I == 4 | HISPCODR == 4 | HISPCODE == 4 | (ORIGIN == 2 & RACEREC == 3) ~ 4),# other
    
    race4 = factor(race, levels=c(1,2,3,4), labels=c("White", "Black", "Hispanic ", "Other")), 
    
    
    # OTHER VARIABLES *****************************************************************************************
    # *********************************************************************************************************

    ## US Born 
    US_born = case_when(USBORN_P==1 | USBRTH_P==1 | GEOBRTH==1 ~ 1,
                        USBORN_P==2 | USBRTH_P==2 | GEOBRTH %in% c(2,3) ~ 0),

    US_born2 = factor(US_born,  levels=c(0,1), labels=c("Not US born", "US born")), # Category at the very end is the reference category
    
    
    ## FAMILY INCOME - ratio of family income to the poverty threshold
    
    income = case_when(RAT_CAT %in% c(1,2,3)   | RAT_CAT3 %in% c(1,2,3,15)    | RAT_CAT5 %in% c(1,2,3,15)   ~ 1,  # poor: <100% of poverty threshold
                       RAT_CAT %in% c(4,5,6,7)  | RAT_CAT3 %in% c(4,5,6,7,16)  | RAT_CAT5 %in% c(4,5,6,7,16)  ~ 2,  # near poor: 100-199% of poverty threshold
                       RAT_CAT %in% c(8,9,10,11)| RAT_CAT3 %in% c(8,9,10,11,17)| RAT_CAT5 %in% c(8,9,10,11,17)~ 3,  # middle income: 200-399% of poverty threshold
                       RAT_CAT %in% c(12,13,14) | RAT_CAT3 %in% c(12,13,14,18) | RAT_CAT5 %in% c(12,13,14,18) ~ 4,  # higher income: >=400% of poverty threshold
                       TRUE ~ 5),   # No income data

    income_v2 = recode(income, `1`=1, `2`=2, `3`=2, `4`=3, `5`=4),     # merge the two middle categories
    
    income5 = factor(income, levels=c(4,1,2,3,5), labels=c("Higher income", "Poor", "Near poor", "Middle income", "Missing")), 
    income4 = factor(income_v2, levels=c(3,1,2,4), labels=c("High", "Low", "Medium", "Missing")), 
    
    
    
    ## PSYCHOLOGICAL DISTRESS
      # Data is consistent from 1997-2012, except that FEELINGS have a different variable name in 2013-2018; make data consistent
      SAD      = if_else(SRVY_YR >= 2013, ASISAD, SAD),  
      EFFORT   = if_else(SRVY_YR >= 2013, ASIEFFRT, EFFORT),
      HOPELESS = if_else(SRVY_YR >= 2013, ASIHOPLS, HOPELESS),
      NERVOUS  = if_else(SRVY_YR >= 2013, ASINERV, NERVOUS),
      RESTLESS = if_else(SRVY_YR >= 2013, ASIRSTLS, RESTLESS),
      WORTHLS  = if_else(SRVY_YR >= 2013, ASIWTHLS, WORTHLS),

      # Remove 'refused', 'not ascertained' and 'don't know'
      SAD      = recode(SAD,      `7`=NA_real_, `8`=NA_real_, `9`=NA_real_),
      EFFORT   = recode(EFFORT,   `7`=NA_real_, `8`=NA_real_, `9`=NA_real_),
      HOPELESS = recode(HOPELESS, `7`=NA_real_, `8`=NA_real_, `9`=NA_real_),
      NERVOUS  = recode(NERVOUS,  `7`=NA_real_, `8`=NA_real_, `9`=NA_real_),
      RESTLESS = recode(RESTLESS, `7`=NA_real_, `8`=NA_real_, `9`=NA_real_),
      WORTHLS  = recode(WORTHLS,  `7`=NA_real_, `8`=NA_real_, `9`=NA_real_),
    
		  # Calculate psych distress score
      EFFORT = 5 - EFFORT,
      HOPELESS = 5 - HOPELESS,
      NERVOUS = 5 - NERVOUS,
      RESTLESS = 5 - RESTLESS,
      SAD = 5 - SAD,
      WORTHLS = 5 - WORTHLS,

  		K6scale = HOPELESS + EFFORT  + SAD + WORTHLS + NERVOUS + RESTLESS,
      K6scale = if_else(is.na(HOPELESS) | is.na(EFFORT) | is.na(SAD) | is.na(WORTHLS) | is.na(NERVOUS) | is.na(RESTLESS), NA_real_, K6scale),

      # Categorize psych distress score
      PsyDistr = case_when( K6scale < 5 ~ 1,                # none to low psychological distress
                            K6scale >= 5 & K6scale < 13 ~ 2, # moderate psychological distress
                            K6scale >= 13 ~ 3),              # severe psychological distress
    
      PsyDistr3 = factor(PsyDistr, levels=c(1,2,3), labels=c("None/low", "Moderate", "Severe")), 
    
    
    ## HEALTH STATUS
    health = PHSTAT, 
    health = recode(health, `7`=NA_real_, `8`=NA_real_, `9`=NA_real_),  # remove Refused, Not ascertained, Don't know*/
    
    health5 = factor(health,  levels=c(1,2,3,4,5), labels=c("Excellent", "Very good", "Good", "Fair", "Poor")),
    
    
    ## HYPERTENSION - 1997-2018
    hypertension = if_else(HYPDIFV==1, 1, NA_real_),     # Hypertension at least 2 times
    hypertension = if_else(HYPDIFV==2, 0, hypertension), # No hypertension
    hypertension = if_else(HYPEV==2, 0, hypertension),   # No hypertension
    
    hypertension2 = factor(hypertension, levels=c(0,1), labels=c("No hypertension", "Hypertension")), 
    
    
    ## DIABETES - 1997-2018 
    DIBEV = if_else(SRVY_YR >= 2016, DIBEV1, DIBEV),   # Variable was renamed in 2016
    diabet = if_else(DIBEV == 1, 2, NA_real_),        # Diabetes
    diabet = if_else(DIBEV == 3, 1, diabet),          # Bordermine
    diabet = if_else(DIBEV == 2, 0, diabet),          # No diabetes
    
    diabet3 = factor(diabet,  levels=c(0,1,2), labels=c("No diabetes", "Bordermine", "Diabetes")),
    
    # Survey year
    srvy_yr = SRVY_YR,
    srvy_yr22 = factor(SRVY_YR),
    
    # Design variables
    new_psu = PSU,
    new_psu = if_else(is.na(new_psu), PSU_P, new_psu),
    new_psu = if_else(is.na(new_psu), PPSU, new_psu),

    new_stratum = STRATUM + 1000,
    new_stratum = if_else(is.na(new_stratum), STRAT_P + 2000, new_stratum),
    new_stratum = if_else(is.na(new_stratum), PSTRAT + 3000, new_stratum),

    new_weight = SA_WGT_NEW / 22,     # divide by # of NHIS surveys that are pooled together
    
    
    # interaction variables
    edu_sex = interaction(edu3, female2), 
    
    # TEMPORARY Variables (need updating)
    # bl_age = AGE_P,       # Baseline age
    allcause_death = MORTSTAT,
    heart_death = ifelse(UCOD_LEADING=="001", 1, 0),
    cancer_death = ifelse(UCOD_LEADING=="002", 1, 0),
    accident_death = ifelse(UCOD_LEADING=="004", 1, 0),
    end_age = bl_age + yrs_followup) %>%    
    
  
  # Select variables to keep
  select (PUBLICID, new_weight, new_psu, new_stratum,
          srvy_yr, srvy_yr22, bl_age, end_age, yrs_followup, 
          
          allcause_death, 
          
          heart_death, cancer_death, accident_death,
          edu, edu3, alc_daily_g, alcohol6, alcohol5, alcohol4, alc6, alc5, alc4, hed, hed4, smk, smk4, bmi, bmi4, phy, phy3,
          female, female2, married, married2, race, race4, edu_sex,
          income, income_v2, income5, income4, K6scale, PsyDistr, PsyDistr3, 
          US_born, US_born2, health, health5, hypertension, hypertension2, diabet, diabet3) %>%

  #filter(srvy_yr <=2014 & !is.na(new_weight))     # TEMPORARY (for testing purposes)



# Review coding and Check frequencies with SAS version
# count(nhis, edu, edu3)
# count(nhis, alcohol6, alcohol5, alcohol4, alc6, alc5, alc4)
# count(nhis, hed, hed4)
# count(nhis, smk, smk4)
# count(nhis, bmi, bmi4)
# count(nhis, phy, phy3)
# count(nhis, female, female2)
# count(nhis, married, married2)
# count(nhis, race, race4)
# count(nhis, income, income_v2, income5, income4)
# count(nhis, PsychDistr, PsychDistr3)
# count(nhis, US_born, US_born2)
# count(nhis, health, health5)
# count(nhis, hypertension, hypertension2)
# count(nhis, diabet, diabet3)


# Mortality data
# The sample weight variable (SA_WGT_NEW) needs to be divided by 22 because 22 survey years were pooled form NHIS


# Finalize data set ---------------------------------------------------------------------------------------------------------------
# Create subset of data with relevant participants     
nhis_clean <- nhis %>%
  # Remove those outside our age range:
  filter(bl_age>=25) %>%
  
  # Remove those with missing data:
  filter(complete.cases(allcause_death, heart_death, end_age, edu3, alc5, bl_age, female, married, race4))

# Create database specific to males or females
nhis_female <- filter(nhis_clean, female==1)
nhis_male <- filter(nhis_clean, female==0)
        


# Finalize data set adjusting for survey weights
# nhis_svy <- svydesign(id=~new_psu, strata=~new_stratum,weights=~new_weight,
#                       nest = TRUE, data= nhis)
# 
# nhis_clean_svy <- subset(nhis_svy, bl_age>=25 & !is.na(allcause_death) & !is.na(heart_death) & !is.na(end_age) &
#                         !is.na(edu3) & !is.na(alc5) & !is.na(bl_age) & !is.na(female) & !is.na(married) & !is.na(race4))
# 
# nhis_female_svy <- subset(nhis_clean_svy, female==1)
# nhis_male_svy <- subset(nhis_clean_svy, female==0)


# Using {srvyr} finalize data set adjusting for survey weights
nhis_svy <- nhis %>%
  as_survey_design(id=new_psu, strata=new_stratum, weights=new_weight, nest = TRUE)

nhis_clean_svy <- nhis_svy %>%
  filter(bl_age>=25) %>%
  filter(complete.cases(allcause_death, heart_death, end_age, edu3, alc5, bl_age, female, married, race4))

nhis_female_svy <- filter(nhis_svy, female==1)
nhis_male_svy <- filter(nhis_svy, female==0)




        
# Save copy of final datasets  
saveRDS(nhis,        paste0(data_new, "nhis_all.rds"))      # NHIS data with all participants
saveRDS(nhis_clean,  paste0(data_new, "nhis_clean.rds"))    # NHIS data to be analyzed
saveRDS(nhis_male,   paste0(data_new, "nhis_male.rds"))     # NHIS data to be analyzed (males only)
saveRDS(nhis_female, paste0(data_new, "nhis_female.rds"))   # NHIS data to be analyzed (females only)
        
saveRDS(nhis_svy,        paste0(data_new, "nhis_all_svy.rds"))      # NHIS_svy data with all participants
saveRDS(nhis_clean_svy,  paste0(data_new, "nhis_clean_svy.rds"))    # NHIS_svy data to be analyzed
saveRDS(nhis_male_svy,   paste0(data_new, "nhis_male_svy.rds"))     # NHIS_svy data to be analyzed (males only)
saveRDS(nhis_female_svy, paste0(data_new, "nhis_female_svy.rds"))   # NHIS_svy data to be analyzed (females only)   
