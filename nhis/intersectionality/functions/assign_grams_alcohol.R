# This script assigns grams of alcohol based on days drank in past year and frequency of drinks per drinking day

# Relevant variables:
# ALC5UPYR	Days had 5+ drinks, past year
# ALCAMT	Average number of drinks on days drank
# ALCSTAT
# ALCDAYSYR	Frequency drank alcohol in past year: Days in past year

assign_grams_alcohol <- function(data){
  data %>% mutate(
    
    # Input missing data for lifetime abstainers, as these questions would not be asked
    ALCDAYSYR = dplyr::if_else(ALCSTAT == 1, 0L, ALCDAYSYR),
    ALC5UPYR = dplyr::if_else(ALCSTAT == 1, 0L, ALC5UPYR),
    ALCAMT = dplyr::if_else(ALCSTAT == 1, 0L, ALCAMT),

    # Calculate average daily grams of alcohol over a year, from days of drinking and average drinks (assuming 14 grams per drink)
      alc_daily_g_crude = (ALCDAYSYR*ALCAMT*14)/ 365 ,   

    # input alcohol_grams for those who didn't drink in past year	
      alc_daily_g_crude = if_else(ALCDAYSYR == 0, 0, alc_daily_g_crude),

    # input alcohol_grams if avg # drinks per drinking day is missing but # drinks a year is very small
      alc_daily_g_crude = if_else(ALCDAYSYR>0 & ALCDAYSYR<12 & is.na(ALCAMT) & (is.na(ALC5UPYR) | ALC5UPYR==0 | ALC5UPYR==1), 1, alc_daily_g_crude),

    # Add grams alcohol from heavy drinking days, among those whose avg drinks per day was <5 drinks; assume 5 drinks on heavy drinking days
      alc_daily_g_heavy = ALC5UPYR / 365 * 5 * 14,  # (heavy drinking days in past year) / 365 * (assume 5 drinks per day) * (14 grams per drink) 

    # Missing alc_grams_heavy set to 0 so that future calculations can still be calculated
     alc_daily_g_heavy = if_else(is.na(alc_daily_g_heavy), 0, alc_daily_g_heavy),

    # If avg drinks per drinking data <5, then add heavy drinking grams to overall grams
     alc_daily_g = if_else(ALCAMT < 5, alc_daily_g_crude + alc_daily_g_heavy, alc_daily_g_crude),
     alc_daily_g = if_else(is.na(alc_daily_g), alc_daily_g_crude, alc_daily_g)) 
}