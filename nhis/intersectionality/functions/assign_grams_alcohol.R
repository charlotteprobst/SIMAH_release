# This script assigns grams of alcohol based on days drank in past year and frequency of drinks per drinking day
# (guided by NHIS/Restricted Data/Data management)

# Relevant variables:
# ALC5UPYR	Days had 5+ drinks, past year
# ALCAMT	Average number of drinks on days drank
# ALCSTAT
# ALCDAYSYR	Frequency drank alcohol in past year: Days in past year

#ALCSTAT1 == 1 ~ 1,

assign_grams_alcohol <- function(data){
  data %>% mutate(
    
  # Convert variables about alcohol use to doubles to facilitate data manipulation
  ALCAMT = as.double(data$ALCAMT),
  ALC5UPYR = as.double(data$ALC5UPYR),
  ALCDAYSYR =  as.double(data$ALCDAYSYR), 
    
  # Assign 0 to alcohol-related Qs for lifetime abstainers and former drinkers (otherwise recorded as NA):
  ALCDAYSYR = dplyr::if_else(ALCSTAT == 1 | ALCSTAT == 2, 0, ALCDAYSYR),
  ALC5UPYR = dplyr::if_else(ALCSTAT == 1 | ALCSTAT == 2, 0, ALC5UPYR),
  ALCAMT = dplyr::if_else(ALCSTAT == 1 | ALCSTAT == 2, 0, ALCAMT),

  ## Generate a column to populate with estimates of average daily grams of alcohol...
  alc_daily_g = case_when(
  
  #...Assign 0 grams to people who didn't drink in last year
  ALCDAYSYR == 0 ~ 0,
  
  #... Assign 1 gram to people with unknown # drinks per occasion, but known to drink < 12 times a year & drink 5+ drinks < twice a year
  ALCDAYSYR > 0 & ALCDAYSYR < 12 & (ALC5UPYR==0 | ALC5UPYR==1) ~ 1,
  
  #... Assign a crude estimate for people who drink >5 drinks on average, those who always consume <5 drinks,
  # and those who drink < 5 drinks on average and for whom data on number of days consuming 5+ drinks is missing
  # (i.e. applying basic quantity/frequency approach)
  ALCAMT > 5 | (ALCAMT <= 5 & ALC5UPYR < 1) | (ALCAMT <= 5 & is.na(ALC5UPYR)) ~ (ALCDAYSYR * ALCAMT * 14)/ 365,  #(assuming 14 grams per drink) ## PROBLEM AFTER THE TILDA
  
  #...Generate a more detailed estimate for people who usually drink <= 5 drinks but sometimes drink >5(expanded quantity/frequency approach)
  ALCAMT <= 5 & ALC5UPYR >0 ~ ((((ALCAMT*(ALCDAYSYR - ALC5UPYR)) + (ALC5UPYR*5)) *14) / 365))
  )
}