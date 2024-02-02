# This script assigns grams of alcohol based on days drank in past year and frequency of drinks per drinking day
# (guided by NHIS/Restricted Data/Data management)

# Relevant variables:
# ALC5UPYR	Days had 5+ drinks, past year
# ALCAMT	Average number of drinks on days drank
# ALCSTAT
# ALCDAYSYR	Frequency drank alcohol in past year: Days in past year

assign_grams_alcohol <- function(data){
  data %>% mutate(
    
  # Convert variables about alcohol use to doubles to facilitate data manipulation
  ALCAMT = as.double(data$ALCAMT),
  ALC5UPYR = as.double(data$ALC5UPYR),
  ALCDAYSYR =  as.double(data$ALCDAYSYR), 
    
  # Assign 0 to alcohol-related Qs for lifetime abstainers and former drinkers (otherwise recorded as NA):
  ALCDAYSYR = dplyr::if_else(ALCSTAT1 == 1 | ALCSTAT1 == 2, 0, ALCDAYSYR),
  ALC5UPYR = dplyr::if_else(ALCSTAT1 == 1 | ALCSTAT1 == 2, 0, ALC5UPYR),
  ALCAMT = dplyr::if_else(ALCSTAT1 == 1 | ALCSTAT1 == 2, 0, ALCAMT),

  ## Generate a column to populate with estimates of average daily grams of alcohol...
  alc_daily_g = case_when(
  
  #...Assign 0 grams to people who didn't drink in last year (in case not picked up by ALCSTAT1)
  ALCDAYSYR == 0 ~ 0,

  #... Assign a crude estimate for people who drink >=5 drinks on average and those who never drink more than 5 drinks
  ALCAMT >= 5 | ALCAMT <5 & ALC5UPYR == 0 ~ (ALCDAYSYR * ALCAMT * 14)/ 365,  #(assuming 14 grams per drink)
  
  #...Generate a more detailed estimate for people who usually drink < 5 drinks but sometimes drink >5 (expanded quantity/frequency approach)
  ALCAMT < 5 & ALC5UPYR > 0 ~ ((((ALCAMT*(ALCDAYSYR - ALC5UPYR)) + (ALC5UPYR*5)) *14) / 365))

  )
}