
# SIMAH - NESARC Alcohol Transitions
# Data Management

library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(survey)     # to accomodate survey weights


## Set the working directory
setwd("C:/Users/klajd/OneDrive/SIMAH")
data <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Data/"



# Edit data - recode and recategorize variables
nesarc <- readRDS(paste0(data, "nesarc.rds")) %>%

  # Re-code variables
  mutate(
    married = recode(marital_stat, `1`= 1, `2`=1, `3`=0, `4`=0, `5`=0, `6`=0),
    edu3 = case_when(edu %in% c(1,2,3,4,5,6,7,8,9) ~ 1,
                     edu %in% c(10,11) ~ 2,
                     edu %in% c(12,13,14) ~ 3),
    income3 = case_when(fam_income %in% c(1,2,3,4,5,6) ~ 1, # split into evenly sized groups based on Wave 1 data
                        fam_income %in% c(7,8,9,10,11) ~ 2,
                        fam_income %in% c(12,13,14,15,16,17,18,19,20,21) ~ 3),
    
    # Calculating alcohol intake:
        # Recode "99" (missing) to NA
        across(c(S2AQ4B, s2aq4cr, S2AQ4D, S2AQ4E, S2AQ4F, S2AQ4G,
                 S2AQ5B, s2aq5cr, S2AQ5D, S2AQ5E, S2AQ5F, S2AQ5G,
                 S2AQ6B, s2aq6cr, S2AQ6D, S2AQ6E, S2AQ6F, S2AQ6G,
                 S2AQ7B, s2aq7cr, S2AQ7D, S2AQ7E, S2AQ7F, S2AQ7G),
          ~ recode(.x, `99`=NA_real_)),
    
        # Recode frequencies to number drinking days (variable: avg # of drinking days)
        across(c(S2AQ4B, S2AQ4F, S2AQ4G, S2AQ5B, S2AQ5F, S2AQ5G, S2AQ6B, S2AQ6F, S2AQ6G, S2AQ7B, S2AQ7F, S2AQ7G), 
          ~ recode(.x, `1`=365, `2`=273, `3`=182, `4`=104, `5`=52, `6`=30, `7`=12, `8`=9, `9`=4.5, `10`=1.5, `11`=0), .names = "{.col}_days"),
        

        # Calculate total drinks per year (as per the manual)
        coolers_yearly = ifelse(S2AQ4D <= 5, 
                                  (S2AQ4D * (S2AQ4B_days - S2AQ4F_days)) + (S2AQ4E * S2AQ4F_days), # quantity if largest drinks <=5
                                  (S2AQ4D * (S2AQ4B_days - S2AQ4F_days)) + ((S2AQ4G_days - S2AQ4F_days)*(exp((log(pmax(5, S2AQ4D)) + log(S2AQ4E - 1)) /2))) + (S2AQ4E*S2AQ4F_days)),      # quantity if largest drinks >5
        beers_yearly = ifelse(S2AQ5D <= 5, 
                                  (S2AQ5D * (S2AQ5B_days - S2AQ5F_days)) + (S2AQ5E * S2AQ5F_days),
                                  (S2AQ5D * (S2AQ5B_days - S2AQ5F_days)) + ((S2AQ5G_days - S2AQ5F_days)*(exp((log(pmax(5, S2AQ5D)) + log(S2AQ5E - 1)) /2))) + (S2AQ5E*S2AQ5F_days)),
        
        wine_yearly = ifelse(S2AQ6D <= 5, 
                                  (S2AQ6D * (S2AQ6B_days - S2AQ6F_days)) + (S2AQ6E * S2AQ6F_days), 
                                  (S2AQ6D * (S2AQ6B_days - S2AQ6F_days)) + ((S2AQ6G_days - S2AQ6F_days)*(exp((log(pmax(5, S2AQ6D)) + log(S2AQ6E - 1)) /2))) + (S2AQ6E*S2AQ6F_days)),
        
        liquor_yearly = ifelse(S2AQ7D <= 5, 
                              (S2AQ7D * (S2AQ7B_days - S2AQ7F_days)) + (S2AQ7E * S2AQ7F_days), 
                              (S2AQ7D * (S2AQ7B_days - S2AQ7F_days)) + ((S2AQ7G_days - S2AQ7F_days)*(exp((log(pmax(5, S2AQ7D)) + log(S2AQ7E - 1)) /2))) + (S2AQ7E*S2AQ7F_days)), 
                        
        # Calculate daily ethnanol intake 
        coolers_daily_oz = (coolers_yearly * (s2aq4cr * coolecf))/365,
        beers_daily_oz = (beers_yearly * (s2aq5cr * beerecf))/365,
        wine_daily_oz = (wine_yearly * (s2aq6cr * wineecf))/365,
        liquor_daily_oz = (liquor_yearly * (s2aq7cr * liqrecf))/365) 
        # ended the mutate() function since it was the only way to get the next line to work (the alc_daily_oz calculation)
  
  nesarc <- nesarc %>%
    mutate(
      #calculate total alcohol ounces per day 
        alc_daily_oz = rowSums(select(., coolers_daily_oz, beers_daily_oz, wine_daily_oz, liquor_daily_oz), na.rm = TRUE), # those with only NAs get a value of 0
        
        # Code as NA those who reported drinking a cooler, beer, wine, or liquor but their alc_daily_oz = 0
        alc_daily_oz = if_else(alc_daily_oz==0 & (S2AQ4A==1 | S2AQ5A==1 | S2AQ6A==1 | S2AQ7A==1), NA_real_, alc_daily_oz),
        
        # Code as NA those who with 'unknown' in at least one of cooler, beer, wine, or liquor and 'No' or 'unknown' to all the others 
        alc_daily_oz = if_else(alc_daily_oz==0 & (S2AQ4A==9 & S2AQ5A%in%c(2,9) & S2AQ6A%in%c(2,9) & S2AQ7A%in%c(2,9)), NA_real_, alc_daily_oz),
        alc_daily_oz = if_else(alc_daily_oz==0 & (S2AQ4A%in%c(2,9) & S2AQ5A==9 & S2AQ6A%in%c(2,9) & S2AQ7A%in%c(2,9)), NA_real_, alc_daily_oz),
        alc_daily_oz = if_else(alc_daily_oz==0 & (S2AQ4A%in%c(2,9) & S2AQ5A%in%c(2,9) & S2AQ6A==9 & S2AQ7A%in%c(2,9)), NA_real_, alc_daily_oz),
        alc_daily_oz = if_else(alc_daily_oz==0 & (S2AQ4A%in%c(2,9) & S2AQ5A%in%c(2,9) & S2AQ6A%in%c(2,9) & S2AQ7A==9), NA_real_, alc_daily_oz),
        
        # Impute 0 ounces daily for non-drinkers
        alc_daily_oz = if_else(drinking_stat%in%c(2,3), 0, alc_daily_oz), 
      
      # Recategorize daily alcohol use using other units
      alc_daily_g = alc_daily_oz * 28.3495,   # Convert daily ounces to grams  
      alc_daily_drinks = alc_daily_oz  / 0.60, # Coverty to daily # of drinks, assuming 0.60oz per drink, as per NESARC guidelines
      
      # Categorize alcohol use as per NESARC guidelines
      alc4_nesarc = case_when(
        # Men:
        female==0 & alc_daily_oz==0 ~ 1,                         # non-drinkers
        female==0 & alc_daily_oz>0 & alc_daily_oz <= 0.257 ~ 2,  # light drinker, i.e., 3 or fewer drinks per week
        female==0 & alc_daily_oz>0.257 & alc_daily_oz <=1.2 ~ 3, # moderate drinker, i.e., 3 to 14 drinks per week
        female==0 & alc_daily_oz>1.2 ~ 4,                        # heavier drinker, i.e., more than 2 drinks
          
        # Woen:
        female==1 & alc_daily_oz==0 ~ 1,                         # non-drinkers
        female==1 & alc_daily_oz>0 & alc_daily_oz <= 0.257 ~ 2,  # light drinker, i.e., 3 or fewer drinks per week
        female==1 & alc_daily_oz>0.257 & alc_daily_oz <=0.6 ~ 3, # moderate drinker, i.e., 3 to 7 drinks per week
        female==1 & alc_daily_oz>0.6 ~ 4),                       # heavier drinker, i.e., more than 1 drink
        
      
      # Categorize alcohol use as per SIMAH protocol
      alc5 = case_when(
        # Men:
        female==0 & alc_daily_g==0 ~ 1,                       # abstinence
        female==0 & alc_daily_g >0  & alc_daily_g <=40 ~ 2,   # low risk
        female==0 & alc_daily_g >40  & alc_daily_g <=60 ~ 3,  # medium risk
        female==0 & alc_daily_g >60  & alc_daily_g <=100 ~ 4, # high risk
        female==0 & alc_daily_g >100 ~ 5,                     # very high risk
        # Women:
        female==1 & alc_daily_g==0 ~ 1,                       # abstinence
        female==1 & alc_daily_g >0  & alc_daily_g <=20 ~ 2,   # low risk
        female==1 & alc_daily_g >20  & alc_daily_g <=40 ~ 3,  # medium risk
        female==1 & alc_daily_g >40  & alc_daily_g <=60 ~ 4,  # high risk
        female==1 & alc_daily_g >60 ~ 5),                     # very high risk
      
      alc4 = recode(alc5, `5`=4),   # merge high-risk and very-high-risk categories

         
    # Calculate heavy episodic drinking (HED)
    hed = case_when(
                    # Men:
                    female==0 & drank5plus_freq %in% c(1,2,3,4,5) ~ 4, # HED >= 1/week
                    female==0 & drank5plus_freq %in% c(6,7) ~ 3,       # HED >= 1/month but < 1/week
                    female==0 & drank5plus_freq %in% c(8,9,10) ~ 2,    # HED < 1 / month
                    female==0 & drank5plus_freq %in% c(11) ~ 1,        # No HED in last year
                    # Women:
                    female==1 & drank4plus_freq %in% c(1,2,3,4,5) ~ 4, # HED >= 1/week
                    female==1 & drank4plus_freq %in% c(6,7) ~ 3,       # HED >= 1/month but < 1/week
                    female==1 & drank4plus_freq %in% c(8,9,10) ~ 2,    # HED < 1 / month
                    female==1 & drank4plus_freq %in% c(11) ~ 1))       # No HED in last year

  
  # Check  
  # count(nesarc, marital_stat, married)
  # count(nesarc, edu, edu3)
  # view(count(nesarc, fam_income, income3))
  # count(nesarc, S2AQ4B, S2AQ4B_days)
  # count(nesarc, S2AQ5B, S2AQ5B_days)
  # select(nesarc, coolers_yearly,beers_yearly, wine_yearly, liquor_yearly) %>% skim()
  #     select(nesarc, idnum, wave, drinking_stat, S2AQ4A, S2AQ4D, S2AQ4B_days, S2AQ4F_days, S2AQ4E, S2AQ4G_days, coolers_yearly) %>% write.csv(paste0(data_orig, "check_coolers.csv", na=""))
  # select(nesarc, coolers_daily_oz, beers_daily_oz, wine_daily_oz, liquor_daily_oz, alc_daily_oz, alc_daily_g) %>% skim()
  #     select(nesarc, idnum, wave, drinking_stat, alc_daily_g, alc_daily_oz, coolers_daily_oz, beers_daily_oz, wine_daily_oz, liquor_daily_oz, 
  #       S2AQ4A, S2AQ4B, s2aq4cr, S2AQ4D, S2AQ4E, S2AQ4F, S2AQ4G, coolecf, 
  #       S2AQ5A, S2AQ5B, s2aq5cr, S2AQ5D, S2AQ5E, S2AQ5F, S2AQ5G, beerecf, 
  #       S2AQ6A, S2AQ6B, s2aq6cr, S2AQ6D, S2AQ6E, S2AQ6F, S2AQ6G, wineecf, 
  #       S2AQ7A, S2AQ7B, s2aq7cr, S2AQ7D, S2AQ7E, S2AQ7F, S2AQ7G, liqrecf) %>% write.csv(paste0(data_orig, "check_alc.csv", na = ""))
  # count(nesarc, alc4_nesarc)
  # count(nesarc, alc5, alc4)
    
  
   
# Create clean copy of data
nesarc_clean <- nesarc %>%
  
  # order data by ID, then by wave (timepoint)
  arrange(idnum, wave) %>% 
  
  # Identify the variables to keep
  select(idnum, wave, psu, stratum, weight, age, years, female, race, married, edu3, income3, 
         alc_daily_oz, alc_daily_g, alc_daily_drinks, alc4_nesarc, alc5, alc4, hed) %>%
  
  # remove those with data at one time point (8,440 observations removed; n=69,306)
  group_by(idnum) %>%
  filter(n()>1) %>%
  ungroup() %>%

  # remove those aged >85 at wave 1 (given that ages >=90 are coded as 90; 711 observations removed; n=68,592)
  filter(!(wave==1 & age>85)) %>% 
  group_by(idnum) %>%
  filter(n()>1) %>%
    
  # remove those with missing alcohol data in either time point (424 observations removed; n=68,168)
  group_by(idnum) %>% 
  filter(!any(is.na(alc4))) %>%
  ungroup() 
  
  
    

# label values
    nesarc_clean$race.factor <- factor(nesarc_clean$race, levels=c(1,2,3,4), 
      labels=c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic", "Other, non-Hispanic"))
      nesarc_clean$race.factor <- relevel(nesarc_clean$race.factor, ref = "White, non-Hispanic")  # specifies the reference category
    
    nesarc_clean$married.factor <- factor(nesarc_clean$married, levels=c(1,0), 
      labels=c("Married/cohab.", "Single"))
      nesarc_clean$married.factor <- relevel(nesarc_clean$married.factor, ref = "Married/cohab.")  # specifies the reference category
    
    nesarc_clean$edu3.factor <- factor(nesarc_clean$edu3, levels=c(1,2,3), 
      labels=c("Low", "Med", "High"))
      nesarc_clean$edu3.factor <- relevel(nesarc_clean$edu3.factor, ref = "High")  # specifies the reference category
    
    nesarc_clean$income3.factor <- factor(nesarc_clean$income3, levels=c(1,2,3), 
      labels=c("Low", "Med", "High"))
      nesarc_clean$income3.factor <- relevel(nesarc_clean$income3.factor, ref = "High")  # specifies the reference category
    
    nesarc_clean$alc5.factor <- factor(nesarc_clean$alc5, levels=c(1,2,3,4,5), 
      labels=c("Abstinence", "Low risk", "Medium rism", "High risk", "Very high risk"))
    
    nesarc_clean$alc4.factor <- factor(nesarc_clean$alc4, levels=c(1,2,3,4), 
      labels=c("Abstinence", "Low risk", "Medium rism", "High risk"))
    
    nesarc_clean$alc4_nesarc.factor <- factor(nesarc_clean$alc4_nesarc, levels=c(1,2,3, 4), 
      labels=c("Non-drinkers", "Light drinker", "Moderate drinker", "Heavy drinker"))
    
    nesarc_clean$hed.factor <- factor(nesarc_clean$hed, levels=c(1,2,3, 4), 
      labels=c("No HED", "HED <1/month", "HED <1/week", "HED >=1/week"))
    

# Save data
saveRDS(nesarc_clean, paste0(data, "nesarc_clean.rds"))
