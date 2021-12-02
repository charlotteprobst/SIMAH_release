
# SIMAH - NESARC Alcohol Transitions
# Data Management

library(tidyverse)       # data management
library(skimr)           # descriptive statistics
library(survey)          # to accomodate survey weights
library(lubridate)       # to work with dates/calculate follow-up time
library(splitstackshape) # To replicate data based on sampling weight


# Specify the data file locations
data <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Data/"


# Edit data - recode and recategorize variables
nesarc <- readRDS(paste0(data, "nesarc_raw.rds")) %>%

  # Re-code variables
  mutate(
    married = recode(marital_stat, `1`= 1, `2`=1, `3`=0, `4`=0, `5`=0, `6`=0),
    edu3 = case_when(edu %in% c(1,2,3,4,5,6,7,8,9) ~ 1,
                     edu %in% c(10,11) ~ 2,
                     edu %in% c(12,13,14) ~ 3),
    income3 = case_when(fam_income %in% c(1,2,3,4,5,6) ~ 1, # split into evenly sized groups based on Wave 1 data
                        fam_income %in% c(7,8,9,10,11) ~ 2,
                        fam_income %in% c(12,13,14,15,16,17,18,19,20,21) ~ 3),
    age3.factor = cut(age, breaks = c(-Inf, 30, 50, Inf), labels = c("18-29", "30-49", "50+"), right=FALSE),
    
    # Calculate follow-up time and baseline version of age, sex, and edu
    intv_date = make_date(year=CYEAR, month=CMON, day=CDAY)) %>%
  
  arrange(idnum, wave) %>%
  group_by(idnum) %>%
      mutate (prev_intv_date = lag(intv_date, 1),
              age_wave1 = lag(age, 1),
              female_wave1 = lag(female, 1),
              edu3_wave1 = lag(edu3, 1),
              race_wave1 = lag(race, 1),
              drinking_stat_wave1 = lag(drinking_stat, 1),
              weight_wave2 = lead(weight, 1)) %>%
  ungroup() %>%
  mutate (years = as.numeric(prev_intv_date %--% intv_date /dyears(1)),
          years = if_else(wave==1, 0, years),
          age_wave1 = if_else(wave==1,age, age_wave1),
          female_wave1 = if_else(wave==1,female, female_wave1),
          edu3_wave1 = if_else(wave==1, edu3, edu3_wave1),
          race_wave1 = if_else(wave==1, race, race_wave1),
          drinking_stat_wave1 = if_else(wave==1, drinking_stat, drinking_stat_wave1),
          weight_wave2 = if_else(wave==2, weight, weight_wave2),

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
        coolers_yearly = ifelse(S2AQ4E <= 5, 
                                  (S2AQ4D * (S2AQ4B_days - S2AQ4F_days)) + (S2AQ4E * S2AQ4F_days), # quantity if largest drinks <=5
                                  (S2AQ4D * (S2AQ4B_days - S2AQ4G_days)) + ((S2AQ4G_days - S2AQ4F_days)*(exp((log(pmax(5, S2AQ4D)) + log(S2AQ4E - 1)) /2))) + (S2AQ4E*S2AQ4F_days)), # quantity if largest drinks >5
        
        beers_yearly = ifelse(S2AQ5E <= 5, 
                                  (S2AQ5D * (S2AQ5B_days - S2AQ5F_days)) + (S2AQ5E * S2AQ5F_days),
                                  (S2AQ5D * (S2AQ5B_days - S2AQ5G_days)) + ((S2AQ5G_days - S2AQ5F_days)*(exp((log(pmax(5, S2AQ5D)) + log(S2AQ5E - 1)) /2))) + (S2AQ5E*S2AQ5F_days)),
        
        wine_yearly = ifelse(S2AQ6E <= 5, 
                                  (S2AQ6D * (S2AQ6B_days - S2AQ6F_days)) + (S2AQ6E * S2AQ6F_days), 
                                  (S2AQ6D * (S2AQ6B_days - S2AQ6G_days)) + ((S2AQ6G_days - S2AQ6F_days)*(exp((log(pmax(5, S2AQ6D)) + log(S2AQ6E - 1)) /2))) + (S2AQ6E*S2AQ6F_days)),
        
        liquor_yearly = ifelse(S2AQ7E <= 5, 
                              (S2AQ7D * (S2AQ7B_days - S2AQ7F_days)) + (S2AQ7E * S2AQ7F_days), 
                              (S2AQ7D * (S2AQ7B_days - S2AQ7G_days)) + ((S2AQ7G_days - S2AQ7F_days)*(exp((log(pmax(5, S2AQ7D)) + log(S2AQ7E - 1)) /2))) + (S2AQ7E*S2AQ7F_days)), 
                        
        # Calculate daily ethnanol intake 
        coolers_daily_oz = (coolers_yearly * (s2aq4cr * coolecf))/365,
        beers_daily_oz = (beers_yearly * (s2aq5cr * beerecf))/365,
        wine_daily_oz = (wine_yearly * (s2aq6cr * wineecf))/365,
        liquor_daily_oz = (liquor_yearly * (s2aq7cr * liqrecf))/365) %>%

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
          
        # Women:
        female==1 & alc_daily_oz==0 ~ 1,                         # non-drinkers
        female==1 & alc_daily_oz>0 & alc_daily_oz <= 0.257 ~ 2,  # light drinker, i.e., 3 or fewer drinks per week
        female==1 & alc_daily_oz>0.257 & alc_daily_oz <=0.6 ~ 3, # moderate drinker, i.e., 3 to 7 drinks per week
        female==1 & alc_daily_oz>0.6 ~ 4),                       # heavier drinker, i.e., more than 1 drink
        
      
      # Categorize alcohol use as per SIMAH protocol
      alc6 = case_when(
        # Men & Women:
        wave==1 & drinking_stat==3 ~ 1,                          # lifetime abstinence
        wave==2 & drinking_stat_wave1==3 & drinking_stat==3 ~ 1, # lifetime abstinence
        
        wave==2 & drinking_stat_wave1!=3 & drinking_stat==2 ~ 2, # former drinker
        wave==2 & drinking_stat_wave1!=3 & drinking_stat==3 ~ 2, # former drinker
        
        drinking_stat==2 ~ 2,                                # former drinker 
        drinking_stat==1 & alc_daily_g==0 ~ 2,               # former drinker (indicated they drink, but had 0 grams of alcohol)

        # Men
        female==0 & alc_daily_g >0 & alc_daily_g <=40 ~ 3,   # low risk
        female==0 & alc_daily_g >40 & alc_daily_g <=60 ~ 4,  # medium risk
        female==0 & alc_daily_g >60 & alc_daily_g <=100 ~ 5, # high risk
        female==0 & alc_daily_g >100 ~ 6,                    # very high risk
        
        # Women:
        female==1 & alc_daily_g >0 & alc_daily_g <=20 ~ 3,   # low risk
        female==1 & alc_daily_g >20 & alc_daily_g <=40 ~ 4,  # medium risk
        female==1 & alc_daily_g >40 & alc_daily_g <=60 ~ 5,  # high risk
        female==1 & alc_daily_g >60 ~ 6),                    # very high risk
      
      alc5 = recode(alc6, `6`=5),       # merge high-risk and very-high-risk categories
      alc4 = recode(alc6, `1`=1, `2`=1, `3`=2, `4`=3, `5`=4, `6`=4), # merge absteiners/former and merge high/very-high categories

         
    # Calculate heavy episodic drinking (HED)
    hed = case_when(
                    # Men:
                    female==0 & drank5plus_freq %in% c(1,2,3,4,5) ~ 5, # HED >= 1/week
                    female==0 & drank5plus_freq %in% c(6,7) ~ 4,       # HED >= 1/month but < 1/week
                    female==0 & drank5plus_freq %in% c(8,9,10) ~ 3,    # HED < 1 / month
                    female==0 & drank5plus_freq %in% c(11) ~ 2,        # No HED in last year
                    
                    # Women:
                    female==1 & drank4plus_freq %in% c(1,2,3,4,5) ~ 5, # HED >= 1/week
                    female==1 & drank4plus_freq %in% c(6,7) ~ 4,       # HED >= 1/month but < 1/week
                    female==1 & drank4plus_freq %in% c(8,9,10) ~ 3,    # HED < 1 / month
                    female==1 & drank4plus_freq %in% c(11) ~ 2),       # No HED in last year
    
    hed = ifelse(alc5 %in% c(1,2), 1, hed)) #Non-drinker


  
  # Check  
  # select(nesarc, idnum, wave, intv_date, prev_intv_date, years, age_diff) %>% view()
  # select(nesarc, idnum, wave, age, age_wave1) %>% view()
  # select(nesarc, idnum, wave, female, female_wave1) %>% view()
  # select(nesarc, idnum, wave, edu3, edu3_wave1) %>% view()
  # select(nesarc, idnum, wave, race, race_wave1) %>% view()
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
  # count(nesarc, alc6, alc5, alc4)
  #check if anyone who's previously drank is coded as a lifetime abstainer in wave 2
        # nesarc %>%
        #   select(idnum, wave, alc5, alc_daily_g) %>%
        #   pivot_wider(names_from="wave", values_from = c("alc5", "alc_daily_g")) %>%
        #   filter(alc5_2==1 & alc5_1%in%c(2,3,4,5)) %>%
        #   view()
        # 
        # nesarc %>% 
        #   filter(is.na(alc6)) %>%
        #   select (idnum, wave, alc6, drinking_stat, drinking_stat_wave1, alc_daily_g) %>%
        #   view()
    # filter(nesarc, wave==1) %>% count(hed)



# Select variables of interest and identify those lost to follow-up or with incomplete data
nesarc <- nesarc %>%
  
  # order data by ID, then by wave (timepoint)
  arrange(idnum, wave) %>% 
  
  # Identify the variables to keep
  select(idnum, wave, psu, stratum, weight, weight_wave2,  years, age, age_wave1, age3.factor, female, female_wave1, race, race_wave1, 
    married, edu3, edu3_wave1, income3, alc_daily_oz, alc_daily_g, alc_daily_drinks, alc4_nesarc, alc6, alc5, alc4, hed) %>% 
  
  # remove those with missing data 
  group_by(idnum) %>%
    mutate(lost = ifelse(n()>1, 0, 1),                    # Identify those with data at one time point (8,440 observations removed; n=69,306)
           lost = ifelse(any(is.na(alc5)), 1, lost),      # Identify those with missing alcohol data in either time point (424 observations removed; n=68,882)
           lost = ifelse(any(is.na(hed)), 1, lost)) %>%   # Identify those with missing HED in either time point (166 observations removed; n=68,716)
    ungroup() 

  

# label values

    # Alcohol variables  
    nesarc$alc6.factor <- factor(nesarc$alc6, levels=c(1,2,3,4,5,6), 
                          labels=c("Lifetime abstainer", "Former drinker", "Low risk", "Medium risk", "High risk", "Very high risk"))
    
    nesarc$alc5.factor <- factor(nesarc$alc5, levels=c(1,2,3,4,5), 
                          labels=c("Abstainer", "Former", "Category I", "Category II", "Category III"))
    
    nesarc$alc4.factor <- factor(nesarc$alc4, levels=c(1,2,3,4), 
                          labels=c("Abstinence", "Low risk", "Medium risk", "High risk"))
    
    nesarc$alc4_nesarc.factor <- factor(nesarc$alc4_nesarc, levels=c(1,2,3, 4), 
                                  labels=c("Non-drinkers", "Light drinker", "Moderate drinker", "Heavy drinker"))
    
    nesarc$hed.factor <- factor(nesarc$hed, levels=c(1,2,3,4,5), 
                          labels=c("Non-drinker", "Drinker, no HED", "Occasional HED", "Monthly HED", "Weekly HED"))

 
    # Covariates (first listed category is the reference)
    nesarc$female.factor <- factor(nesarc$female, levels=c(0,1), labels=c("Men", "Women"))
    nesarc$female_wave1.factor <- factor(nesarc$female_wave1, levels=c(0,1), labels=c("Men", "Women"))
    
           
    nesarc$race.factor <- factor(nesarc$race, levels=c(1,2,3,4), 
                          labels=c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic", "Other, non-Hispanic"))
    nesarc$race_wave1.factor <- factor(nesarc$race_wave1, levels=c(1,2,3,4), 
                            labels=c("White, non-Hispanic", "Black, non-Hispanic", "Hispanic", "Other, non-Hispanic"))

    nesarc$married.factor <- factor(nesarc$married, levels=c(1,0), labels=c("Married/cohab.", "Single"))

    nesarc$edu3.factor <- factor(nesarc$edu3, levels=c(3,1,2), labels=c("High", "Low", "Med"))
    nesarc$edu3_wave1.factor <- factor(nesarc$edu3_wave1, levels=c(3,1,2), labels=c("High", "Low", "Med"))

    nesarc$income3.factor <- factor(nesarc$income3, levels=c(3,1,2), labels=c("High", "Low", "Med"))
    
    nesarc$wave.factor <- factor(nesarc$wave, levels=c(1,2), labels=c("Wave 1", "Wave 2"))
    nesarc$lost.factor <- factor(nesarc$lost, levels=c(0,1), labels=c("Completed Follow-up", "Lost to Follow-up"))


    
# Create dataframe with complete data at both time points (cc=complete case)
nesarc_cc <- nesarc %>% 
  filter(lost==0)  # Remove those with missing data
      
      
      
# Replicate data (and create unique ID variable) to adjust for sampling weight
nesarc_cc_expanded <- nesarc_cc %>%
  mutate (new_weight = weight_wave2 / 100) %>%  # because original weight variable ranged from 455 to 73,192
  expandRows(., "new_weight") %>%  # replicates data
  
  # Generate unique ID
  group_by(idnum, wave) %>%
    mutate(iter = sprintf("%04d", 1:n()),  # the sprintf("%04d", X) command is used to add leading 0s to make it a variable with 4 digits
           idnum = as.numeric(paste0(idnum, iter))) %>%
  ungroup() %>%
  arrange(idnum, wave) # order data by ID then wave (needed for the MSM model)

# check
# filter(nesarc_cc, wave==1) %>% count(hed)


  
# Save data
saveRDS(nesarc, paste0(data, "nesarc_all.rds"))
saveRDS(nesarc_cc, paste0(data, "nesarc_clean.rds"))
saveRDS(nesarc_cc_expanded, paste0(data, "nesarc_clean_expanded.rds"))
