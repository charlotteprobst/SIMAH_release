
# SIMAH - NESARC Alcohol Transitions
# Data Analysis

library(tidyverse)   # data management
library(skimr)       # descriptive statistics
library(janitor)     # descriptive statistics (tabyl function)
library(geepack)    # Needed for the modified Poisson regression

 
# Specify the data and output file locations
data    <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nesarc/Processed data/"  # Location of data
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/AlcUse and AUD/"  # Location of output

# Load data and edit data -----------------------------------------------------------------------------------------------
nesarc <- readRDS(paste0(data, "nesarc_clean.rds")) 

nesarc2 <- nesarc %>%
  arrange(idnum, wave) %>%
  group_by(idnum) %>%
    filter(any(wave==1 & AUD_lifetime2=="No AUD")) %>%              # keep those with no AUD at wave 1
    mutate(alc_daily_g = ifelse(alc_daily_g>200, 200, alc_daily_g), # Cap max g/day to 200
           alc5_wave1 = lag(alc5.factor, 1),                        # Create 'wave 1' version of some variables
           AUD_lifetime2_wave1 = lag(AUD_lifetime2, 1),
           alc_daily_g_wave1 = lag(alc_daily_g, 1)) %>% 
  ungroup() %>%
  filter(wave==2) %>%                                            # Remove baseline assessment
  mutate (incident_AUD = ifelse(AUD_lifetime2=="No AUD", 0, 1)) %>%  # Create numeric version of outcome
  select (idnum, wave, female, 
           age, alc_daily_g_wave1, alc5_wave1, alc5, alc5.factor, incident_AUD, AUD_lifetime2, AUD_lifetime2_wave1)

nesarc2_female <- filter(nesarc2, female==1)
nesarc2_male <- filter(nesarc2, female==0)


# Descriptives 
tabyl(nesarc2, AUD_lifetime2) # AUD incidence
tabyl(nesarc2, alc5_wave1, AUD_lifetime2) # AUD incidence by drinking at baseline
skim(nesarc2, alc_daily_g_wave1)


# Function for Modified Poisson Regression Confidence Intervals
RR_CI <- function(model, unit = 1) {
  require(doBy)
  require(knitr)
  esticon(model, diag(length(coef(model)))) %>%  # diag() specifies number of coefficients
    mutate (name = names(coef(model)),
      RR = round(exp(estimate)^unit,2),
      lower_RR = round(exp(lwr)^unit,2),
      upper_RR = round(exp(upr)^unit,2),
      RR_CI = paste0(RR, " (", lower_RR, ", ", upper_RR, ")"),
      p_value = ifelse(p.value<0.001, "<.001", 
        ifelse(p.value<.05, round(p.value,3), round(p.value,2)))) %>% 
   select(name, RR_CI, p_value) %>%
   kable()
  
  }


# Analyses, Categorical Alcohol use ---------------------------------------------------------------------------------------------------------

# WOMEN *******************************************************************************************
# Descriptives
tabyl(nesarc2_female, alc5_wave1)

# Age-adjusted Modified Poisson Regression
mod_AlcCat_f <- geeglm (incident_AUD ~ alc5_wave1 + age,    
                  family = poisson(link = "log"), corstr = "exchangeable", 
                  id = idnum, data = nesarc2_female)
    summary(mod_AlcCat_f)
    RR_CI(mod_AlcCat_f)
 

# MEN *******************************************************************************************
# Descriptives
tabyl(nesarc2_female, alc5_wave1)


# Age-adjusted Modified Poisson Regression
mod_AlcCat_m <- geeglm (incident_AUD ~ alc5_wave1 + age,    
                  family = poisson(link = "log"), corstr = "exchangeable", 
                  id = idnum, data = nesarc2_male)
    summary(mod_AlcCat_m)
    RR_CI(mod_AlcCat_m)
    
    
    
# Analyses, Continous Alcohol use ---------------------------------------------------------------------------------------------------------

# WOMEN *******************************************************************************************
# Descriptives
skim(nesarc2_female, alc_daily_g_wave1)

mod_AlcCont_f <- geeglm (incident_AUD ~ alc_daily_g_wave1 + age,    
                  family = poisson(link = "log"), corstr = "exchangeable", 
                  id = idnum, data = nesarc2_female)
    summary(mod_AlcCont_f)
    RR_CI(mod_AlcCont_f, unit=10)




# MEN *********************************************************************************************
# Descriptives
skim(nesarc2_male, alc_daily_g_wave1)


mod_AlcCont_m <- geeglm (incident_AUD ~ alc_daily_g_wave1 + age,    
                          family = poisson(link = "log"), corstr = "exchangeable", 
                          id = idnum, data = nesarc2_male)
    summary(mod_AlcCont_m)
    RR_CI(mod_AlcCont_m, unit=10)
