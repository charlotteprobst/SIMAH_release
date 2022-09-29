
# SIMAH - NESARC Alcohol Transitions
# Data Analysis

library(tidyverse)   # data management
library(skimr)       # descriptive statistics
library(janitor)     # descriptive statistics (tabyl function)
library(survey)      # to work with survey data
library(srvyr)       # adds dplyr like syntax to the survey package
library(broom)       # model results
library(splines)

 
# Specify the data and output file locations
data    <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nesarc/Processed data/"  # Location of data
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/AlcUse and AUD/"  # Location of output

# Function to print model results
OR_CI <- function(model, unit=1){
  OR <- round(exp(coef(model))^unit,2)
  CI <- round(exp(confint(model))^unit,2)
  p <- summary(model)$coefficients[,4]
  p <- ifelse(p<.001, "<.001", ifelse(p<.05, round(p,3), round(p,2)))
  results <- cbind(OR, CI, p)|> as.data.frame()
  return(results)
}


# Load data and edit data -----------------------------------------------------------------------------------------------
nesarc <- readRDS(paste0(data, "nesarc_all.rds")) %>%
  group_by(idnum) %>% filter(n()>1) %>% ungroup() %>%   # remove those with data at one time point (8,440 observations removed; n=69,306)
  mutate(alc_daily_g = ifelse(alc_daily_g>200, 200, alc_daily_g), # Cap max g/day to 200
    alc5_wave1 = lag(alc5.factor, 1),                             # Create 'wave 1' version of some variables
    AUD_lifetime2_wave1 = lag(AUD_lifetime2, 1),
    alc_daily_g_wave1 = lag(alc_daily_g, 1)) %>% 
  filter(wave==2) %>%                                               # Remove baseline assessment
  mutate (incident_AUD = ifelse(AUD_lifetime2=="No AUD", 0, 1)) %>% # Create numeric version of outcome
  select (idnum, wave, female, psu, stratum, weight_wave2,  
    age, alc_daily_g_wave1, alc5_wave1, alc5, alc5.factor, incident_AUD, AUD_lifetime2, AUD_lifetime2_wave1)

  
# Prepare dataframe, accounting for survey design
nesarc_srvyr <- nesarc %>%
  as_survey_design(id=psu, strata=stratum, weights=weight_wave2, nest = TRUE) %>%
  filter(AUD_lifetime2_wave1=="No AUD") %>%             # keep those with no AUD at wave 1
  filter(!is.na(alc5_wave1) & !is.na(incident_AUD))     # remove those with no baseline alcohol use
options(survey.lonely.psu="adjust")

nesarc_srvyr_female <- filter(nesarc_srvyr, female==1)
nesarc_srvyr_male <- filter(nesarc_srvyr, female==0)


# Descriptives 
nesarc_srvyr %>% 
  group_by(incident_AUD) %>% 
  summarize(proportion = survey_mean(),
            total = survey_total())
# OR can be interpreted as RR




# Analyses, Categorical Alcohol use ---------------------------------------------------------------------------------------------------------

# WOMEN *******************************************************************************************

model1 <- svyglm(incident_AUD ~ alc5_wave1 + age, family = quasibinomial, design = nesarc_srvyr_female)
    tidy(model1, conf.int = TRUE, exponentiate = TRUE)
    OR_CI(model1)
    
# MEN *********************************************************************************************
model1 <- svyglm(incident_AUD ~ alc5_wave1 + age, family = quasibinomial, design = nesarc_srvyr_male)
  OR_CI(model1)  
  
  
  
  
  
# Analyses, Continuous Alcohol use ---------------------------------------------------------------------------------------------------------

# WOMEN *******************************************************************************************
model2 <- svyglm(incident_AUD ~ alc_daily_g_wave1 + age, family = quasibinomial, design = nesarc_srvyr_female)
  OR_CI(model2, unit=14)  
 
  
# MEN *******************************************************************************************
model2 <- svyglm(incident_AUD ~ alc_daily_g_wave1 + age, family = quasibinomial, design = nesarc_srvyr_male)
  OR_CI(model2, unit=14)  

