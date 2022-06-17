
# SIMAH - NESARC Alcohol Transitions
# Data Analysis

library(tidyverse)   # data management
library(skimr)       # descriptive statistics
library(janitor)     # descriptive statistics (tabyl function)
library(geepack)    # Needed for the modified Poisson regression
library(doBy)       # Needed to calculate the 95%CI

 
# Specify the data and output file locations
data    <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nesarc/Processed data/"  # Location of data
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/AlcUse and AUD/"  # Location of output

# Load data and edit data -----------------------------------------------------------------------------------------------
nesarc <- readRDS(paste0(data, "nesarc_clean.rds")) 

nesarc2 <- nesarc %>%
  arrange(idnum, wave) %>%
  group_by(idnum) %>%
    filter(any(wave==1 & AUD_lifetime2=="No AUD")) %>%           # Remove those with a history of AUD at wave 1
    mutate(alc5_wave1 = lag(alc5.factor, 1),                     # Create 'wave 1' version of some variables
           AUD_lifetime2_wave1 = lag(AUD_lifetime2, 1)) %>% 
  ungroup() %>%
  filter(wave==2) %>%                                            # Remove baseline assessment
  mutate (incident_AUD = ifelse(AUD_lifetime2=="No AUD", 0, 1)) %>%  # Create numeric version of outcome
  select (idnum, wave, female, 
           age, alc5_wave1, alc5, alc5.factor, incident_AUD, AUD_lifetime2, AUD_lifetime2_wave1)

nesarc2_female <- filter(nesarc2, female==1)
nesarc2_male <- filter(nesarc2, female==0)

# Check - AUD incidence among abstainers
nesarc2 %>%
  filter(alc5_wave1=="Abstainer") %>%
  tabyl(alc5.factor, AUD_lifetime2)
  # Everyone who had an incident AUD drank at Wave 2



# Analyses ---------------------------------------------------------------------------------------------------------

# nesarc2$alc5_wave1 <- relevel(nesarc2$alc5_wave1, ref="Category I")

# WOMEN: Age-adjusted Modified Poisson Regression
model1 <- geeglm (incident_AUD ~ alc5_wave1 + age,    
                  family = poisson(link = "log"), corstr = "exchangeable", 
                  id = idnum, data = nesarc2_female)
summary(model1)

model1.coefci <- esticon(model1, diag(6))  # diag() specifies number of coefficients
model1.expci <- exp(cbind(model1.coefci$estimate, model1.coefci$lwr, model1.coefci$upr)) 
rownames(model1.expci) <- names(coef(model1))
colnames(model1.expci) <- c("RR", "Lower RR", "Upper RR")
model1.expci


# MEN: Age-adjusted Modified Poisson Regression
model1 <- geeglm (incident_AUD ~ alc5_wave1 + age,    
                  family = poisson(link = "log"), corstr = "exchangeable", 
                  id = idnum, data = nesarc2_male)
summary(model1)

model1.coefci <- esticon(model1, diag(6))  # diag() specifies number of coefficients
model1.expci <- exp(cbind(model1.coefci$estimate, model1.coefci$lwr, model1.coefci$upr)) 
rownames(model1.expci) <- names(coef(model1))
colnames(model1.expci) <- c("RR", "Lower RR", "Upper RR")
model1.expci
