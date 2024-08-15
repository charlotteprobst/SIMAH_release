# script to generate MSM model for education for education transitions paper

devtools::install_github('chjackson/msm')
library(msm)
library(splitstackshape)
library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(doParallel)
library(foreach)
library(parallel)
library(readxl)

setwd("C:/Users/cmp21seb/Documents/SIMAH/")

source("SIMAH_code/alcohol_transitions_psid/functions/0_setup_alcohol_model.R") 

#### SCRIPT CAN BE STARTED FROM HERE IF REWEIGHTED DATA WITH IDS EXISTS ####
data <- read.csv("SIMAH_workplace/psid/cleaned data/psid_data_1999_2021_050424.csv") # 286343

# Prep data for models
data <- data %>% mutate(
  age_cat = case_when(
    age >= 18 & age <= 24 ~ "18-24",
    age >= 25 & age <= 64 ~ "25-64",
    age >= 65 ~ "65+",
    TRUE ~ NA_character_) # If none of the conditions are met, return NA
  )

data <- data %>% 
  mutate(sex = ifelse(sex == "female", 1, 0)) %>% 
  mutate(sex = factor(sex)) 

data <- data %>% drop_na(gpd, education) # 109444

# Drop anyone who is not either a head or who has reported alc via TAS
data <- data %>% filter(relationship=="head"|!is.na(AlcCAT_TAS)) # 72,298

# Generate a final alcohol category based on TAS and main survey data
data <- data %>% mutate(final_alc_cat=if_else(is.na(AlcCAT_TAS), 
                                              AlcCAT, 
                                              AlcCAT_TAS))

# Unify sample weights (model can't cope with different weights per year)
data <- data %>% group_by(uniqueID) %>% 
  mutate(sampleweight = round(mean(individualweight_cross.sectional))) %>% 
  filter(sampleweight!=0)

# Identify smallest sample weight
min(data$sampleweight) # 47

# Divde by the smallest sample weight
data <- data %>% mutate(sampleweight_downscaled = round(sampleweight/47))

saveRDS(data, "SIMAH_workplace/alcohol_transitions_psid/prepped_data_for_markov_alc.rds")

##### Set-up an individual model for each time period
# NB. Need to prep the data for each different model type using setup_alcohol_model function 
# (re-codes education/race, drops individuals with one year of data, scales age data, drops anyone who transitions backwards etc.)

# Model has 4 states
# Non-drinker
# Cat 1, low risk
# Cat 2: Medium risk
# Cat 3: High risk
Q <- rbind(c(0.5, 0.25, 0, 0),
             c(0.25, 0.5, 0.25, 0),
             c(0, 0.25, 0.5, 0.25),
             c(0, 0, 0.25, 0.5))

# MODEL 1: 2005-2010
datat1 <- data %>% filter(year>=2005 & year<=2010) 
datat1 <- setup_alcohol_model(datat1)
datat1 <- datat1[order(datat1$uniqueID, datat1$year),] # Order data for msm package
length(unique(datat1$uniqueID)) # Number of individuals 
# length(unique(datat1$newID)) # Number of replicated individuals (currently not replicating)
# remove anyone with only one year of data
datat1 <- datat1 %>% ungroup() %>% group_by(uniqueID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
# Make sure all variables are factors
datat1 <- datat1 %>% 
  mutate(sex = factor(sex)) %>%
  mutate(education = factor(education)) %>%
  mutate(race = factor(race)) %>%
  mutate(age_cat = factor(age_cat))

# Specify the initial values to start the search for the maximum likelihood estimates
Q1 <- crudeinits.msm(final_alc_cat~year, subject=uniqueID, qmatrix=Q, data=datat1)
# Run the markov model
modelt1 <- msm(final_alc_cat~year, uniqueID, data=datat1, qmatrix=Q1,
                                   center=FALSE,
                                   covariates=~age_cat + sex + race + education,
                                  # subject.weights=datat1$sampleweight_downscaled, # Optimisation does not converge to the max likelihood when including weights
                        control=list(trace=1, maxit=1000, fnscale = 3000000))
# Warning message:
# Optimisation has probably not converged to the maximum likelihood - Hessian is not positive definite.

# MODEL 2: 2011-2019
datat2 <- data %>% filter(year>=2011 & year<=2019)
datat2 <- setup_alcohol_model(datat2)
datat2 <- datat2[order(datat2$uniqueID, datat2$year),]
datat2 <- datat2 %>% ungroup() %>% 
  group_by(uniqueID) %>% 
  add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 

length(unique(datat2$uniqueID)) # 14,462
# Make sure all variables are factors
datat2 <- datat2 %>% 
  mutate(sex = factor(sex)) %>%
  mutate(education = factor(education)) %>%
  mutate(race = factor(race)) %>%
  mutate(age_cat = factor(age_cat))

Q2 <- crudeinits.msm(final_alc_cat~year, uniqueID, qmatrix=Q, data=datat2)
modelt2 <- msm(final_alc_cat~year, uniqueID, data=datat2, qmatrix=Q2,
               center=FALSE,
               covariates=~age_cat + sex + race + education,
               subject.weights=datat2$sampleweight_downscaled, 
               control=list(trace=1, maxit=1000, fnscale = 3000000))

# MODEL 3: 2019-2021 
datat3 <- data %>% filter(year>=2019 & year<=2021)
datat3 <- setup_alcohol_model(datat3)
datat3 <- datat3[order(datat3$uniqueID, datat3$year),]
datat3 <- datat3 %>% ungroup() %>% 
  group_by(uniqueID) %>% 
  add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 

length(unique(datat3$uniqueID)) # 10,721
# Make sure all variables are factors
datat3 <- datat3 %>% 
  mutate(sex = factor(sex)) %>%
  mutate(education = factor(education)) %>%
  mutate(race = factor(race)) %>%
  mutate(age_cat = factor(age_cat))

Q3 <- crudeinits.msm(final_alc_cat~year, uniqueID, qmatrix=Q, data=datat3)
modelt3 <- msm(final_alc_cat~year, uniqueID, data=datat3, qmatrix=Q3,
               center=FALSE,
               covariates=~age_cat + sex + race + education,
               subject.weights=datat3$sampleweight_downscaled, 
               control=list(trace=1, maxit=1000, fnscale = 3000))

# MODEL 4: One combined model with a covariate for time period
datat4 <- data %>% filter(year>=2005)
datat4 <- setup_alcohol_model(datat4)
datat4$timevary <- cut(datat4$year,
                     breaks=c(0,2010,2019, Inf),
                     labels=c("2005-2010","2011-2018","2019-2021"))
datat4 <- datat4[order(datat4$uniqueID, datat4$year),]
length(unique(datat4$uniqueID))
datat4 <- datat4 %>% ungroup() %>% group_by(uniqueID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
# Relevel the 'timevary' variable to set "2005-2010" as the reference category
datat4$timevary <- relevel(datat4$timevary, ref = "2005-2010")
# Make sure all variables are factors
datat4 <- datat4 %>% 
  mutate(sex = factor(sex)) %>%
  mutate(education = factor(education)) %>%
  mutate(race = factor(race)) %>%
  mutate(age_cat = factor(age_cat))

Q4 <- crudeinits.msm(final_alc_cat~year, uniqueID, qmatrix=Q, data=datat4)
modelt4 <- msm(final_alc_cat~year, uniqueID, data=datat4, qmatrix=Q4,
               center=FALSE,
               covariates=~age_cat + sex + race + education + timevary,
               control=list(trace=1, maxit=1000,fnscale = 96115))
# Warning message:
# Optimisation has probably not converged to the maximum likelihood - Hessian is not positive definite.

# Save all models

saveRDS(modelt1, "SIMAH_workplace/alcohol_transitions_psid/markov_models/psid_alcohol_model_2005_2010.RDS")
saveRDS(modelt2, "SIMAH_workplace/alcohol_transitions_psid/markov_models/psid_alcohol_model_2011_2019_incl_sample_weights.RDS")
saveRDS(modelt3, "SIMAH_workplace/alcohol_transitions_psid/markov_models/psid_alcohol_model_2019_2021_incl_sample_weights.RDS")
saveRDS(modelt4, "SIMAH_workplace/alcohol_transitions_psid/markov_models/psid_model_4_timeperiod.RDS")


