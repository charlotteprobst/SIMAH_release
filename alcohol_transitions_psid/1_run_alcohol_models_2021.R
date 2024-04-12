# script to generate MSM model for education for education transitions paper
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(readxl)
library(doParallel)
library(foreach)
library(parallel)
library(readxl)

setwd("C:/Users/cmp21seb/Documents/SIMAH/")

source("SIMAH_code/alcohol_transitions/functions/0_setup_alcohol_model.R") # TO GENERATE

#### SCRIPT CAN BE STARTED FROM HERE IF REWEIGHTED DATA WITH IDS EXISTS ####
data <- read_csv("SIMAH_workplace/education_transitions/2021/data_to_model/new_PSID_weighted_IDs_2021.csv")

# Prep data for models
data <- data %>% mutate(
  age_cat = case_when(
    age >= 18 & age <= 24 ~ "18-24",
    age >= 25 & age <= 64 ~ "25-64",
    age >= 65 ~ "65+",
    TRUE ~ NA_character_  # If none of the conditions are met, return NA
  )
)

data <- data %>% mutate(sex=factor(sex), sex=ifelse(sex=="female",1,0)) 

saveRDS(data, "SIMAH_workplace/alcohol_transitions/prepped_data_for_markov_alc.rds")

##### Set-up an individual model for each time period
# NB. Need to prep the data for each different model type using setup_alcohol_model function 
# (re-codes education/race, drops individuals with one year of data, scales age data, drops anyone who transitions backwards etc.)

# Model has 4 states
# Non-drinker
# Cat 1, low risk
# Cat 2: Medium risk
# Cat 3: High risk
Q <- rbind( c(0.5, 0.5, 0, 0),
            c(0, 0.5, 0.5, 0),
            c(0, 0, 0.5, 0.5),
            c(0, 0, 0, 0.5))


# MODEL 1: 2005-2010
datat1 <- data %>% filter(year>=2005 & year<=2010)
datat1 <- setup_alcohol_model(datat1)
datat1 <- datat1[order(datat1$newID, datat1$year),] # Order data for msm package
length(unique(datat1$uniqueID)) # Number of individuals
length(unique(datat1$newID)) # Number of replicated individuals
# remove anyone with only one year of data
datat1 <- datat1 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 

# Specify the initial values to start the search for the maximum likelihood estimates
Q1 <- crudeinits.msm(alc_state~year, subject=newID, qmatrix=Q, data=datat1)
# Run the markov model
modelt1 <- msm(alc_state~year, newID, data=datat1, qmatrix=Q1,
                                   center=FALSE,
                                   covariates=~agecat + sex + racefinal2 + education,
                        control=list(trace=1, fnscale=271181, maxit=200))

# MODEL 2: 2011-2019
datat2 <- data %>% filter(year>=2011 & year<=2019)
datat2 <- setup_education_model(datat2)
datat2 <- datat2[order(datat2$newID, datat2$year),]
length(unique(datat2$uniqueID))
length(unique(datat2$newID))
datat2 <- datat2 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 

Q2 <- crudeinits.msm(alc_state~year, newID, qmatrix=Q, data=datat2)
modelt2 <- msm(alc_state~year, newID, data=datat2, qmatrix=Q2,
               center=FALSE,
               covariates=~agecat + sex + racefinal2 + education,
               control=list(trace=1, fnscale=271181, maxit=200))

# MODEL 3: 2019-2021 
datat3 <- data %>% filter(year>=2019 & year<=2021)
datat3 <- setup_alcohol_model_2021(datat3)
datat3 <- datat3[order(datat3$newID, datat3$year),]
length(unique(datat3$uniqueID))
length(unique(datat3$newID))
datat3 <- datat3 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 

Q3 <- crudeinits.msm(alc_state~year, newID, qmatrix=Q, data=datat3)
modelt3 <- msm(alc_state~year, newID, data=datat4, qmatrix=Q,
               center=FALSE,
               covariates=~agecat + sex + racefinal2 + education,
               control=list(trace=1, fnscale=271181, maxit=200))

# MODEL 4: One combined model with a covariate for time period
datat4 <- data %>% filter(year>=2005)
datat4 <- setup_alcohol_model(datat4)
datat4$timevary <- cut(datat4$year,
                     breaks=c(0,2010,2019),
                     labels=c("2005-2010","2011-2018","2019-2021"))
datat4 <- datat4[order(datat4$newID, datat4$year),]
length(unique(datat4$uniqueID))
length(unique(datat4$newID))
datat4 <- datat4 %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 
# Relevel the 'timevary' variable to set "2005-2010" as the reference category
datat4$timevary <- relevel(datat4$timevary, ref = "2005-2010")

Q4 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat4)
modelt5 <- msm(educNUM~year, newID, data=datat4, qmatrix=Q4,
               center=FALSE,
               covariates=~agecat + sex + racefinal2 + education + timevary,
               control=list(trace=1, fnscale=271181, maxit=200))

# Save all models
saveRDS(modelt1, "SIMAH_workplace/alcohol_transitions/markov_models/alcohol_model_2005_2010.RDS")
saveRDS(modelt2, "SIMAH_workplace/alcohol_transitions/markov_models/covid_model_2011_2019.RDS")
saveRDS(modelt3, "SIMAH_workplace/alcohol_transitions/markov_models/covid_model_2019_2021.RDS")
saveRDS(modelt4, "SIMAH_workplace/alcohol_transitions/markov_models/covid_model_timeperiod.RDS")

