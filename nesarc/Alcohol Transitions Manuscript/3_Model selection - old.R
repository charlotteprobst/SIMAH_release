
# SIMAH - NESARC Alcohol Transitions
# Model selection

# Load packages and data

library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(janitor)    # data management
library(msm)        # model transition probabilities
library(knitr)      # create descriptives table
# options(scipen=999) # prevent the use of scientific notation
memory.limit(size=1e+13)

# Specify the data and output file locations
data    <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nesarc/Processed data/"  # Location of data
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Output/"                     # Location of tables and figures 
models  <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nesarc/Models/"          # Location of saved MSM models
source("0_Functions.R")

# Load data / functions
nesarc_all      <- readRDS(paste0(data, "nesarc_all.rds")) # Contains those with missing data 
nesarc          <- readRDS(paste0(data, "nesarc_clean.rds")) 
nesarc_expanded <- readRDS(paste0(data, "nesarc_clean_expanded.rds")) 

# Model 1:  AlcUse (6levels), Age (3levels),    adjacent transitions----------------------------------------------------------

# 1) Run MSM Model *******************************************************************************************************
# Specify allowed transition
# only allow adjacent transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker

Q1 <- rbind ( c(0,    0,     0.25, 0,    0,    0),
              c(0,    0,     0.25, 0,    0,    0),
              c(0,    0.25,  0,    0.25, 0,    0),
              c(0,    0,     0.25, 0,    0.25, 0),
              c(0,    0,     0,    0.25, 0,    0.25), 
              c(0,    0,     0,    0,    0.25, 0))

# Specifies initial values
Q1 <- crudeinits.msm(alc6 ~ years, idnum, data=nesarc_expanded, qmatrix=Q1)


# Run MSM model (adjusted for covariates)
msm1 <- msm ( alc6 ~ years, subject=idnum, data = nesarc_expanded,
              qmatrix = Q1, center=FALSE, control = list(trace=1, maxit=1000, fnscale = 3000000),
              covariates = ~ female_w1 + age3 + edu3 + race_w1)
saveRDS(msm1, paste0(models, "msm1.RDS")) # Save Results


# Load the model
msm1 <- readRDS(paste0(models, "msm1.RDS"))

AIC(msm1)


# 2) Average annual TP  ***************************************************************************************************
predicted_TP(msm1, 1, nrow(nesarc), nrow(nesarc_expanded)) %>%
  kable()



# 3) Annual TP for each category ******************************************************************************************

# First, specify the covariate values
age_cat <- unique(nesarc_expanded$age3)
sex <- unique(nesarc_expanded$female_w1)
race <- unique(nesarc_expanded$race_w1)
edu <- unique(nesarc_expanded$edu3)



# Function to extract annual TP 
msm1_aTP <- predicted_TP_covs (msm1, 1, age_cat, sex, race, edu) %>%
  # Rename states
  mutate(From = recode(From,"State 1" = "Abstainer",  "State 2" = "Former", "State 3" = "Category I", "State 4" = "Category II", "State 5" = "Category III"),
    To = recode(To, "State.1" = "Abstainer", "State.2" = "Former", "State.3" = "Category I", "State.4" = "Category II", "State.5" = "Category III"),
    Transition = paste(From, To, sep = "->"),
    Transition = fct_relevel(Transition,         
      "Abstainer->Category I", "Former->Category I",	"Category I->Category II",	"Category II->Category III",
      "Category III->Category II",	"Category II->Category I",	"Category I->Former"),
    cat = paste(sex, age_cat, edu, race, From, sep="_")) %>% 
  select(cat, To, Probability) %>% 
  group_by(cat) %>% 
  mutate(cumsum = cumsum(Probability)) %>%
  ungroup()



# Load and set up the initial population (based on NESARC wave 1)
msm1_basepop <- nesarc_expanded %>%
  select(idnum, wave, age, female_w1, race_w1, edu3, alc6.factor) %>%  
  rename( sex = female_w1, 
          race = race_w1) %>%
  pivot_wider(names_from="wave", values_from=c("alc6.factor", "age", "edu3")) %>%  
  mutate (predicted_cat = alc6.factor_1,
    AlcUse_1 = alc6.factor_1,
    AlcUse_2 = alc6.factor_2,
    age = age_1,
    edu = edu3_1) %>%
  select(idnum, AlcUse_1, AlcUse_2, sex, age, edu, race, predicted_cat)




# 4) Compare observed (at NESARC III) vs predicted  *****************************************************************************

# Predicted: Simulate population at 11 years follow-up
predicted_pop <- simulate_pop(msm1_basepop, msm1_aTP, transition_alc6, 11)

observed_pop  <- readRDS(paste0(data, "nesarc3_clean_expanded.rds"))%>%
  mutate(observed_cat = alc6.factor) # observed category of alcohol use

compare_pct(predicted_pop, observed_pop)





# Model 2:  AlcUse (6levels), Age (continuous), adjacent transitions----------------------------------------------------------
nesarc_ageC <- nesarc %>%
  filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate (age.c = (age - mean(age)) / sd(age)) 

nesarc_expanded_ageC <- nesarc_expanded %>%
  filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate (age.c = (age - mean(age)) / sd(age)) 

# 1) Run MSM Model *******************************************************************************************************
# Specify allowed transitions; only allow adjacent transitions
# only allow adjacent transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q2 <- rbind ( c(0,    0,     0.25, 0,    0,    0),
              c(0,    0,     0.25, 0,    0,    0),
              c(0,    0.25,  0,    0.25, 0,    0),
              c(0,    0,     0.25, 0,    0.25, 0),
              c(0,    0,     0,    0.25, 0,    0.25), 
              c(0,    0,     0,    0,    0.25, 0))


# Specifies initial values
Q2 <- crudeinits.msm(alc6 ~ years, idnum, data=nesarc_expanded_ageC, qmatrix=Q2)

# Run MSM model (adjusted for covariates)
msm2 <- msm(alc6 ~ years, subject=idnum, data = nesarc_expanded_ageC, qmatrix = Q2,
  center=FALSE, control = list(trace=1, maxit=5000, fnscale = 750000),
  covariates = ~ female_w1 + age.c + edu3 + race_w1)
saveRDS(msm2, paste0(models, "msm2.RDS")) # Save Results


# Load the models
msm2 <- readRDS(paste0(models, "msm2.RDS"))
AIC(msm2)

# Model 3:  AlcUse (6levels), Age (squared),    adjacent transitions----------------------------------------------------------
nesarc_ageSQ <- nesarc %>%
  filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate (age_sq = age ^ 2, 
    age.c = (age - mean(age)) / sd(age),
    age_sq.c = (age_sq - mean(age_sq)) / sd(age_sq)) 

nesarc_expanded_ageSQ <- nesarc_expanded %>%
  filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate (age_sq = age ^ 2, 
    age.c = (age - mean(age)) / sd(age),
    age_sq.c = (age_sq - mean(age_sq)) / sd(age_sq)) 

#1) Run MSM Model *******************************************************************************************************
# Specify allowed transitions; only allow adjacent transitions
# only allow adjacent transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q3 <- rbind ( c(0,    0,     0.25, 0,    0,    0),
              c(0,    0,     0.25, 0,    0,    0),
              c(0,    0.25,  0,    0.25, 0,    0),
              c(0,    0,     0.25, 0,    0.25, 0),
              c(0,    0,     0,    0.25, 0,    0.25), 
              c(0,    0,     0,    0,    0.25, 0))


# Specifies initial values
Q3 <- crudeinits.msm(alc6 ~ years, idnum, data=nesarc_expanded_ageSQ, qmatrix=Q3)

# Run MSM model (adjusted for covariates)
msm3 <- msm(alc6 ~ years, subject=idnum, data = nesarc_expanded_ageSQ, qmatrix = Q3,
  center=FALSE, control = list(trace=1, maxit=5000, fnscale = 750000),
  covariates = ~ female_w1 + age.c + age_sq.c + edu3 + race_w1)
saveRDS(msm3, paste0(models, "msm3.RDS")) # Save Results


# Load the models
msm3 <- readRDS(paste0(models, "msm3.RDS"))
AIC(msm3)



# Model 4:  AlcUse (6levels), Age (7levels),    adjacent transitions----------------------------------------------------------

# 1) Run MSM Model *******************************************************************************************************
# Specify allowed transition
# only allow adjacent transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker

Q4 <- rbind ( c(0,    0,     0.25, 0,    0,    0),
              c(0,    0,     0.25, 0,    0,    0),
              c(0,    0.25,  0,    0.25, 0,    0),
              c(0,    0,     0.25, 0,    0.25, 0),
              c(0,    0,     0,    0.25, 0,    0.25), 
              c(0,    0,     0,    0,    0.25, 0))

# Specifies initial values
Q4 <- crudeinits.msm(alc6 ~ years, idnum, data=nesarc_expanded, qmatrix=Q4)


# Run MSM model (adjusted for covariates)
msm4 <- msm ( alc6 ~ years, subject=idnum, data = nesarc_expanded,
              qmatrix = Q4, center=FALSE, control = list(trace=1, maxit=1000, fnscale = 3000000),
              covariates = ~ female_w1 + age7 + edu3 + race_w1)
saveRDS(msm4, paste0(models, "msm4.RDS")) # Save Results


# Load the model
msm4 <- readRDS(paste0(models, "msm4.RDS"))
AIC(msm4)

# Model 5:  AlcUse (5levels), Age (3levels),    adjacent transitions----------------------------------------------------------

# 1) Run MSM Model *******************************************************************************************************
# Specify allowed transitions
# only allow adjacent transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker

Q5 <- rbind ( c(0,     0,    0.25,  0,    0),
              c(0,     0,    0.25,  0,    0),
              c(0,     0.25, 0,     0.25, 0),
              c(0,     0,    0.25,  0,    0.25),
              c(0,     0,    0,     0.25, 0))

# Specifies initial values
Q5 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q5)


# Run MSM model (adjusted for covariates)
msm5 <- msm ( alc5 ~ years, subject=idnum, data = nesarc_expanded,
                  qmatrix = Q5, center=FALSE, control = list(trace=1, maxit=500, fnscale = 3000000),
                  covariates = ~ female_w1 + age3 + edu3 + race_w1)
saveRDS(msm5, paste0(models, "msm5.RDS")) # Save Results


# Load the model
msm5 <- readRDS(paste0(models, "msm5.RDS"))
AIC(msm5)


# 2) Average annual TP  ***************************************************************************************************
predicted_TP(msm6, 1, nrow(nesarc), nrow(nesarc_expanded)) %>%
  kable()



# 3) Annual TP for each category ******************************************************************************************

# First, specify the covariate values
age_cat <- unique(nesarc_expanded$age3)
sex <- unique(nesarc_expanded$female_w1)
race <- unique(nesarc_expanded$race_w1)
edu <- unique(nesarc_expanded$edu3)


# Function to extract annual TP 
msm5_aTP <- predicted_TP_covs (msm5, 1, age_cat, sex, race, edu) %>%
  # Rename states
  mutate(From = recode(From,"State 1" = "Abstainer",  "State 2" = "Former", "State 3" = "Category I", "State 4" = "Category II", "State 5" = "Category III"),
    To = recode(To, "State.1" = "Abstainer", "State.2" = "Former", "State.3" = "Category I", "State.4" = "Category II", "State.5" = "Category III"),
    Transition = paste(From, To, sep = "->"),
    Transition = fct_relevel(Transition,         
      "Abstainer->Category I", "Former->Category I",	"Category I->Category II",	"Category II->Category III",
      "Category III->Category II",	"Category II->Category I",	"Category I->Former"),
    cat = paste(sex, age_cat, edu, race, From, sep="_")) %>% 
  select(cat, To, Probability) %>% 
  group_by(cat) %>% 
  mutate(cumsum = cumsum(Probability)) %>%
  ungroup()



# Load and set up the initial population (based on NESARC wave 1)
msm5_basepop <- nesarc_expanded %>%
  select(idnum, wave, age, female_w1, race_w1, edu3, alc5.factor) %>%  
  rename( sex = female_w1, 
          race = race_w1) %>%
  pivot_wider(names_from="wave", values_from=c("alc5.factor", "age", "edu3")) %>%  
  mutate (predicted_cat = alc5.factor_1,
    AlcUse_1 = alc5.factor_1,
    AlcUse_2 = alc5.factor_2,
    age = age_1,
    edu = edu3_1) %>%
  select(idnum, AlcUse_1, AlcUse_2, sex, age, edu, race, predicted_cat)




# 4) Compare observed (at NESARC III) vs predicted  *****************************************************************************

# Predicted: Simulate population at 11 years follow-up
predicted_pop <- simulate_pop(msm5_basepop, msm5_aTP, transition_alc5, 11)

observed_pop  <- readRDS(paste0(data, "nesarc3_clean_expanded.rds"))%>%
  mutate(observed_cat = alc5.factor) # observed category of alcohol use

compare_pct(predicted_pop, observed_pop)






# Model 6:  AlcUse (5levels), Age (continuous), adjacent transitions----------------------------------------------------------
nesarc_ageC <- nesarc %>%
  filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate (age.c = (age - mean(age)) / sd(age)) 

nesarc_expanded_ageC <- nesarc_expanded %>%
  filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate (age.c = (age - mean(age)) / sd(age)) 


# 1) Run MSM Model *******************************************************************************************************
# Specify allowed transitions; only allow adjacent transitions
# only allow adjacent transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q6 <- rbind ( c(0,     0,    0.25,  0,    0),
              c(0,     0,    0.25,  0,    0),
              c(0,     0.25, 0,     0.25, 0),
              c(0,     0,    0.25,  0,    0.25),
              c(0,     0,    0,     0.25, 0))

# Specifies initial values
Q6 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded_ageC, qmatrix=Q6)

# Run MSM model (adjusted for covariates)
msm6 <- msm(alc5 ~ years, subject=idnum, data = nesarc_expanded_ageC, qmatrix = Q6,
                  center=FALSE, control = list(trace=1, maxit=5000, fnscale = 750000),
                  covariates = ~ female_w1 + age.c + edu3 + race_w1)
saveRDS(msm6, paste0(models, "msm6.RDS")) # Save Results


# Load the models
msm6 <- readRDS(paste0(models, "msm6.RDS"))
AIC(msm6)


# 2) Average annual TP  ************************************************************************************************************

predicted_TP(msm8, 1, nrow(nesarc_ageC), nrow(nesarc_expanded_ageC)) %>%
  kable()



# 3) Extract and prepare the annual TP for each category **********************************************************************************************

# Specify the covariate values
mapping <- nesarc_expanded_ageC %>% select(age, age.c) %>% distinct()
age <- sort(unique(nesarc_expanded_ageC$age))
sex <- unique(nesarc_expanded_ageC$female_w1)
race <- unique(nesarc_expanded_ageC$race_w1)
edu <- unique(nesarc_expanded_ageC$edu3)


msm6_aTP <- predicted_TP_covs3 (msm6, 1, age, sex, race, edu) %>% 
  # Rename states
  mutate(From = recode(From,"State 1" = "Abstainer",  "State 2" = "Former", "State 3" = "Category I", "State 4" = "Category II", "State 5" = "Category III"),
    To = recode(To, "State.1" = "Abstainer", "State.2" = "Former", "State.3" = "Category I", "State.4" = "Category II", "State.5" = "Category III"),
    Transition = paste(From, To, sep = "->"),
    Transition = fct_relevel(Transition,         
      "Abstainer->Category I", "Former->Category I",	"Category I->Category II",	"Category II->Category III",
      "Category III->Category II",	"Category II->Category I",	"Category I->Former")) %>%
  mutate(cat = paste(age, sex, edu, race, From, sep="_")) %>% 
  select(cat, To, Probability) %>% 
  group_by(cat) %>% 
  mutate(cumsum = cumsum(Probability)) %>%
  ungroup()




# 4) Set up the baseline Population ********************************************************************************************

msm6_basepop <- nesarc_expanded_ageC %>%
  select(idnum, wave, female_w1, age, race_w1, edu3, alc5.factor) %>%  
  rename( sex = female_w1, 
    race = race_w1) %>%
  pivot_wider(names_from="wave", values_from=c("alc5.factor", "edu3", "age")) %>%  
  mutate (predicted_cat = alc5.factor_1,
    AlcUse_1 = alc5.factor_1,
    AlcUse_2 = alc5.factor_2,
    edu = edu3_1,
    age = age_1) %>%
  select(idnum, AlcUse_1, AlcUse_2, sex, age, edu, race, predicted_cat)



# 4.2) Compare observed (at NESARC III) vs predicted  *****************************************************************************

predicted_pop <-simulate_pop(msm6_basepop, msm6_aTP, transition_alc5, 11) 

observed_pop <- readRDS(paste0(data, "nesarc3_clean_expanded.rds")) %>% 
  filter (age < 90) %>%
  mutate(observed_cat = alc5.factor) # observed category of alcohol use

compare_pct(predicted_pop, observed_pop)




# Model 7:  AlcUse (5levels), Age (squared),    adjacent transitions----------------------------------------------------------
nesarc_ageSQ <- nesarc %>%
  filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate (age_sq = age ^ 2, 
    age.c = (age - mean(age)) / sd(age),
    age_sq.c = (age_sq - mean(age_sq)) / sd(age_sq)) 

nesarc_expanded_ageSQ <- nesarc_expanded %>%
  filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate (age_sq = age ^ 2, 
    age.c = (age - mean(age)) / sd(age),
    age_sq.c = (age_sq - mean(age_sq)) / sd(age_sq)) 


# 1) Run MSM Model *******************************************************************************************************
# Specify allowed transitions; only allow adjacent transitions
# only allow adjacent transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q7 <- rbind ( c(0,     0,    0.25,  0,    0),
              c(0,     0,    0.25,  0,    0),
              c(0,     0.25, 0,     0.25, 0),
              c(0,     0,    0.25,  0,    0.25),
              c(0,     0,    0,     0.25, 0))

# Specifies initial values
Q7 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded_ageSQ, qmatrix=Q7)

# Run MSM model (adjusted for covariates)
msm7 <- msm(alc5 ~ years, subject=idnum, data = nesarc_expanded_ageSQ, qmatrix = Q7,
                      center=FALSE, control = list(trace=1, maxit=5000, fnscale = 750000),
                      covariates = ~ female_w1 + age.c + age_sq.c + edu3 + race_w1)
saveRDS(msm7, paste0(models, "msm7.RDS")) # Save Results


# Load the models
msm7<- readRDS(paste0(models, "msm7.RDS"))
AIC(msm7)



# 2) Average annual TP  ************************************************************************************************************

predicted_TP(msm7, 1, nrow(nesarc_ageSQ), nrow(nesarc_expanded_ageSQ)) %>%
  kable()


# 3) Extract and prepare the annual TP for each category **********************************************************************************************

# Specify the covariate values
mapping <- nesarc_expanded_ageSQ %>% select(age, age.c, age_sq.c) %>% distinct()
age <- sort(unique(nesarc_expanded_ageSQ$age))
sex <- unique(nesarc_expanded_ageSQ$female_w1)
race <- unique(nesarc_expanded_ageSQ$race_w1)
edu <- unique(nesarc_expanded_ageSQ$edu3)


msm7_aTP <- predicted_TP_covs4 (msm7, 1, age, sex, race, edu) %>% 
  # Rename states
  mutate(From = recode(From,"State 1" = "Abstainer",  "State 2" = "Former", "State 3" = "Category I", "State 4" = "Category II", "State 5" = "Category III"),
    To = recode(To, "State.1" = "Abstainer", "State.2" = "Former", "State.3" = "Category I", "State.4" = "Category II", "State.5" = "Category III"),
    Transition = paste(From, To, sep = "->"),
    Transition = fct_relevel(Transition,         
      "Abstainer->Category I", "Former->Category I",	"Category I->Category II",	"Category II->Category III",
      "Category III->Category II",	"Category II->Category I",	"Category I->Former")) %>% 
  mutate(cat = paste(age, sex, edu, race, From, sep="_")) %>% 
  select(cat, To, Probability) %>% 
  group_by(cat) %>% 
  mutate(cumsum = cumsum(Probability)) %>%
  ungroup()




# 4) Set up the baseline Population ********************************************************************************************

msm7_basepop <- nesarc_expanded_ageSQ %>%
  select(idnum, wave, female_w1, age, race_w1, edu3, alc5.factor) %>%  
  rename( sex = female_w1, 
    race = race_w1) %>%
  pivot_wider(names_from="wave", values_from=c("alc5.factor", "edu3", "age")) %>%  
  mutate (predicted_cat = alc5.factor_1,
    AlcUse_1 = alc5.factor_1,
    AlcUse_2 = alc5.factor_2,
    edu = edu3_1,
    age = age_1) %>%
  select(idnum, AlcUse_1, AlcUse_2, sex, age, edu, race, predicted_cat)



# 5) Compare observed (at NESARC III) vs predicted  *****************************************************************************
predicted_pop <- alc_sim3(msm7_basepop, msm7_aTP, transition_alc5, 11) 

observed_pop <- readRDS(paste0(data, "nesarc3_clean_expanded.rds")) %>% 
  filter (age < 90) %>%
  mutate(observed_cat = alc5.factor) # observed category of alcohol use

compare_pct(predicted_pop, observed_pop)






# Model 8:  AlcUse (5levels), Age (7levles),    adjacent transitions----------------------------------------------------------
# 1) Run MSM Model *******************************************************************************************************

# Specify allowed transitions
# only allow adjacent transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q8 <- rbind ( c(0,     0,    0.25,  0,    0),
                  c(0,     0,    0.25,  0,    0),
                  c(0,     0.25, 0,     0.25, 0),
                  c(0,     0,    0.25,  0,    0.25),
                  c(0,     0,    0,     0.25, 0))

# Specifies initial values
Q8 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q8)


# Run MSM model (adjusted for covariates)
msm8 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded,
                  qmatrix = Q8, center=FALSE,
                  control = list(trace=1, maxit=600, fnscale = 3000000),
                  covariates = ~ female_w1 + age7 + edu3 + race_w1)
saveRDS(msm8, paste0(models, "msm8.RDS")) # Save Results


# Load the model
msm8 <- readRDS(paste0(models, "msm8.RDS"))
AIC(msm8)



# 2) Average annual TP  ***************************************************************************************************
predicted_TP(msm8, 1, nrow(nesarc), nrow(nesarc_expanded)) %>% 
  kable()


# 3) Extract and prepare the annual TP for each category *******************************************************************

# Specify the covariate values
age_cat <- unique(nesarc_expanded$age7)
sex <- unique(nesarc_expanded$female_w1)
race <- unique(nesarc_expanded$race_w1)
edu <- unique(nesarc_expanded$edu3)


# Extract annual TP 
msm8_aTP <- predicted_TP_covs (msm8, 1, age_cat, sex, race, edu) %>%
  # Rename states
  mutate(From = recode(From,"State 1" = "Abstainer",  "State 2" = "Former", "State 3" = "Category I","State 4" = "Category II", "State 5" = "Category III"),
    To = recode(To, "State.1" = "Abstainer", "State.2" = "Former", "State.3" = "Category I", "State.4" = "Category II", "State.5" = "Category III"),
    Transition = paste(From, To, sep = "->"),
    Transition = fct_relevel(Transition, "Abstainer->Category I",     # re-arrange order of transition variable
      "Former->Category I",	"Category I->Category II",	"Category II->Category III",
      "Category III->Category II",	"Category II->Category I",	"Category I->Former")) %>% 
  mutate(cat = paste(sex, age_cat, edu, race, From, sep="_")) %>% 
  select(cat, To, Probability) %>% 
  group_by(cat) %>% 
  mutate(cumsum = cumsum(Probability)) %>%
  ungroup()

# 4) Set up the baseline Population ********************************************************************************************
msm8_basepop <- nesarc_expanded %>%
  select(idnum, wave, age, female_w1, race_w1, edu3, alc5.factor) %>%  
  rename( sex = female_w1, 
    race = race_w1) %>%
  pivot_wider(names_from="wave", values_from=c("alc5.factor", "age", "edu3")) %>%  
  mutate (predicted_cat = alc5.factor_1,
    AlcUse_1 = alc5.factor_1,
    AlcUse_2 = alc5.factor_2,
    age = age_1,
    edu = edu3_1) %>%
  select(idnum, AlcUse_1, AlcUse_2, sex, age, edu, race, predicted_cat)



# 5) Compare observed (at NESARC III) vs predicted  *****************************************************************************

# Predicted 11 years after baseline

predicted_pop <-simulate_pop(msm8_basepop, msm8_aTP, transition_alc5, 11) %>%  # starts with stimulated data
  mutate(age7 = case_when(age < 21 ~ "18-20",
    age >= 21 & age <26 ~ "21-25",
    age >= 26 & age <30 ~ "26-29",
    age >= 30 & age <40 ~ "30-39",
    age >= 40 & age <50 ~ "40-49",
    age >= 50 & age <65 ~ "50-64",
    age >= 65 ~ "65+"),
    race = recode(race, "White, non-Hispanic" = "White", "Black, non-Hispanic" = "Black", "Other, non-Hispanic" = "Other")) 


# Observed at NESARC 3 (different population)
observed_pop <- readRDS(paste0(data, "nesarc3_clean_expanded.rds")) %>% # starts with observed data
  mutate(
    observed_cat = alc5.factor, # observed category of alcohol use
    sex = female.factor,
    edu = edu3,
    race = recode(race.factor, "White, non-Hispanic" = "White", "Black, non-Hispanic" = "Black", "Other, non-Hispanic" = "Other")) %>% 
  select(idnum, observed_cat, sex, age, age7, edu, race)


compare_pct(predicted_pop, observed_pop)
compare_pct(predicted_pop, observed_pop, sex)
compare_pct(predicted_pop, observed_pop, edu)
compare_pct(predicted_pop, observed_pop, race)
compare_pct(predicted_pop, observed_pop, age7)



# Model 9:  AlcUse (4levels), Age (3levels),    adjacent transitions----------------------------------------------------------
# 1) Run MSM Model *******************************************************************************************************

# Specify allowed transitions; only allow adjacent transitions
Q9 <- rbind ( c(0,    0.25,  0,    0),
              c(0.25, 0,     0.25, 0),
              c(0,    0.25,  0,    0.25),
              c(0,    0,     0.25, 0))

# Specifies initial values
Q9 <- crudeinits.msm(alc4 ~ years, idnum, data=nesarc_expanded, qmatrix=Q9)


# Run MSM model (adjusted for covariates)
msm9 <- msm (alc4 ~ years, subject=idnum, data = nesarc_expanded,
                  qmatrix = Q9, center=FALSE,
                  control = list(trace=1, maxit=600, fnscale = 3000000),
                  covariates = ~ female_w1 + age3 + edu3 + race_w1)
saveRDS(msm, paste0(models, "msm11.RDS")) # Save Results


# Load the model
msm9 <- readRDS(paste0(models, "msm9.RDS"))
AIC(msm9)


# 2) Average annual TP  ***************************************************************************************************
predicted_TP(msm9, 1, nrow(nesarc), nrow(nesarc_expanded)) %>% 
  kable()


# 3) Annual TP for each category ******************************************************************************************

# Specify the covariate values
age_cat <- unique(nesarc_expanded$age3)
sex <- unique(nesarc_expanded$female_w1)
race <- unique(nesarc_expanded$race_w1)
edu <- unique(nesarc_expanded$edu3)


# Load and format the transition probabilities
msm9_aTP <- predicted_TP_covs (msm9, 1, age_cat, sex, race, edu) %>%
  # Rename states
  mutate(From = recode(From,"State 1" = "Non-drinker",  "State 2" = "Category I", "State 3" = "Category II", "State 4" = "Category III"),
    To = recode(To, "State.1" = "Non-drinker", "State.2" = "Category I", "State.3" = "Category II", "State.4" = "Category III"),
    Transition = paste(From, To, sep = "->"),
    
    # re-arrange order of transition variable
    Transition = fct_relevel(Transition,         
      "Non-drinker->Category I", "Category I->Category II",	"Category II->Category III",
      "Category III->Category II",	"Category II->Category I",	"Category I->Non-drinker"),
    cat = paste(sex, age_cat, edu, race, From, sep="_")) %>% 
  select(cat, To, Probability) %>% 
  group_by(cat) %>% 
  mutate(cumsum = cumsum(Probability)) %>%
  ungroup()


# Load and set up the initial population (based on NESARC wave 1)
msm9_basepop <- nesarc_expanded %>%
  select(idnum, wave, age, female_w1, race_w1, edu3, alc4.factor) %>%  
  rename( sex = female_w1, 
    race = race_w1) %>%
  pivot_wider(names_from="wave", values_from=c("alc4.factor", "age", "edu3")) %>%  
  mutate (predicted_cat = alc4.factor_1,
    AlcUse_1 = alc4.factor_1,
    AlcUse_2 = alc4.factor_2,
    age = age_1,
    edu = edu3_1) %>%
  select(idnum, AlcUse_1, AlcUse_2, sex, age, edu, race, predicted_cat)


# 5) Compare observed (at NESARC III) vs predicted  *****************************************************************************

# Predicted: Simulate population at 11 years follow-up
predicted_pop <- simulate_pop(msm9_basepop, msm9_aTP, transition_alc4, 11) 

observed_pop <- readRDS(paste0(data, "nesarc3_clean_expanded.rds")) %>%
  mutate(observed_cat = alc4.factor) # observed category of alcohol use

compare_pct(predicted_pop, observed_pop)




# Model 10: AlcUse (4levels), Age (continuous), adjacent transitions----------------------------------------------------------
nesarc_ageC <- nesarc %>%
  filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate (age.c = (age - mean(age)) / sd(age)) 

nesarc_expanded_ageC <- nesarc_expanded %>%
  filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate (age.c = (age - mean(age)) / sd(age)) 

# 1) Run MSM Model *******************************************************************************************************
# Specify allowed transitions; only allow adjacent transitions
# only allow adjacent transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q10 <- rbind (c(0,    0.25,  0,    0),
              c(0.25, 0,     0.25, 0),
              c(0,    0.25,  0,    0.25),
              c(0,    0,     0.25, 0))


# Specifies initial values
Q10 <- crudeinits.msm(alc4 ~ years, idnum, data=nesarc_expanded_ageC, qmatrix=Q10)

# Run MSM model (adjusted for covariates)
msm10 <- msm(alc4 ~ years, subject=idnum, data = nesarc_expanded_ageC, qmatrix = Q10,
              center=FALSE, control = list(trace=1, maxit=5000, fnscale = 750000),
              covariates = ~ female_w1 + age.c + edu3 + race_w1)
saveRDS(msm10, paste0(models, "msm10.RDS")) # Save Results


# Load the models
msm10 <- readRDS(paste0(models, "msm10.RDS"))
AIC(msm10)

# Model 11: AlcUse (4levels), Age (squared),    adjacent transitions----------------------------------------------------------
nesarc_ageSQ <- nesarc %>%
  filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate (age_sq = age ^ 2, 
    age.c = (age - mean(age)) / sd(age),
    age_sq.c = (age_sq - mean(age_sq)) / sd(age_sq)) 

nesarc_expanded_ageSQ <- nesarc_expanded %>%
  filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate (age_sq = age ^ 2, 
    age.c = (age - mean(age)) / sd(age),
    age_sq.c = (age_sq - mean(age_sq)) / sd(age_sq)) 

#1) Run MSM Model *******************************************************************************************************
# Specify allowed transitions; only allow adjacent transitions
# only allow adjacent transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q11 <- rbind (c(0,    0.25,  0,    0),
              c(0.25, 0,     0.25, 0),
              c(0,    0.25,  0,    0.25),
              c(0,    0,     0.25, 0))


# Specifies initial values
Q11 <- crudeinits.msm(alc4 ~ years, idnum, data=nesarc_expanded_ageSQ, qmatrix=Q11)

# Run MSM model (adjusted for covariates)
msm11 <- msm(alc4 ~ years, subject=idnum, data = nesarc_expanded_ageSQ, qmatrix = Q11,
  center=FALSE, control = list(trace=1, maxit=5000, fnscale = 750000),
  covariates = ~ female_w1 + age.c + age_sq.c + edu3 + race_w1)
saveRDS(msm11, paste0(models, "msm11.RDS")) # Save Results


# Load the models
msm11 <- readRDS(paste0(models, "msm11.RDS"))
AIC(msm11)




# Model 12: AlcUse (4levels), Age (7levels),    adjacent transitions----------------------------------------------------------

# 1) Run MSM Model *******************************************************************************************************

# Specify allowed transitions
# only allow adjacent transitions
Q12 <- rbind ( c(0,    0.25,  0,    0),
              c(0.25, 0,     0.25, 0),
              c(0,    0.25,  0,    0.25),
              c(0,    0,     0.25, 0))

# Specifies initial values
Q12 <- crudeinits.msm(alc4 ~ years, idnum, data=nesarc_expanded, qmatrix=Q12)


# Run MSM model (adjusted for covariates)
msm12 <- msm (alc4 ~ years, subject=idnum, data = nesarc_expanded,
                      qmatrix = Q12, center=FALSE,
                      control = list(trace=1, maxit=600, fnscale = 3000000),
                      covariates = ~ female_w1 + age7 + edu3 + race_w1)
saveRDS(msm12, paste0(models, "msm12.RDS")) # Save Results


# Load the model
msm12 <- readRDS(paste0(models, "msm12.RDS"))
AIC(msm12)


# 2) Average annual TP  ***************************************************************************************************
predicted_TP(msm12, 1, nrow(nesarc), nrow(nesarc_expanded)) %>%
  kable()


# 3) Extract and prepare the annual TP for each category *******************************************************************

# Specify the covariate values
age_cat <- unique(nesarc_expanded$age7)
sex <- unique(nesarc_expanded$female_w1)
race <- unique(nesarc_expanded$race_w1)
edu <- unique(nesarc_expanded$edu3)


# Extract annual TP 
msm12_aTP <- predicted_TP_covs (msm12, 2, age_cat, sex, race, edu) %>%
  # Rename states
  mutate(From = recode(From,"State 1" = "Non-drinker", "State 2" = "Category I", "State 3" = "Category II", "State 4" = "Category III"),
    To = recode(To, "State.1" = "Non-drinker", "State.2" = "Category I", "State.3" = "Category II", "State.4" = "Category III"),
    Transition = paste(From, To, sep = "->"),
    Transition = fct_relevel(Transition, "Non-drinker->Category I",	"Category I->Category II",	"Category II->Category III",
      "Category III->Category II",	"Category II->Category I",	"Category I->Non-drinker")) %>% 
  mutate(cat = paste(sex, age_cat, edu, race, From, sep="_")) %>% 
  select(cat, To, Probability) %>% 
  group_by(cat) %>% 
  mutate(cumsum = cumsum(Probability)) %>%
  ungroup()



# 4) Set up the baseline Population ********************************************************************************************
msm12_basepop <- nesarc_expanded %>%
  select(idnum, wave, age, age7, female_w1, race_w1, edu3, alc4.factor) %>%  
  rename( sex = female_w1, 
    race = race_w1) %>%
  pivot_wider(names_from="wave", values_from=c("alc4.factor", "age", "age7", "edu3")) %>%  
  mutate (predicted_cat = alc4.factor_1,
    AlcUse_1 = alc4.factor_1,
    AlcUse_2 = alc4.factor_2,
    age = age_1,
    age7 = age7_1,
    edu = edu3_1) %>%
  select(idnum, AlcUse_1, AlcUse_2, sex, age, age7, edu, race, predicted_cat)



# 5) Compare observed (at NESARC III) vs predicted  *****************************************************************************

# Predicted 11 years after baseline
predicted_pop <- simulate_pop(msm12_basepop, msm12_aTP, transition_alc4, 11) %>%  # starts with stimulated data
  mutate(race = recode(race, "White, non-Hispanic" = "White", "Black, non-Hispanic" = "Black", "Other, non-Hispanic" = "Other")) 


# Observed at NESARC 3 (different population)
observed_pop <- readRDS(paste0(data, "nesarc3_clean_expanded.rds")) %>%  # starts with observed data
  mutate(
         observed_cat = alc4.factor, 
         sex = female.factor,
         edu = edu3,
         race = recode(race.factor, "White, non-Hispanic" = "White", "Black, non-Hispanic" = "Black", "Other, non-Hispanic" = "Other")) %>% 
  select(idnum, observed_cat, sex, age, age7, edu, race)


compare_pct(predicted_pop, observed_pop)
compare_pct(predicted_pop, observed_pop, sex)
compare_pct(predicted_pop, observed_pop, edu)
compare_pct(predicted_pop, observed_pop, race)
compare_pct(predicted_pop, observed_pop, age7)


# Load and compare all MSM models -----------------------------------------------------------------------------------

msm1  <- readRDS(paste0(models, "msm1.RDS"))
msm2  <- readRDS(paste0(models, "msm2.RDS"))
msm3  <- readRDS(paste0(models, "msm3.RDS"))
msm4  <- readRDS(paste0(models, "msm4.RDS"))
msm5  <- readRDS(paste0(models, "msm5.RDS"))
msm6  <- readRDS(paste0(models, "msm6.RDS"))
msm7  <- readRDS(paste0(models, "msm7.RDS"))
msm8  <- readRDS(paste0(models, "msm8.RDS"))
msm9  <- readRDS(paste0(models, "msm9.RDS"))
msm10 <- readRDS(paste0(models, "msm10.RDS"))
msm11 <- readRDS(paste0(models, "msm11.RDS"))
msm12 <- readRDS(paste0(models, "msm11.RDS"))

AIC(msm1, msm2, msm3,msm4,msm5,msm6,msm7,msm8,msm9,msm10,msm11, msm12) %>% arrange(AIC)

