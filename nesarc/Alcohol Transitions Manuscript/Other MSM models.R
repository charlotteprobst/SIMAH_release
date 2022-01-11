
# SIMAH - NESARC Alcohol Transitions
# Data Analysis

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



# 1) AlcUse: 4 categories: Combine Abstainers & Former drinkers ------------------------------------------------------------

# 1.1) Run MSM Model *******************************************************************************************************

# Specify allowed transitions; only allow adjacent transitions
Q_alc4 <- rbind ( c(0,    0.25,  0,    0),
                  c(0.25, 0,     0.25, 0),
                  c(0,    0.25,  0,    0.25),
                  c(0,    0,     0.25, 0))

# Specifies initial values
Q_alc4 <- crudeinits.msm(alc4 ~ years, idnum, data=nesarc_expanded, qmatrix=Q_alc4)  


# Run MSM model (adjusted for covariates)
alc4.msm <- msm (alc4 ~ years, subject=idnum, data = nesarc_expanded, 
  qmatrix = Q_alc4, center=FALSE,                            
  control = list(trace=1, maxit=600, fnscale = 3000000),
  covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
saveRDS(alc4.msm, paste0(models, "alc4.msm.RDS")) # Save Results


# Load the model
alc4.msm       <- readRDS(paste0(models, "alc4.msm.RDS"))



# 1.2) Average annual TP  ***************************************************************************************************
alc4_aTP <- predicted_TP(model=alc4.msm, year=1, original_n = nrow(nesarc), expanded_n = nrow(nesarc_expanded))
kable(alc4_aTP)



# 1.3) Annual TP for each category ******************************************************************************************

# Specify the covariate values
age_cat <- unique(nesarc_expanded$age3.factor)
sex <- unique(nesarc_expanded$female_wave1.factor)
race <- unique(nesarc_expanded$race_wave1.factor)
edu <- unique(nesarc_expanded$edu3.factor)


# Function to extract annual TP 
aTP_alc4 <- predicted_TP_covs (alc4.msm, 1, age_cat, sex, race, edu) %>%
  # Rename states
  mutate(From = recode(From,"State 1" = "Non-drinker",  "State 2" = "Category I", "State 3" = "Category II", "State 4" = "Category III"),
         To = recode(To, "State.1" = "Non-drinker", "State.2" = "Category I", "State.3" = "Category II", "State.4" = "Category III"),
         Transition = paste(From, To, sep = "->"),
    
    # re-arrange order of transition variable
    Transition = fct_relevel(Transition,         
                            "Non-drinker->Category I", "Category I->Category II",	"Category II->Category III",
                            "Category III->Category II",	"Category II->Category I",	"Category I->Non-drinker"))

write_csv(aTP_alc4, paste0(output, "AlcUse_4 Annual Transition Probabilities.csv")) # Save TP





# 1.4) Simulate population using aTP   *************************************************************************************

# 1.4.1) Load the required files

    # Load and format the transition probabilities
    aTP_alc4 <- read_csv(paste0(output, "AlcUse_4 Annual Transition Probabilities.csv")) %>% 
      mutate(cat = paste(sex, age_cat, edu, race, From, sep="_")) %>% 
      select(cat, To, Probability) %>% 
      group_by(cat) %>% 
      mutate(cumsum = cumsum(Probability)) %>%
      ungroup()
    
    
    # Load and set up the initial population (based on NESARC wave 1)
    AlcUse4_basepop <- nesarc_expanded %>%
      select(idnum, wave, age, female_wave1.factor, race_wave1.factor, edu3.factor, alc4.factor) %>%  
      rename( sex = female_wave1.factor, 
        race = race_wave1.factor) %>%
      pivot_wider(names_from="wave", values_from=c("alc4.factor", "age", "edu3.factor")) %>%  
      mutate (AlcUse_pred = alc4.factor_1,
        AlcUse_1 = alc4.factor_1,
        AlcUse_2 = alc4.factor_2,
        age = age_1,
        edu = edu3.factor_1) %>%
      select(idnum, AlcUse_1, AlcUse_2, sex, age, edu, race, AlcUse_pred)



# 1.4.2) Compare observed (at Wave 2) vs predicted *****************************************************************************

    # Predicted: Simulate population at 3 years follow-up
    AlcUse4_year3 <-alc4_sim(3)
    
    
    # Compare observed and predicted at the group
    observed <- count(AlcUse4_year3, AlcUse_2) %>%
      rename(observed = n, AlcUse = AlcUse_2) %>%
      mutate(obs_pct = round(observed / sum(observed) * 100,2))
    
    
    predicted <- count(AlcUse4_year3, AlcUse_pred) %>% 
      rename(predicted = n, AlcUse = AlcUse_pred) %>%
      mutate(pred_pct = round(predicted / sum(predicted) * 100,2)) 
    
    comparison_AlcUse4 <- full_join (observed, predicted, by="AlcUse") %>% 
      mutate(diff_pct = round((pred_pct-obs_pct), 2))%>%
      select (AlcUse, obs_pct, pred_pct, diff_pct)
    
    kable(comparison_AlcUse4)



# 1.4.2) Compare observed (at NESARC III) vs predicted  *****************************************************************************

    # Predicted: Simulate population at 11 years follow-up
    AlcUse_year11 <-alc4_sim(11)
    
    predicted_11 <- count(AlcUse_year11, AlcUse_pred) %>% 
      rename(predicted = n, AlcUse = AlcUse_pred) %>%
      mutate(pred_pct = round(predicted / sum(predicted) * 100,2)) 
    
    # Observed at Wave 3 (different population)
    nesarc3_expanded <- readRDS(paste0(data, "nesarc3_clean_expanded.rds")) 
    
    observed_11 <- count(nesarc3_expanded, alc4.factor) %>%
      rename(observed = n, AlcUse = alc4.factor) %>%
      mutate(obs_pct = round(observed / sum(observed) * 100,2))
    
    AlcUse4_comparison_11 <- full_join (observed_11, predicted_11, by="AlcUse") %>% 
      mutate(diff_pct = round((pred_pct-obs_pct), 2))%>%
      select (AlcUse, obs_pct, pred_pct, diff_pct)
    
    kable(AlcUse4_comparison_11)

    
    
# 2) AlcUse, Stratify by age ------------------------------------------------------------
    
# 2.1) Run MSM Model *******************************************************************************************************

# First statify data
nesarc_ages18_30   <- nesarc %>% filter (age_wave1<=30) 
nesarc_ages30_50   <- nesarc %>% filter (age_wave1>30 & age_wave1<50)
nesarc_ages50plus <- nesarc %>% filter (age_wave1>=50)
    
nesarc_exp_ages18_30   <- nesarc_expanded %>% filter (age_wave1<=30) 
nesarc_exp_ages30_50   <- nesarc_expanded %>% filter (age_wave1>30 & age_wave1<50)
nesarc_exp_ages50plus <- nesarc_expanded %>% filter (age_wave1>=50)


# Specify allowed transitions; only allow adjacent transitions
# only allow adjacent transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q_alc5 <- rbind ( c(0,     0,    0.25,  0,    0),
                  c(0,     0,    0.25,  0,    0),
                  c(0,     0.25, 0,     0.25, 0),
                  c(0,     0,    0.25,  0,    0.25),
                  c(0,     0,    0,     0.25, 0))
    

# Ages 18 - 30
    # Specifies initial values
    Q_alc5_1 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_exp_ages18_30, qmatrix=Q_alc5)  
    
    
    # Run MSM model (adjusted for covariates)
    alc5_age1.msm <- msm (alc5 ~ years, subject=idnum, data = nesarc_exp_ages18_30, qmatrix = Q_alc5_1, 
                    center=FALSE, control = list(trace=1, maxit=5000, fnscale = 750000, reltol = 1e-16),  
                    covariates = ~ female_wave1.factor + edu3.factor + race_wave1.factor)
    saveRDS(alc5_age1.msm, paste0(models, "alc5_age1.msm.RDS")) # Save Results

    
    
# Ages 30 - 50
    # Specifies initial values
    Q_alc5_2 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_exp_ages30_50, qmatrix=Q_alc5)  
    
    
    # Run MSM model (adjusted for covariates)
    alc5_age2.msm <- msm (alc5 ~ years, subject=idnum, data = nesarc_exp_ages30_50, qmatrix = Q_alc5_2, 
                          center=FALSE, control = list(trace=1, maxit=600, fnscale = 3000000),
                          covariates = ~ female_wave1.factor + edu3.factor + race_wave1.factor)
    saveRDS(alc5_age2.msm, paste0(models, "alc5_age2.msm.RDS")) # Save Results
    

    
# Ages 50+
    # Specifies initial values
    Q_alc5_3 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_exp_ages_50plus, qmatrix=Q_alc5)  
    
    
    # Run MSM model (adjusted for covariates)
    alc5_age3.msm <- msm (alc5 ~ years, subject=idnum, data = nesarc_exp_ages_50plus, qmatrix = Q_alc5_3, 
                          center=FALSE, control = list(trace=1, maxit=600, fnscale = 3000000),
                          covariates = ~ female_wave1.factor + edu3.factor + race_wave1.factor)
    saveRDS(alc5_age3.msm, paste0(models, "alc5_age3.msm.RDS")) # Save Results
    

    

# Load the models
alc5_age1.msm       <- readRDS(paste0(models, "alc5_age1.msm.RDS"))
alc5_age2.msm       <- readRDS(paste0(models, "alc5_age2.msm.RDS"))
alc5_age3.msm       <- readRDS(paste0(models, "alc5_age3.msm.RDS"))

    

    
# 2.2) Average annual TP  ************************************************************************************************************

# Ages 30 - 50
alc_aTP_ages30_50 <- predicted_TP(model=alc5_age2.msm, year=1, original_n = nrow(nesarc_ages30_50), expanded_n = nrow(nesarc_exp_ages30_50))
kable(alc_aTP_ages30_50)


# Ages 50+
alc_aTP_ages50plus <-predicted_TP(model=alc5_age3.msm, year=1, original_n = nrow(nesarc_ages50plus), expanded_n = nrow(nesarc_exp_ages50plus))
kable(alc_aTP_ages50plus)


# 2.3) Extract and prepare the annual TP for each category **********************************************************************************************

# Specify the covariate values
sex <- unique(nesarc_expanded$female_wave1.factor)
race <- unique(nesarc_expanded$race_wave1.factor)
edu <- unique(nesarc_expanded$edu3.factor)


# Ages 30 - 50 
aTP_alc5_age2 <- predicted_TP_covs2 (alc5_age2.msm, 1, sex, race, edu) %>%
  # Rename states
  mutate(From = recode(From,"State 1" = "Abstainer",  "State 2" = "Former", "State 3" = "Category I", "State 4" = "Category II", "State 5" = "Category III"),
    To = recode(To, "State.1" = "Abstainer", "State.2" = "Former", "State.3" = "Category I", "State.4" = "Category II", "State.5" = "Category III"),
    Transition = paste(From, To, sep = "->"),
    Transition = fct_relevel(Transition,         
      "Abstainer->Category I", "Former->Category I",	"Category I->Category II",	"Category II->Category III",
      "Category III->Category II",	"Category II->Category I",	"Category I->Former")) %>% 
  mutate(cat = paste(sex, edu, race, From, sep="_")) %>% 
  select(cat, To, Probability) %>% 
  group_by(cat) %>% 
  mutate(cumsum = cumsum(Probability)) %>%
  ungroup()



# Ages 50+
aTP_alc5_age3 <- predicted_TP_covs2 (alc5_age3.msm, 1, sex, race, edu) %>%
  # Rename states
  mutate(From = recode(From,"State 1" = "Abstainer",  "State 2" = "Former", "State 3" = "Category I", "State 4" = "Category II", "State 5" = "Category III"),
    To = recode(To, "State.1" = "Abstainer", "State.2" = "Former", "State.3" = "Category I", "State.4" = "Category II", "State.5" = "Category III"),
    Transition = paste(From, To, sep = "->"),
    Transition = fct_relevel(Transition,         
      "Abstainer->Category I", "Former->Category I",	"Category I->Category II",	"Category II->Category III",
      "Category III->Category II",	"Category II->Category I",	"Category I->Former"))%>% 
  mutate(cat = paste(sex, edu, race, From, sep="_")) %>% 
  select(cat, To, Probability) %>% 
  group_by(cat) %>% 
  mutate(cumsum = cumsum(Probability)) %>%
  ungroup()



# 2.4) Set up the baseline Population ********************************************************************************************

# Ages 30 - 50 
AlcUse5_basepop2 <- nesarc_exp_ages30_50 %>%
  select(idnum, wave, female_wave1.factor, race_wave1.factor, edu3.factor, alc5.factor) %>%  
  rename( sex = female_wave1.factor, 
          race = race_wave1.factor) %>%
  pivot_wider(names_from="wave", values_from=c("alc5.factor", "edu3.factor")) %>%  
  mutate (AlcUse_pred = alc5.factor_1,
          AlcUse_1 = alc5.factor_1,
          AlcUse_2 = alc5.factor_2,
          edu = edu3.factor_1) %>%
  select(idnum, AlcUse_1, AlcUse_2, sex, edu, race, AlcUse_pred)



# Ages 50+ 
AlcUse5_basepop3 <- nesarc_exp_ages50plus %>%
  select(idnum, wave, female_wave1.factor, race_wave1.factor, edu3.factor, alc5.factor) %>%  
  rename( sex = female_wave1.factor, 
    race = race_wave1.factor) %>%
  pivot_wider(names_from="wave", values_from=c("alc5.factor", "edu3.factor")) %>%  
  mutate (AlcUse_pred = alc5.factor_1,
    AlcUse_1 = alc5.factor_1,
    AlcUse_2 = alc5.factor_2,
    edu = edu3.factor_1) %>%
  select(idnum, AlcUse_1, AlcUse_2, sex, edu, race, AlcUse_pred)



# 2.4.2) Compare observed (at NESARC III) vs predicted  *****************************************************************************

# Ages 30 - 50 
    
    # Predicted 11 years after baseline
    predicted <-alc_sim2(AlcUse5_basepop2, aTP_alc5_age2, transition_alc5, 11) %>%  # starts with stimulated data
      count(AlcUse_pred) %>%                                                        # Count number in each category, then reorganizes it
      rename(predicted = n, AlcUse = AlcUse_pred) %>%
      mutate(pred_pct = round(predicted / sum(predicted) * 100,2)) 


    # Observed at NESARC 3 (different population)
    observed <- readRDS(paste0(data, "nesarc3_clean_expanded.rds")) %>% # starts with observed data
      filter (age>41 & age<61) %>%                                      # specifies age range of participants from simulated data
      count(alc5.factor) %>%                                            # Count number in each category, then reorganizes it
      rename(observed = n, AlcUse = alc5.factor) %>%
      mutate(obs_pct = round(observed / sum(observed) * 100,2))
    
    comparison <- full_join (observed, predicted, by="AlcUse") %>% 
      mutate(diff_pct = round((pred_pct-obs_pct), 2))%>%
      select (AlcUse, obs_pct, pred_pct, diff_pct)
    
    kable(comparison)



# Ages 50+ 
    
    # Predicted 11 years after baseline
    predicted <-alc_sim2(AlcUse5_basepop3, aTP_alc5_age3, transition_alc5, 11) %>%  # starts with stimulated data
      count(AlcUse_pred) %>%                                                        # Count number in each category, then reorganizes it
      rename(predicted = n, AlcUse = AlcUse_pred) %>%
      mutate(pred_pct = round(predicted / sum(predicted) * 100,2)) 
    
    
    # Observed at NESARC 3 (different population)
    observed <- readRDS(paste0(data, "nesarc3_clean_expanded.rds")) %>% # starts with observed data
      filter (age>=61) %>%                                              # specifies age range of participants from simulated data
      count(alc5.factor) %>%                                            # Count number in each category, then reorganizes it
      rename(observed = n, AlcUse = alc5.factor) %>%
      mutate(obs_pct = round(observed / sum(observed) * 100,2))
    
    comparison <- full_join (observed, predicted, by="AlcUse") %>% 
      mutate(diff_pct = round((pred_pct-obs_pct), 2))%>%
      select (AlcUse, obs_pct, pred_pct, diff_pct)
    
    kable(comparison)
    
    
    

# 3) AlcUse, Age continuous ------------------------------------------------------------

nesarc_ageC <- nesarc %>%
    filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
    mutate (age.c = (age - mean(age)) / sd(age)) 
    
nesarc_expanded_ageC <- nesarc_expanded %>%
    filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
    mutate (age.c = (age - mean(age)) / sd(age)) 
    
    
# 3.1) Run MSM Model *******************************************************************************************************
# Specify allowed transitions; only allow adjacent transitions
# only allow adjacent transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q_alc5 <- rbind ( c(0,     0,    0.25,  0,    0),
                  c(0,     0,    0.25,  0,    0),
                  c(0,     0.25, 0,     0.25, 0),
                  c(0,     0,    0.25,  0,    0.25),
                  c(0,     0,    0,     0.25, 0))

# Specifies initial values
Q_alc5 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded_ageC, qmatrix=Q_alc5)  

# Run MSM model (adjusted for covariates)
alc5_ageC.msm <- msm(alc5 ~ years, subject=idnum, data = nesarc_expanded_ageC, qmatrix = Q_alc5, 
                  center=FALSE, control = list(trace=1, maxit=5000, fnscale = 750000),  
                  covariates = ~ female_wave1.factor + age.c + edu3.factor + race_wave1.factor)
saveRDS(alc5_ageC.msm, paste0(models, "alc5_ageC.msm.RDS")) # Save Results


# Load the models
alc5_ageC.msm <- readRDS(paste0(models, "alc5_ageC.msm.RDS"))




# 3.2) Average annual TP  ************************************************************************************************************

alc5_ageC_aTP <- predicted_TP(model=alc5_ageC.msm, year=1, original_n = nrow(nesarc_ageC), expanded_n = nrow(nesarc_expanded_ageC))
kable(alc5_ageC_aTP)



# 3.3) Extract and prepare the annual TP for each category **********************************************************************************************

# Specify the covariate values
mapping <- nesarc_expanded_ageC %>% select(age, age.c) %>% distinct()
age <- sort(unique(nesarc_expanded_ageC$age))
sex <- unique(nesarc_expanded_ageC$female_wave1.factor)
race <- unique(nesarc_expanded_ageC$race_wave1.factor)
edu <- unique(nesarc_expanded_ageC$edu3.factor)


aTP_alc5_ageC <- predicted_TP_covs3 (alc5_ageC.msm, 1, age, sex, race, edu) %>% 
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




# 3.4) Set up the baseline Population ********************************************************************************************

AlcUse5_ageC_basepop <- nesarc_expanded_ageC %>%
  select(idnum, wave, female_wave1.factor, age, race_wave1.factor, edu3.factor, alc5.factor) %>%  
  rename( sex = female_wave1.factor, 
          race = race_wave1.factor) %>%
  pivot_wider(names_from="wave", values_from=c("alc5.factor", "edu3.factor", "age")) %>%  
  mutate (AlcUse_pred = alc5.factor_1,
    AlcUse_1 = alc5.factor_1,
    AlcUse_2 = alc5.factor_2,
    edu = edu3.factor_1,
    age = age_1) %>%
  select(idnum, AlcUse_1, AlcUse_2, sex, age, edu, race, AlcUse_pred)



# 3.4.2) Compare observed (at NESARC III) vs predicted  *****************************************************************************

# Predicted 11 years after baseline
predicted <- alc_sim3(AlcUse5_ageC_basepop, aTP_alc5_ageC, transition_alc5, 11)

predicted <-alc_sim3(AlcUse5_ageC_basepop, aTP_alc5_ageC, transition_alc5, 11) %>%  # starts with stimulated data
  count(AlcUse_pred) %>%                                                        # Count number in each category, then reorganizes it
  rename(predicted = n, AlcUse = AlcUse_pred) %>%
  mutate(pred_pct = round(predicted / sum(predicted) * 100,2)) 

# Observed at NESARC 3 (different population)
observed <- readRDS(paste0(data, "nesarc3_clean_expanded.rds")) %>% # starts with observed data
  filter (age < 90) %>% 
  count(alc5.factor) %>%                                            # Count number in each category, then reorganizes it
  rename(observed = n, AlcUse = alc5.factor) %>%
  mutate(obs_pct = round(observed / sum(observed) * 100,2))

comparison <- full_join (observed, predicted, by="AlcUse") %>% 
  mutate(diff_pct = round((pred_pct-obs_pct), 2))%>%
  select (AlcUse, obs_pct, pred_pct, diff_pct)

kable(comparison)





# 4) AlcUse, Age squared ------------------------------------------------------------

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


# 4.1) Run MSM Model *******************************************************************************************************
# Specify allowed transitions; only allow adjacent transitions
# only allow adjacent transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q_alc5 <- rbind ( c(0,     0,    0.25,  0,    0),
  c(0,     0,    0.25,  0,    0),
  c(0,     0.25, 0,     0.25, 0),
  c(0,     0,    0.25,  0,    0.25),
  c(0,     0,    0,     0.25, 0))

# Specifies initial values
Q_alc5 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded_ageSQ, qmatrix=Q_alc5)  

# Run MSM model (adjusted for covariates)
alc5_ageSQ.msm <- msm(alc5 ~ years, subject=idnum, data = nesarc_expanded_ageSQ, qmatrix = Q_alc5, 
  center=FALSE, control = list(trace=1, maxit=5000, fnscale = 750000),  
  covariates = ~ female_wave1.factor + age.c + age_sq.c + edu3.factor + race_wave1.factor)
saveRDS(alc5_ageSQ.msm, paste0(models, "alc5_ageSQ.msm.RDS")) # Save Results


# Load the models
alc5_ageSQ.msm <- readRDS(paste0(models, "alc5_ageSQ.msm.RDS"))




# 4.2) Average annual TP  ************************************************************************************************************

predicted_TP(model=alc5_ageSQ.msm, year=1, original_n = nrow(nesarc_ageSQ), expanded_n = nrow(nesarc_expanded_ageSQ)) %>%
  kable()


# 4.3) Extract and prepare the annual TP for each category **********************************************************************************************

# Specify the covariate values
mapping <- nesarc_expanded_ageSQ %>% select(age, age.c, age_sq.c) %>% distinct()
age <- sort(unique(nesarc_expanded_ageSQ$age))
sex <- unique(nesarc_expanded_ageSQ$female_wave1.factor)
race <- unique(nesarc_expanded_ageSQ$race_wave1.factor)
edu <- unique(nesarc_expanded_ageSQ$edu3.factor)


aTP_alc5_ageSQ <- predicted_TP_covs4 (alc5_ageSQ.msm, 1, age, sex, race, edu) %>% 
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




# 4.4) Set up the baseline Population ********************************************************************************************

AlcUse5_ageSQ_basepop <- nesarc_expanded_ageSQ %>%
  select(idnum, wave, female_wave1.factor, age, race_wave1.factor, edu3.factor, alc5.factor) %>%  
  rename( sex = female_wave1.factor, 
    race = race_wave1.factor) %>%
  pivot_wider(names_from="wave", values_from=c("alc5.factor", "edu3.factor", "age")) %>%  
  mutate (AlcUse_pred = alc5.factor_1,
    AlcUse_1 = alc5.factor_1,
    AlcUse_2 = alc5.factor_2,
    edu = edu3.factor_1,
    age = age_1) %>%
  select(idnum, AlcUse_1, AlcUse_2, sex, age, edu, race, AlcUse_pred)



# 4.5) Compare observed (at NESARC III) vs predicted  *****************************************************************************

# Predicted 11 years after baseline

predicted <- alc_sim3(AlcUse5_ageSQ_basepop, aTP_alc5_ageSQ, transition_alc5, 11) %>%  # starts with stimulated data
  count(AlcUse_pred) %>%                                                        # Count number in each category, then reorganizes it
  rename(predicted = n, AlcUse = AlcUse_pred) %>%
  mutate(pred_pct = round(predicted / sum(predicted) * 100,2)) 

# Observed at NESARC 3 (different population)
observed <- readRDS(paste0(data, "nesarc3_clean_expanded.rds")) %>% # starts with observed data
  filter (age < 90) %>% 
  count(alc5.factor) %>%                                            # Count number in each category, then reorganizes it
  rename(observed = n, AlcUse = alc5.factor) %>%
  mutate(obs_pct = round(observed / sum(observed) * 100,2))

comparison <- full_join (observed, predicted, by="AlcUse") %>% 
  mutate(diff_pct = round((pred_pct-obs_pct), 2))%>%
  select (AlcUse, obs_pct, pred_pct, diff_pct)

kable(comparison)





# 5) AlcUse, Age continuous, 4 alcohol categories ------------------------------------------------------------

nesarc_ageC <- nesarc %>%
  filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate (age.c = (age - mean(age)) / sd(age)) 

nesarc_expanded_ageC <- nesarc_expanded %>%
  filter (age<90) %>% group_by(idnum) %>% filter(n() > 1) %>% ungroup() %>% 
  mutate (age.c = (age - mean(age)) / sd(age)) 


# 3.1) Run MSM Model *******************************************************************************************************
# Specify allowed transitions; only allow adjacent transitions
# only allow adjacent transitions

# Specify allowed transitions; only allow adjacent transitions
Q_alc4 <- rbind ( c(0,    0.25,  0,    0),
  c(0.25, 0,     0.25, 0),
  c(0,    0.25,  0,    0.25),
  c(0,    0,     0.25, 0))

# Specifies initial values
Q_alc4 <- crudeinits.msm(alc4 ~ years, idnum, data=nesarc_expanded_ageC, qmatrix=Q_alc4)  


# Run MSM model (adjusted for covariates)
alc4_ageC.msm <- msm(alc4 ~ years, subject=idnum, data = nesarc_expanded_ageC, qmatrix = Q_alc4, 
                      center=FALSE, control = list(trace=1, maxit=5000, fnscale = 750000),  
                      covariates = ~ female_wave1.factor + age.c + edu3.factor + race_wave1.factor)
saveRDS(alc4_ageC.msm, paste0(models, "alc4_ageC.msm.RDS")) # Save Results


# Load the models
alc4_ageC.msm <- readRDS(paste0(models, "alc4_ageC.msm.RDS"))




# 3.2) Average annual TP  ************************************************************************************************************

alc4_ageC_aTP <- predicted_TP(model=alc4_ageC.msm, year=1, original_n = nrow(nesarc_ageC), expanded_n = nrow(nesarc_expanded_ageC))
kable(alc4_ageC_aTP)



# 3.3) Extract and prepare the annual TP for each category **********************************************************************************************

# Specify the covariate values
mapping <- nesarc_expanded_ageC %>% select(age, age.c) %>% distinct()
age <- sort(unique(nesarc_expanded_ageC$age))
sex <- unique(nesarc_expanded_ageC$female_wave1.factor)
race <- unique(nesarc_expanded_ageC$race_wave1.factor)
edu <- unique(nesarc_expanded_ageC$edu3.factor)


aTP_alc4_ageC <- predicted_TP_covs3 (alc4_ageC.msm, 1, age, sex, race, edu) %>% 
  # Rename states
  mutate( From = recode(From,"State 1" = "Non-drinker", "State 2" = "Category I", "State 3" = "Category II", "State 4" = "Category III"),
          To = recode(To, "State.1" = "Non-drinker", "State.2" = "Category I", "State.3" = "Category II", "State.4" = "Category III"),
          Transition = paste(From, To, sep = "->"),
          Transition = fct_relevel(Transition,         
            "Non-drinker->Category I",	"Category I->Category II",	"Category II->Category III",
            "Category III->Category II",	"Category II->Category I",	"Category I->Non-drinker")) %>%
  mutate(cat = paste(age, sex, edu, race, From, sep="_")) %>% 
  select(cat, To, Probability) %>% 
  group_by(cat) %>% 
  mutate(cumsum = cumsum(Probability)) %>%
  ungroup()



# 3.4) Set up the baseline Population ********************************************************************************************

AlcUse4_ageC_basepop <- nesarc_expanded_ageC %>%
  select(idnum, wave, female_wave1.factor, age, race_wave1.factor, edu3.factor, alc4.factor) %>%  
  rename( sex = female_wave1.factor, 
          race = race_wave1.factor) %>%
  pivot_wider(names_from="wave", values_from=c("alc4.factor", "edu3.factor", "age")) %>%  
  mutate (AlcUse_pred = alc4.factor_1,
          AlcUse_1 = alc4.factor_1,
          AlcUse_2 = alc4.factor_2,
          edu = edu3.factor_1,
          age = age_1) %>%
  select(idnum, AlcUse_1, AlcUse_2, sex, age, edu, race, AlcUse_pred)



# 3.4.2) Compare observed (at NESARC III) vs predicted  *****************************************************************************

# Predicted 11 years after baseline

predicted <-alc_sim3(AlcUse4_ageC_basepop, aTP_alc4_ageC, transition_alc4, 11) %>%  # starts with stimulated data
  count(AlcUse_pred) %>%                                                        # Count number in each category, then reorganizes it
  rename(predicted = n, AlcUse = AlcUse_pred) %>%
  mutate(pred_pct = round(predicted / sum(predicted) * 100,2)) 

# Observed at NESARC 3 (different population)
observed <- readRDS(paste0(data, "nesarc3_clean_expanded.rds")) %>% # starts with observed data
  filter (age < 90) %>% 
  count(alc4.factor) %>%                                            # Count number in each category, then reorganizes it
  rename(observed = n, AlcUse = alc4.factor) %>%
  mutate(obs_pct = round(observed / sum(observed) * 100,2))

comparison <- full_join (observed, predicted, by="AlcUse") %>% 
  mutate(diff_pct = round((pred_pct-obs_pct), 2))%>%
  select (AlcUse, obs_pct, pred_pct, diff_pct)

kable(comparison)




