
# SIMAH - NESARC Alcohol Transitions
# Data Analysis

library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(janitor)    # data management
library(msm)        # model transition probabilities
library(tableone)   # create descriptives table
library(irr)        # calculate kappa for true and predicted alcohol use
# options(scipen=999) # prevent the use of scientific notation
#memory.limit(size=1e+13)

# Specify the data and output file locations
data    <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Data/"
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Output/"
models  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Output/Models/"
source("0_Functions.R")

# Load data / functions
nesarc <- readRDS(paste0(data, "nesarc_clean.rds")) 
nesarc_expanded <- readRDS(paste0(data, "nesarc_clean_expanded.rds")) 
nesarc_all <- readRDS(paste0(data, "nesarc_all.rds")) # Contains those with missing data 

# Load Models (from sections 2.1 and 3.1)
alc5.msm_unadj <- readRDS(paste0(models, "alc5.msm_unadj.RDS"))
alc5.msm <- readRDS(paste0(models, "alc5.msm9.RDS"))
hed.msm_unadj <- readRDS(paste0(models, "hed.msm_unadj.RDS"))
hed.msm<- readRDS(paste0(models, "hed.msm.RDS"))


# 1) Descriptives ------------------------------------------------------------------------------------------

variables <- c("female", "age", "age3.factor", "race.factor", "edu3.factor", "alc5.factor", "hed.factor")
factor_vars <- c("female", "age3.factor", "race.factor", "edu3.factor", "alc5.factor", "hed.factor")

# Descriptives at basleine and follow-up of included participants 
tab1 <-CreateTableOne(vars= variables, factorVars = factor_vars, strata="wave.factor", data=nesarc)
table1 <- print(tab1, noSpaces = TRUE, catDigits = 0, contDigits = 1, pDigits = 2, printToggle = FALSE, test=FALSE)   
write.csv(table1, file=paste0(output,"Table 1 - Descriptives.csv"))  # export to excel, to copy/paste into manuscript
kableone(table1)

    # years follow-up
    nesarc %>%
      filter(wave==2) %>%
      select (years) %>% 
      skim()

    
# Descriptives for attrition 
nesarc1_all <- filter(nesarc_all, wave==1) # select baseline data
tab1_attr <-CreateTableOne(vars= variables, factorVars = factor_vars, strata="lost.factor", data=nesarc1_all)
table1_attr <- print(tab1_attr, noSpaces = TRUE, catDigits = 0, contDigits = 1, pDigits = 2, printToggle = FALSE, test=FALSE, smd=TRUE)   
write.csv(table1_attr, file=paste0(output,"Table S1 - Attrition Descriptives.csv"))  # export to excel, to copy/paste into manuscript
kableone(table1_attr)                             # view in R; R Markdown friendly version



# Descriptives of expanded data
tab_exp <-CreateTableOne(vars= variables, factorVars = factor_vars, strata="wave.factor", data=nesarc_expanded)
table_exp <- print(tab_exp, noSpaces = TRUE, catDigits = 0, contDigits = 1, pDigits = 2, printToggle = FALSE, test=FALSE, format="p")  # Shows only % 
write.csv(table_exp, file=paste0(output,"Table S2 - Descriptives of expanded data.csv")) 
kableone(table1_exp)



# 2) ALCOHOL CONSUMPTION  ---------------------------------------------------------------------------------------
#   2.1) Run MSM model ------------------------------------------------------------------------------------------

# Specify allowed transitions
# only allow adjacent transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q_alc5 <- rbind ( c(0,     0,    0.25,  0,    0),
                  c(0,     0,    0.25,  0,    0),
                  c(0,     0.25, 0,     0.25, 0),
                  c(0,     0,    0.25,  0,    0.25),
                  c(0,     0,    0,     0.25, 0))

# Specifies initial values
Q_alc5 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q_alc5)  


# Run MSM model (unadjusted)
alc5.msm_unadj <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q_alc5, center=FALSE,                             
                        control = list(trace=1, maxit=500, fnscale = 3000000))
                  saveRDS(alc5.msm_unadj, paste0(models, "alc5.msm_unadj.RDS")) # Save Results




# Run MSM model (adjusted for covariates)
alc5.msm <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                qmatrix = Q_alc5, center=FALSE,                            
                control = list(trace=1, maxit=500, fnscale = 3000000),
                covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
            saveRDS(alc5.msm, paste0(models, "alc5.msm.RDS")) # Save Results


# Preliminary view of model results 
alc5.msm                              # model summary
pmatrix.msm(alc5.msm, t=1, ci="norm") # Transition probabilities at year = t; covariates set to their mean value
hazard.msm(alc5.msm)                  # Hazard ratios for transition



#   2.2) Average annual TP (Table 2a)   ---------------------------------------------------------------------------

# Function to extract Annual Transition Probabilities (aTP) and correct CI to original sample size 

# Final, adjusted model
alc5_aTP <- predicted_TP(model=alc5.msm, year=1, original_n = nrow(nesarc), expanded_n = nrow(nesarc_expanded))
write_csv(alc5_aTP, paste0(output, "Table 2a - AlcUse Annual TP.csv")) # save results for paper
alc5_aTP


# Unadjusted model
alc5_aTP_unadj <- predicted_TP(model=alc5.msm_unadj, year=1, original_n = nrow(nesarc), expanded_n = nrow(nesarc_expanded))
write_csv(alc5_aTP_unadj, paste0(output, "Table S5a - AlcUse Unadjusted Annual TP.csv")) # save results for paper
alc5_aTP_unadj


#   2.3) Annual TP for each category (Supplement 1) ---------------------------------------------------------------

# First, specify the covariate values
age_cat <- unique(nesarc_expanded$age3.factor)
sex <- unique(nesarc_expanded$female_wave1.factor)
race <- unique(nesarc_expanded$race_wave1.factor)
edu <- unique(nesarc_expanded$edu3.factor)


# Function to extract annual TP 
aTP_alc5 <- predicted_TP_covs (alc5.msm, 1, age_cat, sex, race, edu) %>%
  mutate(From = recode(From,"State 1" = "Abstainer",  # Rename states
                            "State 2" = "Former",
                            "State 3" = "Category I",
                            "State 4" = "Category II",
                            "State 5" = "Category III"),
        To = recode(To, "State.1" = "Abstainer",
                        "State.2" = "Former",
                        "State.3" = "Category I",
                        "State.4" = "Category II",
                        "State.5" = "Category III"),
        Transition = paste(From, To, sep = "->"),
        Transition = fct_relevel(Transition, "Abstainer->Category I",     # re-arrange order of transition variable
                                "Former->Category I",	"Category I->Category II",	"Category II->Category III",
                                "Category III->Category II",	"Category II->Category I",	"Category I->Former"))
write_csv(aTP_alc5, paste0(output, "Supplement 1 - AlcUse Annual Transition Probabilities.csv")) # Save TP




#   2.4) Compare observed vs predicted (Table 3a) ------------------------------------------------------------------

# Load and format the transition probabilities
aTP_alc5 <- read_csv(paste0(output, "Supplement 1 - AlcUse Annual Transition Probabilities.csv")) %>% 
  mutate(cat = paste(sex, age_cat, edu, race, From, sep="_")) %>% 
  select(cat, To, Probability) %>% 
  group_by(cat) %>% 
  mutate(cumsum = cumsum(Probability)) %>%
  ungroup()


# Load and set up the initial population (based on NESARC wave 1)
AlcUse_basepop <- nesarc_expanded %>%
  select(idnum, wave, age, female_wave1.factor, race_wave1.factor, edu3.factor, alc5.factor) %>%  
  rename( sex = female_wave1.factor, 
    race = race_wave1.factor) %>%
  pivot_wider(names_from="wave", values_from=c("alc5.factor", "age", "edu3.factor")) %>%  
  mutate (AlcUse_pred = alc5.factor_1,
    AlcUse_1 = alc5.factor_1,
    AlcUse_2 = alc5.factor_2,
    age = age_1,
    edu = edu3.factor_1) %>%
  select(idnum, AlcUse_1, AlcUse_2, sex, age, edu, race, AlcUse_pred)


# Simulate population at 3 years follow-up
AlcUse_year3 <-alc_sim(3)


# Compare observed and predicted at the group
observed <- count(AlcUse_year3, AlcUse_2) %>% rename(observed = n, AlcUse = AlcUse_2) 
predicted <- count(AlcUse_year3, AlcUse_pred) %>% rename(predicted = n, AlcUse = AlcUse_pred)  
comparison_AlcUse <- full_join (observed, predicted, by="AlcUse") %>%
  mutate(difference = abs(predicted-observed),  
    diff_percent = difference/observed * 100) %>%
  adorn_totals("row") %>%  # Add row totals
  mutate(diff_percent = ifelse(AlcUse=="Total", difference/predicted * 100, diff_percent),
    predicted = ifelse(AlcUse=="Total", "", predicted), 
    diff_percent = round(diff_percent, 1))

kableone(comparison_AlcUse)
write_csv(comparison_AlcUse, paste0(output, "Table 3a - AlcUse Observed vs Predicted.csv")) # save results



#   2.5) Hazard ratios  (Table 4) -------------------------------------------------------------------------------
# Function to extract HR results, rearrange, and correct CI to original sample size 
HR_alc5 <- HR_table(alc5.msm, original_n = nrow(nesarc), expanded_n = nrow(nesarc_expanded))  %>%
  rename( "Abstainer->Category I"   = "State 1 - State 3",
          "Former->Category I"      = "State 2 - State 3",
          "Category I->Former"      = "State 3 - State 2",
          "Category I->Category II"  = "State 3 - State 4",
          "Category II->Category I"  = "State 4 - State 3",
          "Category II->Category III" = "State 4 - State 5",
          "Category III->Category II" = "State 5 - State 4") %>%
  select(Variable, "Abstainer->Category I", "Former->Category I", "Category I->Category II", "Category II->Category III", "Category III->Category II", "Category II->Category I", "Category I->Former")
write_csv(HR_alc5, paste0(output, "Table 4 - AlcUse Hazard Ratios.csv")) # save results for paper
kableone(HR_alc5)





#   2.6) Plot TP over multiple years (Figure S1) ----------------------------------------------------------------

# Simulate population over multiple years (over 10 years)
AlcUse_overtime <- rbind(alc_sim(1), alc_sim(2), alc_sim(3), alc_sim(4), alc_sim(5), alc_sim(6))


# Plot yearly TP for each age category
AlcUse_overtime %>%
  # Format data
  mutate(age_cat=recode(age_cat, "18-29" = "Ages 18-29 years", 
                                 "30-49" = "Ages 30-49 years", 
                                 "50+" = "Ages 50+ years"),
         AlcUse_1 = recode (AlcUse_1, "Abstainer" = "Initial state: Lifetime abstainer",
                                      "Former" = "Initial state: Former Drinker",
                                      "Category I" = "Initial state: Category I",
                                      "Category II" = "Initial state: Category II",
                                      "Category III" = "Initial state: Category III"),
         AlcUse_pred = recode (AlcUse_pred,"Abstainer" = "Lifetime abstainer",
                                           "Former" = "Former Drinker"),
         AlcUse_pred = fct_relevel(AlcUse_pred, "Lifetime abstainer", "Former Drinker",       # re-order the categories
                                            "Category I", "Category II", "Category III")) %>%
  group_by (year, AlcUse_1, age_cat, AlcUse_pred) %>%
    count() %>%  ungroup() %>%
  group_by(year, AlcUse_1, age_cat) %>%
    mutate(total = sum(n),
           pct_total = n / total * 100) %>%
  ungroup() %>%
  # plot data
  ggplot(aes(x=year, y=pct_total, group=AlcUse_pred)) + 
  geom_line(aes(color=AlcUse_pred), size=1) +
  # geom_ribbon(aes(ymin=newLower, ymax=newUpper, fill=To), alpha=0.2) + 
  facet_grid(age_cat~AlcUse_1) +
  labs(x = "Years follow-up", y="Proportion (%)", color="State at Follow-up:") +
  theme(legend.position = "top",
    panel.grid.major=element_line(color="grey90"), 
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(linetype = "solid", fill = NA)) + 
  scale_y_continuous(breaks=seq(0, 100, by= 20)) + 
  scale_x_continuous(breaks=seq(0, 6, by= 1))
ggsave(paste0(output, "Figure S1 - AlcUse over time.tiff"), dpi=600, width=12, height = 7)




#   2.7) Transition frequencies (Tables S1a, S2a) --------------------------------------------------------------

statetable.msm(alc5, idnum, data=nesarc)          # Table S3a
statetable.msm(alc5, idnum, data=nesarc_expanded) # Table S4a


# 3) HEAVY EPISODIC DRINKING  ----------------------------------------------------------------------------------
#   3.1) Run MSM model ------------------------------------------------------------------------------------------

# Specify allowed transitions
# only allow adjacent transitions
Q_hed <- rbind (c(0,     0.25,    0,     0,    0),
                c(0.25,  0,       0.25,  0,    0),
                c(0,     0.25,    0,     0.25, 0),
                c(0,     0,       0.25,  0,    0.25),
                c(0,     0,       0,     0.25, 0))

# Specify initial values 
Q_hed <- crudeinits.msm(hed ~ years, idnum, data=nesarc_expanded, qmatrix=Q_hed)



# Run MSM model (unadjusted)
hed.msm_unadj <- msm (hed ~ years, subject=idnum, data = nesarc_expanded, 
                      qmatrix = Q_hed, center=FALSE,
                      control = list(trace=1, maxit=500, fnscale = 3000000))
      saveRDS(hed.msm_unadj, paste0(models, "hed.msm_unadj.RDS"))




# Run MSM model (adjusted for covariates)
hed.msm <- msm (hed ~ years, subject=idnum, data = nesarc_expanded, 
                qmatrix = Q_hed, center=FALSE,
                control = list(trace=1, maxit=500, fnscale = 3000000),
                covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
      saveRDS(hed.msm, paste0(models, "hed.msm.RDS"))



# View mode results
hed.msm                              # model summary
pmatrix.msm(hed.msm, t=1, ci="norm") # Transition probabilities at year = t; covariates set to their mean value
hazard.msm(hed.msm)                  # Hazard ratios for transition


#   3.2) Average annual TP (Table 2b)   ---------------------------------------------------------------------------

# Function to extract Annual Transition Probabilities (aTP) and correct CI to original sample size 

# Final, adjusted model
hed_aTP <- predicted_TP(model=hed.msm, year=1, original_n = nrow(nesarc), expanded_n = nrow(nesarc_expanded))
write_csv(hed_aTP, paste0(output, "Table 2b - HED Annual TP.csv")) 
hed_aTP


# Unadjusted model
hed_aTP_unadj <- predicted_TP(model=hed.msm, year=1, original_n = nrow(nesarc), expanded_n = nrow(nesarc_expanded))
write_csv(hed_aTP_unadj, paste0(output, "Table S5b - HED Unadjusted Annual TP.csv")) 
hed_aTP_unadj



#   3.3) Annual TP for each category (Supplement 2) ---------------------------------------------------------------

# First, specify the covariate values
age_cat <- unique(nesarc_expanded$age3.factor)
sex <- unique(nesarc_expanded$female_wave1.factor)
race <- unique(nesarc_expanded$race_wave1.factor)
edu <- unique(nesarc_expanded$edu3.factor)


# Function to extract annual TP 
aTP_hed <- predicted_TP_covs (hed.msm, 1, age_cat, sex, race, edu) %>%
  mutate(From = recode(From,"State 1" = "Non-drinker",  # Rename states
                            "State 2" = "Drinker, no HED",
                            "State 3" = "Occasional HED",
                            "State 4" = "Monthly HED",
                            "State 5" = "Weekly HED"),
          To = recode(To, "State.1" = "Non-drinker",
                          "State.2" = "Drinker, no HED",
                          "State.3" = "Occasional HED",
                          "State.4" = "Monthly HED",
                          "State.5" = "Weekly HED"),
    Transition = paste(From, To, sep = "->"),
    Transition = fct_relevel(Transition, "Non-drinker->Drinker, no HED",    # re-arrange order of transition variable
                            "Drinker, no HED->Occasional HED", "Occasional HED->Monthly HED", "Monthly HED->Weekly HED",
                            "Weekly HED->Monthly HED", "Monthly HED->Occasional HED", "Occasional HED->Drinker, no HED", 
                            "Drinker, no HED->Non-drinker"))
write_csv(aTP_hed, paste0(output, "Supplement 2 - HED Annual Transition Probabilities.csv")) # Save TP


#   3.4) Compare observed vs predicted (Table 3b) ------------------------------------------------------------------

# Load and format the transition probabilities
aTP_hed <- read_csv(paste0(output, "Supplement 2 - HED Annual Transition Probabilities.csv")) %>%
  mutate(cat = paste(sex, age_cat, edu, race, From, sep="_")) %>% 
  select(cat, To, Probability) %>% 
  group_by(cat) %>% 
  mutate(cumsum = cumsum(Probability)) %>%
  ungroup()



# Load and set up the initial population (based on NESARC wave 1)
hed_basepop <- nesarc_expanded %>%
  select(idnum, wave, age, female_wave1.factor, race_wave1.factor, edu3.factor, hed.factor) %>%
  rename(sex = female_wave1.factor, 
         race = race_wave1.factor) %>%
  pivot_wider(names_from="wave", values_from=c("hed.factor", "age", "edu3.factor")) %>%
  mutate (hed_pred = hed.factor_1,
          hed_1 = hed.factor_1,
          hed_2 = hed.factor_2,
          age = age_1,
          edu = edu3.factor_1) %>%
  select(idnum, hed_1, hed_2, sex, age, edu, race, hed_pred)
  
 
# Simulate population at 3 years follow-up
hed_year3 <-hed_sim(3)


# Compare observed and predicted proportions
observed <- count(hed_year3, hed_2) %>% rename(observed = n, hed = hed_2) 
predicted <- count(hed_year3, hed_pred) %>% rename(predicted = n, hed = hed_pred)  
comparison_hed <- full_join (observed, predicted, by="hed") %>%
    mutate(difference = abs(predicted-observed),  
           diff_percent = difference/observed * 100) %>%
    adorn_totals("row") %>%  # Add row totals
    mutate(diff_percent = ifelse(hed=="Total", difference/predicted * 100, diff_percent),
           predicted = ifelse(hed=="Total", NA, predicted), 
           diff_percent = round(diff_percent, 1))

kableone(comparison_hed)
write_csv(comparison_hed, paste0(output, "Table 3b - HED Observed vs Predicted.csv")) # save results for paper



#   3.5) Hazard ratios  (Table 5) -------------------------------------------------------------------------------

# Function to extract HR results, rearrange, and correct CI to original sample size 
HR_hed <- HR_table(hed.msm, original_n = nrow(nesarc), expanded_n = nrow(nesarc_expanded))  %>%
  rename( "Non-drinker->Drinker, no HED"    = "State 1 - State 2",
          "Drinker, no HED->Non-drinker"    = "State 2 - State 1",
          "Drinker, no HED->Occasional HED" = "State 2 - State 3",
          "Occasional HED->Drinker, no HED" = "State 3 - State 2",
          "Occasional HED->Monthly HED"     = "State 3 - State 4",
          "Monthly HED->Occasional HED"     = "State 4 - State 3",
          "Monthly HED->Weekly HED"         = "State 4 - State 5",
          "Weekly HED->Monthly HED"         = "State 5 - State 4") %>%
  select(Variable,  "Non-drinker->Drinker, no HED", "Drinker, no HED->Occasional HED", "Occasional HED->Monthly HED", "Monthly HED->Weekly HED",
                    "Weekly HED->Monthly HED", "Monthly HED->Occasional HED", "Occasional HED->Drinker, no HED", "Drinker, no HED->Non-drinker")
write_csv(HR_hed, paste0(output, "Table 5 - HED Hazard Ratios.csv")) # save results for paper
HR_hed
  
  

#   3.6) Plot TP over multiple years (Figure S2) ----------------------------------------------------------------

# Simulate population over multiple years
hed_overtime <- rbind(hed_sim(1), hed_sim(2), hed_sim(3), hed_sim(4), hed_sim(5), hed_sim(6))



# Plot yearly TP for each age category
hed_overtime %>%
  # Format data
  mutate(age_cat=recode(age_cat, "18-29" = "Ages 18-29 years", 
                                 "30-49" = "Ages 30-49 years", 
                                 "50+" = "Ages 50+ years"),
          hed_1 = recode (hed_1,  "Non-drinker" = "Initial state: Non-drinker",
                                  "Drinker, no HED" = "Initial state: Drinker, no HED",
                                  "Occasional HED" = "Initial state: Occasional HED",
                                  "Monthly HED" = "Initial state: Monthly HED",
                                  "Weekly HED" = "Initial state: Weekly HED"),
          hed_pred = fct_relevel(hed_pred, "Non-drinker", "Drinker, no HED", "Occasional HED",
                                  "Monthly HED", "Weekly HED")) %>%
  group_by (year, hed_1, age_cat, hed_pred) %>%
  count() %>%  ungroup() %>%
  group_by(year, hed_1, age_cat) %>%
  mutate(total = sum(n),
    pct_total = n / total * 100) %>%
  ungroup() %>% 
  # plot data
  ggplot(aes(x=year, y=pct_total, group=hed_pred)) + 
  geom_line(aes(color=hed_pred), size=1) +
  facet_grid(age_cat~hed_1) +
  labs(x = "Years follow-up", y="Proportion (%)", color="State at Follow-up:") +
  theme(legend.position = "top",
        panel.grid.major=element_line(color="grey90"), 
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(linetype = "solid", fill = NA)) + 
  scale_y_continuous(breaks=seq(0, 100, by= 20)) + 
  scale_x_continuous(breaks=seq(0, 6, by= 1))
ggsave(paste0(output, "Figure S2 - HED over time.tiff"), dpi=600, width=12, height = 7)




#   3.7) Transition frequencies (Tables S1b, S2b) --------------------------------------------------------------

statetable.msm(hed, idnum, data=nesarc)          # Table S3b
statetable.msm(hed, idnum, data=nesarc_expanded) # Table S4b




# 4) SENSITIVITYT ANALYSES -------------------------------------------------------------------------------------
#   4.1) AlcUse Alternative MSM Model --------------------------------------------------------------------------

# Additionally allow for transitions from any drinking state to former drinking
Q2_alc5 <- rbind (c(0,     0,    0.25,  0,    0),
                  c(0,     0,    0.25,  0,    0),
                  c(0,     0.25, 0,     0.25, 0),
                  c(0,     0.25, 0.25,  0,    0.25),
                  c(0,     0.25, 0,     0.25, 0))

# Specifies initial values; change Q value here, if needed
Q2_alc5 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q2_alc5) 


# Run MSM model
alc5.msm2 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                  qmatrix = Q2_alc5, center=FALSE,
                  control = list(trace=1, maxit=2000, fnscale = 3000000, reltol = 1e-16, ndeps = rep(1e-6, 4)),
                  covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
saveRDS(alc5.msm2, paste0(models, "alc5.msm2.RDS")) # Save Results


# Compare models
AIC(alc5.msm, alc5.msm2)





#   4.2) HED Alternative MSM Model --------------------------------------------------------------------------

# Additionally allow for transitions from any drinking state to non-drinker
Q2_hed <- rbind ( c(0,     0.25,    0,     0,    0),
                  c(0.25,  0,       0.25,  0,    0),
                  c(0.25,  0.25,    0,     0.25, 0),
                  c(0.25,  0,       0.25,  0,    0.25),
                  c(0.25,  0,       0,     0.25, 0))


# Specifies initial values; change Q value here, if needed
Q2_hed <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q2_hed) 


# Run MSM model
hed.msm2 <- msm (hed ~ years, subject=idnum, data = nesarc_expanded, 
                  qmatrix = Q2_hed, center=FALSE,
                  control = list(trace=1, maxit=2000, fnscale = 3000000, reltol = 1e-16, ndeps = rep(1e-6, 4)),
                  covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
saveRDS(hed.msm2, paste0(models, "hed.msm2.RDS")) # Save Results


# Compare models
AIC(hed.msm, hed.msm2)
