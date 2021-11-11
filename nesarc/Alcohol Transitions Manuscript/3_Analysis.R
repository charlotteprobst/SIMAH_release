
# SIMAH - NESARC Alcohol Transitions
# Data Analysis

library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(janitor)    # data management
library(msm)        # model transition probabilities
library(tableone)   # create descriptives table
library(irr)        # calculate kappa for true and predicted alcohol use
# options(scipen=999) # prevent the use of scientific notation


# Specify the data and output file locations
data    <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Data/"
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Output/"
models  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Output/Models/"
source("0_Functions.R")


# Load data / functions
nesarc <- readRDS(paste0(data, "nesarc_clean.rds")) 
nesarc_expanded <- readRDS(paste0(data, "nesarc_clean_expanded.rds")) 

    # for HED analyses, remove  those with missing HED data
    nesarc_hed          <- nesarc %>% group_by(idnum) %>% filter(!any(is.na(hed))) %>% ungroup()
    nesarc_expanded_hed <- nesarc_expanded %>% group_by(idnum) %>% filter(!any(is.na(hed)))%>% ungroup()



# 1) Descriptives ------------------------------------------------------------------------------------------

tab1 <-CreateTableOne(vars= c("female", "age", "age3.factor", "race.factor", "edu3.factor", "alc5.factor", "hed.factor"), 
                      factorVars = c("female", "age3.factor", "race.factor", "edu3.factor", "alc5.factor", "hed.factor"), 
                      strata="wave", data=nesarc)
summary(tab1)
table1 <- print(tab1, noSpaces = TRUE, catDigits = 0, contDigits = 1, pDigits = 2, printToggle = FALSE)   
write.csv(table1, file=paste0(output,"Table1-Descriptives.csv"))  # export to excel, to copy/paste into manuscript
kableone(table1)                             # view in R; R Markdown friendly version


# years follow-up
nesarc %>%
  filter(wave==2) %>%
  select (years) %>% 
  skim()



# 2) ALCOHOL CONSUMPTION  ------------------------------------------------------------------------------------------------
## 2.1.1) Run AlcUse MSM Model (9 models; least to most restrained, sequentally) ------------------------------------------------------------------------------------------------
# Count of transitions 
statetable.msm(alc5, idnum, data=nesarc)
statetable.msm(alc5, idnum, data=nesarc_expanded)

      # For Jurgen:
      nesarc_under45 <- filter(nesarc, age_wave1 < 45)
      nesarc_over45 <- filter(nesarc, age_wave1 >= 45)
      
      statetable.msm(alc5, idnum, data=nesarc_under45)
      statetable.msm(alc5, idnum, data=nesarc_over45)


# Run different models using different transition intensity matrixes (Q) - i.e., what (instanteneous) transitions are allowed (specified by the non-zero entries)
# Start with the full model and gradually restricted the allowed transitions beginning with the most remote transitions;

# Model 1: allow all transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q1<- rbind ( c(0,     0,    0.25,  0.25, 0.25),
             c(0,     0,    0.25,  0.25, 0.25),
             c(0,     0.25, 0,     0.25, 0.25),
             c(0,     0.25, 0.25,  0,    0.25),
             c(0,     0.25, 0.25,  0.25, 0))

    # Run MSM model
    Q1 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q1)   # Specifies initial values; change Q value here, if needed
    alc5.msm1 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                       qmatrix = Q1, center=FALSE,                                # Change Q value here, if needed
                       control = list(trace=1, maxit=1000, fnscale = 3100000),    # the value of 3100000 was obtained from running the same model earlier (not shown) but also including the statement: fixedpars=TRUE 
                       covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
    saveRDS(alc5.msm1, paste0(models, "alc5.msm1.RDS")) # Save Results


    
# Model 2: remove Abstinence --> Category III
Q2<- rbind (c(0,     0,    0.25,  0.25, 0),
            c(0,     0,    0.25,  0.25, 0.25),
            c(0,     0.25, 0,     0.25, 0.25),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0.25, 0.25,  0.25, 0))
    
      # Run MSM model
      Q2 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q2)   # Specifies initial values; change Q value here, if needed
      alc5.msm2 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q2, center=FALSE,                                # Change Q value here, if needed
                        control = list(trace=1, maxit=1000, fnscale = 3000000),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
      saveRDS(alc5.msm2, paste0(models, "alc5.msm2.RDS")) # Save Results
      
      
      
      
      
# Model 3: remove Former --> Category III
Q3<- rbind (c(0,     0,    0.25,  0.25, 0),
            c(0,     0,    0.25,  0.25, 0),
            c(0,     0.25, 0,     0.25, 0.25),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0.25, 0.25,  0.25, 0))
    
      # Run MSM model
      Q3 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q3)   # Specifies initial values; change Q value here, if needed
      alc5.msm3 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q3, center=FALSE,                                # Change Q value here, if needed
                        control = list(trace=1, maxit=1000, fnscale = 3000000),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
      saveRDS(alc5.msm3, paste0(models, "alc5.msm3.RDS")) # Save Results

    

      
          
# Model 4: remove Category III --> Former
Q4<- rbind (c(0,     0,    0.25,  0.25, 0),
            c(0,     0,    0.25,  0.25, 0),
            c(0,     0.25, 0,     0.25, 0.25),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0,    0.25,  0.25, 0))
  
      # Run MSM model
      Q4 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q4)   # Specifies initial values; change Q value here, if needed
      alc5.msm4 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q4, center=FALSE,                                # Change Q value here, if needed
                        control = list(trace=1, maxit=1000, fnscale = 3000000),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
      saveRDS(alc5.msm4, paste0(models, "alc5.msm4.RDS")) # Save Results



      
      
      
# Model 5: remove Abstainer --> Category II
Q5<- rbind (c(0,     0,    0.25,  0,    0),
            c(0,     0,    0.25,  0.25, 0),
            c(0,     0.25, 0,     0.25, 0.25),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0,    0.25,  0.25, 0))

      # Run MSM model
      Q5 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q5)   # Specifies initial values; change Q value here, if needed
      alc5.msm5 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q5, center=FALSE,                                # Change Q value here, if needed
                        control = list(trace=1, maxit=1000, fnscale = 3000000),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
      saveRDS(alc5.msm5, paste0(models, "alc5.msm5.RDS")) # Save Results


      
      
    
# Model 6: remove Former --> Category II
Q6<- rbind (c(0,     0,    0.25,  0,    0),
            c(0,     0,    0.25,  0,    0),
            c(0,     0.25, 0,     0.25, 0.25),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0,    0.25,  0.25, 0))

      # Run MSM model
      Q6 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q6)   # Specifies initial values; change Q value here, if needed
      alc5.msm6 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q6, center=FALSE,                                # Change Q value here, if needed
                        control = list(trace=1, maxit=1000, fnscale = 3000000),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
      saveRDS(alc5.msm6, paste0(models, "alc5.msm6.RDS")) # Save Results


      
      

# Model 7: remove Category I --> III
Q7<- rbind (c(0,     0,    0.25,  0,    0),
            c(0,     0,    0.25,  0,    0),
            c(0,     0.25, 0,     0.25, 0),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0,    0.25,  0.25, 0))

      # Run MSM model
      Q7 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q7)   # Specifies initial values; change Q value here, if needed
      alc5.msm7 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q7, center=FALSE,                                # Change Q value here, if needed
                        control = list(trace=1, maxit=1000, fnscale = 3000000),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
      saveRDS(alc5.msm7, paste0(models, "alc5.msm7.RDS")) # Save Results


      
      

# Model 8: remove Category III --> I
Q8<- rbind (c(0,     0,    0.25,  0,    0),
            c(0,     0,    0.25,  0,    0),
            c(0,     0.25, 0,     0.25, 0),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0,    0,  0.25, 0))


      # Run MSM model
      Q8 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q8)   # Specifies initial values; change Q value here, if needed
      alc5.msm8 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q8, center=FALSE,                                # Change Q value here, if needed
                        control = list(trace=1, maxit=1000, fnscale = 3000000),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
      saveRDS(alc5.msm8, paste0(models, "alc5.msm8.RDS")) # Save Results


      
      

# Model 9: remove Category II --> Former
# Only allows transitions to an adjacent state, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q9 <- rbind ( c(0,     0,    0.25,  0,    0),
              c(0,     0,    0.25,  0,    0),
              c(0,     0.25, 0,     0.25, 0),
              c(0,     0,    0.25,  0,    0.25),
              c(0,     0,    0,     0.25, 0))
    

    # Run MSM model
    Q9 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q9)  # Specifies initial values; Change Q value here, if needed
    alc5.msm9 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                        qmatrix = Q9, center=FALSE,                             # Change Q value here, if needed
                        control = list(trace=1, maxit=500, fnscale = 3000000),
                        covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
    saveRDS(alc5.msm9, paste0(models, "alc5.msm9.RDS")) # Save Results
    

    
# Load the models
memory.limit(size=1e+13)
alc5.msm1 <- readRDS(paste0(models, "alc5.msm1.RDS")) # does not run
alc5.msm2 <- readRDS(paste0(models, "alc5.msm2.RDS")) # does not run
alc5.msm3 <- readRDS(paste0(models, "alc5.msm3.RDS")) # does not run
alc5.msm4 <- readRDS(paste0(models, "alc5.msm4.RDS")) # does not run
alc5.msm5 <- readRDS(paste0(models, "alc5.msm5.RDS")) # does not run
alc5.msm6 <- readRDS(paste0(models, "alc5.msm6.RDS")) # runs
alc5.msm7 <- readRDS(paste0(models, "alc5.msm7.RDS")) # runs
alc5.msm8 <- readRDS(paste0(models, "alc5.msm8.RDS")) # runs
alc5.msm9 <- readRDS(paste0(models, "alc5.msm9.RDS")) # runs





# Compare models
AIC(alc5.msm1, alc5.msm2, alc5.msm3, alc5.msm4, alc5.msm5, alc5.msm6, alc5.msm7, alc5.msm8, alc5.msm9)
    

    
## 2.1.2) Run AlcUse MSM Model (3 models; therory based) ------------------------------------------------------------------------------------------------
# Count of transitions 
statetable.msm(alc5, idnum, data=nesarc)
statetable.msm(alc5, idnum, data=nesarc_expanded)

# Run different models using different transition intensity matrixes (Q) - i.e., what (instanteneous) transitions are allowed (specified by the non-zero entries)
# Start with the full model (no restrictions), then remove non-adjacent worsening transitions (allow non-adjacent recovery transitions), then only allow adjacent transitions


# Model 1: allow all transitions, except for transitions back to lifetime abstainers, and abstainer->former drinker
Q1<- rbind ( c(0,     0,    0.25,  0.25, 0.25),
  c(0,     0,    0.25,  0.25, 0.25),
  c(0,     0.25, 0,     0.25, 0.25),
  c(0,     0.25, 0.25,  0,    0.25),
  c(0,     0.25, 0.25,  0.25, 0))

# Run MSM model
Q1 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q1)   # Specifies initial values; change Q value here, if needed
alc5.msm1_v2 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                      qmatrix = Q1, center=FALSE,                                # Change Q value here, if needed
                      control = list(trace=1, maxit=1000, fnscale = 3100000),    # the value of 3100000 was obtained from running the same model earlier (not shown) but also including the statement: fixedpars=TRUE 
                      covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
saveRDS(alc5.msm1_v2, paste0(models, "alc5.msm1_v2.RDS")) # Save Results



# Model 2: remove non-adjacent 'worsening' transitions
Q2<- rbind (c(0,     0,    0.25,  0,    0),
            c(0,     0,    0.25,  0,    0),
            c(0,     0.25, 0,     0.25, 0),
            c(0,     0.25, 0.25,  0,    0.25),
            c(0,     0.25, 0.25,  0.25, 0))

# Run MSM model
Q2 <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q2)   # Specifies initial values; change Q value here, if needed
alc5.msm2_v2 <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                      qmatrix = Q2, center=FALSE,                                # Change Q value here, if needed
                      control = list(trace=1, maxit=1000, fnscale = 3000000, reltol = 1e-16),
                      covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)
saveRDS(alc5.msm2_v2, paste0(models, "alc5.msm2_v2.RDS")) # Save Results





# Model 3: remove all non-adjacent transitions; its the same as the model above

# Load Models:
alc5.msm1_v2 <- readRDS(paste0(models, "alc5.msm1_v2.RDS"))
alc5.msm2_v2 <- readRDS(paste0(models, "alc5.msm2_v2.RDS"))
alc5.msm3_v2 <- readRDS(paste0(models, "alc5.msm9.RDS"))

alc5.msm1_v2
alc5.msm2_v2
alc5.msm3_v2

# Compare models
AIC(alc5.msm1_v2, alc5.msm2_v2, alc5.msm3_v2)



## 2.2) Load Model and View Results -----------------------------------------------------------------------------------------------
alc5.msm <- readRDS(paste0(models, "alc5.msm9.RDS"))
alc5.msm

      # Transition probabilities at year = t; covariates set to their mean value
      pmatrix.msm(alc5.msm, t=1, ci="norm") # CI based on drawing random samples, default size=1000
      
      # Hazard ratios for transition
      hazard.msm(alc5.msm)


# Table 2a - Function to extract Annual Transition Probabilities (aTP) and correct CI to original sample size 
alc5_annual_TP <- predicted_TP(model=alc5.msm, year=1, original_n = 68330, expanded_n = 4069764)
alc5_annual_TP
write_csv(alc5_annual_TP, paste0(output, "Table2a-Annual TP.csv")) # save results for paper




# Table 3 - Function to extract HR results, rearrange, and correct CI to original sample size 
HR_alc5 <- HR_table(alc5.msm, original_n = 68330, expanded_n = 4069764)  %>%
  rename( "Abstainer->LowRisk"   = "State 1 - State 3",
          "Former->LowRisk"      = "State 2 - State 3",
          "LowRisk->Former"      = "State 3 - State 2",
          "LowRisk->MediumRisk"  = "State 3 - State 4",
          "MediumRisk->LowRisk"  = "State 4 - State 3",
          "MediumRisk->HighRisk" = "State 4 - State 5",
          "HighRisk->MediumRisk" = "State 5 - State 4") %>%
  select(Variable, "Abstainer->LowRisk", "Former->LowRisk", "LowRisk->MediumRisk", "MediumRisk->HighRisk", "HighRisk->MediumRisk", "MediumRisk->LowRisk", "LowRisk->Former")

HR_alc5
write_csv(HR_alc5, paste0(output, "Table4-HR alc5.csv")) # save results for paper
  
  
## 2.3) Extract Transition Probabilities (TP)  --------------------------------------------------------------------------------------------------

alc5.msm <- readRDS(paste0(models, "alc5.msm9.RDS"))

# First, specify the covariate values
age_cat <- unique(nesarc_expanded$age3.factor)
sex <- unique(nesarc_expanded$female_wave1.factor)
race <- unique(nesarc_expanded$race_wave1.factor)
edu <- unique(nesarc_expanded$edu3.factor)



# Function to extract annual TP (to provide as a supplement)
alc5_annual_TP_covs <- predicted_TP_covs (alc5.msm, 1, age_cat, sex, race, edu) %>%
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

# Save TP
write_csv(alc5_annual_TP_covs, paste0(output, "Supplement 1- Annual Transition Probabilities - AlcUse"))


# Function to extract TP after 3 years (for internal validation)
alc5_TP_3yrs <- predicted_TP_covs (alc5.msm, 3, age_cat, sex, race, edu) %>%
  mutate(From = recode(From, "State 1" = "Abstainer",  # Rename states
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

# Save TP
write_csv(alc5_TP_3yrs, paste0(output, "Transition Probabilities - AlcUse after 3 years.csv"))





# Function to extract TP over 10 years (to plot)
alc5_yearly_TP <- predicted_TP_overtime (alc5.msm, 10, 68330, 4069764) %>%
  mutate(
    From = recode(From, "State 1" = "Initial State: Abstainer",
                        "State 2" = "Initial State: Former",
                        "State 3" = "Initial State: Category I",
                        "State 4" = "Initial State: Category II",
                        "State 5" = "Initial State: Category III"),
    To = recode(To, "State.1" = "Abstainer",
                    "State.2" = "Former",
                    "State.3" = "Category I",
                    "State.4" = "Category II",
                    "State.5" = "Category III"),
    # Re-arrange order
    From = fct_relevel(From, "Initial State: Abstainer",  "Initial State: Former",	"Initial State: Category I",	"Initial State: Category II", "Initial State: Category III"),
    To = fct_relevel(To, "Abstainer",  "Former",	"Category I",	"Category II", "Category III"))


        # Plot TP Over time
        alc5_yearly_TP %>%
          ggplot(aes(x=Year, y=Estimate, group=To)) + geom_line(aes(color=To), size=0.5) + 
          geom_ribbon(aes(ymin=newLower, ymax=newUpper, fill=To), alpha=0.2) + 
          facet_wrap(~From) +
          theme_bw() + labs(x = "Years Follow-up", y="Transition probability (%)", color="Transition To", fill="Transition To") +
          theme(legend.position = c(.85, 0.25)) +
          scale_y_continuous(breaks=seq(0, 100, by= 10)) + 
          scale_x_continuous(breaks=seq(0, 10, by= 2))
        ggsave(paste0(output, "AlcUse TP over time 1.tiff"), dpi=600, width=7.5, height = 5)



# Function to extract TP over 10 years for each age category

alc5_yearly_TP2 <- predicted_TP_overtime_age (alc5.msm, 10, age_cat, 68330, 4069764) %>%
  mutate(
        From = recode(From, "State 1" = "Initial State: Abstainer",
                            "State 2" = "Initial State: Former",
                            "State 3" = "Initial State: Category I",
                            "State 4" = "Initial State: Category II",
                            "State 5" = "Initial State: Category III"),
        To = recode(To, "State.1" = "Abstainer",
                        "State.2" = "Former",
                        "State.3" = "Category I",
                        "State.4" = "Category II",
                        "State.5" = "Category III"),
        # Re-arrange order
        From = fct_relevel(From, "Initial State: Abstainer",  "Initial State: Former",	"Initial State: Category I",	"Initial State: Category II", "Initial State: Category III"),
        To = fct_relevel(To, "Abstainer",  "Former",	"Category I",	"Category II", "Category III"))


      
        # Plot yearly TP for each age category
        alc5_yearly_TP2 %>%
          mutate(age_cat=recode(age_cat, "18-29" = "Ages 18-29 years", "30-49" = "Ages 30-49 years", "50+" = "Ages 50+ years")) %>%
          ggplot(aes(x=Year, y=Estimate, group=To)) + geom_line(aes(color=To), size=0.5) + 
          geom_ribbon(aes(ymin=newLower, ymax=newUpper, fill=To), alpha=0.2) + 
          facet_grid(age_cat~From) +
          theme_bw() + labs(x = "Years Follow-up", y="Transition probability (%)", color="Transition To", fill="Transition To") +
          theme(legend.position = "top") +
          scale_y_continuous(breaks=seq(0, 100, by= 20)) + 
          scale_x_continuous(breaks=seq(0, 10, by= 1))
        ggsave(paste0(output, "AlcUse TP over time 2.tiff"), dpi=600, width=12, height = 7)
        




## 2.4) Check Model Fit--------------------------------------------------------------------------------------

# Load model
alc5.msm <- readRDS(paste0(models, "alc5.msm9.RDS"))


# Load Transition probabilities
probabilities_alc5 <- read_csv(paste0(output, "Transition Probabilities - AlcUse after 3 years.csv")) %>% 
    mutate(cat = paste(sex, age_cat, edu, race, From, sep="_")) %>% 
    select(cat, To, Probability) %>% 
    group_by(cat) %>% 
    mutate(cumsum = cumsum(Probability)) %>%
    ungroup()


# Function to apply alcohol consumption transition probabilities
transition_alc5 <- function(data, transitions){
  selected <- unique(data$cat)
  rates <- transitions %>% filter(cat == selected)
  data$AlcUse_2_pred <- ifelse(data$prob<=rates$cumsum[1], "Abstainer",
                          ifelse(data$prob<=rates$cumsum[2] & data$prob>rates$cumsum[1], "Former",
                            ifelse(data$prob<=rates$cumsum[3] & data$prob>rates$cumsum[2],"Category I",
                              ifelse(data$prob<=rates$cumsum[4] & data$prob>rates$cumsum[3],"Category II",
                                ifelse(data$prob<=rates$cumsum[5] & data$prob>rates$cumsum[4],"Category III",NA)))))
  
  return(data)
}



# Get predicted alcohol use at wave 3
alc5_data <- nesarc %>%
  # Rename variables
  select(idnum, wave, years, age3.factor, female_wave1.factor, race_wave1.factor, edu3.factor, alc5.factor) %>%
  rename(age_cat = age3.factor,
         sex = female_wave1.factor, 
         race = race_wave1.factor,
         edu = edu3.factor,
         AlcUse = alc5.factor) %>%
  
  # Tranform to wide format
  pivot_wider(names_from="wave", values_from=c("AlcUse", "years", "age_cat", "edu")) %>%
  
  # Create variable combing level of each covariate and the state at baseline
  mutate(cat = paste(sex, age_cat_1, edu_1, race, AlcUse_1, sep="_"),
         prob = runif(nrow(.))) %>% # generate random prob
  
  # Apply transition function
  group_by(cat) %>%
    do(transition_alc5(., probabilities_alc5)) %>% # use 'do( )' to run the function defined earlier
  ungroup() %>% 
  mutate(AlcUse_2_pred = factor(AlcUse_2_pred, levels=c("Abstainer", "Former", "Category I", "Category II", "Category III"))) %>%
  select(-cat, - prob, -years_1)
  


# Compare observed and predicted at the group
observed <- count(alc5_data, AlcUse_2) %>% rename(observed = n, AlcUse = AlcUse_2) 
predicted <- count(alc5_data, AlcUse_2_pred) %>% rename(predicted = n, AlcUse = AlcUse_2_pred)  
comparison_AlcUse <- full_join (observed, predicted, by="AlcUse") %>%
  mutate(difference = abs(predicted-observed),  
    diff_percent = difference/observed * 100) %>%
  adorn_totals("row") %>%  # Add row totals
  mutate(diff_percent = ifelse(AlcUse=="Total", difference/predicted * 100, diff_percent),
         predicted = ifelse(AlcUse=="Total", NA, predicted), 
         diff_percent = round(diff_percent, 2))

kableone(comparison_AlcUse)


# 3) HEAVY EPISODIC DRINKING  ------------------------------------------------------------------------------------------------
## 3.1) Run HED MSM Model ------------------------------------------------------------------------------------------------

# Count of transitions 
statetable.msm(hed, idnum, data=nesarc_hed)
statetable.msm(hed, idnum, data=nesarc_expanded_hed)


# Specify transition intensity matrix (Q) - i.e., what (instanteneous) transitions are allowed (specified by the non-zero entries)
# Will only allow transitions to an adjacent state 

Q <- rbind ( c(0,     0.25,    0,     0,    0),
             c(0.25,  0,       0.25,  0,    0),
             c(0,     0.25,    0,     0.25, 0),
             c(0,     0,       0.25,  0,    0.25),
             c(0,     0,       0,     0.25, 0))

# Specify initial values 
Q <- crudeinits.msm(hed ~ years, idnum, data=nesarc_expanded_hed, qmatrix=Q)




# Run MSM model
hed.msm <- msm (hed ~ years, subject=idnum, data = nesarc_expanded_hed, 
  qmatrix = Q, center=FALSE,
  control = list(trace=1, maxit=500, fnscale = 3000000),
  covariates = ~ female_wave1.factor + age3.factor + edu3.factor + race_wave1.factor)

# Save Results
saveRDS(hed.msm, paste0(models, "hed.msm.RDS"))




## 3.2) Load Model and View Results -----------------------------------------------------------------------------------------------
hed.msm<- readRDS(paste0(models, "hed.msm.RDS"))
hed.msm

# Transition probabilities at year = t; covariates set to their mean value
pmatrix.msm(hed.msm, t=1, ci="norm") # CI based on drawing random samples, default size=1000

# Hazard ratios for transition
hazard.msm(hed.msm)


# Table 2b - Extract Annual Transition Probabilities (aTP) and correct CI to original sample size 
hed_annual_TP <- predicted_TP(model=hed.msm, year=1, original_n = 68166, expanded_n = 4060048)
hed_annual_TP
write_csv(hed_annual_TP, paste0(output, "Table2b-Annual TP.csv")) # save results for paper




# Table 4 - Function to extract HR results, rearrange, and correct CI to original sample size 
HR_hed <- HR_table(hed.msm, original_n = 68166, expanded_n = 4060048)  %>%
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

HR_hed
write_csv(HR_hed, paste0(output, "Table5-HR hed.csv")) # save results for paper


## 3.3) Extract Transition Probabilities (TP)  --------------------------------------------------------------------------------------------------

# Load MSM model
hed.msm<- readRDS(paste0(models, "hed.msm.RDS"))

# First, specify the covariate values
age_cat <- unique(nesarc_expanded$age3.factor)
sex <- unique(nesarc_expanded$female_wave1.factor)
race <- unique(nesarc_expanded$race_wave1.factor)
edu <- unique(nesarc_expanded$edu3.factor)

# Function to extract annual TP 
hed_annual_TP2 <- predicted_TP_covs (hed.msm, 1, age_cat, sex, race, edu) %>%
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

# Save TP
write_csv(hed_annual_TP2, paste0(output, "Supplement 2- Annual Transition Probabilities - HED.csv"))



# Function to extract TP after 3 years 
hed_TP_3yrs <- predicted_TP_covs (hed.msm, 3, age_cat, sex, race, edu) %>%
  mutate(From = recode(From, "State 1" = "Non-drinker",  # Rename states
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

# Save TP
write_csv(hed_TP_3yrs, paste0(output, "Transition Probabilities - HED after 3 years.csv"))



# Function to extract TP over 10 years  
hed_yearly_TP <- predicted_TP_overtime (hed.msm, 10, 68166, 4060048) %>%
  mutate(
    From = recode(From, "State 1" = "Initial State: Non-drinker",
      "State 2" = "Initial State: Drinker, no HED",
      "State 3" = "Initial State: Occasional HED",
      "State 4" = "Initial State: Monthly HED",
      "State 5" = "Initial State: Weekly HED"),
    To = recode(To, "State.1" = "Non-drinker",
      "State.2" = "Drinker, no HED",
      "State.3" = "Occasional HED",
      "State.4" = "Monthly HED",
      "State.5" = "Weekly HED"),
    # Re-arrange order
    From = fct_relevel(From, "Initial State: Non-drinker",  "Initial State: Drinker, no HED",	"Initial State: Occasional HED",	"Initial State: Monthly HED", "Initial State: Weekly HED"),
    To = fct_relevel(To, "Non-drinker",  "Drinker, no HED",	"Occasional HED",	"Monthly HED", "Weekly HED"))


      # Plot TP Over time
      hed_yearly_TP %>%
        ggplot(aes(x=Year, y=Estimate, group=To)) + geom_line(aes(color=To), size=0.5) + 
        geom_ribbon(aes(ymin=newLower, ymax=newUpper, fill=To), alpha=0.2) + 
        facet_wrap(~From) +
        theme_bw() + labs(x = "Years Follow-up", y="Transition probability (%)", color="Transition To", fill="Transition To") +
        theme(legend.position = c(.85, 0.25)) +
        scale_y_continuous(breaks=seq(0, 100, by= 10)) + 
        scale_x_continuous(breaks=seq(0, 10, by= 2))
      ggsave(paste0(output, "HED TP over time 1.tiff"), dpi=600, width=7.5, height = 5)



# Function to extract TP over 10 years for each age category

hed_yearly_TP2 <- predicted_TP_overtime_age (hed.msm, 10, age_cat, 68166, 4060048) %>%
  mutate(
    From = recode(From, "State 1" = "Initial State: Non-drinker",
                        "State 2" = "Initial State: Drinker, no HED",
                        "State 3" = "Initial State: Occasional HED",
                        "State 4" = "Initial State: Monthly HED",
                        "State 5" = "Initial State: Weekly HED"),
    To = recode(To, "State.1" = "Non-drinker",
                    "State.2" = "Drinker, no HED",
                    "State.3" = "Occasional HED",
                    "State.4" = "Monthly HED",
                    "State.5" = "Weekly HED"),
    # Re-arrange order
    From = fct_relevel(From, "Initial State: Non-drinker",  "Initial State: Drinker, no HED",	"Initial State: Occasional HED",	"Initial State: Monthly HED", "Initial State: Weekly HED"),
    To = fct_relevel(To, "Non-drinker",  "Drinker, no HED",	"Occasional HED",	"Monthly HED", "Weekly HED"))


          # Plot yearly TP for each age category
          hed_yearly_TP2 %>%
              mutate(age_cat=recode(age_cat, "18-29" = "Ages 18-29 years", "30-49" = "Ages 30-49 years", "50+" = "Ages 50+ years")) %>%
              ggplot(aes(x=Year, y=Estimate, group=To)) + geom_line(aes(color=To), size=0.5) + 
              geom_ribbon(aes(ymin=newLower, ymax=newUpper, fill=To), alpha=0.2) + 
              facet_grid(age_cat~From) +
              theme_bw() + labs(x = "Years Follow-up", y="Transition probability (%)", color="Transition To", fill="Transition To") +
              theme(legend.position = "top") +
              scale_y_continuous(breaks=seq(0, 100, by= 20)) + 
              scale_x_continuous(breaks=seq(0, 10, by= 1))
          ggsave(paste0(output, "HED TP over time 2.tiff"), dpi=600, width=12, height = 7)


## 3.4) Check Model Fit--------------------------------------------------------------------------------------

# Load model
hed.msm<- readRDS(paste0(models, "hed.msm.RDS"))

# Load Transition probabilities
probabilities_hed <- read_csv(paste0(output, "Transition Probabilities - HED after 3 years.csv")) %>%
  mutate(cat = paste(sex, age_cat, edu, race, From, sep="_")) %>% 
  select(cat, To, Probability) %>% 
  group_by(cat) %>% 
  mutate(cumsum = cumsum(Probability)) %>%
  ungroup()


# Function to apply HED transition probabilities
transition_hed <- function(data, transitions){
  selected <- unique(data$cat)
  rates <- transitions %>% filter(cat == selected)
  data$HED_2_pred <- ifelse(data$prob <= rates$cumsum[1], "Non-drinker",
                        ifelse(data$prob <= rates$cumsum[2] & data$prob > rates$cumsum[1], "Drinker, no HED",
                          ifelse(data$prob <= rates$cumsum[3] & data$prob > rates$cumsum[2],"Occasional HED",
                            ifelse(data$prob <= rates$cumsum[4] & data$prob > rates$cumsum[3],"Monthly HED",
                              ifelse(data$prob <= rates$cumsum[5] & data$prob > rates$cumsum[4],"Weekly HED",NA)))))
  
  return(data)
}



# Get predicted HED at wave 3
hed_data <- nesarc_hed %>%
  # Rename variables
  select(idnum, wave, years, age3.factor, female_wave1.factor, race_wave1.factor, edu3.factor, hed.factor) %>%
  rename(age_cat = age3.factor,
    sex = female_wave1.factor, 
    race = race_wave1.factor,
    edu = edu3.factor,
    HED = hed.factor) %>%
  
  # Tranform to wide format
  pivot_wider(names_from="wave", values_from=c("HED", "years", "age_cat", "edu")) %>%
  
  # Create variable combing level of each covariate and the state at baseline
  mutate(cat = paste(sex, age_cat_1, edu_1, race, HED_1, sep="_"),
    prob = runif(nrow(.))) %>%  # generate random prob
  
  # Apply transition function
  group_by(cat) %>%
  do(transition_hed(., probabilities_hed)) %>% # use 'do( )' to run the function defined earlier
  ungroup() %>%
  mutate(HED_2_pred = factor(HED_2_pred, levels=c("Non-drinker",  "Drinker, no HED",	"Occasional HED",	"Monthly HED", "Weekly HED"))) %>%
  select(-cat, - prob, -years_1)




# Compare observed and predicted proportions
observed <- count(hed_data, HED_2) %>% rename(observed = n, HED = HED_2) 
predicted <- count(hed_data, HED_2_pred) %>% rename(predicted = n, HED = HED_2_pred)  
comparison_HED <- full_join (observed, predicted, by="HED") %>%
  mutate(difference = abs(predicted-observed),  
      diff_percent = difference/observed * 100) %>%
  adorn_totals("row") %>%  # Add row totals
  mutate(diff_percent = ifelse(HED=="Total", difference/predicted * 100, diff_percent),
          predicted = ifelse(HED=="Total", NA, predicted), 
          diff_percent = round(diff_percent, 2))

kableone(comparison_HED)

