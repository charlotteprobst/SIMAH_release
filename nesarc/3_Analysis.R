
# SIMAH - NESARC Alcohol Transitions
# Data Analysis

library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(msm)        # model transition probabilities
library(tableone)   # create descriptives table


## Set the working directory
setwd("C:/Users/klajd/OneDrive/SIMAH")
data    <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Data/"
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/Output/"

# Load data / functions
nesarc <- readRDS(paste0(data, "nesarc_clean.rds")) 
nesarc_expanded <- readRDS(paste0(data, "nesarc_clean_expanded.rds")) 
source("SIMAH_code/nesarc/0_Extract TP function.R")

# Descriptives ------------------------------------------------------------------------------------------

# At baseline
nesarc_wave1 <- filter(nesarc, wave==1)

tab1 <-CreateTableOne(vars= c("female", "age", "race.factor", "married.factor", "edu3.factor", "alc5.factor", "hed.factor"), 
                      factorVars = c("female", "race.factor", "married.factor", "edu3.factor", "alc5.factor", "hed.factor"), 
                      data=nesarc_wave1)
summary(tab1)
table1 <- print(tab1, noSpaces = TRUE, catDigits = 0, contDigits = 1, pDigits = 2, printToggle = FALSE)   
write.csv(table1, file=paste0(output,"Table1-Descriptives.csv"))  # export to excel, to copy/paste into manuscript
kableone(table1)                             # view in R; R Markdown friendly version


# years follow-up
nesarc %>%
  filter(wave==2) %>%
  select (years) %>% 
  skim()

# Run MSM Model ------------------------------------------------------------------------------------------------
# Count of transitions 
statetable.msm(alc5, idnum, data=nesarc)


# Specify transition intensity matrix (Q) - i.e., what (instanteneous) transitions are allowed (specified by the non-zero entries)
# Will only allow transitions to an adjacent state , except for transitions back to lifetime abstainers, and abstainer->former drinker

Q <- rbind ( c(0,     0,    0.25,  0,    0),
             c(0,     0,    0.25,  0,    0),
             c(0,     0.25, 0,     0.25, 0),
             c(0,     0,    0.25,  0,    0.25),
             c(0,     0,    0,     0.25, 0))

# Specify initial values 
Q <- crudeinits.msm(alc5 ~ years, idnum, data=nesarc_expanded, qmatrix=Q)

# Run MSM model
alc5.msm <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
                 qmatrix = Q, center=FALSE,
                 control = list(trace=1, maxit=500, fnscale = 3000000),
                 covariates = ~ female_wave1 + age_scaled + edu3.factor + race_wave1.factor)

    # Save Results
    saveRDS(alc5.msm, paste0(output, "alc5.msm.RDS"))


# Run MSM model with interaction
alc5.msm_int <- msm (alc5 ~ years, subject=idnum, data = nesarc_expanded, 
  qmatrix = Q, center=FALSE,
  control = list(trace=1, maxit=1000, fnscale = 3000000),
  covariates = ~ female_wave1 + age_scaled + edu3.factor*race_wave1.factor)


    # Save Results
    saveRDS(alc5.msm_int, paste0(output, "alc5.msm_int.RDS"))
    
    
# Compare the two models
AIC(alc5.msm, alc5.msm_int)
    
    
# Load Model and View Results -----------------------------------------------------------------------------------------------
alc5.msm <- readRDS(paste0(output, "alc5.msm.RDS"))

      # Transition probabilities at year = t; covariates set to their mean value
      pmatrix.msm(alc5.msm, t=1, ci="norm") # CI based on drawing random samples, default size=1000
      
      # Hazard ratios for transition
      hazard.msm(alc5.msm)
      


# Table 2 - Extract Annual Transition Probabilities (aTP) and correct CI to original sample size 
aTP.results <- data.frame(print(pmatrix.msm(alc5.msm, t=1, ci="norm"))) %>%
  mutate(From = row.names(.)) %>%
  pivot_longer(cols = -From, names_to = "To") %>%
  separate(value, into=c("Estimate","Lower","Upper", NA), sep="\\(|\\,|\\)", convert=TRUE) %>%  # separated based on "(" "," and  ")"  convert=TRUE names variables numeric
  mutate(
    SE = (Upper - Lower) / 3.92,
    SD = SE * sqrt(4061624),  # sample size of expanded data
    newSE = SD / sqrt(68168), # sample size of original data
    newLower = round((Estimate - (newSE * 1.96))*100, digits=1),
    newUpper = round((Estimate + (newSE * 1.96))*100, digits=1),
    Estimate = round(Estimate*100, digits=1), 
    EstimateCI = paste0(Estimate, " (", newLower, ", ", newUpper, ")")) %>%
  select (From, To, EstimateCI) %>%
  pivot_wider(names_from = "To", values_from = "EstimateCI")

  # save results for paper
  write_csv(aTP.results, paste0(output, "Table2-Annual TP.csv"))


# Table 3 - Extract HR results, rearrange, and correct CI to original sample size 
HR.results <- data.frame(hazard.msm(alc5.msm))%>%
  mutate(transition = row.names(.)) %>%
  pivot_longer(cols=-transition) %>%
  extract(name, into=c("Variable","Type"), regex="(.*)\\.(.*)") %>%   # Separate the string (name) into the variable and type of estimate (HR, Upper, Lower), separate at last occuring period .
  pivot_wider(names_from="Type", values_from = "value") %>%
  mutate(
    SE=(log(U) - log(L))/3.92, 
    SD = SE * sqrt(4061624),  # sample size of expanded data
    newSE = SD / sqrt(68168), # sample size of original data
    newLower = round(exp((log(HR)) - (newSE * 1.96)), digits=2), 
    newUpper = round(exp((log(HR)) + (newSE * 1.96)), digits=2), 
    HR = round(HR, digits=2),
    EstimateCI = paste0(HR, " (", newLower, ", ", newUpper, ")")) %>%
  select(transition, Variable, EstimateCI) %>%
  pivot_wider(names_from = "transition", values_from = EstimateCI) %>%
  rename("Abstainer->LowRisk"   = "State 1 - State 3",
         "Former->LowRisk"      = "State 2 - State 3",
         "LowRisk->Former"      = "State 3 - State 2",
         "LowRisk->MediumRisk"  = "State 3 - State 4",
         "MediumRisk->LowRisk"  = "State 4 - State 3",
         "MediumRisk->HighRisk" = "State 4 - State 5",
         "HighRisk->MediumRisk" = "State 5 - State 4") %>%
  select(Variable, "Abstainer->LowRisk", "Former->LowRisk", "LowRisk->MediumRisk", "MediumRisk->HighRisk", "HighRisk->MediumRisk", "MediumRisk->LowRisk", "LowRisk->Former")

  # save results for paper
  write_csv(HR.results, paste0(output, "Table3-HR.csv"))

  
  
  
# Extract and Plot TP --------------------------------------------------------------------------------------------------

  # Specify the covariate values
age <- sort(unique(nesarc_expanded$age_scaled))
distinct_ages <- select(nesarc_expanded, age_scaled) %>% distinct() # Identify the unique ages
sex <- c(0,1)
race <- unique(nesarc_expanded$race_wave1.factor)
edu <- unique(nesarc_expanded$edu3_wave1.factor)


# Run function to extract TP
prob_alc5 <- extractTP(alc5.msm, age, distinct_ages, sex, race, edu) 

  write_csv(prob_alc5, paste0(output, "Transition Probabilities.csv"))


# Plot Transition probabilities *******************************************************************
# 'Poor' Transitions, among non-Hispanic Whites 
prob_alc5 %>%
  filter(Transition %in%c("Abstainer -> Low risk", "Former -> Low risk",  "Low risk -> Medium risk", "Medium risk -> High risk")) %>%
  filter(race=="White, non-Hispanic") %>%
  ggplot(aes(x=as.numeric(age), y=Probability, color=edu, linetype=edu)) + geom_line(size=1.5)+
  facet_grid(cols=vars(sex), rows=vars(Transition), scale="free") +
  scale_x_continuous(limits=c(18, 85), breaks=seq(20, 85, by= 5)) + 
  labs(title= "'Poor' Transitions", subtitle="White, non-Hispanic", x = "age", y="Transition probability") + 
  theme_bw() + theme(legend.position="bottom")


# 'Better' Transitions, among non-Hispanic Whites 
prob_alc5 %>%
  filter(Transition %in%c("High risk -> Medium risk", "Medium risk -> Low risk", "Low risk -> Former")) %>%
  filter(race=="White, non-Hispanic") %>%
  ggplot(aes(x=as.numeric(age), y=Probability, color=edu, linetype=edu)) + geom_line(size=1.5)+
  facet_grid(cols=vars(sex), rows=vars(Transition), scale="free") +
  scale_x_continuous(limits=c(18, 85), breaks=seq(20, 85, by= 5)) + 
  labs(title= "'Better' Transitions", subtitle="White, non-Hispanic", x = "age", y="Transition probability") + 
  theme_bw() + theme(legend.position="bottom")
