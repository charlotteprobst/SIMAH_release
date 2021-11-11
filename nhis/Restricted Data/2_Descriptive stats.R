

# SES x Lifestyle Differential Vulnerability & Exposure Project
# Descriptive Statistics

# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(gmodels)    # CrossTable command
library(tableone)   # create table one
library(survival)   # surivval analyses
library(survminer)  # surivval analyses
library(timereg)    # additive survival models
library(survey)     # for survey weighted cox model
library(biostat3)   # survRate command
library(foreach)    # to loop function


# Specify the data and output file locations

# Specify the data and output file locations
data    <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/Data/"
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/Restricted NHIS Data/Output"


# Load data
nhis        <- readRDS (paste0(data, "nhis.rds"))
nhis_male   <- readRDS (paste0(data, "nhis_male.rds"))
nhis_female <- readRDS (paste0(data, "nhis_female.rds"))



# Table 1: Participant characteristics -----------------------------------------------------------------------------------------------  

# specify the variables to be included in the table

all_vars <- c("female2", "bl_age", "alc_daily_g", "alc5","smk4",  "bmi4", "phy3",
                     "race4", "income5", "employed3", "married2", "PsyDistr3")

categorical_vars <- c("female.factor", "alc5", "smk4", "bmi4", "phy3",
                      "race4", "income5", "employed3", "married2", "PsyDistr3")



# Table 1_v1: Stratified by education
tab1_v1 <-CreateTableOne(vars= all_vars, factorVars = categorical_vars, 
                         strata= c("edu3"), addOverall = TRUE, data=nhis)
  table1_v1 <- print(tab1_v1, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)  
  write.csv(table1_v1, file = file.path(output, "Table1_v1 Demographics by education.csv"))
  kableone(table1_v1)

  # Person years (tstop)
  survRate(Surv(yrs_followup, allcause_deaths) ~ 1, data=nhis)          # overall 
  survRate(Surv(yrs_followup, allcause_deaths) ~ edu, data=nhis)        # for each category 
  
  
  
  
# Table 1_v2: Stratified by education and sex
tab1_v2 <-CreateTableOne(vars= all_vars, factorVars = categorical_vars, 
                         strata= c("edu3", "female2"), addOverall = TRUE, data=nhis)
  table1_v2 <- print(tab1_v2, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)  
  write.csv(table1_v2, file = file.path(output, "Table1_v2 Demographics by education, sex.csv"))
  kableone(table1_v2)
  
  # Person years (tstop)
  survRate(Surv(yrs_followup, allcause_deaths) ~ 1, data=nhis)           # overall 
  survRate(Surv(yrs_followup, allcause_deaths) ~ female+edu, data=nhis)  # for each category 
  
  
  
  
# Table 1_v3: Stratified by education and age group
tab1_v3 <-CreateTableOne(vars= all_vars, factorVars = categorical_vars, 
                         strata= c("edu3", "age_group"), addOverall = TRUE, data=nhis)
  table1_v3 <- print(tab1_v3, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)  
  write.csv(table1_v3, file = file.path(output, "Table1_v3 Demographics by edycation, age group.csv"))
  kableone(table1_v3)
  
  # Person years (tstop)
  survRate(Surv(yrs_followup, allcause_deaths) ~ 1, data=nhis)              # overall 
  survRate(Surv(yrs_followup, allcause_deaths) ~ age_group+edu, data=nhis)  # for each category 


  
    
# Table 1_v4: Stratified by income
tab1_v4 <-CreateTableOne(vars= all_vars, factorVars = categorical_vars, 
                         strata= c("income5"), addOverall = TRUE, data=nhis)
  table1_v4 <- print(tab1_v4, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)  
  write.csv(table1_v4, file = file.path(output, "Table1_v4 Demographics by income"))
  kableone(table1_v4)
  
  # Person years (tstop)
  survRate(Surv(yrs_followup, allcause_deaths) ~ 1, data=nhis)          # overall 
  survRate(Surv(yrs_followup, allcause_deaths) ~ income, data=nhis)     # for each category
  
  
  
  
# Table 1_v5: Stratified by ethnicity
tab1_v5 <-CreateTableOne(vars= all_vars, factorVars = categorical_vars, 
                           strata= c("race4"), addOverall = TRUE, data=nhis)
  table1_v5 <- print(tab1_v5, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)  
  write.csv(table1_v5, file = file.path(output, "Table1_v5 Demographics by ethnicity.csv"))
  kableone(table1_v5)
  
  # Person years (tstop)
  survRate(Surv(yrs_followup, allcause_deaths) ~ 1, data=nhis)           # overall 
  survRate(Surv(yrs_followup, allcause_deaths) ~ ethnicity, data=nhis)   # for each category
  
  
  
  
  
  
  
# Table 2: Descriptives of frequency and rate of death --------------------------------------------------------------------------------------  

# Function to calculate death rate overall and create Table 2
overall_rate <- function(data, death_list){  

  rate<-list()
  foreach (i = death_list) %do% {
  
    # Data preparation 
    data <- mutate(data, cause_of_death = .data[[i]])

    # Calculate rate and modify output 
    rate[[i]] <- survRate(Surv(yrs_followup, cause_of_death) ~ 1, data=data) %>%  # Calculate frequency and rate
        mutate(n_deaths = event,                                             # modify output to create formatted table
               n_tot = nrow(data),
               percent_deaths = round(n_deaths / n_tot * 100, 0),
               n_percent = paste0(n_deaths, " (", percent_deaths, ")"),
          
               rate = round(rate*10000,1),
               lower = round(lower*10000,1), 
               upper = round(upper*10000,1),
               rate_CI_10000py = paste0(rate, " (", lower, ", ", upper, ")")) %>%
    
        dplyr::select(n_percent, rate_CI_10000py) %>%
        pivot_longer(cols = everything()) %>% 
        add_row (name = toupper(i), .before=1)
  }
  rate <- do.call(rbind, rate) 
  return(rate)
}
          
# Function to calculate death rate for each strata to create Table 2
strata_rate <- function(data, death_list, strata){  
  
  rate<-list()
  foreach (i = death_list) %do% {
    
    formula <- as.formula(paste0("Surv(yrs_followup, ", i, ") ~ ", strata))
    rate[[i]] <- survRate(formula, data=data) %>%  # Calculate frequency and rate
      mutate(n_deaths = event,                     # modify output to create formatted table
        n_tot = nrow(data),
        percent_deaths = round(n_deaths / n_tot * 100, 0),
        n_percent = paste0(n_deaths, " (", percent_deaths, ")"),
        
        rate = round(rate*10000,1),
        lower = round(lower*10000,1), 
        upper = round(upper*10000,1),
        rate_CI_10000py = paste0(rate, " (", lower, ", ", upper, ")")) %>%
      
      dplyr::select(-c(tstop, event, rate, lower, upper, n_deaths, n_tot, percent_deaths)) %>%
      pivot_longer(cols = c("n_percent", "rate_CI_10000py")) %>%
      pivot_wider(names_from = -c("name", "value"), values_from="value") %>% 
      add_row (name = toupper(i), .before=1)
  }
  rate <- do.call(rbind, rate) 
  return(rate)
}


# Causes of death ----------------------------------------------------------------------------------------------
# Specify the causes of death (to be used below)
death_list <- c("allcause_deaths", "alc_deaths", "despair_deaths", "vehicle_deaths", "accident_deaths", 
                "AUD_deaths", "self_harm_deaths", "liver_deaths", "diabetes_deaths", "IHD_deaths", 
                "stroke_deaths", "hyperten_deaths", "poisoning_deaths", "other_deaths")

death_list <- c("allcause_death", "heart_death")


# Table 2 - Overall frequency and rate of death
table2_overall <- overall_rate(nhis, death_list)
write.csv(table2_overall, file = file.path(output, "Table2 Deaths overall, sex.csv"))
kableone(table2_overall)
      

# Table 2_v1: Stratified by education
table2_v1 <- strata_rate(nhis, death_list, "edu3")
write.csv(table2_v1, file = file.path(output, "Table2_v1 Deaths by education.csv"))
kableone(table2_v1)  



# Table 2_v2: Stratified by education and sex
table2_v2 <- strata_rate(nhis, death_list, "edu3+female.factor")
write.csv(table2_v2, file = file.path(output, "Table2_v2 Deaths by education, sex.csv"))
kableone(table2_v2)



# Table 2_v3: Stratified by education and age group
table2_v3 <-strata_rate(nhis, death_list, "edu3+age_group.factor")
write.csv(table2_v3, file = file.path(output, "Table2_v3 Deaths by education, age group.csv"))
kableone(table2_v3)



# Table 2_v4: Stratified by income
table2_v4 <- strata_rate(nhis, death_list, "income5")
write.csv(table2_v4, file = file.path(output, "Table2_v4 Deaths by income"))
kableone(table2_v4)

      

# Table 2_v5: Stratified by ethnicity
table2_v5 <-  strata_rate(nhis, death_list, "race4")
write.csv(table2_v5, file = file.path(output, "Table2_v5 Deaths by ethnicity.csv"))
kableone(table2_v5)


