
# SIMAH Restricted-access Data
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
library(survey)     # working with weighted data
library(biostat3)   # survRate command
library(foreach)    # to loop function


# Specify the data and output file locations

# Specify the data and output file locations
data    <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Restricted access data/Data/"
output  <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Restricted access data/Output/Descriptives/"

# Load data
nhis <- readRDS (paste0(data, "nhis_clean.rds"))
nhis_svy <- readRDS (paste0(data, "nhis_clean_svy.rds"))

# Need to first create the 'age_group' vaariable, based on the results of the assumption checks



# Function to resolve a conflict between {survey} and {tableone} -------------------------------------------------------
# print() would not work with continuous variables in objects created by svyCreateTableOne()
svyQuant_alt <- function (vars, design, q = 0.5) {
  res <- vector()
  for (i in 1:length(vars)) {
    var <- vars[i]
    res[i] <- oldsvyquantile(design$variables[var], design = design,
      quantiles = q[1], na.rm = TRUE)
  }
  out <- as.vector(res)
  names(out) <- vars
  out
}

environment(svyQuant_alt) <- asNamespace('tableone')
assignInNamespace("svyQuant", svyQuant_alt, ns = "tableone")



# Table 1: Participant characteristics -----------------------------------------------------------------------------------------------  

# specify the variables to be included in the table
all_vars <- c("female2", "bl_age", "alc_daily_g", "alc5","smk4", "bmi4", "phy3", "race4", "income5", "married2", "PsyDistr3")


# Table 1_v1: Stratified by education *************************************************************************************
# Raw Data
CreateTableOne(vars= all_vars, strata= "edu3", addOverall = TRUE, data=nhis) %>% 
  print(noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE) %>%
  write.csv(paste0(output, "Table1_v1 Demographics by education.csv"))


# Survey adjusted Data
svyCreateTableOne(vars= all_vars, strata= "edu3", addOverall = TRUE, data=nhis_svy) %>% 
  print(noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE) %>%
  write.csv(paste0(output, "Table1_v1_svy Demographics by education.csv"))


  
# Table 1_v2: Stratified by education and sex  *****************************************************************************
# Raw Data
CreateTableOne(vars= all_vars, strata= c("edu3", "female2"), addOverall = TRUE, data=nhis) %>%
  print(noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE) %>%
  write.csv(paste0(output, "Table1_v2 Demographics by education, sex.csv"))
 

# Survey adjusted Data
svyCreateTableOne(vars= all_vars, strata= c("edu3", "female2"), addOverall = TRUE, data=nhis_svy) %>%
  print(noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE) %>%
  write.csv(paste0(output, "Table1_v2_svy Demographics by education, sex.csv"))
  
  
  
# Table 1_v3: Stratified by education and age group ***************************************************************
CreateTableOne(vars= all_vars, strata= c("edu3", "age_group"), addOverall = TRUE, data=nhis) %>%
  print(noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE) %>% 
  write.csv(paste0(output, "Table1_v3 Demographics by education, age group.csv"))
  

# Survey adjusted Data
svyCreateTableOne(vars= all_vars, strata= c("edu3", "age_group"), addOverall = TRUE, data=nhis_svy) %>%
  print(noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE) %>% 
  write.csv(paste0(output, "Table1_v3_svy Demographics by education, age group.csv"))
  
  
    
# Table 1_v4: Stratified by income  ***********************************************************************************
CreateTableOne(vars= all_vars, strata= c("income5"), addOverall = TRUE, data=nhis) %>% 
  print(noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE) %>% 
  write.csv(paste0(output, "Table1_v4 Demographics by income.csv"))
  

# Survey adjusted Data
svyCreateTableOne(vars= all_vars, strata= c("income5"), addOverall = TRUE, data=nhis_svy) %>% 
  print(noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE) %>% 
  write.csv(paste0(output, "Table1_v4_svy Demographics by income.csv"))
  
  
  
# Table 1_v5: Stratified by ethnicity  ********************************************************************************
CreateTableOne(vars= all_vars, strata= c("race4"), addOverall = TRUE, data=nhis) %>% 
  print(noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE) %>% 
  write.csv(paste0(output, "Table1_v5 Demographics by race.csv"))
  

# Survey adjusted Data
svyCreateTableOne(vars= all_vars, strata= c("race4"), addOverall = TRUE, data=nhis_svy) %>% 
  print(noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE) %>% 
  write.csv(paste0(output, "Table1_v5_svy Demographics by race.csv"))

  
  
  
  
# Table 2: Descriptives of frequency and rate of death --------------------------------------------------------------------------------------  

# Function to calculate death rate overall and for each strata to create Table 2
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
        add_row (name = "death_cause", value = toupper(i), .before=1) 
  }
  rate <- do.call(rbind, rate) 
  return(rate)
}
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

# Survey-adjusted Function to calculate death rate overall and for each strata to create Table 2
overall_rate_svy <- function(design, death_list){  
  
  final<-list()
  foreach (i = death_list) %do% {
    
    formula <- as.formula(paste0( "~" , i)) # put the variable of interest in a formula 
    
    yrs <- svytotal(~yrs_followup, design) 
    events <- svytotal(formula, design) 
    ci <- confint(events)
    
    rate <- as.data.frame(cbind(events, ci, yrs)) %>%
      mutate(rate = round(events / yrs * 10000, 1),
             lower = round(`2.5 %` / yrs * 10000, 1),
             upper = round(`97.5 %` / yrs * 10000, 1),
             rate_ci_10000py = paste0(rate, " (", lower, ", ", upper, ")")) %>%
      dplyr::select(events, rate_ci_10000py) %>%
      remove_rownames()
      
    total <- svytable(formula, design) %>% 
       as.data.frame() %>% pivot_wider(names_from = i, values_from=Freq) %>%
       mutate (n_total = `0` + `1`) %>%
       dplyr::select(n_total)
      
    
    final[[i]] <- cbind(rate, total) %>%
      mutate (percent = round(events / n_total * 100, 0),
              events = round(events, 0),
              n_percent = paste0(events," (", percent, ")")) %>%
      dplyr::select(n_percent, rate_ci_10000py) %>%
      t() %>% as.data.frame() %>% rename (value = V1) %>% rownames_to_column("name") %>%
      add_row (name = toupper(i), .before=1)
    
    
  }
  final <- do.call(rbind, final)
  return(final)
}
strata_rate_svy <- function(design, death_list, strata){  
  
  combined<-list()
  foreach (i = death_list) %do% {

      formula_strata <- as.formula(paste0( "~", strata)) 
      formula_death <- as.formula(paste0( "~", i))
    
      yrs       <- svyby(~yrs_followup, formula_strata, design, svytotal)
      events    <- svyby(formula_death, formula_strata, design, svytotal) %>% rename(n_events = i)
      ci        <- confint(events) 
      events_ci <- cbind(events, ci) %>% dplyr::select (-se)
      
      rate <- full_join(events_ci, yrs, by=strata) %>% 
        mutate (rate = round(n_events / yrs_followup * 10000, 1),
                lower = round(`2.5 %` / yrs_followup * 10000, 1),
                upper = round(`97.5 %` / yrs_followup * 10000, 1),
                rate_ci_10000py = paste0(rate, " (", lower, ", ", upper, ")")) %>%
        dplyr::select (strata, n_events, rate_ci_10000py)

      formula <- as.formula(paste0( "~", strata, "+", i))

      total <- svytable(formula, design) %>%
        as.data.frame() %>% pivot_wider(names_from = i, values_from=Freq) %>%
        mutate (n_total = `0` + `1`) %>% dplyr::select(strata, n_total)

      combined[[i]] <- full_join (rate, total, by=strata) %>%
        mutate (percent = round(n_events / n_total * 100,0),
                n_events = round(n_events, 0),
                n_percent = paste0(n_events, " (", percent, ")")) %>%
        dplyr::select(strata, n_percent, rate_ci_10000py) %>%
        pivot_longer(cols = c("n_percent", "rate_ci_10000py")) %>%
        pivot_wider(names_from = -c("name", "value"), values_from="value") %>%
        add_row (name = toupper(i), .before=1)
  }
  combined <- do.call(rbind, combined)
  return(combined)
}

overall_rate_svy(nhis_svy, death_list)


# Causes of death *********************************************************************************************************************
# Specify the causes of death (to be used below)
death_list <- c("allcause_death", "alc_death", "despair_death", "vehicle_death", "accident_death", 
                "AUD_death", "self_harm_death", "liver_death", "diabetes_death", "IHD_death", 
                "stroke_death", "hyperten_death", "poisoning_death", "other_death")

#Temporary list:
death_list <- c("allcause_death", "heart_death", "cancer_death")


# Table 2 - Overall frequency and rate of death
overall_rate(nhis, death_list) %>%  write.csv(paste0(output, "Table2 Deaths overall, sex.csv"), row.names=FALSE, na="")
overall_rate_svy(nhis_svy, death_list) %>% write.csv(paste0(output, "Table2_svy Deaths overall, sex.csv"), row.names=FALSE, na="")
  

# Table 2_v1: Stratified by education
strata_rate(nhis, death_list, "edu3") %>% write.csv(paste0(output, "Table2_v1 Deaths by education.csv"), row.names=FALSE, na="")
strata_rate_svy(nhis_svy, death_list, "edu3") %>% write.csv(paste0(output, "Table2_v1_svy Deaths by education.csv"), row.names=FALSE, na="")


# Table 2_v2: Stratified by education and sex
strata_rate(nhis, death_list, "edu3+female2") %>% write.csv(paste0(output, "Table2_v2 Deaths by education, sex.csv"), row.names=FALSE, na="")
strata_rate_svy(nhis_svy, death_list, "edu_sex") %>% write.csv(paste0(output, "Table2_v2_svy Deaths by education, sex.csv"), row.names=FALSE, na="")


# Table 2_v3: Stratified by education and age group
strata_rate(nhis, death_list, "edu3+age_group") %>% write.csv(paste0(output, "Table2_v3 Deaths by education, age group.csv"), row.names=FALSE, na="")
strata_rate_svy(nhis_svy, death_list, "edu3+age_group") %>% write.csv(paste0(output, "Table2_v3_svy Deaths by education, age group.csv"), row.names=FALSE, na="")


# Table 2_v4: Stratified by income
strata_rate(nhis, death_list, "income5") %>% write.csv(paste0(output, "Table2_v4 Deaths by income.csv"), row.names=FALSE, na="")
strata_rate_svy(nhis_svy, death_list, "income5") %>% write.csv(paste0(output, "Table2_v4_svy Deaths by income.csv"), row.names=FALSE, na="")


# Table 2_v5: Stratified by ethnicity
strata_rate(nhis, death_list, "race4") %>% write.csv(paste0(output, "Table2_v5 Deaths by race.csv"), row.names=FALSE, na="")
strata_rate_svy(nhis_svy, death_list, "race4") %>% write.csv(paste0(output, "Table2_v5_svy Deaths by race.csv"), row.names=FALSE, na="")



