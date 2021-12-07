# SES x Lifestyle Differential Vulnerability & Exposure Project
# Sensitivity Analyses 5: Multiplicative Hazards

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
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions


# Specify the data and output file locations
data   <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Processed data/"                # Location of data
output <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/SES x Lifestyle/Sensitivity/"   # Location of model output
source("Function - Format Results.R")
source("Function - CausalMed Results.R")

    
# Load data
nhis_all    <- readRDS (file.path(data, "nhis_all.rds"))         # NHIS data with all participants
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))
nhis_svyWeights <- readRDS (file.path(data, "nhis_svyWeights.rds"))
nhis_svyWeights_female <- readRDS (file.path(data, "nhis_svyWeights_female.rds"))
nhis_svyWeights_male <- readRDS (file.path(data, "nhis_svyWeights_male.rds"))

# Aalen Models (Multiplicative Hazard Ratios) ---------------------------------------------------------------------------------------------------

# Create Function to run Cox Models with NO survey weights
cox_noweights <- function(data, lifestyle, lifestyle_edu, loc1, loc2, loc3, loc4){
    
    # data setup
    data <- data %>%
      mutate (lifestyle = {{lifestyle}},
              lifestyle_edu = {{lifestyle_edu}})
  
    # interaction model
    cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*lifestyle + married.factor + ethnicity.factor + factor(srvy_yr), data=data)
    
    # joint effect model
    cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ lifestyle_edu + married.factor + ethnicity.factor + factor(srvy_yr), data=data)
    
    # model check
    cox_check <- cox.zph(cox)

    # print results
    cox_HR(cox, loc1) ; cox_HR(cox, loc2) ; cox_HR(cox2, loc3); cox_HR(cox, loc4) ; print(cox_check) ; plot(cox_check, col = "red")
  
}


# Create Function to run Cox Models WITH survey weights
cox_weights <- function(female_var, lifestyle, lifestyle_edu, loc1, loc2, loc3, loc4){
  
  # data setup
  nhis_all <- nhis_all %>%
    mutate (lifestyle = {{lifestyle}},
      lifestyle_edu = {{lifestyle_edu}})

      nhis_svyWeights_all <- svydesign(id = ~new_psu,
                          strata  = ~new_stratum,
                          weights = ~new_weight,
                          nest    = TRUE,
                          data    = nhis_all)
  
      # Create subset with no missing data, correcting for survey weights
      nhis_svyWeights <- subset(nhis_svyWeights_all, 
        !is.na(yrs_followup) & !is.na(mortstat) & 
          !is.na(alcohol5v2) & !is.na(bmi_cat) & !is.na(smoking4) & !is.na(phy_act3) &
          !is.na(edu) & !is.na(age) & !is.na(female) & !is.na(married) & !is.na(ethnicity) & 
          (age>=25 & age <85))
      
      
      # Create subset with males or females only
      nhis_svyWeights <- subset(nhis_svyWeights,  female==female_var)
  
  
  # interaction model
  cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor * lifestyle + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights)
  
  # joint effect model
  cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ lifestyle_edu + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights)
  
  # model check
  cox_wt_check <- cox.zph(cox_wt)
  
  # print results
  cox_HR(cox_wt, loc1) ; cox_HR(cox_wt, loc2) ; cox_HR(cox_wt2, loc3); cox_HR(cox_wt, loc4) ; print(cox_wt_check) ; plot(cox_wt_check, col = "red")

}



# Alcohol * Education
cox_noweights(nhis_female, alcohol5v2.factor, edu.alc, 1, 6, 13, 34)
cox_weights(female_var=1, alcohol5v2.factor, edu.alc, 1, 6, 13, 34)

cox_noweights(nhis_male, alcohol5v2.factor, edu.alc, 1, 6, 13, 34)
cox_weights(female_var=0, alcohol5v2.factor, edu.alc, 1, 6, 13, 34)
        

# Smoking * Education
cox_noweights(nhis_female, smoking4.factor, edu.smk, 1, 5, 10, 31)
cox_weights(female_var=1, smoking4.factor, edu.smk, 1, 5, 10, 31)

cox_noweights(nhis_male, smoking4.factor, edu.smk, 1, 5, 10, 31)
cox_weights(female_var=0, smoking4.factor, edu.smk, 1, 5, 10, 31)


# BMI * Education
cox_noweights(nhis_female, bmi_cat.factor, edu.bmi, 1, 5, 10, 31)
cox_weights(female_var=1, bmi_cat.factor, edu.bmi, 1, 5, 10, 31)

cox_noweights(nhis_male, bmi_cat.factor, edu.bmi, 1, 5, 10, 31)
cox_weights(female_var=0, bmi_cat.factor, edu.bmi, 1, 5, 10, 31)


# Physical activity * Education
cox_noweights(nhis_female, phy_act3.factor, edu.phy, 1, 3, 4, 26)
cox_weights(female_var=1, phy_act3.factor, edu.phy, 1, 3, 4, 26)

cox_noweights(nhis_male, phy_act3.factor, edu.phy, 1, 3, 4, 26)
cox_weights(female_var=0, phy_act3.factor, edu.phy, 1, 3, 4, 26)
