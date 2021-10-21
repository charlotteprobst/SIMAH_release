

# SES x Lifestyle Differential Vulnerability & Exposure Project
# Sensitivity Analyses 4: Multiplicative Hazards

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
data    <- "SIMAH_workspace/nhis/Data/"
output  <- "SIMAH_workspace/nhis/SES x Behavior/Output/Sensitivity/"
source("Function - Format Results.R")
source("Function - CausalMed Results.R")

    
# Load data
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))
nhis_svyWeights <- readRDS (file.path(data, "nhis_svyWeights.rds"))
nhis_svyWeights_female <- readRDS (file.path(data, "nhis_svyWeights_female.rds"))
nhis_svyWeights_male <- readRDS (file.path(data, "nhis_svyWeights_male.rds"))



# Aalen Models (Multiplicative Hazard Ratios) ---------------------------------------------------------------------------------------------------

# Alcohol * Education *************************************************************************************************************************************

# WOMEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*alcohol5v2.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.alc + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox_HR(cox, 1) ; cox_HR(cox, 6) ; cox_HR(cox2, 13); cox_HR(cox, 34)
    
          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print

# WOMEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*alcohol5v2.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.alc + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 6) ; cox_HR(cox_wt2, 13); cox_HR(cox_wt, 34)

          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print


          
# MEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*alcohol5v2.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.alc + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox_HR(cox, 1) ; cox_HR(cox, 6) ; cox_HR(cox2, 13); cox_HR(cox, 34)
          
          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print
          
# MEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*alcohol5v2.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.alc + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 6) ; cox_HR(cox_wt2, 13); cox_HR(cox_wt, 34)
          
          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print
          
          
          

          
# Smoking * Education ***********************************************************************************************************************************************************
# WOMEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*smoking4.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.smk + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox_HR(cox, 1) ; cox_HR(cox, 5) ; cox_HR(cox2, 10); cox_HR(cox, 31)
         
          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print
          
# WOMEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*smoking4.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.smk + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 5) ; cox_HR(cox_wt2, 10); cox_HR(cox_wt, 31)
          
          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print
          
          
        
# MEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*smoking4.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.smk + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox_HR(cox, 1) ; cox_HR(cox, 5) ; cox_HR(cox2, 10); cox_HR(cox, 31)
          
          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print
          
# MEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*smoking4.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.smk + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 5) ; cox_HR(cox_wt2, 10); cox_HR(cox_wt, 31)
          
          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print
          
          
          
        

          
          
# BMI * Education ***********************************************************************************************************************************************************
# WOMEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*bmi_cat.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.bmi + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox_HR(cox, 1) ; cox_HR(cox, 5) ; cox_HR(cox2, 10); cox_HR(cox, 31)

          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print
          
# WOMEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*bmi_cat.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.bmi + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 5) ; cox_HR(cox_wt2, 10); cox_HR(cox_wt, 31)
          
          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print
          
          
          
# MEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*bmi_cat.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.bmi + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox_HR(cox, 1) ; cox_HR(cox, 5) ; cox_HR(cox2, 10); cox_HR(cox, 31)
          
          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print
          
# MEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*bmi_cat.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.bmi + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 5) ; cox_HR(cox_wt2, 10); cox_HR(cox_wt, 31)
          
          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print
          
          
          
          
          
          
# Physical activity * Education ****************************************************************************************************************************************************
# WOMEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*phy_act3.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.phy + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_female)
cox_HR(cox, 1) ; cox_HR(cox, 3) ; cox_HR(cox2, 4); cox_HR(cox, 26)

          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print
          
# WOMEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*phy_act3.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.phy + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_female)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 3) ; cox_HR(cox_wt2, 4); cox_HR(cox_wt, 26)
          
          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print
          
          
          
# MEN: No survey weights - interaction model and joint effect model
cox <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*phy_act3.factor + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox2 <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.phy + married.factor + ethnicity.factor + factor(srvy_yr), data=nhis_male)
cox_HR(cox, 1) ; cox_HR(cox, 3) ; cox_HR(cox2, 4); cox_HR(cox, 26)
          
          # Assessing proportional hazards
          cox_check <- cox.zph(cox)
          print(cox_check)
          plot(cox_check, col = "red")
          # ggcoxzph(cox_check) # is slower to print
          
# MEN: With survey weights 
cox_wt <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*phy_act3.factor + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_wt2 <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.phy + married.factor + ethnicity.factor + factor(srvy_yr), design=nhis_svyWeights_male)
cox_HR(cox_wt, 1) ; cox_HR(cox_wt, 3) ; cox_HR(cox_wt2, 4); cox_HR(cox_wt, 26)
          
          # Assessing proportional hazards
          cox_wt_check <- cox.zph(cox_wt)
          print(cox_wt_check)
          plot(cox_wt_check, col = "red")
          # ggcoxzph(cox_wt_check) # is slower to print
          
          

