
# SES x Lifestyle Differential Vulnerability & Exposure Project
# Assumptions of Aalen Models File


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
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions
memory.limit(size=1e+13)


# Specify the data and output file locations
data    <- "SIMAH_workspace/nhis/Data"
output  <- "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/"
source("Function - Format Results.R")


# Load data
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))


# ASSUMPTIONS, Additive Hazard Models ------------------------------------------------------------------------------------------------------------------

# First, check the time-invariant assumption (in our case, referred to as 'age-invariant'); whether the effect of covariates 
# is age-varying or constant with time (similar to proportional hazard assumption in Cox models). The "const()" wrapper is 
# used to make the effect of a variable age-invariant; without this wrapper the effect of the variable will be age-varying. 
# Start by fitting the model where all components of the model have age-varying effects, then iteratively simplify the model 
# by making the variables age-invariant one at a time (based on the plot and the Kolmogorov-Smirnov / Cramer von Mises tests). 

# Ultimately, the variables that are part of an interaction have to have a age-invariant effect, and sensitivity analyses 
# (stratifying by age group) were  ran to examine the potential impact if the assumption was violated. 

# For more details and theoretical justification/description see:
      # Rod et al. 2012 https://doi.org/10.1097/EDE.0b013e31825fa218
      # Scheike TH, Martinussen T. Dynamic Regression models for survival data: Springer, NY.; 2006.

      
      
# Assumption: Alcohol x Education *********************************************************************************************************************
# *****************************************************************************************************************************************************

## WOMEN: Checking assumptions for Alcohol x Education model 
# Iteration 1 - Start with all variables as age-varying
model <- "_alc_f_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + edu.factor + married.factor + 
                                                              ethnicity.factor + factor(srvy_yr), data = nhis_female)
        saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # RESULT: SrvyYear should be made age-invariant

    
# Iteration 2 
model <- "_alc_f_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + edu.factor + married.factor + 
                                                              ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                  saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
                  pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                  assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
                  summary(assump_aalen)
                  # RESULT: Marital Status should be made age-invariant
    
       
# Iteration 3 
model <- "_alc_f_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + edu.factor + const(married.factor) + 
                                                                            ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                # RESULT: Education should be made age-invariant
        
            
                
# Iteration 4
model <- "_alc_f_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) +  const(married.factor) +
                                                            ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                 # RESULT: alcohol (former, high risk) and ethnicity should be kept age-varying  
        
    
              
         
                     
              
## MEN: Checking assumptions for Alcohol x Education model 
# Iteration 1 - Start with all variables as age-varying
model <- "_alc_m_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + edu.factor + married.factor + 
                  ethnicity.factor + factor(srvy_yr), data = nhis_male)
          saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
          pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
          assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
          summary(assump_aalen)
          # RESULT: SrvyYear should be made age-invariant

          
# Iteration 2 
model <- "_alc_m_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + edu.factor + married.factor + 
              ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
          saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
          pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
          assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
          summary(assump_aalen)
          # RESULT: Education should be made age-invariant
              
        
# Iteration 3
model <- "_alc_m_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + 
                                                             married.factor + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                      saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
                      pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                      assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
                      summary(assump_aalen)
                      # RESULT: Marital Status should be made age-invariant
              
              
# Iteration 4
model <- "_alc_m_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + 
                                                     const(married.factor) + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                    saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                    # RESULT: alcohol (former, high risk) and ethnicity should be kept age-varying 
              
              
                      
              
              
                                 
           
# Assumption: Smoking x Education **********************************************************************************************************************
# ******************************************************************************************************************************************************
              
## WOMEN: 
# Iteration 1
model <- "_smk_f_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + married.factor + 
                                                           ethnicity.factor + factor(srvy_yr), data = nhis_female)
      saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: SrvyYear should be made age-invariant
      
      
# Iteration 2     
model <- "_smk_f_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + married.factor + 
                                                  ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
      saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: Married should be made age-invariant              
      
      
      
# Iteration 3
model <- "_smk_f_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + 
                                                   const(married.factor) + ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                      saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
                      pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                      assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
                      summary(assump_aalen)
                      # RESULT: Smoking, Education (highschool) and ethnicity should be age-varying
             
              
              
                      
              
## MEN: 
# Iteration 1
model <- "_smk_m_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + married.factor +
                                                       ethnicity.factor + factor(srvy_yr), data = nhis_male)
          saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
          pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
          assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
          summary(assump_aalen)
           # RESULT: SrvyYear should be made age-invariant
          
          
          
# Iteration 2
model <- "_smk_m_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + married.factor +
                                        ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
          saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
          pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
          assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
          summary(assump_aalen)
          # RESULT: Education should be made age-invariant
          

                      
# Iteration 3 
model <- "_smk_m_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + const(edu.factor) + 
                                                       married.factor + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                    saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                   # RESULT: Marital Status should be made age-invariant
              
              
# Iteration 4
model <- "_smk_m_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + const(edu.factor) + 
                                                     const(married.factor) + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                    saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                    # RESULT: Smoking (former, everyday) and ethnicity should be kept age-varying 
              
           
              
              
 
              
              
              
                           
# Assumption: BMI x Education ********************************************************************************************************************
# ************************************************************************************************************************************************
              
## WOMEN: 
# Iteration 1
model <- "_bmi_f_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + edu.factor + married.factor + 
                                                    ethnicity.factor + factor(srvy_yr), data = nhis_female)
        saveRDS(assump_aalen, paste0(output,  "assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output,  "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output,  "assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # RESULT: SrvyYear should be made age-invariant
        
        
        
# Iteration 2
model <- "_bmi_f_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + edu.factor + married.factor + 
                                                ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
        saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # RESULT: Education should be made age-invariant
        
              
      
# Iteration 3 
model <- "_bmi_f_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + 
                                          married.factor + ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                    saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                    # RESULT: Marital Status should be made age-invariant
              
              
# Iteration 4
model <- "_bmi_f_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + 
                                  const(married.factor) + ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                    saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                    # RESULT: BMI (Obese) can be made age-invariant
              
              
              
              
              
## MEN: 
# Iteration 1
model <- "_bmi_m_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + edu.factor + married.factor + 
                                                       ethnicity.factor + factor(srvy_yr), data = nhis_male)
      saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: SrvyYear should be made age-invariant
      
      
# Iteration 2 
model <- "_bmi_m_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + edu.factor + married.factor + 
                                                ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
      saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: Education should be made age-invariant

                    
      
# Iteration 3
model <- "_bmi_m_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + 
                                                     married.factor + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                  saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                  pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                  assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                  summary(assump_aalen)
                  # RESULT: Marital Status should be made age-invariant
              
                  
              
# Iteration 4
model <- "_bmi_m_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + 
                                                    const(married.factor) + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                    saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                   # RESULT: BMI (Obese) can be made age-invariant
              
              
           
              
              
              
              
              
              
# Assumption: Physical Activity x Education **********************************************************************************************************
# ****************************************************************************************************************************************************
              
## WOMEN
# Iteration 1
model <- "_phy_f_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + edu.factor + married.factor + 
                                                     ethnicity.factor + factor(srvy_yr), data = nhis_female)
      saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: SrvyYear should be made age-invariant
      
      
# Iteration 2
model <- "_phy_f_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + edu.factor + married.factor + 
                                                  ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
      saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: Education should be made age-invariant
              
      
      
# Iteration 3
model <- "_phy_f_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + 
                                                 married.factor + ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                  saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                  pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                  assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                  summary(assump_aalen)
                  # RESULT: Marital Status should be made age-invariant
              
              
                  
# Iteration 4
model <- "_phy_f_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + 
                                           const(married.factor) + ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                    saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                    # RESULT: Physical activity should be made age-invariant
              
              
              
              
## MEN
# Iteration 1
model <- "_phy_m_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + edu.factor + married.factor + 
                                             ethnicity.factor + factor(srvy_yr), data = nhis_male)
      saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: SrvyYear should be made age-invariant
          
      
      
# Iteration 2
model <- "_phy_m_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + edu.factor + married.factor + 
                                                             ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
      saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: Education should be made age-invariant
      
      
# Iteration 3 
model <- "_phy_m_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + 
                                             married.factor + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                    saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                    # RESULT: Marital Status should be made age-invariant
              
              
# Iteration 4
model <- "_phy_m_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + 
                                          const(married.factor) + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                    saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                   # RESULT: Physical Activity (Sedentary) and ethnicity should be kept age-varying 
              
             
              

              
              
                            
# Assumption: Causal Mediation ********************************************************************************************************************
# *************************************************************************************************************************************************

# To check the time-invariant assumption, run the full model with all variables as age-varying; then simplify by making variables
# age-invariant (those not significant in the Kolmogorov-Smirnov / Cramer von Mises test)


# FEMALES
model <- "_CMed_f_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  edu.factor + alcohol5v2.factor + smoking4.factor +
                                               bmi_cat.factor + phy_act3.factor + married.factor + ethnicity.factor, data=nhis_female)    
        saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # Result: Marital status should be made age-invariant
        
        
# Iteration 2 
model <- "_CMed_f_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  edu.factor + alcohol5v2.factor + smoking4.factor +
                                              bmi_cat.factor + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_female)
                saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                # Result: education should be made age-invariant
        
        
        
# Iteration 3
model <- "_CMed_f_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + alcohol5v2.factor + smoking4.factor +
                                              bmi_cat.factor + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_female)
                saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                # Result: BMI should be made age-invariant
        
        
# Iteration 4 
model <- "_CMed_f_4"   # Used to name the files appropriately
                assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + alcohol5v2.factor + smoking4.factor +
                                       const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_female)
                saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                # Result: alcohol should be made age-invariant
        
        
# Iteration 5
model <- "_CMed_f_5"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + const(alcohol5v2.factor) + smoking4.factor +
                                        const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_female)
                saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
        
        
        # Final result: 
        # Age-invariant variables: marital status, education, BMI, alcohol
        # Age-varying variables: smoking4, physical activity, race/ethnicity




# MALES
model <- "_CMed_m_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  edu.factor + alcohol5v2.factor + smoking4.factor +
                                            bmi_cat.factor + phy_act3.factor + married.factor + ethnicity.factor, data=nhis_male)
        saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # Result: marital status should be made age-invariant
        
        
# Iteration 2 
model <- "_CMed_m_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  edu.factor + alcohol5v2.factor + smoking4.factor +
                                             bmi_cat.factor + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_male)
                saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                # Result: Education should be made age-invariant
        
        
        
# Iteration 3   
model <- "_CMed_m_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + alcohol5v2.factor + smoking4.factor +
                                          bmi_cat.factor + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_male)
                saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                # Result: BMI should be made age-invariant
        
        
              
# Iteration 4 
model <- "_CMed_m_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + alcohol5v2.factor + smoking4.factor +
                                    const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_male)
                saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                # Result: Alcohol should be made age-invariant
        
        
# Iteration 5  
model <- "_CMed_m_5"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + const(alcohol5v2.factor) + smoking4.factor +
                                     const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_male)
                saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                
        # Final result: 
        # Age-invariant variables: marital status, education, BMI, alcohol
        # Age-varying variables: smoking, physical activity, race/ethnicity




# ASSUMPTIONS, Additive Hazard Models - Stratified by age ------------------------------------------------------------------------------------------------------------------------------------------ 
    
# Assumption: Alcohol x Education ********************************************************************************************************************
# ****************************************************************************************************************************************************

# FEMALES

# Time-varying covariates
# Iteration 1 - Ages 25-65
aalen_alc_f_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
  start.time=25, max.time=64.999, data = nhis_female)
saveRDS(aalen_alc_f_assump1_tvc2565, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_f_assump1_tvc2565.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_f_assump1_tvc2565.pdf"); plot(aalen_alc_f_assump1_tvc2565); dev.off()   # save plot 
aalen_alc_f_assump1_tvc2565 <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_f_assump1_tvc2565.rds")               # load model results
summary(aalen_alc_f_assump1_tvc2565)
   


# Iteration 1 - Ages 65+
aalen_alc_f_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
  start.time=65, data = nhis_female)
saveRDS(aalen_alc_f_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_f_assump1_tvc65up.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_f_assump1_tvc65up.pdf"); plot(aalen_alc_f_assump1_tvc65up); dev.off()   # save plot 
aalen_alc_f_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_f_assump1_tvc65up.rds")               # load model results
summary(aalen_alc_f_assump1_tvc65up)
   
    
    
  
    
# MALES
    
# Iteration 1 - Ages 25-65
aalen_alc_m_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
  start.time=25, max.time=64.999, data = nhis_male)
saveRDS(aalen_alc_m_assump1_tvc2565, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc2565.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc2565.pdf"); plot(aalen_alc_m_assump1_tvc2565); dev.off()   # save plot 
aalen_alc_m_assump1_tvc2565 <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc2565.rds")               # load model results
summary(aalen_alc_m_assump1_tvc2565)
   


# Iteration 1 - Ages 65+
aalen_alc_m_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
  start.time=65, data = nhis_male)
saveRDS(aalen_alc_m_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc65up.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc65up.pdf"); plot(aalen_alc_m_assump1_tvc65up); dev.off()   # save plot 
aalen_alc_m_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc65up.rds")               # load model results
summary(aalen_alc_m_assump1_tvc65up)
   
    
    
    
# Assumption: Smoking x Education **********************************************************************************************************
# ****************************************************************************************************************************************************
    
# FEMALES
    
# Iteration 1 - Ages 25-65
aalen_smk_f_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + const(married.factor) + ethnicity.factor, 
  start.time=25, max.time=64.999, data = nhis_female)
saveRDS(aalen_smk_f_assump1_tvc2565, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_f_assump1_tvc2565.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_f_assump1_tvc2565.pdf"); plot(aalen_smk_f_assump1_tvc2565); dev.off()   # save plot 
aalen_smk_f_assump1_tvc2565 <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_f_assump1_tvc2565.rds")               # load model results
summary(aalen_smk_f_assump1_tvc2565)
   


# Iteration 1 - Ages 65+
aalen_smk_f_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + const(married.factor) + ethnicity.factor, 
  start.time=65, data = nhis_female)
saveRDS(aalen_smk_f_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_f_assump1_tvc65up.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_f_assump1_tvc65up.pdf"); plot(aalen_smk_f_assump1_tvc65up); dev.off()   # save plot 
aalen_smk_f_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_f_assump1_tvc65up.rds")               # load model results
summary(aalen_smk_f_assump1_tvc65up)
   
    
    
    
    
# MALES
# Iteration 1 - Ages 25-65
aalen_smk_m_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
  start.time=25, max.time=64.999, data = nhis_male)
saveRDS(aalen_smk_m_assump1_tvc2565, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc2565.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc2565.pdf"); plot(aalen_smk_m_assump1_tvc2565); dev.off()   # save plot 
aalen_smk_m_assump1_tvc2565 <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc2565.rds")               # load model results
summary(aalen_smk_m_assump1_tvc2565)
   


# Iteration 1 - Ages 65+
aalen_smk_m_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
  start.time=65, data = nhis_male)
saveRDS(aalen_smk_m_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc65up.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc65up.pdf"); plot(aalen_smk_m_assump1_tvc65up); dev.off()   # save plot 
aalen_smk_m_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc65up.rds")               # load model results
summary(aalen_smk_m_assump1_tvc65up)
   
    
    
    
    
    
# Assumption: BMI x Education **********************************************************************************************************
# ****************************************************************************************************************************************************
    
# FEMALES
# Iteration 1 - Ages 25-65
aalen_bmi_f_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
  start.time=25, max.time=64.999, data = nhis_female)
saveRDS(aalen_bmi_f_assump1_tvc2565, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1_tvc2565.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1_tvc2565.pdf"); plot(aalen_bmi_f_assump1_tvc2565); dev.off()   # save plot 
aalen_bmi_f_assump1_tvc2565 <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1_tvc2565.rds")               # load model results
summary(aalen_bmi_f_assump1_tvc2565)  
 


# Iteration 1 - Ages 65+
aalen_bmi_f_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
  start.time=65, data = nhis_female)
saveRDS(aalen_bmi_f_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1_tvc65up.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1_tvc65up.pdf"); plot(aalen_bmi_f_assump1_tvc65up); dev.off()   # save plot 
aalen_bmi_f_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1_tvc65up.rds")               # load model results
summary(aalen_bmi_f_assump1_tvc65up) 
 
    
    



# MALES
# Iteration 1 - Ages 25-65
aalen_bmi_m_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
  start.time=25, max.time=64.999,  data = nhis_male)
saveRDS(aalen_bmi_m_assump1_tvc2565, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc2565.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc2565.pdf"); plot(aalen_bmi_m_assump1_tvc2565); dev.off()   # save plot 
aalen_bmi_m_assump1_tvc2565 <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc2565.rds")               # load model results
summary(aalen_bmi_m_assump1_tvc2565)
 

# Iteration 1 - Ages 65+
aalen_bmi_m_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
  start.time=65, data = nhis_male)
saveRDS(aalen_bmi_m_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc65up.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc65up.pdf"); plot(aalen_bmi_m_assump1_tvc65up); dev.off()   # save plot 
aalen_bmi_m_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc65up.rds")               # load model results
summary(aalen_bmi_m_assump1_tvc65up)
 



# Assumption: Physical Activity x Education **********************************************************************************************************
# ****************************************************************************************************************************************************

# FEMALE
# Iteration 1 - Ages 25-65
aalen_phy_f_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
  start.time=25, max.time=64.999, data = nhis_female)
saveRDS(aalen_phy_f_assump1_tvc2565, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_f_assump1_tvc2565.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_f_assump1_tvc2565.pdf"); plot(aalen_phy_f_assump1_tvc2565); dev.off()   # save plot 
aalen_phy_f_assump1_tvc2565 <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_f_assump1_tvc2565.rds")               # load model results
summary(aalen_phy_f_assump1_tvc2565)
 

# Iteration 1 - Ages 65+
aalen_phy_f_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
  start.time=65, data = nhis_female)
saveRDS(aalen_phy_f_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_f_assump1_tvc65up.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_f_assump1_tvc65up.pdf"); plot(aalen_phy_f_assump1_tvc65up); dev.off()   # save plot 
aalen_phy_f_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_f_assump1_tvc65up.rds")               # load model results
summary(aalen_phy_f_assump1_tvc65up)
 



# MALE
# Iteration 1 - Ages 25-65
aalen_phy_m_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
  start.time=25, max.time=64.999, data = nhis_male)
saveRDS(aalen_phy_m_assump1_tvc2565, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc2565.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc2565.pdf"); plot(aalen_phy_m_assump1_tvc2565); dev.off()   # save plot 
aalen_phy_m_assump1_tvc2565 <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc2565.rds")               # load model results
summary(aalen_phy_m_assump1_tvc2565)
 

# Iteration 1 - Ages 65+
aalen_phy_m_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
  start.time=65, data = nhis_male)
saveRDS(aalen_phy_m_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc65up.rds");               # Save model results
pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc65up.pdf"); plot(aalen_phy_m_assump1_tvc65up); dev.off()   # save plot 
aalen_phy_m_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc65up.rds")               # load model results
summary(aalen_phy_m_assump1_tvc65up)
 



