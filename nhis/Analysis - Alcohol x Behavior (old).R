

### SIMAH - NHIS Data
### SES x Health Behavior interaction and mediation


# LOAD DATA AND SET FILE LOCATIONS ----------------------------------------------------------------------------------

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



# Set the working directory and other file locations



# Set the working directory and other file locations

# # Personal Computer:
kp <- "C:/Users/klajd/OneDrive/SIMAH"
setwd(kp)
data    <- "SIMAH_workspace/nhis/Data"
output  <- "SIMAH_workspace/nhis/SES x Behavior/Output/"
source("SIMAH_code/nhis/Function_CausalMed_Results.R")
source("SIMAH_code/nhis/Function_Formatted_results.R")


# HCC Server:
# kp <- "/external/mgmt3/imaging/scratch/Imhpr/kpuka/nhis/"
# setwd(kp)
# data    <- "Data"
# output  <- "Output/" 
# source("Function_CausalMed_Results.R")

    
# Load data
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))



# DESCRIPTIVES -----------------------------------------------------------------------------------------------------------

# Participant characteristics 
tab1 <-CreateTableOne(vars= c("yrs_followup","allcause_death.factor", "age",
                              "alcohol5v2.factor","smoking4.factor",  "bmi_cat.factor", "phy_act3.factor",
                               "ethnicity.factor",  "married.factor", "employed.factor", "income.factor"),
                      factorVars = c("allcause_death.factor",
                                      "alcohol5v2.factor", "smoking4.factor", "bmi_cat.factor", "phy_act3.factor",
                                     "ethnicity.factor",  "married.factor", "employed.factor", "income.factor"), 
                      strata= c("edu.factor", "female.factor"), addOverall = TRUE, data=nhis)
  table1_v1 <- print(tab1, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)  # Shows sample size and %
  table1_v2 <- print(tab1, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE, format="p") # shows % only
  write.csv(table1_v1, file = file.path(output, "Table1 Demographics_V1.csv"))
  write.csv(table1_v2, file = file.path(output, "Table1 Demographics_V2.csv"))
  kableone(table1_v1)

  
  
# Person years and death rate: tstop = person years; event = # events; rate = events per person year 
survRate(Surv(yrs_followup, allcause_death)~1, data=nhis)          # overall 
survRate(Surv(yrs_followup, allcause_death)~edu, data=nhis)        # for each SES category 
survRate(Surv(yrs_followup, allcause_death)~female+edu, data=nhis) # for each SES * Sex category 
 

# Survival plot 
ggsurvplot_facet(fit = survfit(Surv(bl_age, end_age, allcause_death) ~ edu, data = nhis), 
  data=nhis, facet.by="female.factor", censor = FALSE,xlim = c(25, 100), 
  conf.int = TRUE, 
  legend.labs = c("Low SES", "Medium SES", "High SES"),
  xlab = "Age (years)", 
  ylab = "Overall survival probability") 

      # Age Medium Survival:
      survfit(Surv(bl_age, end_age, allcause_death) ~ edu, data = nhis)
      survfit(Surv(bl_age, end_age, allcause_death) ~ edu, data = nhis_female)
      survfit(Surv(bl_age, end_age, allcause_death) ~ edu, data = nhis_male)


      
      
# ASSUMPTIONS: Additive Hazard Models ------------------------------------------------------------------------------------------------------------------

# First, check time-invariant assumption; whether the effect of covariates is time-varying or constant with time (similar to proportional hazard assumption in Cox models).
# To make the effect of a variable constant with time, use the wrapper "const()" around the variable; without this wrapper the effect of the variable will be time-varying.
# Start by fitting the model where all components of the model have time-varying effects (i.e. don't use the const() wrapper)  
# Then start to simply model by a number of successive tests to making variables age-invariant; those that have a straight line in the plot and/or are not significant in the Kolmogorov-Smirnov / Cramer von Mises test
# Ultimately, the variables that are part of an interaction have to have a age-invariant effect (use const() wrapper).
      # and sensitivity analyses (stratifying by age group) will be ran to examine the potential impact of violating the assumption (if the assumption was violated)

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
        saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # RESULT: SrvyYear should be made age-invariant

    
# Iteration 2 
model <- "_alc_f_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + edu.factor + married.factor + 
                                                              ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                  saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                  pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                  assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
                  summary(assump_aalen)
                  # RESULT: Marital Status should be made age-invariant
    
       
# Iteration 3 
model <- "_alc_f_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + edu.factor + const(married.factor) + 
                                                                            ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                # RESULT: Education should be made age-invariant
        
            
                
# Iteration 4
model <- "_alc_f_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) +  const(married.factor) +
                                                            ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                 # RESULT: alcohol (former, high risk) and ethnicity should be kept age-varying  
        
    
              
         
                     
              
## MEN: Checking assumptions for Alcohol x Education model 
# Iteration 1 - Start with all variables as age-varying
model <- "_alc_m_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + edu.factor + married.factor + 
                  ethnicity.factor + factor(srvy_yr), data = nhis_male)
          saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
          pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
          assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
          summary(assump_aalen)
          # RESULT: SrvyYear should be made age-invariant

          
# Iteration 2 
model <- "_alc_m_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + edu.factor + married.factor + 
              ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
          saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
          pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
          assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
          summary(assump_aalen)
          # RESULT: Education should be made age-invariant
              
        
# Iteration 3
model <- "_alc_m_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + 
                                                             married.factor + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                      saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                      pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                      assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
                      summary(assump_aalen)
                      # RESULT: Marital Status should be made age-invariant
              
              
# Iteration 4
model <- "_alc_m_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + 
                                                     const(married.factor) + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                    saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                    # RESULT: alcohol (former, high risk) and ethnicity should be kept age-varying 
              
              
                      
              
              
                                 
           
# Assumption: Smoking x Education **********************************************************************************************************************
# ******************************************************************************************************************************************************
              
## WOMEN: 
# Iteration 1
model <- "_smk_f_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + married.factor + 
                                                           ethnicity.factor + factor(srvy_yr), data = nhis_female)
      saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: SrvyYear should be made age-invariant
      
      
# Iteration 2     
model <- "_smk_f_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + married.factor + 
                                                  ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
      saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: Married should be made age-invariant              
      
      
      
# Iteration 3
model <- "_smk_f_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + 
                                                   const(married.factor) + ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                      saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                      pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                      assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
                      summary(assump_aalen)
                      # RESULT: Smoking, Education (highschool) and ethnicity should be age-varying
             
              
              
                      
              
## MEN: 
# Iteration 1
model <- "_smk_m_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + married.factor +
                                                       ethnicity.factor + factor(srvy_yr), data = nhis_male)
          saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
          pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
          assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
          summary(assump_aalen)
           # RESULT: SrvyYear should be made age-invariant
          
          
          
# Iteration 2
model <- "_smk_m_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + married.factor +
                                        ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
          saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
          pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
          assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
          summary(assump_aalen)
          # RESULT: Education should be made age-invariant
          

                      
# Iteration 3 
model <- "_smk_m_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + const(edu.factor) + 
                                                       married.factor + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                    saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                   # RESULT: Marital Status should be made age-invariant
              
              
# Iteration 4
model <- "_smk_m_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + const(edu.factor) + 
                                                     const(married.factor) + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                    saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                    # RESULT: Smoking (former, everyday) and ethnicity should be kept age-varying 
              
           
              
              
 
              
              
              
                           
# Assumption: BMI x Education ********************************************************************************************************************
# ************************************************************************************************************************************************
              
## WOMEN: 
# Iteration 1
model <- "_bmi_f_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + edu.factor + married.factor + 
                                                    ethnicity.factor + factor(srvy_yr), data = nhis_female)
        saveRDS(assump_aalen, paste0(output,  "Assumptions/assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output,  "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output,  "Assumptions/assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # RESULT: SrvyYear should be made age-invariant
        
        
        
# Iteration 2
model <- "_bmi_f_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + edu.factor + married.factor + 
                                                ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
        saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # RESULT: Education should be made age-invariant
        
              
      
# Iteration 3 
model <- "_bmi_f_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + 
                                          married.factor + ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                    saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                    # RESULT: Marital Status should be made age-invariant
              
              
# Iteration 4
model <- "_bmi_f_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + 
                                  const(married.factor) + ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                    saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                    # RESULT: BMI (Obese) can be made age-invariant
              
              
              
              
              
## MEN: 
# Iteration 1
model <- "_bmi_m_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + edu.factor + married.factor + 
                                                       ethnicity.factor + factor(srvy_yr), data = nhis_male)
      saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: SrvyYear should be made age-invariant
      
      
# Iteration 2 
model <- "_bmi_m_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + edu.factor + married.factor + 
                                                ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
      saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: Education should be made age-invariant

                    
      
# Iteration 3
model <- "_bmi_m_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + 
                                                     married.factor + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                  saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                  pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                  assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
                  summary(assump_aalen)
                  # RESULT: Marital Status should be made age-invariant
              
                  
              
# Iteration 4
model <- "_bmi_m_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + 
                                                    const(married.factor) + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                    saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                   # RESULT: BMI (Obese) can be made age-invariant
              
              
           
              
              
              
              
              
              
# Assumption: Physical Activity x Education **********************************************************************************************************
# ****************************************************************************************************************************************************
              
## WOMEN
# Iteration 1
model <- "_phy_f_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + edu.factor + married.factor + 
                                                     ethnicity.factor + factor(srvy_yr), data = nhis_female)
      saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: SrvyYear should be made age-invariant
      
      
# Iteration 2
model <- "_phy_f_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + edu.factor + married.factor + 
                                                  ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
      saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: Education should be made age-invariant
              
      
      
# Iteration 3
model <- "_phy_f_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + 
                                                 married.factor + ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                  saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                  pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                  assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
                  summary(assump_aalen)
                  # RESULT: Marital Status should be made age-invariant
              
              
                  
# Iteration 4
model <- "_phy_f_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + 
                                           const(married.factor) + ethnicity.factor + const(factor(srvy_yr)), data = nhis_female)
                    saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                    # RESULT: Physical activity should be made age-invariant
              
              
              
              
## MEN
# Iteration 1
model <- "_phy_m_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + edu.factor + married.factor + 
                                             ethnicity.factor + factor(srvy_yr), data = nhis_male)
      saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: SrvyYear should be made age-invariant
          
      
      
# Iteration 2
model <- "_phy_m_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + edu.factor + married.factor + 
                                                             ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
      saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
      # RESULT: Education should be made age-invariant
      
      
# Iteration 3 
model <- "_phy_m_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + 
                                             married.factor + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                    saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
                    summary(assump_aalen)
                    # RESULT: Marital Status should be made age-invariant
              
              
# Iteration 4
model <- "_phy_m_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + 
                                          const(married.factor) + ethnicity.factor + const(factor(srvy_yr)), data = nhis_male)
                    saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                    pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                    assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
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
        saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # Result: Marital status should be made age-invariant
        
        
# Iteration 2 
model <- "_CMed_f_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  edu.factor + alcohol5v2.factor + smoking4.factor +
                                              bmi_cat.factor + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_female)
                saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                # Result: education should be made age-invariant
        
        
        
# Iteration 3
model <- "_CMed_f_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + alcohol5v2.factor + smoking4.factor +
                                              bmi_cat.factor + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_female)
                saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                # Result: BMI should be made age-invariant
        
        
# Iteration 4 
model <- "_CMed_f_4"   # Used to name the files appropriately
                assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + alcohol5v2.factor + smoking4.factor +
                                       const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_female)
                saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                # Result: alcohol should be made age-invariant
        
        
# Iteration 5
model <- "_CMed_f_5"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + const(alcohol5v2.factor) + smoking4.factor +
                                        const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_female)
                saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
        
        
        # Final result: 
        # Age-invariant variables: marital status, education, BMI, alcohol
        # Age-varying variables: smoking4, physical activity, race/ethnicity




# MALES
model <- "_CMed_m_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  edu.factor + alcohol5v2.factor + smoking4.factor +
                                            bmi_cat.factor + phy_act3.factor + married.factor + ethnicity.factor, data=nhis_male)
        saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # Result: marital status should be made age-invariant
        
        
# Iteration 2 
model <- "_CMed_m_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  edu.factor + alcohol5v2.factor + smoking4.factor +
                                             bmi_cat.factor + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_male)
                saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                # Result: Education should be made age-invariant
        
        
        
# Iteration 3   
model <- "_CMed_m_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + alcohol5v2.factor + smoking4.factor +
                                          bmi_cat.factor + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_male)
                saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                # Result: BMI should be made age-invariant
        
        
              
# Iteration 4 
model <- "_CMed_m_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + alcohol5v2.factor + smoking4.factor +
                                    const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_male)
                saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                # Result: Alcohol should be made age-invariant
        
        
# Iteration 5  
model <- "_CMed_m_5"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  const(edu.factor) + const(alcohol5v2.factor) + smoking4.factor +
                                     const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + ethnicity.factor, data=nhis_male)
                saveRDS(assump_aalen, paste0(output, "Assumptions/assump_aalen", model, ".rds"))                # Save model results
                pdf(paste0(output, "Assumptions/assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
                assump_aalen <-readRDS(paste0(output, "Assumptions/assump_aalen", model, ".rds"))               # load model results
                summary(assump_aalen)
                
        # Final result: 
        # Age-invariant variables: marital status, education, BMI, alcohol
        # Age-varying variables: smoking, physical activity, race/ethnicity




# OBJECTIVE 1: Joint Effects, Hazard Models - Stratified by Sex---------------------------------------------------------------------------------------------------------------------

# The effect estimates from the model can be directly interpreted as the number of additional events (deaths) per 1 person year at risk
      # Multiple the estimate by, say 10,000, to get the number of additional events per 10,000 person years
      # For more details and theoretical justification/description see:
            # Rod et al. 2012 https://doi.org/10.1097/EDE.0b013e31825fa218
            # Scheike TH, Martinussen T. Dynamic Regression models for survival data: Springer, NY.; 2006.         
                

                            
## Education * Alcohol 
    ## WOMEN
    model <- "_alc_f"   # Used to name the files appropriately: specify health behavior and sex strata
    aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2.factor) + 
                                const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
             # Save model and plots
             saveRDS(aalen, paste0(output, "Interaction/aalen", model, ".rds"))                
             pdf(paste0(output, "Interaction/aalen", model, ".pdf")); plot(aalen); dev.off()  
             #load model and results
             aalen <-readRDS(paste0(output, "Interaction/aalen", model, ".rds"))             
             aalen_10000py(aalen, 1); aalen_10000py(aalen, 6); aalen_10000py(aalen, 31);
             summary(aalen)
             
             
             
    ## MEN
    model <- "_alc_m"   # Used to name the files appropriately: specify health behavior and sex strata
    aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2.factor) + 
                              const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
           # Save model and plots
           saveRDS(aalen, paste0(output, "Interaction/aalen", model, ".rds"))                
           pdf(paste0(output, "Interaction/aalen", model, ".pdf")); plot(aalen); dev.off()    
           # load model and results
           aalen <-readRDS(paste0(output, "Interaction/aalen", model, ".rds"))        
           aalen_10000py(aalen, 1); aalen_10000py(aalen, 6); aalen_10000py(aalen, 31);
           summary(aalen)
           
   
             
                           
##Smoking * Education 
             
      ## WOMEN
      model <- "_smk_f"   # Used to name the files appropriately: specify health behavior and sex strata
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(smoking4.factor) + 
                 const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
            # Save model and plots
            saveRDS(aalen, paste0(output, "Interaction/aalen", model, ".rds"))                
            pdf(paste0(output, "Interaction/aalen", model, ".pdf")); plot(aalen); dev.off()    
            # load model and results
            aalen <-readRDS(paste0(output, "Interaction/aalen", model, ".rds"))        
            aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen, 28);
            summary(aalen)      
             
             
      ## MEN
      model <- "_smk_m"   # Used to name the files appropriately: specify health behavior and sex strata
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(smoking4.factor) + 
                 const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
            # Save model and plots
            saveRDS(aalen, paste0(output, "Interaction/aalen", model, ".rds"))                
            pdf(paste0(output, "Interaction/aalen", model, ".pdf")); plot(aalen); dev.off()    
            # load model and results
            aalen <-readRDS(paste0(output, "Interaction/aalen", model, ".rds"))        
            aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen, 28);
            summary(aalen)
    

             
             
            
## BMI * Education
             
      ## WOMEN
      model <- "_bmi_f"   # Used to name the files appropriately: specify health behavior and sex strata
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(bmi_cat.factor) + 
                 const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
            # Save model and plots
            saveRDS(aalen, paste0(output, "Interaction/aalen", model, ".rds"))                
            pdf(paste0(output, "Interaction/aalen", model, ".pdf")); plot(aalen); dev.off()    
            # load model and results
            aalen <-readRDS(paste0(output, "Interaction/aalen", model, ".rds"))        
            aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen, 28);
            summary(aalen)
             
             
      ## MEN
      model <- "_bmi_m"   # Used to name the files appropriately: specify health behavior and sex strata
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(bmi_cat.factor) + 
                 const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
            # Save model and plots
            saveRDS(aalen, paste0(output, "Interaction/aalen", model, ".rds"))                
            pdf(paste0(output, "Interaction/aalen", model, ".pdf")); plot(aalen); dev.off()    
            # load model and results
            aalen <-readRDS(paste0(output, "Interaction/aalen", model, ".rds"))        
            aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen, 28);
            summary(aalen)
             
             
             
             
             
## Physical Activity * Education
  
      ## WOMEN
      model <- "_phy_f"   # Used to name the files appropriately: specify health behavior and sex strata
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(phy_act3.factor) + 
                 const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
            # Save model and plots
            saveRDS(aalen, paste0(output, "Interaction/aalen", model, ".rds"))                
            pdf(paste0(output, "Interaction/aalen", model, ".pdf")); plot(aalen); dev.off()    
            # load model and results
            aalen <-readRDS(paste0(output, "Interaction/aalen", model, ".rds"))        
            aalen_10000py(aalen, 1); aalen_10000py(aalen, 3); aalen_10000py(aalen, 23);
            summary(aalen)
             
             
      ## MEN
      model <- "_phy_m"   # Used to name the files appropriately: specify health behavior and sex strata
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(phy_act3.factor) + 
                 const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
            # Save model and plots
            saveRDS(aalen, paste0(output, "Interaction/aalen", model, ".rds"))                
            pdf(paste0(output, "Interaction/aalen", model, ".pdf")); plot(aalen); dev.off()    
            # load model and results
            aalen <-readRDS(paste0(output, "Interaction/aalen", model, ".rds"))        
            aalen_10000py(aalen, 1); aalen_10000py(aalen, 3); aalen_10000py(aalen, 23);
            summary(aalen)
      
             


# OBJECTIVE 2: Causal Mediation - FEMALES -------------------------------------------------------------------------------------------------------------

# For more details and theoretical justification/description see:
    # Lange et al. 2014 https//doi.org/10.1093/aje/kwt270
    # Lange et al. 2012 https//doi.org/10.1093/aje/kwr525
    # Lange et al. 2011 https//doi.org/10.1097/EDE.0b013e31821c680c
             
             
### Step 0: Select data to use **************************************************************************************************************************
# *******************************************************************************************************************************************************
mydata <- nhis %>%
  mutate(A.edu = edu,
    M1.alc = alcohol5v2,
    M2.smk = smoking4,
    M3.bmi = bmi_cat,
    M4.phy = phy_act3) %>%
  filter (female.factor=="Female") %>%
  dplyr::select(A.edu, M1.alc, M2.smk, M3.bmi, M4.phy, allcause_death, bl_age, end_age, married, ethnicity, srvy_yr)

    # specifies the reference category
    mydata$A.edu <- factor(mydata$A.edu, levels=c(1,2,3), labels = c("Low", "Med", "High"))
    mydata$A.edu <- relevel(mydata$A.edu, ref = "High")

    # NOTE: For technical reasons, the mediators should be coded as integers starting with 1
    

# Select random subset of the sample (if needed to improve speed of analyses)
# set.seed(1234)
# mydata <- sample_frac(mydata, .10) # selects X% of sample at random






### Step 1: Fit a model for each mediator ***************************************************************************************************************
# *******************************************************************************************************************************************************

# Fit model for each mediator, conditioning on exposure (education) and all confounders

mydata$ATemp <- mydata$A.edu # first, create and use a copy of the exposure variable (for technical reasons related to R)
fitM1 <- vglm(M1.alc ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 3))
fitM2 <- vglm(M2.smk ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 1))
fitM3 <- vglm(M3.bmi ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 2))
fitM4 <- vglm(M4.phy ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 3))






### Step 2: Construct copies of ID and exposure *********************************************************************************************************
# *******************************************************************************************************************************************************

#Create ID Variable
mydata$ID <- 1:nrow(mydata) # construct id variable

# Create counterfactual version of exposure (education); repeated 4 times because there are 4 mediators
levelsOfEDU <- unique(mydata$A.edu)
myData1 <- mydata
myData2 <- mydata
myData3 <- mydata
myData1$edu_M1.alc <- levelsOfEDU[1]
myData2$edu_M1.alc <- levelsOfEDU[2]
myData3$edu_M1.alc <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$edu_M2.smk <- levelsOfEDU[1]
myData2$edu_M2.smk <- levelsOfEDU[2]
myData3$edu_M2.smk <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$edu_M3.bmi <- levelsOfEDU[1]
myData2$edu_M3.bmi <- levelsOfEDU[2]
myData3$edu_M3.bmi <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$edu_M4.phy <- levelsOfEDU[1]
myData2$edu_M4.phy <- levelsOfEDU[2]
myData3$edu_M4.phy <- levelsOfEDU[3]
newMyData <- rbind(myData1, myData2, myData3)






### Step 3: Construct weights  *********************************************************************************************************************
# **************************************************************************************************************************************************

# M1: alcohol
newMyData$ATemp <- newMyData$A.edu
tempDir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]

newMyData$ATemp <- newMyData$edu_M1.alc
tempIndir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]

newMyData$weight1 <- tempIndir1/tempDir1


#M2: Smoking
newMyData$ATemp <- newMyData$A.edu
tempDir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]

newMyData$ATemp <- newMyData$edu_M2.smk
tempIndir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]

newMyData$weight2 <- tempIndir2/tempDir2


#M3: BMI
newMyData$ATemp <- newMyData$A.edu
tempDir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]

newMyData$ATemp <- newMyData$edu_M3.bmi
tempIndir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]

newMyData$weight3 <- tempIndir3/tempDir3


#M4: Physical activity
newMyData$ATemp <- newMyData$A.edu
tempDir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]

newMyData$ATemp <- newMyData$edu_M4.phy
tempIndir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]

newMyData$weight4 <- tempIndir4/tempDir4



# Final weight
newMyData$weightM <- newMyData$weight1 * newMyData$weight2 * newMyData$weight3 * newMyData$weight4
hist(newMyData$weightM)


# Remove temporary items
rm(fitM1, fitM2, fitM3, fitM4,
   tempIndir1, tempIndir2, tempIndir3, tempIndir4, 
   tempDir1, tempDir2, tempDir3, tempDir4)


## save expanded data
saveRDS(newMyData, file.path(output, "CausMed/expandedData_fem.rds"))





### Step 4: Fit model *****************************************************************************************************************************
# *************************************************************************************************************************************************

## FEMALES
expandedData <-readRDS(file.path(output, "CausMed/expandedData_fem.rds"))
CMed_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.alc) + 
                                                          const(A.edu) * const(edu_M2.smk) +
                                                          const(A.edu) * const(edu_M3.bmi) +
                                                          const(A.edu) * const(edu_M4.phy) +
                                                          const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                            data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID)
                  saveRDS(CMed_f, file.path(output, "CausMed/CMed_f.rds"))       # Save model results
                  CMed_model <-readRDS(file.path(output, "CausMed/CMed_f.rds"))  # Load model results


                  
# Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
summary(CMed_model)   #Estimates and SE
getTE_Robust(CMed_model, c(1,3,5,7,9,29,33,37,41))  # Simulated estimate and SE for total effect and mediated proportions for other effects
getIE_Robust(CMed_model, c(3,5,7,9,29,33,37,41))    # Estimate and simulated SE for indirect combined effect
getTE_IE_Robust(CMed_model, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) # Mediated proportion and simulated 95% CI for mediated proportion of indirect combined effect




# OBJECTIVE 2: Causal Mediation - MALES -------------------------------------------------------------------------------------------------------------

# For more details and theoretical justification/description see:
    # Lange et al. 2014 https//doi.org/10.1093/aje/kwt270
    # Lange et al. 2012 https//doi.org/10.1093/aje/kwr525
    # Lange et al. 2011 https//doi.org/10.1097/EDE.0b013e31821c680c


### Step 0: Select data and load functions **************************************************************************************************************
# *******************************************************************************************************************************************************
mydata <- nhis %>%
  mutate(A.edu = edu,
    M1.alc = alcohol5v2,
    M2.smk = smoking4,
    M3.bmi = bmi_cat,
    M4.phy = phy_act3) %>%
  filter (female.factor=="Male") %>%
  dplyr::select(A.edu, M1.alc, M2.smk, M3.bmi, M4.phy,
    allcause_death, bl_age, end_age, married, ethnicity, srvy_yr)

# specifies the reference category
mydata$A.edu <- factor(mydata$A.edu, levels=c(1,2,3), labels = c("Low", "Med", "High"))
mydata$A.edu <- relevel(mydata$A.edu, ref = "High")

# NOTE: For technical reasons, the mediators should be coded as integers starting with 1


# Select random subset of the sample
# set.seed(1234)
# mydata <- sample_frac(mydata, .10) # selects X% of sample at random







### Step 1: Fit a model for each mediator ***************************************************************************************************************
# *******************************************************************************************************************************************************

# Fit model for each mediator, conditioning on exposure (education) and confounders

mydata$ATemp <- mydata$A.edu # first, create and use a copy of the exposure variable (for technical reasons related to R)
fitM1 <- vglm(M1.alc ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 3))
fitM2 <- vglm(M2.smk ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 1))
fitM3 <- vglm(M3.bmi ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 2))
fitM4 <- vglm(M4.phy ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 3))





### Step 2: Construct copies of ID and exposure *********************************************************************************************************
# *******************************************************************************************************************************************************

#Create ID Variable
mydata$ID <- 1:nrow(mydata) # construct id variable

# Create counterfactual version of exposure (education); repeated 4 times because there are 4 mediators
levelsOfEDU <- unique(mydata$A.edu)
myData1 <- mydata
myData2 <- mydata
myData3 <- mydata
myData1$edu_M1.alc <- levelsOfEDU[1]
myData2$edu_M1.alc <- levelsOfEDU[2]
myData3$edu_M1.alc <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$edu_M2.smk <- levelsOfEDU[1]
myData2$edu_M2.smk <- levelsOfEDU[2]
myData3$edu_M2.smk <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$edu_M3.bmi <- levelsOfEDU[1]
myData2$edu_M3.bmi <- levelsOfEDU[2]
myData3$edu_M3.bmi <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$edu_M4.phy <- levelsOfEDU[1]
myData2$edu_M4.phy <- levelsOfEDU[2]
myData3$edu_M4.phy <- levelsOfEDU[3]
newMyData <- rbind(myData1, myData2, myData3)






### Step 3: Construct weights  *********************************************************************************************************************
# **************************************************************************************************************************************************

# M1: alcohol
newMyData$ATemp <- newMyData$A.edu
tempDir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]

newMyData$ATemp <- newMyData$edu_M1.alc
tempIndir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]

newMyData$weight1 <- tempIndir1/tempDir1


#M2: Smoking
newMyData$ATemp <- newMyData$A.edu
tempDir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]

newMyData$ATemp <- newMyData$edu_M2.smk
tempIndir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]

newMyData$weight2 <- tempIndir2/tempDir2


#M3: BMI
newMyData$ATemp <- newMyData$A.edu
tempDir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]

newMyData$ATemp <- newMyData$edu_M3.bmi
tempIndir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]


newMyData$weight3 <- tempIndir3/tempDir3


#M4: Physical activity
newMyData$ATemp <- newMyData$A.edu
tempDir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]

newMyData$ATemp <- newMyData$edu_M4.phy
tempIndir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]

newMyData$weight4 <- tempIndir4/tempDir4



# Final weight
newMyData$weightM <- newMyData$weight1 * newMyData$weight2 * newMyData$weight3 * newMyData$weight4
hist(newMyData$weightM)


# Remove temporary items
rm(fitM1, fitM2, fitM3, fitM4,
  tempIndir1, tempIndir2, tempIndir3, tempIndir4, 
  tempDir1, tempDir2, tempDir3, tempDir4)


## save expanded data
saveRDS(newMyData, file.path(output, "CausMed//expandedData_male.rds"))





### Step 4: Fit model *****************************************************************************************************************************
# *************************************************************************************************************************************************

## MALES
expandedData <- readRDS(file.path(output, "CausMed/expandedData_male.rds"))
CMed_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.alc) +
                                                          const(A.edu) * const(edu_M2.smk) +
                                                          const(A.edu) * const(edu_M3.bmi) +
                                                          const(A.edu) * const(edu_M4.phy) +
                                                          const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                                data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID)
                      saveRDS(CMed_m, file.path(output, "CausMed/CMed_m.rds"))       # Save model results
                      CMed_model <-readRDS(file.path(output, "CausMed/CMed_m.rds"))  # Load model results

                      
                      
# Get final results (ensure that the Causal Mediation Functions are loaded). 
# NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
summary(CMed_model)   #Estimates and SE
getTE_Robust(CMed_model, c(1,3,5,7,9,29,33,37,41))  #Simulated estimate and SE for total effect and mediated proportions for other effects
getIE_Robust(CMed_model, c(3,5,7,9,29,33,37,41))    #Estimate and simulated SE for indirect combined effect
getTE_IE_Robust(CMed_model, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) #Mediated proportion and simulated 95% CI for mediated proportion of indirect combined effect




# SENSITIVITY ANALYSES----------------------------------------------------------------------------------------------------------------------------
### Sensitivity 1: Analyses on entire sample---------------------------------------------------------------------------------------------------
########### Hazard Models - All Participants ------------------------------------------------------------------------

## Alcohol x Education 
aalen_allcause_alc_edu <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2.factor) + 
    female.factor + const(married.factor) + ethnicity.factor,  data = nhis, robust=0)
    saveRDS(aalen_allcause_alc_edu, "SIMAH_workspace/nhis/SES x Behavior/Output/interaction/alc/aalen_allcause_alc_edu.rds")                # Save model results
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/interaction/alc/aalen_allcause_alc_edu.rds"); plot(aalen_allcause_alc_edu); dev.off()   # save plot
    aalen_allcause_alc_edu <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/interaction/alc/aalen_allcause_alc_edu.rds")               # load model results
    summary(aalen_allcause_alc_edu)

    
## Smoking x Education 
aalen_allcause_smk_edu <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(smoking4.factor) + 
        female.factor + const(married.factor) + ethnicity.factor,  data = nhis, robust=0)
    saveRDS(aalen_allcause_smk_edu, "SIMAH_workspace/nhis/SES x Behavior/Output/interaction/smk/aalen_allcause_smk_edu.rds")
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/interaction/smk/aalen_allcause_smk_edu.rds"); plot(aalen_allcause_smk_edu); dev.off()
    aalen_allcause_smk_edu <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/interaction/smk/aalen_allcause_smk_edu.rds")
    summary(aalen_allcause_smk_edu)
    
    
    
# BMI x Education
aalen_allcause_bmi_edu <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(bmi_cat.factor) + 
        female.factor + const(married.factor) + ethnicity.factor,  data = nhis, robust=0)
    saveRDS(aalen_allcause_bmi_edu, "SIMAH_workspace/nhis/SES x Behavior/Output/interaction/bmi/aalen_allcause_bmi_edu.rds")
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/interaction/bmi/aalen_allcause_bmi_edu.rds"); plot(aalen_allcause_bmi_edu); dev.off()
    aalen_allcause_bmi_edu <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/interaction/bmi/aalen_allcause_bmi_edu.rds")
    summary(aalen_allcause_bmi_edu)
    
    
    
    
# Physical Activity x Education
aalen_allcause_phy_edu <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(phy_act3.factor) + 
        female.factor + const(married.factor) + ethnicity.factor,  data = nhis, robust=0)
    saveRDS(aalen_allcause_phy_edu, "SIMAH_workspace/nhis/SES x Behavior/Output/interaction/phy/aalen_allcause_phy_edu.rds")
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/interaction/phy/aalen_allcause_phy_edu.rds"); plot(aalen_allcause_phy_edu); dev.off()
    aalen_allcause_phy_edu <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/interaction/phy/aalen_allcause_phy_edu.rds")
    summary(aalen_allcause_phy_edu)
    
    

########### Causal Mediation - All Participants -------------------------------------------------------------------------------------------------------------
    
### Step 0: Select data and load functions **************************************************************************************************************
# *******************************************************************************************************************************************************
mydata <- nhis %>%
  mutate(A.edu = edu,
        M1.alc = alcohol5v2,
        M2.smk = smoking4,
        M3.bmi = bmi_cat,
        M4.phy = phy_act3) %>%
  dplyr::select(A.edu, M1.alc, M2.smk, M3.bmi, M4.phy,
        allcause_death, bl_age, end_age, female, married, ethnicity, srvy_yr)
    
# specifies the reference category
mydata$A.edu <- factor(mydata$A.edu, levels=c(1,2,3), labels = c("Low", "Med", "High"))
mydata$A.edu <- relevel(mydata$A.edu, ref = "High")
    
    # NOTE: For technical reasons, the mediators should be coded as integers starting with 1
    
    
# Select random subset of the sample
set.seed(1234)
mydata <- sample_frac(mydata, .70) # selects X% of sample at random
    
    
    
    
    
### Step 1: Fit a model for each mediator ***************************************************************************************************************
# *******************************************************************************************************************************************************
    
# Fit model for each mediator, conditioning on exposure (education) and confounders
    
mydata$ATemp <- mydata$A.edu # first, create and use a copy of the exposure variable (for technical reasons related to R)
fitM1 <- vglm(M1.alc ~ ATemp + bl_age + female + married + factor(ethnicity), data = mydata, family=multinomial(refLevel = 3))
fitM2 <- vglm(M2.smk ~ ATemp + bl_age + female + married + factor(ethnicity), data = mydata, family=multinomial(refLevel = 1))
fitM3 <- vglm(M3.bmi ~ ATemp + bl_age + female + married + factor(ethnicity), data = mydata, family=multinomial(refLevel = 2))
fitM4 <- vglm(M4.phy ~ ATemp + bl_age + female + married + factor(ethnicity), data = mydata, family=multinomial(refLevel = 3))
    
    
    
    
    
### Step 2: Construct copies of ID and exposure *********************************************************************************************************
# *******************************************************************************************************************************************************
    
#Create ID Variable
mydata$ID <- 1:nrow(mydata) # construct id variable
    
# Create counterfactual version of exposure (education); repeated 4 times because there are 4 mediators
levelsOfEDU <- unique(mydata$A.edu)
myData1 <- mydata
myData2 <- mydata
myData3 <- mydata
myData1$edu_M1.alc <- levelsOfEDU[1]
myData2$edu_M1.alc <- levelsOfEDU[2]
myData3$edu_M1.alc <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)
    
myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$edu_M2.smk <- levelsOfEDU[1]
myData2$edu_M2.smk <- levelsOfEDU[2]
myData3$edu_M2.smk <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)
    
myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$edu_M3.bmi <- levelsOfEDU[1]
myData2$edu_M3.bmi <- levelsOfEDU[2]
myData3$edu_M3.bmi <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)
    
myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$edu_M4.phy <- levelsOfEDU[1]
myData2$edu_M4.phy <- levelsOfEDU[2]
myData3$edu_M4.phy <- levelsOfEDU[3]
newMyData <- rbind(myData1, myData2, myData3)
    
    
    
    
    
    
### Step 3: Construct weights  *********************************************************************************************************************
# **************************************************************************************************************************************************

# M1: alcohol
newMyData$ATemp <- newMyData$A.edu
tempDir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]

newMyData$ATemp <- newMyData$edu_M1.alc
tempIndir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]

newMyData$weight1 <- tempIndir1/tempDir1


#M2: Smoking
newMyData$ATemp <- newMyData$A.edu
tempDir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]

newMyData$ATemp <- newMyData$edu_M2.smk
tempIndir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]

newMyData$weight2 <- tempIndir2/tempDir2


#M3: BMI
newMyData$ATemp <- newMyData$A.edu
tempDir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]

newMyData$ATemp <- newMyData$edu_M3.bmi
tempIndir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]


newMyData$weight3 <- tempIndir3/tempDir3


#M4: Physical activity
newMyData$ATemp <- newMyData$A.edu
tempDir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]

newMyData$ATemp <- newMyData$edu_M4.phy
tempIndir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]

newMyData$weight4 <- tempIndir4/tempDir4



# Final weight
newMyData$weightM <- newMyData$weight1 * newMyData$weight2 * newMyData$weight3 * newMyData$weight4
hist(newMyData$weightM)
    

# Remove temporary items
rm(fitM1, fitM2, fitM3, fitM4,
  tempIndir1, tempIndir2, tempIndir3, tempIndir4, 
  tempDir1, tempDir2, tempDir3, tempDir4)


## save expanded data
saveRDS(newMyData, "data/expandedData_all.rds") 





### Step 4: Fit model *****************************************************************************************************************************
# *************************************************************************************************************************************************

## ALL Participants
expandedData <-readRDS("data/expandedData_f.rds")
CMed_all <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.alc) +
                                                        const(A.edu) * const(edu_M2.smk) +
                                                        const(A.edu) * const(edu_M3.bmi) +
                                                        const(A.edu) * const(edu_M4.phy) +
                                                        const(married) + female + factor(ethnicity) + const(factor(srvy_yr)),
            data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID)
          saveRDS(CMed_all, file.path(output, "CausMed/CMed_all.rds"))    # Save model results
          CMed_model <-readRDS(file.path(output, "CausMed/CMed_all.rds"))  # Load model results


# Direct, indirect and mediated interactive effects and standard errors are derived directly from the summary() command
# Total effect is obtained by the sum of the three separate effects
# Confidence intervals for total effects and mediated proportions are computed using the code below:

# Get final results NOTE: THE NUMBERS BELOW HAVE TO BE CHANGED*********************
summary(CMed_model)   #Estimates and SE
getTE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41))  #Simulated estimate and SE for total effect and mediated proportions for other effects
getIE_NotRobust(CMed_model, c(3,5,7,9,29,33,37,41))    #Estimate and simulated SE for indirect combined effect
getTE_IE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) #Mediated proportion and simulated 95% CI for mediated proportion of indirect combined effect

    
    
    
### Sensitivity 2: XXXXX ------------------------------------------------------------------------------------------------------------------------------
### Sensitivity 3: Stratified by age ------------------------------------------------------------------------------------------------------------------------------
############ Assumptions: Stratified by age ------------------------------------------------------------------------------------------------------------------------------------------ 
    
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
    # RESULT: ??  
    
    
    # Iteration 1 - Ages 65+
    aalen_alc_f_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_female)
    saveRDS(aalen_alc_f_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_f_assump1_tvc65up.rds");               # Save model results
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_f_assump1_tvc65up.pdf"); plot(aalen_alc_f_assump1_tvc65up); dev.off()   # save plot 
    aalen_alc_f_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_f_assump1_tvc65up.rds")               # load model results
    summary(aalen_alc_f_assump1_tvc65up)
    # RESULT: ??  
    
    
  
    
# MALES
    
    # Iteration 1 - Ages 25-65
    aalen_alc_m_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=25, max.time=64.999, data = nhis_male)
    saveRDS(aalen_alc_m_assump1_tvc2565, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc2565.rds");               # Save model results
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc2565.pdf"); plot(aalen_alc_m_assump1_tvc2565); dev.off()   # save plot 
    aalen_alc_m_assump1_tvc2565 <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc2565.rds")               # load model results
    summary(aalen_alc_m_assump1_tvc2565)
    # RESULT: ??  
    
    
    # Iteration 1 - Ages 65+
    aalen_alc_m_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ alcohol5v2.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_male)
    saveRDS(aalen_alc_m_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc65up.rds");               # Save model results
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc65up.pdf"); plot(aalen_alc_m_assump1_tvc65up); dev.off()   # save plot 
    aalen_alc_m_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_alc_m_assump1_tvc65up.rds")               # load model results
    summary(aalen_alc_m_assump1_tvc65up)
    # RESULT: ??  
    
    
    
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
    # RESULT: ??  
    
    
    # Iteration 1 - Ages 65+
    aalen_smk_f_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + edu.factor + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_female)
    saveRDS(aalen_smk_f_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_f_assump1_tvc65up.rds");               # Save model results
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_f_assump1_tvc65up.pdf"); plot(aalen_smk_f_assump1_tvc65up); dev.off()   # save plot 
    aalen_smk_f_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_f_assump1_tvc65up.rds")               # load model results
    summary(aalen_smk_f_assump1_tvc65up)
    # RESULT: ??  
    
    
    
    
# MALES
    # Iteration 1 - Ages 25-65
    aalen_smk_m_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=25, max.time=64.999, data = nhis_male)
    saveRDS(aalen_smk_m_assump1_tvc2565, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc2565.rds");               # Save model results
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc2565.pdf"); plot(aalen_smk_m_assump1_tvc2565); dev.off()   # save plot 
    aalen_smk_m_assump1_tvc2565 <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc2565.rds")               # load model results
    summary(aalen_smk_m_assump1_tvc2565)
    # RESULT: ??  
    
    
    # Iteration 1 - Ages 65+
    aalen_smk_m_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_male)
    saveRDS(aalen_smk_m_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc65up.rds");               # Save model results
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc65up.pdf"); plot(aalen_smk_m_assump1_tvc65up); dev.off()   # save plot 
    aalen_smk_m_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/alc/aalen_smk_m_assump1_tvc65up.rds")               # load model results
    summary(aalen_smk_m_assump1_tvc65up)
    # RESULT: ??  
    
    
    
    
    
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
    # RESULT: ??
    
    
    # Iteration 1 - Ages 65+
    aalen_bmi_f_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_female)
    saveRDS(aalen_bmi_f_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1_tvc65up.rds");               # Save model results
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1_tvc65up.pdf"); plot(aalen_bmi_f_assump1_tvc65up); dev.off()   # save plot 
    aalen_bmi_f_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_f_assump1_tvc65up.rds")               # load model results
    summary(aalen_bmi_f_assump1_tvc65up) 
    # RESULT: ??
    
    
    
    
    
# MALES
    # Iteration 1 - Ages 25-65
    aalen_bmi_m_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=25, max.time=64.999,  data = nhis_male)
    saveRDS(aalen_bmi_m_assump1_tvc2565, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc2565.rds");               # Save model results
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc2565.pdf"); plot(aalen_bmi_m_assump1_tvc2565); dev.off()   # save plot 
    aalen_bmi_m_assump1_tvc2565 <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc2565.rds")               # load model results
    summary(aalen_bmi_m_assump1_tvc2565)
    # RESULT: ??
    
    # Iteration 1 - Ages 65+
    aalen_bmi_m_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_male)
    saveRDS(aalen_bmi_m_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc65up.rds");               # Save model results
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc65up.pdf"); plot(aalen_bmi_m_assump1_tvc65up); dev.off()   # save plot 
    aalen_bmi_m_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/bmi/aalen_bmi_m_assump1_tvc65up.rds")               # load model results
    summary(aalen_bmi_m_assump1_tvc65up)
    # RESULT: ??
    
    
    
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
    # RESULT: ??
    
    # Iteration 1 - Ages 65+
    aalen_phy_f_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_female)
    saveRDS(aalen_phy_f_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_f_assump1_tvc65up.rds");               # Save model results
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_f_assump1_tvc65up.pdf"); plot(aalen_phy_f_assump1_tvc65up); dev.off()   # save plot 
    aalen_phy_f_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_f_assump1_tvc65up.rds")               # load model results
    summary(aalen_phy_f_assump1_tvc65up)
    # RESULT: ??
    
    
    
# MALE
    # Iteration 1 - Ages 25-65
    aalen_phy_m_assump1_tvc2565 <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=25, max.time=64.999, data = nhis_male)
    saveRDS(aalen_phy_m_assump1_tvc2565, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc2565.rds");               # Save model results
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc2565.pdf"); plot(aalen_phy_m_assump1_tvc2565); dev.off()   # save plot 
    aalen_phy_m_assump1_tvc2565 <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc2565.rds")               # load model results
    summary(aalen_phy_m_assump1_tvc2565)
    # RESULT: ??
    
    # Iteration 1 - Ages 65+
    aalen_phy_m_assump1_tvc65up <- aalen(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor + const(edu.factor) + const(married.factor) + ethnicity.factor, 
      start.time=65, data = nhis_male)
    saveRDS(aalen_phy_m_assump1_tvc65up, "SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc65up.rds");               # Save model results
    pdf("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc65up.pdf"); plot(aalen_phy_m_assump1_tvc65up); dev.off()   # save plot 
    aalen_phy_m_assump1_tvc65up <-readRDS("SIMAH_workspace/nhis/SES x Behavior/Output/Assumptions/Interaction/phy/aalen_phy_m_assump1_tvc65up.rds")               # load model results
    summary(aalen_phy_m_assump1_tvc65up)
    # RESULT: ??
    
    
    
### Sensitivity 4: Multiplicative Hazard Ratios---------------------------------------------------------------------------------------------------

#### Alcohol * Education

      # No survey weights 
      cox_alc_noweight <- coxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*alcohol5v2.factor + 
                                female.factor + married.factor + ethnicity.factor, data=nhis)
            summary(cox_alc_noweight)
      
      
      # Using survey weights
      cox_alc_weights <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ edu.factor*alcohol5v2.factor + 
                                  female.factor + married.factor + ethnicity.factor, design=nhis_svyWeights)
              summary(cox_allcause_alc_edu)


                # Assessing proportional hazards
                cox_alc_weights_check <- cox.zph(cox_alc_weights)
                print(cox_alc_weights_check)
                plot(cox_alc_weights_check, col = "red")
                ggcoxzph(cox_alc_weights_check, var=1, point.alpha = 0.5)
                ggcoxzph(cox_alc_weights_check, var=2)
                ggcoxzph(cox_alc_weights_check, var=3)
                ggcoxzph(cox_alc_weights_check, var=4)
                ggcoxzph(cox_alc_weights_check, var=5)

                



#### Smoking * Education
                
          # No survey weights 
          cox_smk_noweight <- coxph(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor*alcohol5v2.factor + 
                                    female.factor + married.factor + ethnicity.factor, data=nhis)
                summary(cox_smk_noweight)
                
                
          # Using survey weights
          cox_smk_weights <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor*alcohol5v2.factor + 
                                female.factor + married.factor + ethnicity.factor, design=nhis_svyWeights)
                summary(cox_allcause_smk_edu)
                
                
                # Assessing proportional hazards
                cox_smk_weights_check <- cox.zph(cox_smk_weights)
                print(cox_smk_weights_check)
                plot(cox_smk_weights_check, col = "red")






#### BMI * Education
                
        # No survey weights 
        cox_bmi_noweight <- coxph(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor*alcohol5v2.factor + 
                              female.factor + married.factor + ethnicity.factor, data=nhis)
                summary(cox_bmi_noweight)
                
                
        # Using survey weights
        cox_bmi_weights <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor*alcohol5v2.factor + 
                                  female.factor + married.factor + ethnicity.factor, design=nhis_svyWeights)
                summary(cox_allcause_bmi_edu)
                
                
                # Assessing proportional hazards
                cox_bmi_weights_check <- cox.zph(cox_bmi_weights)
                print(cox_bmi_weights_check)
                plot(cox_bmi_weights_check, col = "red")




                
#### Physical Activity * Education
                
          # No survey weights 
          cox_phy_noweight <- coxph(Surv(bl_age, end_age, allcause_death) ~ phy_act3.factor*alcohol5v2.factor + 
                                female.factor + married.factor + ethnicity.factor, data=nhis)
                summary(cox_phy_noweight)
                
                
          # Using survey weights
          cox_phy_weights <- svycoxph(Surv(bl_age, end_age, allcause_death) ~ smoking4.factor*alcohol5v2.factor + 
                                female.factor + married.factor + ethnicity.factor, design=nhis_svyWeights)
                summary(cox_allcause_phy_edu)
                
                
                # Assessing proportional hazards
                cox_phy_weights_check <- cox.zph(cox_phy_weights)
                print(cox_phy_weights_check)
                plot(cox_phy_weights_check, col = "red")

