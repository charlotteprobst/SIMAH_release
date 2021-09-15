### SIMAH - NHIS Data
### SES x Health Behavior interaction and mediation


# SENSITIVITY ANALYSES 3: Stratified by Age
# MALES

# NOTE: the data file used is the same as the expanded data used for the main analyses 
# i.e., there is not 'prep' file specific to this sensitivity analysis


# LOAD DATA AND SET FILE LOCATIONS --------------------------------------------------------------------------


# load libraries
library(tidyverse)  # data management
library(timereg)    # additive survival models
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions



# Set the working directory and other file locations

# Personal Computer:
# kp <- "C:/Users/klajd/OneDrive/SIMAH"
# setwd(kp)
# data    <- "SIMAH_workspace/nhis/Data"
# output  <- "SIMAH_workspace/nhis/SES x Behavior/Output/CausMed/"
# source("SIMAH_code/nhis/0_Function_CausalMed_Results.R")


# HCC Server:
kp <- "/external/mgmt3/imaging/scratch/Imhpr/kpuka/nhis/"
setwd(kp)
data    <- "Data/"
output  <- "Output/"
source("0_Function_CausalMed_Results.R")



# Causal Mediation analysis ----------------------------------------------------------------------------------
# Load data
expandedData <-readRDS(file.path(output, "expandedData_male.rds"))

# Run model for those aged 25 - 59 years
CMed_m_25_59 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.alc) + 
                                                          const(A.edu) * const(edu_M2.smk) +
                                                          const(A.edu) * const(edu_M3.bmi) +
                                                          const(A.edu) * const(edu_M4.phy) +
                                                          const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                            start.time=25, max.time=59.999,
                            data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
                  saveRDS(CMed_m_25_59, file.path(output, "CMed_m_25_59.rds"))       # Save model results
                  CMed_m_model_25_59 <-readRDS(file.path(output, "CMed_m_25_59.rds"))  # Load model results


                
      # Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
      summary(CMed_m_model_25_59)   #Estimates and SE
      getTE_NotRobust(CMed_m_model_25_59, c(1,3,5,7,9,29,33,37,41))  # Simulated estimate and SE for total effect and mediated proportions for other effects
      getIE_NotRobust(CMed_m_model_25_59, c(3,5,7,9,29,33,37,41))    # Estimate and simulated SE for indirect combined effect
      getTE_IE_NotRobust(CMed_m_model_25_59, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) # Mediated proportion and simulated 95% CI for mediated proportion of indirect combined effect


      

# Run model for those aged 60 - 69 years
CMed_m_60_69 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.alc) + 
                                                              const(A.edu) * const(edu_M2.smk) +
                                                              const(A.edu) * const(edu_M3.bmi) +
                                                              const(A.edu) * const(edu_M4.phy) +
                                                              const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                          start.time=60, max.time=69.999,
                          data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
                  saveRDS(CMed_m_60_69, file.path(output, "CMed_m_60_69.rds"))       # Save model results
                  CMed_m_model_60_69 <-readRDS(file.path(output, "CMed_m_60_69.rds"))  # Load model results

      
      # Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
      summary(CMed_m_model_60_69)   #Estimates and SE
      getTE_NotRobust(CMed_m_model_60_69, c(1,3,5,7,9,29,33,37,41))  # Simulated estimate and SE for total effect and mediated proportions for other effects
      getIE_NotRobust(CMed_m_model_60_69, c(3,5,7,9,29,33,37,41))    # Estimate and simulated SE for indirect combined effect
      getTE_IE_NotRobust(CMed_m_model_60_69, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) # Mediated proportion and simulated 95% CI for mediated proportion of indirect combined effect



      

# Run model for those aged 70 - 85 years
CMed_m_70_85 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.alc) + 
                                                              const(A.edu) * const(edu_M2.smk) +
                                                              const(A.edu) * const(edu_M3.bmi) +
                                                              const(A.edu) * const(edu_M4.phy) +
                                                              const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                          start.time=70, max.time=84.999,
                          data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
                  saveRDS(CMed_m_70_85, file.path(output, "CMed_m_70_85.rds"))       # Save model results
                  CMed_m_model_70_85 <-readRDS(file.path(output, "CMed_m_70_85.rds"))  # Load model results


      
      # Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
      summary(CMed_m_model_70_85)   #Estimates and SE
      getTE_NotRobust(CMed_m_model_70_85, c(1,3,5,7,9,29,33,37,41))  # Simulated estimate and SE for total effect and mediated proportions for other effects
      getIE_NotRobust(CMed_m_model_70_85, c(3,5,7,9,29,33,37,41))    # Estimate and simulated SE for indirect combined effect
      getTE_IE_NotRobust(CMed_m_model_70_85, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) # Mediated proportion and simulated 95% CI for mediated proportion of indirect combined effect




