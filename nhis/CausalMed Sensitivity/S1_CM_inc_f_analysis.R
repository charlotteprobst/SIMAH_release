### SIMAH - NHIS Data
### SES x Health Behavior interaction and mediation


# SENSITIVITY ANALYSES 1: SES as INCOME
# FEMALES 


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
expandedData <-readRDS(file.path(output, "expandedData_income_fem.rds"))

# Run model
CMed_incom_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.income) * const(incom_M1.alc) + 
                                                          const(A.income) * const(incom_M2.smk) +
                                                          const(A.income) * const(incom_M3.bmi) +
                                                          const(A.income) * const(incom_M4.phy) +
                                                          const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                            data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
                  saveRDS(CMed_incom_f, file.path(output, "CMed_incom_f.rds"))       # Save model results
                  CMed_model <-readRDS(file.path(output, "CMed_income_f.rds"))  # Load model results


                  
                  
# Direct, indirect and mediated interactive effects and standard errors are derived directly from the summary() command
# Total effect is obtained by the sum of the three separate effects
# Confidence intervals for total effects and mediated proportions are computed using the code below:

# Get final results NOTE: THE NUMBERS BELOW HAVE TO BE CHANGED*********************
summary(CMed_model)   #Estimates and SE
getTE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41))  #Simulated estimate and SE for total effect and mediated proportions for other effects
getIE_NotRobust(CMed_model, c(3,5,7,9,29,33,37,41))    #Estimate and simulated SE for indirect combined effect
getTE_IE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) #Mediated proportion and simulated 95% CI for mediated proportion of indirect combined effect



