### SIMAH - NHIS Data
### SES x Health Behavior interaction and mediation

# SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(timereg)    # additive survival models
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions


# Set the working directory and other file locations

# Personal Computer:
kp <- "C:/Users/klajd/OneDrive/SIMAH"
setwd(kp)
data    <- "SIMAH_workspace/nhis/Data"
output  <- "SIMAH_workspace/nhis/SES x Behavior/Output/CausMed/"
source("SIMAH_code/nhis/0_Function_CausalMed_Results.R")


# HCC Server:
kp <- "/external/mgmt3/imaging/scratch/Imhpr/kpuka/nhis/"
setwd(kp)
data    <- "Data"
output  <- "Output/"
source("0_Function_CausalMed_Results.R")

   

# OBJECTIVE 2: Causal Mediation - MALES
# Load data
expandedData <- readRDS(file.path(output, "expandedData_male.rds"))

# Run Model
CMed_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.alc) +
                                                          const(A.edu) * const(edu_M2.smk) +
                                                          const(A.edu) * const(edu_M3.bmi) +
                                                          const(A.edu) * const(edu_M4.phy) +
                                                          const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                                data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID)
                      saveRDS(CMed_m, file.path(output, "CMed_m.rds"))       # Save model results
                      CMed_model <-readRDS(file.path(output, "CMed_m.rds"))  # Load model results

                      
                      
# Get final results (ensure that the Causal Mediation Functions are loaded). 
# NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
summary(CMed_model)   #Estimates and SE
getTE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41))  #Simulated estimate and SE for total effect and mediated proportions for other effects
getIE_NotRobust(CMed_model, c(3,5,7,9,29,33,37,41))    #Estimate and simulated SE for indirect combined effect
getTE_IE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) #Mediated proportion and simulated 95% CI for mediated proportion of indirect combined effect




