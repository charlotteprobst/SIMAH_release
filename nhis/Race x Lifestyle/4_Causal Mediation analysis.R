
# Race x Lifestyle Differential Vulnerability & Exposure Project
# Objective 2: Causal Mediation File 


# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(timereg)    # additive survival models
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions
memory.limit(size=1e+13)

# Specify the data and output file locations
data   <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Processed data/"            # Location of data
output <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Race x Lifestyle/CausMed/"  # Location of model assumptions
source("Function - AalenModel.R")
source("Function - CausalMed.R")



# Load data
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))



# Causal Mediation

# The causal mediation analyses involves four steps:
# Data Preparation:
# 1) Fit separate multinomial logistic regressions with each mediator (M1, M2, M3, and M4) as the outcome.
# 2) Create copies of the dataset to account for all possible combinations of the exposure and mediators; the dataset was expanded from 229,994 to 58,878,464 (women) and 185,770 to 47,557,120 (men) pseudobservations. 
# 3) Using the expanded dataset, calculate weights for each mediator using the predicted probabilities from Step 1. 
# Run Model: 
# 4) Fit a marginal structural model using Aalen additive hazards with the weight as weight and the id as a cluster level; this ensures thatrobust standard errors are calculated. The model with robust variance and resampling (robust=TRUE) was not used because of computation limiations.  


# Data Preparation ----------------------------------------------------------------------------------------
# CMed_prep(nhis_female) %>% saveRDS(paste0(output, "expandedData_fem.rds"))
# CMed_prep(nhis_male)   %>% saveRDS(paste0(output, "expandedData_male.rds"))


# Run Analyses, WOMEN ----------------------------------------------------------------------------------------------------------------
# Load data
expandedData <-readRDS(file.path(output, "expandedData_fem.rds"))

hist(expandedData$weightM)

# Run model
CMed_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.race) * const(race_M1.alc) + 
                                                        const(A.race) * const(race_M2.smk) +
                                                        const(A.race) * const(race_M3.bmi) +
                                                        const(A.race) * const(race_M4.phy) +
                                                        const(married) + const(factor(edu)) + const(factor(srvy_yr)),
                          data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
                
# Save model results
saveRDS(CMed_f, file.path(output, "CMed_f.rds"))       


# Load model results
CMed_model <-readRDS(file.path(output, "CMed_f.rds"))  

# The 'summary(model)' command will produce the direct effect, indirect effects, and mediated interaction effects. 
# Functions were used to extract the other details. 
              
# Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
summary(CMed_model)   

# Black (ref=White)
getTE_NotRobust(CMed_model, c(1,4,7,10,13,36,45,54,63))                             # Function to get the total effect and proportion mediated
getIE_NotRobust(CMed_model, c(4,7,10,13,36,45,54,63))                               # Function to get the total combined indirect effect  
getTE_IE_NotRobust(CMed_model, c(1,4,7,10,13,36,45,54,63), c(4,7,10,13,36,45,54,63))  # Function to get the proportion mediated of the combined indirect effect
      

# Hispanic (ref=White)
getTE_NotRobust(CMed_model, c(2,5,8,11,14,40,49,58,67))                             # Function to get the total effect and proportion mediated
getIE_NotRobust(CMed_model, c(5,8,11,14,40,49,58,67))                               # Function to get the total combined indirect effect  
getTE_IE_NotRobust(CMed_model, c(2,5,8,11,14,40,49,58,67), c(5,8,11,14,40,49,58,67))  # Function to get the proportion mediated of the combined indirect effect


# Other (ref=White)
getTE_NotRobust(CMed_model, c(3,6,9,12,15,44,53,62,71))                             # Function to get the total effect and proportion mediated
getIE_NotRobust(CMed_model, c(6,9,12,15,44,53,62,71))                               # Function to get the total combined indirect effect  
getTE_IE_NotRobust(CMed_model, c(3,6,9,12,15,44,53,62,71), c(6,9,12,15,44,53,62,71))  # Function to get the proportion mediated of the combined indirect effect







# Run Analyses, MEN ----------------------------------------------------------------------------------------------------------------

# Load data
expandedData <- readRDS(file.path(output, "expandedData_male.rds")) 

hist(expandedData$weightM)


# Run model
CMed_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.race) * const(race_M1.alc) + 
                                                        const(A.race) * const(race_M2.smk) +
                                                        const(A.race) * const(race_M3.bmi) +
                                                        const(A.race) * const(race_M4.phy) +
                                                        const(married) + const(factor(edu)) + const(factor(srvy_yr)),
                                                      data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  

# Save model results
saveRDS(CMed_m, file.path(output, "CMed_m.rds"))       


# Load model results
CMed_model <-readRDS(file.path(output, "CMed_m.rds"))  

# The 'summary(model)' command will produce the direct effect, indirect effects, and mediated interaction effects. 
# Functions were used to extract the other details. 

# Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
summary(CMed_model)   

# Black (ref=White)
getTE_NotRobust(CMed_model, c(1,4,7,10,13,36,45,54,63))                             # Function to get the total effect and proportion mediated
getIE_NotRobust(CMed_model, c(4,7,10,13,36,45,54,63))                               # Function to get the total combined indirect effect  
getTE_IE_NotRobust(CMed_model, c(1,4,7,10,13,36,45,54,63), c(4,7,10,13,36,45,54,63))  # Function to get the proportion mediated of the combined indirect effect


# Hispanic (ref=White)
getTE_NotRobust(CMed_model, c(2,5,8,11,14,40,49,58,67))                             # Function to get the total effect and proportion mediated
getIE_NotRobust(CMed_model, c(5,8,11,14,40,49,58,67))                               # Function to get the total combined indirect effect  
getTE_IE_NotRobust(CMed_model, c(2,5,8,11,14,40,49,58,67), c(5,8,11,14,40,49,58,67))  # Function to get the proportion mediated of the combined indirect effect


# Other (ref=White)
getTE_NotRobust(CMed_model, c(3,6,9,12,15,44,53,62,71))                             # Function to get the total effect and proportion mediated
getIE_NotRobust(CMed_model, c(6,9,12,15,44,53,62,71))                               # Function to get the total combined indirect effect  
getTE_IE_NotRobust(CMed_model, c(3,6,9,12,15,44,53,62,71), c(6,9,12,15,44,53,62,71))  # Function to get the proportion mediated of the combined indirect effect


