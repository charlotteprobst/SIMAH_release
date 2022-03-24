
# # SIMAH Restricted-access Data
# Causal Mediation  


# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(timereg)    # additive survival models
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions


# Specify the data and output file locations

data    <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Restricted access data/Data"
output  <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Restricted access data/Output/Causal mediation/"
source("5_Causal Mediation functions.R")

 
# Load data
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))



# OBJECTIVE 2: Causal Mediation

# The causal mediation analyses involves four steps:
# 1) Fit separate multinomial logistic regressions with each mediator (M1, M2, M3, and M4) as the outcome.
# 2) Create copies of the dataset to account for all possible combinations of the exposure and mediators (3^4*= 81); the dataset was expanded from 229,994 to 18,629,514 (women) and 185,770 to 15,047,370 (men) pseudobservations. 
# 3) Using the expanded dataset, calculate weights for each mediator using the predicted probabilities from Step 1. 
# 4) Fit a marginal structural model using Aalen additive hazards with the weight as weight and the id as a cluster level; this ensures thatrobust standard errors are calculated. The model with robust variance and resampling (robust=TRUE) was not used because of computation limiations.  

# For more details and theoretical justification/description see:
# Lange et al. 2014 https//doi.org/10.1093/aje/kwt270
# Lange et al. 2012 https//doi.org/10.1093/aje/kwr525
# Lange et al. 2011 https//doi.org/10.1097/EDE.0b013e31821c680c


# Data Preparation ----------------------------------------------------------------------------------------
CMed_edu3_prep(nhis_female) %>% saveRDS(paste0(output, "expandedData_fem.rds"))
CMed_edu3_prep(nhis_male)   %>% saveRDS(paste0(output, "expandedData_male.rds"))


# Run Analyses, WOMEN ----------------------------------------------------------------------------------------------------------------

# Load data
expandedData <-readRDS(file.path(output, "expandedData_fem.rds"))

hist(newMyData$weightM)

# Run model
CMed_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.alc) + 
                                                        const(A.edu) * const(edu_M2.smk) +
                                                        const(A.edu) * const(edu_M3.bmi) +
                                                        const(A.edu) * const(edu_M4.phy) +
                                                        const(married2) + race4 + const(srvy_yr22),
                          data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
                
# Save model results
saveRDS(CMed_f, file.path(output, "CMed_f.rds"))       


# Load model results
CMed_model <-readRDS(file.path(output, "CMed_f.rds"))  

# The 'summary(model)' command will produce the direct effect, indirect effects, and mediated interaction effects. 
# Functions were used to extract the other details. 
              
# Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
summary(CMed_model)   
getTE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41))                             # Function to get the total effect and proportion mediated
getIE_NotRobust(CMed_model, c(3,5,7,9,29,33,37,41))                               # Function to get the total combined indirect effect  
getTE_IE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41))  # Function to get the proportion mediated of the combined indirect effect
      

 
     

# Run Analyses, MEN ----------------------------------------------------------------------------------------------------------------

# Load data
expandedData <- readRDS(file.path(output, "expandedData_male.rds"))


# Run Model
CMed_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.alc) +
                                                        const(A.edu) * const(edu_M2.smk) +
                                                        const(A.edu) * const(edu_M3.bmi) +
                                                        const(A.edu) * const(edu_M4.phy) +
                                                        const(married2) + race4 + const(srvy_yr22),
                          data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID)


# Save model results
saveRDS(CMed_m, file.path(output, "CMed_m.rds"))       

# Load model results
CMed_model <-readRDS(file.path(output, "CMed_m.rds"))  


# The 'summary(model)' command will produce the direct effect, indirect effects, and mediated interaction effects. 
# Functions were used to extract the other details. 

# Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
summary(CMed_model) 
getTE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41))                              # Function to get the total effect and proportion mediated
getIE_NotRobust(CMed_model, c(3,5,7,9,29,33,37,41))                                # Function to get the total combined indirect effect  
getTE_IE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41))   # Function to get the proportion mediated of the combined indirect effect


