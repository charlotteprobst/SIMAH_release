
# Race x Lifestyle Differential Vulnerability & Exposure Project
# Assumptions of Aalen Models 

# LOAD DATA AND SET FILE LOCATIONS 

# load libraries
library(tidyverse)  # data management
library(timereg)    # additive survival models
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions


# Specify the data and output file locations
data   <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Processed data/"                # Location of data
output <- "C:/Users/klajd/Documents/2021 CAMH/SIMAH/SIMAH_workplace/nhis/Race x Lifestyle/Assumptions/" # Location of output


# SCC; ; specify locations 
# setwd("/external/mgmt3/imaging/scratch/Imhpr/kpuka/nhis/")
# data    <- "Data/"
# output  <- "Output/"


# Load data
nhis        <- readRDS (paste0(data, "nhis18_85.rds"))
nhis_male   <- filter(nhis, female==0)
nhis_female <- filter(nhis, female==1)


# ASSUMPTIONS, Additive Hazard Models **********************************************************************************************************

# First, check the time-invariant assumption (in our case, referred to as 'age-invariant'); whether the effect of covariates 
# is age-varying or constant with time (similar to proportional hazard assumption in Cox models). The "const()" wrapper is 
# used to make the effect of a variable age-invariant; without this wrapper the effect of the variable will be age-varying. 
# Start by fitting the model where all components of the model have age-varying effects, then iteratively simplify the model 
# by making the variables age-invariant (based on the plot and the Kolmogorov-Smirnov / Cramer von Mises tests). 

# Ultimately, the variables that are part of an interaction have to have a age-invariant effect, and sensitivity analyses 
# (stratifying by age group) were  ran to examine the potential impact if the assumption was violated. 



# WOMEN *****************************************************************************************************************************************
# Iteration 1
model <- "_CMed_f_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  ethnicity.factor + alcohol5v2.factor + smoking4.factor +
                                               bmi_cat.factor + phy_act3.factor + married.factor + edu.factor, data=nhis_female)    
        saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # Result: Marital status should be made age-invariant
        
        
# Iteration 2 
model <- "_CMed_f_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  ethnicity.factor  + alcohol5v2.factor + smoking4.factor +
                                              bmi_cat.factor + phy_act3.factor + const(married.factor) + edu.factor, data=nhis_female)
        saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # Result: education should be made age-invariant
        
        
        
# Iteration 3
model <- "_CMed_f_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ ethnicity.factor + alcohol5v2.factor + smoking4.factor +
                                              bmi_cat.factor + phy_act3.factor + const(married.factor) +const(edu.factor), data=nhis_female)
        saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # Result: BMI should be made age-invariant
        
        
# Iteration 4 
model <- "_CMed_f_4"   # Used to name the files appropriately
                assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  ethnicity.factor + alcohol5v2.factor + smoking4.factor +
                                       const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + const(edu.factor), data=nhis_female)
        saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # Result: alcohol should be made age-invariant
        
        
# Iteration 5
model <- "_CMed_f_5"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  ethnicity.factor + const(alcohol5v2.factor) + smoking4.factor +
                                        const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + const(edu.factor), data=nhis_female)
      saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
      pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
      assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
      summary(assump_aalen)
        
        # Final result: 
        # Age-invariant variables: marital status, education, BMI, alcohol
        # Age-varying variables: smoking, physical activity, race/ethnicity




# MEN *****************************************************************************************************************************************
model <- "_CMed_m_1"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  ethnicity.factor + alcohol5v2.factor + smoking4.factor +
                                            bmi_cat.factor + phy_act3.factor + married.factor + edu.factor, data=nhis_male)
        saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # Result: marital status should be made age-invariant
        
        
# Iteration 2 
model <- "_CMed_m_2"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  ethnicity.factor + alcohol5v2.factor + smoking4.factor +
                                             bmi_cat.factor + phy_act3.factor + const(married.factor) + edu.factor, data=nhis_male)
        saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # Result: Education should be made age-invariant
        
        
        
# Iteration 3   
model <- "_CMed_m_3"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~  ethnicity.factor + alcohol5v2.factor + smoking4.factor +
                                          bmi_cat.factor + phy_act3.factor + const(married.factor) + const(edu.factor), data=nhis_male)
        saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # Result: BMI should be made age-invariant
        
        
              
# Iteration 4 
model <- "_CMed_m_4"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ ethnicity.factor + alcohol5v2.factor + smoking4.factor +
                                    const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + const(edu.factor), data=nhis_male)
        saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
        # Result: Alcohol should be made age-invariant
        
        
# Iteration 5  
model <- "_CMed_m_5"   # Used to name the files appropriately
assump_aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ ethnicity.factor + const(alcohol5v2.factor) + smoking4.factor +
                                     const(bmi_cat.factor) + phy_act3.factor + const(married.factor) + const(edu.factor), data=nhis_male)
        saveRDS(assump_aalen, paste0(output, "assump_aalen", model, ".rds"))                # Save model results
        pdf(paste0(output, "assump_aalen", model, ".pdf")); plot(assump_aalen); dev.off()   # save plot 
        assump_aalen <-readRDS(paste0(output, "assump_aalen", model, ".rds"))               # load model results
        summary(assump_aalen)
                
        # Final result: 
        # Age-invariant variables: marital status, education, BMI, alcohol
        # Age-varying variables: smoking, physical activity, race/ethnicity
        
