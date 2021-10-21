
# SES x Lifestyle Differential Vulnerability & Exposure Project
# Objective 1: Aalen additive hazard models file


# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(survival)   # surivval analyses
library(survminer)  # surivval analyses
library(timereg)    # additive survival models
memory.limit(size=1e+13)


# Specify the data and output file locations
data    <- "SIMAH_workspace/nhis/Data"
output  <- "SIMAH_workspace/nhis/SES x Behavior/Output/Interaction/"
source("Function - Format Results.R")

    
# Load data
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))


# OBJECTIVE 1: Joint Effects, Hazard Models - Stratified by Sex --------------------------------------------------

# The effect estimates from the model can be directly interpreted as the number of additional events (deaths) per 1 person year at risk
# Two different versions of the model were ran identify the interaction effect (model with the interaction term) and the joint effect (model with interacting variable) 

# A "aalen_10000py" function was created to extract the coefficients and multiple them by 10,000 to get estimates per 10,000 person years
# The results from the function to extract results (as written below) pertain to the effect of:
    # "Low SES & 'best' lifestyle", 
    # "High SES & 'poor' lifestyle", 
    # "Low SES & 'poor' behavior", 
    # "Interaction effect"
     


# FIRST: Run all models and save results  ***************************************************************************************

### Education * Alcohol *********************************************************************************************************

# WOMEN: Interaction model
model <- "_alc_f"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
       saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
       pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  

       # WOMEN: Joint effect model
       model <- "_alc_f2"   # Used to name the file 
       aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.alc) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
       saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
       
       

# MEN: Interaction model
model <- "_alc_m"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(alcohol5v2.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
     saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
     pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    

     # MEN: Joint effect model
     model <- "_alc_m2"   # Used to name the file
     aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.alc) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
     saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
     
     
     
     
       
### Smoking * Education *********************************************************************************************************
     
# WOMEN: Interaction model
model <- "_smk_f"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(smoking4.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
      pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    

      # WOMEN: Joint effect model
      model <- "_smk_f2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.smk) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
      
       
      
      
# MEN: Joint effect model
model <- "_smk_m"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(smoking4.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
      pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()      
      
      # WOMEN: Joint effect model
      model <- "_smk_m2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.smk) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
      
      
      
      
      
### BMI * Education *********************************************************************************************************

# WOMEN: Interaction model
model <- "_bmi_f"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(bmi_cat.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
      pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    

      # WOMEN: Joint effect model
      model <- "_bmi_f2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.bmi) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
      
      
      
      
# MEN: Interaction model
model <- "_bmi_m"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(bmi_cat.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
      pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    

      # MEN: Joint effect model
      model <- "_bmi_m2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.bmi) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
      
      
      
      
      

### Physical Activity * Education *********************************************************************************************************
      
# WOMEN: Interaction model
model <- "_phy_f"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(phy_act3.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
      pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    

      # MEN: Joint effect model
      model <- "_phy_f2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.phy) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
      
      
      
# MEN: Interaction model
model <- "_phy_m"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(phy_act3.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
      pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    

      
      # MEN: Joint effect model
      model <- "_phy_m2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.phy) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))                
      
      
      
      

# Second: Load and view model results  ***************************************************************************************

  # A "aalen_10000py" function was created to extract the coefficients and multiple them by 10,000 to get estimates per 10,000 person years
  # The results from the function to extract results (as written below) pertain to the effect of:
      # "Low SES & 'best' lifestyle", 
      # "High SES & 'poor' lifestyle", 
      # "Low SES & 'poor' behavior", 
      # "Interaction effect"
  
      
      
### WOMEN: Education * Alcohol 
model <- "_alc_f" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))      # Name and load the interaction model         
model <- "_alc_f2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))    # Name and load the joint effect model  
aalen_10000py(aalen, 1); aalen_10000py(aalen, 6); aalen_10000py(aalen2, 13); aalen_10000py(aalen, 31)     # print results of interest

### MEN: Education * Alcohol 
model <- "_alc_m"  ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))  
model <- "_alc_m2"  ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))  
aalen_10000py(aalen, 1); aalen_10000py(aalen, 6); aalen_10000py(aalen2, 13); aalen_10000py(aalen, 31)



### WOMEN: Smoking * Education 
model <- "_smk_f" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_smk_f2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))   
aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)

### MEN: Smoking * Education 
model <- "_smk_m" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_smk_m2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))        
aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)



### WOMEN: BMI * Education
model <- "_bmi_f" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_bmi_f2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))        
aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)

### MEN: BMI * Education
model <- "_bmi_m" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_bmi_m2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))        
aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 10); aalen_10000py(aalen, 28)



### WOMEN: Physical Activity * Education
model <- "_phy_f" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_phy_f2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))        
aalen_10000py(aalen, 1); aalen_10000py(aalen, 3); aalen_10000py(aalen2, 4); aalen_10000py(aalen, 23)

### MEN: Physical Activity * Education
model <- "_phy_m"  ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_phy_m2"  ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))        
aalen_10000py(aalen, 1); aalen_10000py(aalen, 3); aalen_10000py(aalen2, 4); aalen_10000py(aalen, 23)




