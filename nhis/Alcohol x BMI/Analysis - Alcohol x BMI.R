
### SIMAH - NHIS Data
### Alcohol x BMI Interaction 

library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(gmodels)    # CrossTable command
library(tableone)   # create table one
library(survival)   # surivval analyses
library(survminer)  # surivval analyses
library(timereg)    # additive survival models
library(biostat3)   # survRate command
memory.limit(size=1e+13)


# Set the working directory and load data
kp <- "C:/Users/klajd/OneDrive/SIMAH"
setwd(kp)


nhis <-readRDS ("SIMAH_workspace/nhis/Data/nhis.rds")
nhis_male <- readRDS ("SIMAH_workspace/nhis/Data/nhis_male.rds")
nhis_female <- readRDS("SIMAH_workspace/nhis/Data/nhis_female.rds")
nhis_lowSES <- filter(nhis, edu.factor == "Highschool")
nhis_highSES <- filter(nhis, edu.factor == "Bachelors")


# Create second version of BMI variable, to get the simple slope of alcohol among obese individuals
nhis$bmi_cat.factor2 <- relevel(nhis$bmi_cat.factor, ref = "Obese")  
nhis_male$bmi_cat.factor2 <- relevel(nhis_male$bmi_cat.factor, ref = "Obese")  
nhis_female$bmi_cat.factor2 <- relevel(nhis_female$bmi_cat.factor, ref = "Obese")  

# load functions
source("SIMAH_code/nhis/Function_Formatted_results.R")


## DESCRIPTIVES ----------------------------------------------------------------------
# All Participants
table1_alc_bmi <-CreateTableOne(vars= c("alcohol_daily_grams", "allcause_death", "heart_death", "neoplasm_death", "cvd_death", "diabetes_death"),
                  factorVars = c("allcause_death", "heart_death", "neoplasm_death", "cvd_death", "diabetes_death"), 
                  strata= c("bmi_cat.factor"), addOverall = TRUE, data=nhis)
    table1_alc_bmi <- print(table1_alc_bmi, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)
    write.csv(table1_alc_bmi, file="SIMAH_workspace/nhis/Alcohol x BMI/Output/Table1 Alcohol x BMI.csv")
   
# Low SES
table1_alc_bmi <-CreateTableOne(vars= c("alcohol_daily_grams", "allcause_death", "heart_death", "neoplasm_death", "cvd_death", "diabetes_death"),
      factorVars = c("allcause_death", "heart_death", "neoplasm_death", "cvd_death", "diabetes_death"), 
      strata= c("bmi_cat.factor"), addOverall = TRUE, data=nhis_lowSES)
    table1_alc_bmi <- print(table1_alc_bmi, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)
    write.csv(table1_alc_bmi, file="SIMAH_workspace/nhis/Alcohol x BMI/Output/Table1 Alcohol x BMI low SES.csv")
    
# High SES
table1_alc_bmi <-CreateTableOne(vars= c("alcohol_daily_grams", "allcause_death", "heart_death", "neoplasm_death", "cvd_death", "diabetes_death"),
      factorVars = c("allcause_death", "heart_death", "neoplasm_death", "cvd_death", "diabetes_death"), 
      strata= c("bmi_cat.factor"), addOverall = TRUE, data=nhis_highSES)
    table1_alc_bmi <- print(table1_alc_bmi, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)
    write.csv(table1_alc_bmi, file="SIMAH_workspace/nhis/Alcohol x BMI/Output/Table1 Alcohol x BMI high SES.csv")
    
    
    skim(nhis$alcohol_daily_grams)
    nhis %>%
        group_by(alcohol5v2.factor) %>%
        skim(alcohol_daily_grams)
                 
    
# Survival plot 
nhis_plot <- filter(nhis, bmi_cat.factor == c("Healthy weight", "Obese"))
nhis_plot <- filter(nhis_plot, alcohol4.factor == c("Abstinence", "High risk"))

ggsurvplot_facet(fit = survfit(Surv(bl_age, end_age, allcause_death) ~ bmi_cat.factor, data = nhis_plot), 
   data=nhis_plot, facet.by="alcohol4.factor", censor = FALSE, xlim = c(25, 100), 
   conf.int = TRUE, 
   xlab = "Age (years)", 
   ylab = "Overall survival probability") 
     

ggsurvplot_facet(fit = survfit(Surv(bl_age, end_age, allcause_death) ~ alcohol4.factor, data = nhis_plot), 
  data=nhis_plot, facet.by="bmi_cat.factor", censor = FALSE, xlim = c(25, 100), 
  conf.int = TRUE, 
  xlab = "Age (years)", 
  ylab = "Overall survival probability") 
    



# Additive Hazard Models  -----------------------------------------------------------------------------------------------------------
                     
## Model 1: Adjusted for only age ----------------------------------------------------------------------------------------------------  
    
    
# Causes of death:
  # allcause_death 
  # heart_death 
  # neoplasm_death
  # cvd_death
  # diabetes_death  
    

### All Participants
model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams),  data = nhis, robust=0)
model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams),  data = nhis, robust=0)
      aalen_aalen_10000py_10000py(model1, 3); aalen_aalen_10000py_10000py_10x(model1, 4); aalen_aalen_10000py_10000py_10x(model2, 4); aalen_aalen_10000py_10000py_10x(model1, 7)

         
      ## MEN
      model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams),  data = nhis_male, robust=0)
      model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams),  data = nhis_male, robust=0)
         aalen_aalen_10000py_10000py(model1, 3); aalen_aalen_10000py_10000py_10x(model1, 4); aalen_aalen_10000py_10000py_10x(model2, 4); aalen_aalen_10000py_10000py_10x(model1, 7)
                
                
      ## WOMEN
      model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams),  data = nhis_female, robust=0)
      model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams),  data = nhis_female, robust=0)
          aalen_aalen_10000py_10000py(model1, 3); aalen_aalen_10000py_10000py_10x(model1, 4); aalen_aalen_10000py_10000py_10x(model2, 4); aalen_aalen_10000py_10000py_10x(model1, 7) 
                
                
      ## Low SES
      model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams),  data = nhis_lowSES, robust=0)
      model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams),  data = nhis_lowSES, robust=0)
          aalen_aalen_10000py_10000py(model1, 3); aalen_aalen_10000py_10000py_10x(model1, 4); aalen_aalen_10000py_10000py_10x(model2, 4); aalen_aalen_10000py_10000py_10x(model1, 7)          
          
          
      ## High SES
      model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams),  data = nhis_highSES, robust=0)
      model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams),  data = nhis_highSES, robust=0)
          aalen_aalen_10000py_10000py(model1, 3); aalen_aalen_10000py_10000py_10x(model1, 4); aalen_aalen_10000py_10000py_10x(model2, 4); aalen_aalen_10000py_10000py_10x(model1, 7)          
            
     
          
          
                     
## Model 2: Fully adjusted model - adjusted for age (as timescale), education, ethnicity/race, marital status, and smoking -----------------------------------------------------------------  

# Causes of death:
  # allcause_death 
  # heart_death 
  # neoplasm_death
  # cvd_death
  # diabetes_death  
       
          
### All Participants
model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + female.factor + ethnicity.factor + smoking4.factor,  data = nhis, robust=0)
model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + female.factor + ethnicity.factor + smoking4.factor,  data = nhis, robust=0)
      aalen_10000py(model1, 3); aalen_10000py_10x(model1, 4); aalen_10000py_10x(model2, 4); aalen_10000py_10x(model1, 10)        

          ## MEN
          model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + ethnicity.factor + smoking4.factor,  data = nhis_male, robust=0)
          model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + ethnicity.factor + smoking4.factor,  data = nhis_male, robust=0)
                  aalen_10000py(model1, 3); aalen_10000py_10x(model1, 4); aalen_10000py_10x(model2, 4); aalen_10000py_10x(model1, 10)        
                
                    
          ## WOMEN
          model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + ethnicity.factor + smoking4.factor,  data = nhis_female, robust=0)
          model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + ethnicity.factor + smoking4.factor,  data = nhis_female, robust=0)
                  aalen_10000py(model1, 3); aalen_10000py_10x(model1, 4); aalen_10000py_10x(model2, 4); aalen_10000py_10x(model1, 10)     

                                  
          ## Low SES
          model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(married.factor) + female.factor + ethnicity.factor + smoking4.factor,  data = nhis_lowSES, robust=0)
          model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(married.factor) + female.factor + ethnicity.factor + smoking4.factor,  data = nhis_lowSES, robust=0)
                  aalen_10000py(model1, 3); aalen_10000py_10x(model1, 4); aalen_10000py_10x(model2, 4); aalen_10000py_10x(model1, 8)        
                
                  
          ## High SES
          model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(married.factor) + female.factor + ethnicity.factor + smoking4.factor,  data = nhis_highSES, robust=0)
          model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(married.factor) + female.factor + ethnicity.factor + smoking4.factor,  data = nhis_highSES, robust=0)
                  aalen_10000py(model1, 3); aalen_10000py_10x(model1, 4); aalen_10000py_10x(model2, 4); aalen_10000py_10x(model1, 8)        
                  
                  

                

                
                
## Model 3: Fully adjusted (except for ethinicity)   ------------------------------------------------------------------------------  
         
# Causes of death:
  # allcause_death 
  # heart_death 
  # neoplasm_death
  # cvd_death
  # diabetes_death  
                  
                         
### All Participants
model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + female.factor + smoking4.factor,  data = nhis, robust=0)
model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + female.factor + smoking4.factor,  data = nhis, robust=0)
      aalen_10000py(model1, 3); aalen_10000py_10x(model1, 4); aalen_10000py_10x(model2, 4); aalen_10000py_10x(model1, 10)        
                
                
              
        ## MEN
        model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + smoking4.factor,  data = nhis_male, robust=0)
        model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) +const(married.factor) + smoking4.factor,  data = nhis_male, robust=0)
                aalen_10000py(model1, 3); aalen_10000py_10x(model1, 4); aalen_10000py_10x(model2, 4); aalen_10000py_10x(model1, 10)        
                
                
        ## WOMEN
        model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + smoking4.factor,  data = nhis_female, robust=0)
        model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + smoking4.factor,  data = nhis_female, robust=0)
                aalen_10000py(model1, 3); aalen_10000py_10x(model1, 4); aalen_10000py_10x(model2, 4); aalen_10000py_10x(model1, 10)     
                
                        
        ## Low SES
        model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(married.factor) + female.factor + smoking4.factor,  data = nhis_lowSES, robust=0)
        model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(married.factor) + female.factor + smoking4.factor,  data = nhis_lowSES, robust=0)
                aalen_10000py(model1, 3); aalen_10000py_10x(model1, 4); aalen_10000py_10x(model2, 4); aalen_10000py_10x(model1, 8)        
                
                
        ## High SES
        model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(married.factor) + female.factor + smoking4.factor,  data = nhis_highSES, robust=0)
        model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(married.factor) + female.factor + smoking4.factor,  data = nhis_highSES, robust=0)
                aalen_10000py(model1, 3); aalen_10000py_10x(model1, 4); aalen_10000py_10x(model2, 4); aalen_10000py_10x(model1, 8)        
                
          

             
## Model 4: Stratified: Non-Hispanic White Only; Adjusted for age (as timescale), education, marital status, and smoking. -----------------------------------------------------------------  

nhis_white <- filter(nhis, ethnicity.factor=="Non-Hispanic White")
nhis_female_white <- filter(nhis_female, ethnicity.factor=="Non-Hispanic White")
nhis_male_white <- filter(nhis_male, ethnicity.factor=="Non-Hispanic White")
nhis_lowSES_white <- filter(nhis_lowSES, ethnicity.factor=="Non-Hispanic White")
nhis_highSES_white <- filter(nhis_highSES, ethnicity.factor=="Non-Hispanic White")             
                              
# Causes of death:
  # allcause_death 
  # heart_death 
  # neoplasm_death
  # cvd_death
  # diabetes_death  

               
       
### All Participants
model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + female.factor + smoking4.factor,  data = nhis_white, robust=0)
model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + female.factor + smoking4.factor,  data = nhis_white, robust=0)
       aalen_10000py(model1, 3); aalen_10000py_10x(model1, 4); aalen_10000py_10x(model2, 4); aalen_10000py_10x(model1, 10)     
       
       
       ## MEN
       model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + smoking4.factor,  data = nhis_male_white, robust=0)
       model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + smoking4.factor,  data = nhis_male_white, robust=0)
             aalen_10000py(model1, 3); aalen_10000py_10x(model1, 4); aalen_10000py_10x(model2, 4); aalen_10000py_10x(model1,10)     
       
       
       ## WOMEN
       model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + smoking4.factor,  data = nhis_female_white, robust=0)
       model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + const(married.factor) + smoking4.factor,  data = nhis_female_white, robust=0)
              aalen_10000py(model1, 3); aalen_10000py_10x(model1, 4); aalen_10000py_10x(model2, 4); aalen_10000py_10x(model1, 10)     
       
              
        ### Low SES
        model1 <- aalen(Surv(bl_age, end_age, diabetes_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(married.factor) + female.factor + smoking4.factor,  data = nhis_lowSES_white, robust=0)
        model2 <- aalen(Surv(bl_age, end_age, diabetes_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(married.factor) + female.factor + smoking4.factor,  data = nhis_lowSES_white, robust=0)
              aalen_10000py(model1, 3); aalen_10000py_10x(model1, 4); aalen_10000py_10x(model2, 4); aalen_10000py_10x(model1, 8)     

        ### High SES
        model1 <- aalen(Surv(bl_age, end_age, diabetes_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(married.factor) + female.factor + smoking4.factor,  data = nhis_highSES_white, robust=0)
        model2 <- aalen(Surv(bl_age, end_age, diabetes_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(married.factor) + female.factor + smoking4.factor,  data = nhis_highSES_white, robust=0)
              aalen_10000py(model1, 3); aalen_10000py_10x(model1, 4); aalen_10000py_10x(model2, 4); aalen_10000py_10x(model1, 8)     
              
       