
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


# Create second version of BMI variable, to get the simple slope of alcohol among obese individuals
nhis$bmi_cat.factor2 <- relevel(nhis$bmi_cat.factor, ref = "Obese")  
nhis_male$bmi_cat.factor2 <- relevel(nhis_male$bmi_cat.factor, ref = "Obese")  
nhis_female$bmi_cat.factor2 <- relevel(nhis_female$bmi_cat.factor, ref = "Obese")  


# Function to extract results
results <- function(model, x) {
    mu <- model$gamma[x]       
    var <-  model$var.gamma[x,x]
    confint.lower <- round((mu - (1.96 * sqrt(var)))*100000,2) # CI * 100,000 to get result per 10,000py
    confint.upper <- round((mu + (1.96 * sqrt(var)))*100000,2) # CI * 100,000 to get result per 10,000py
    mu <- round(mu*100000,2)                                   # mu * 100,000 to get result per 10,000py 
    output<-paste0(mu, " (",confint.lower,", ", confint.upper, ")")
    return(cat(output, "\n"))}   #cat() returns the text without quotes and without the leading numbers [1], [2]...



## DESCRIPTIVES ----------------------------------------------------------------------
table1_alc_bmi <-CreateTableOne(vars= c("alcohol_daily_grams", "allcause_death", "heart_death", "neoplasm_death", "cvd_death", "diabetes_death"),
                  factorVars = c("allcause_death", "heart_death", "neoplasm_death", "cvd_death", "diabetes_death"), 
                  strata= c("bmi_cat.factor"), addOverall = TRUE, data=nhis)
    table1_alc_bmi <- print(table1_alc_bmi, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)
    write.csv(table1_alc_bmi, file="SIMAH_workspace/nhis/Alcohol x BMI/Output/Table1 Alcohol x BMI.csv")
   
    
    
    skim(nhis$alcohol_daily_grams)
    nhis %>%
        group_by(alcohol5v2.factor) %>%
        skim(alcohol_daily_grams)
                 

## Additive Hazard Models  ----------------------------------------------------------------------
                     
# Model 1: Adjusted for only age -----------------------------------------------------------------  
    
# Change the death type and re-run analyses; causes of death:
# allcause_death 
# heart_death 
# neoplasm_death
# cvd_death
# diabetes_death
                
    
    ## All Participants
    model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams),  data = nhis, robust=0)
    model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams),  data = nhis, robust=0)
      results(model1, 3); results(model1, 4); results(model2, 4); results(model1, 7)


      
         
      ## MEN
      model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams),  data = nhis_male, robust=0)
      model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams),  data = nhis_male, robust=0)
         results(model1, 3); results(model1, 4); results(model2, 4); results(model1, 7)
                
                
        ## WOMEN
         model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams),  data = nhis_female, robust=0)
         model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams),  data = nhis_female, robust=0)
            results(model1, 3); results(model1, 4); results(model2, 4); results(model1, 7) 
                
                
                
            
                
                
# Model 2: Fully adjusted model - adjusted for age (as timescale), education, ethnicity/race, marital status, and smoking -----------------------------------------------------------------  

# allcause_death 
# heart_death 
# neoplasm_death
# cvd_death
# diabetes_death              
            
        ## All Participants
        model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                        const(married.factor) + female.factor + ethnicity.factor + smoking4.factor,  data = nhis, robust=0)
        
        model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                        const(married.factor) + female.factor + ethnicity.factor + smoking4.factor,  data = nhis, robust=0)
                
        results(model1, 3); results(model1, 4); results(model2, 4); results(model1, 10)        

                
        
                ## MEN
                model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                                     const(married.factor) + ethnicity.factor + smoking4.factor,  data = nhis_male, robust=0)
                
                model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                                    const(married.factor) + ethnicity.factor + smoking4.factor,  data = nhis_male, robust=0)
                
                results(model1, 3); results(model1, 4); results(model2, 4); results(model1, 10)        
                
                    
                    
                
                ## WOMEN
                model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                                const(married.factor) + ethnicity.factor + smoking4.factor,  data = nhis_female, robust=0)
                
                model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                                const(married.factor) + ethnicity.factor + smoking4.factor,  data = nhis_female, robust=0)
                
                results(model1, 3); results(model1, 4); results(model2, 4); results(model1, 10)     
                
                
                
# Change the death type and re-run analyses; causes of death:
# allcause_death 
# heart_death 
# neoplasm_death
# cvd_death
# diabetes_death
                
                
# Model 3: Fully adjusted (except for ethinicity)   ------------------------------------------------------------------------------  
                
## All Participants
        model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                            const(married.factor) + female.factor + smoking4.factor,  data = nhis, robust=0)
                    
        model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                            const(married.factor) + female.factor + smoking4.factor,  data = nhis, robust=0)
                
                results(model1, 3); results(model1, 4); results(model2, 4); results(model1, 10)        
                
                
                
        ## MEN
        model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                            const(married.factor) + smoking4.factor,  data = nhis_male, robust=0)
                
        model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                            const(married.factor) + smoking4.factor,  data = nhis_male, robust=0)
                
                results(model1, 3); results(model1, 4); results(model2, 4); results(model1, 10)        
                
                
                
                
        ## WOMEN
        model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                            const(married.factor) + smoking4.factor,  data = nhis_female, robust=0)
                
        model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                            const(married.factor) + smoking4.factor,  data = nhis_female, robust=0)
                
                results(model1, 3); results(model1, 4); results(model2, 4); results(model1, 10)     
                
                        
                
          

             
# Model 4: Stratified: Non-Hispanic White Only; Adjusted for age (as timescale), education, marital status, and smoking. -----------------------------------------------------------------  
# Change the death type and re-run analyses; causes of death:
# allcause_death 
# heart_death 
# neoplasm_death
# cvd_death
# diabetes_death         
       nhis_white <- filter(nhis, ethnicity.factor=="Non-Hispanic White")
       nhis_female_white <- filter(nhis_female, ethnicity.factor=="Non-Hispanic White")
       nhis_male_white <- filter(nhis_male, ethnicity.factor=="Non-Hispanic White")
                
       
       ## All Participants
       model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                        const(married.factor) + female.factor + smoking4.factor,  data = nhis_white, robust=0)
       
       model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                        const(married.factor) + female.factor + smoking4.factor,  data = nhis_white, robust=0)
       
       results(model1, 3); results(model1, 4); results(model2, 4); results(model1, 10)     
       
       
       
       
       ## MEN
       model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                        const(married.factor) + smoking4.factor,  data = nhis_male_white, robust=0)
       
       model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                        const(married.factor) + smoking4.factor,  data = nhis_male_white, robust=0)
       
       results(model1, 3); results(model1, 4); results(model2, 4); results(model1,10)     
       
       
       
       
       ## WOMEN
       model1 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                        const(married.factor) + smoking4.factor,  data = nhis_female_white, robust=0)
       
       model2 <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(bmi_cat.factor2)*const(alcohol_daily_grams) + const(edu.factor) + 
                                                        const(married.factor) + smoking4.factor,  data = nhis_female_white, robust=0)
       
       results(model1, 3); results(model1, 4); results(model2, 4); results(model1, 10)     
       
       
       
       