### SIMAH - NHIS Data
### SES x Health Behavior interaction and mediation

# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(gmodels)    # CrossTable command
library(tableone)   # create table one
library(survival)   # surivval analyses
library(survminer)  # surivval analyses
library(timereg)    # additive survival models
library(survey)     # for survey weighted cox model
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions
#memory.limit(size=1e+13)

# Set the working directory and other file locations

# Personal Computer:
kp <- "C:/Users/klajd/OneDrive/SIMAH"
setwd(kp)
data    <- "SIMAH_workspace/nhis/Data/"
output  <- "SIMAH_workspace/nhis/SES x Behavior/Output/Sensitivity/"
source("SIMAH_code/nhis/0_Function_Formatted_results.R")

    
# Load data
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))

# ****************ANALYSES USING INCOME AS THE MEASURE OF SES *********************
# Aalen Models, Income as SES measure ------------------------------------------------------------------------------------------------------------------------------

# The effect estimates from the model can be directly interpreted as the number of additional events (deaths) per 1 person year at risk
  # Two different versions of the model were ran identify the interaction effect (model with the interaction term) and the joint effect (model with interacting variable) 
  # A "aalen_10000py" function was created to extract the coefficients and multiple them by 10,000 to get estimes per 10,000 person years
  # The results from each model pertain to the effect of "Low SES & healthy behavior", "Bad behavior & high SES", "Low SES & Bad behavior", "Interaction effect"


# FIRST: Run all models and save results  ***************************************************************************************

### Income * Alcohol ************************************************************************************************************

# WOMEN: Interaction model
model <- "_inc_alc_f"   # Used to name the files appropriately: specify health behavior and sex strata
    aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income4.factor)*const(alcohol5v2.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
    saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  
   
      # WOMEN: Joint effect model
      model <- "_inc_alc_f2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.alc) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  
    
    
    
# MEN: Interaction model
model <- "_inc_alc_m"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income4.factor)*const(alcohol5v2.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    

      # MEN: Joint effect model
      model <- "_inc_alc_m2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.alc) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    


    
    
### Smoking * Income *********************************************************************************************************
    
# WOMEN: Interaction model
model <- "_inc_smk_f"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income4.factor)*const(smoking4.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  

      # WOMEN: Joint effect model
      model <- "_inc_smk_f2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.smk) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  


      
# MEN: Interaction model
model <- "_inc_smk_m"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income4.factor)*const(smoking4.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    

      # MEN: Joint effect model
      model <- "_inc_smk_m2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.smk) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    




    
### Income * BMI *********************************************************************************************************
    
# WOMEN: Interaction model
model <- "_inc_bmi_f"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income4.factor)*const(bmi_cat.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  
    
      # WOMEN: Joint effect model
      model <- "_inc_bmi_f2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.bmi) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  
    
     
    
# MEN: Interaction model
model <- "_inc_bmi_m"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income4.factor)*const(bmi_cat.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    
    
      # MEN: Joint effect model
      model <- "_inc_bmi_m2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.bmi) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    
    
    
      
      
### Income * Physical Activity *********************************************************************************************************
    
# WOMEN: Interaction model
model <- "_inc_phy_f"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income4.factor)*const(phy_act3.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  
    
    # WOMEN: Joint effect model
    model <- "_inc_phy_f2"   # Used to name the file
    aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.phy) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
    saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  
    
    
    
# MEN: Interaction model
model <- "_inc_phy_m"   # Used to name the file
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(income4.factor)*const(phy_act3.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    
    
    # MEN: Joint effect model
    model <- "_inc_phy_m2"   # Used to name the file
    aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(inc.phy) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
    saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    
    
    

# Second: Load and view model results  ***************************************************************************************


### Income * Alcohol 
model <- "_inc_alc_f" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))     # Name and load the interaction model 
model <- "_inc_alc_f2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))   # Name and load the joint effect model  
aalen_10000py(aalen, 2); aalen_10000py(aalen, 8); aalen_10000py(aalen2, 21); aalen_10000py(aalen, 40)          # print results of interest

model <- "_inc_alc_m"  ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_inc_alc_m2"  ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))        
aalen_10000py(aalen, 2); aalen_10000py(aalen, 8); aalen_10000py(aalen2, 21); aalen_10000py(aalen, 40)



#### Income * Smoking 
model <- "_inc_smk_f" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))           
model <- "_inc_smk_f2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))           
aalen_10000py(aalen, 2); aalen_10000py(aalen, 7); aalen_10000py(aalen2, 16); aalen_10000py(aalen, 35)

model <- "_inc_smk_m" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_inc_smk_m2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))        
aalen_10000py(aalen, 2); aalen_10000py(aalen, 7); aalen_10000py(aalen2, 16); aalen_10000py(aalen, 35)


#### Income * BMI 
model <- "_inc_bmi_f" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))           
model <- "_inc_bmi_f2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))           
aalen_10000py(aalen, 2); aalen_10000py(aalen, 7); aalen_10000py(aalen2, 16); aalen_10000py(aalen, 35)

model <- "_inc_bmi_m"  ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_inc_bmi_m2"  ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))        
aalen_10000py(aalen, 2); aalen_10000py(aalen, 7); aalen_10000py(aalen2, 16); aalen_10000py(aalen, 35)



#### Income * Physical Activity 
model <- "_inc_phy_f" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))           
model <- "_inc_phy_f2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))           
aalen_10000py(aalen, 2); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 6); aalen_10000py(aalen, 26)

model <- "_inc_phy_m"  ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_inc_phy_m2"  ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))        
aalen_10000py(aalen, 2); aalen_10000py(aalen, 5); aalen_10000py(aalen2, 6); aalen_10000py(aalen, 26)






# WOMEN - Causal Mediation Data Preparation (Income as SES) -----------------------------------------------------------------------------------------

### Step 0: Select data to use **************************************************************************************************************************
# *******************************************************************************************************************************************************
mydata <- nhis_female %>%
  mutate(A.income = income4,
    M1.alc = alcohol5v2,
    M2.smk = smoking4,
    M3.bmi = bmi_cat,
    M4.phy = phy_act3) %>%
  dplyr::select(A.income, M1.alc, M2.smk, M3.bmi, M4.phy, allcause_death, bl_age, end_age, married, ethnicity, srvy_yr)


# specifies the reference category
mydata$A.income <- factor(mydata$A.income, levels=c(1,2,3,4), labels = c("Low","Medium", "High", "Missing"))
mydata$A.income <- relevel(mydata$A.income, ref = "High")

# NOTE: For technical reasons, the mediators should be coded as integers starting with 1


# Select random subset of the sample (if needed to improve speed of analyses)
# set.seed(1234)
# mydata <- sample_frac(mydata, .01) # selects X% of sample at random






### Step 1: Fit a model for each mediator ***************************************************************************************************************
# *******************************************************************************************************************************************************

# Fit model for each mediator, conditioning on exposure (income) and all confounders

mydata$ATemp <- mydata$A.income # first, create and use a copy of the exposure variable (for technical reasons related to R)
fitM1 <- vglm(M1.alc ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 3))
fitM2 <- vglm(M2.smk ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 1))
fitM3 <- vglm(M3.bmi ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 2))
fitM4 <- vglm(M4.phy ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 3))






### Step 2: Construct copies of ID and exposure *********************************************************************************************************
# *******************************************************************************************************************************************************

#Create ID Variable
mydata$ID <- 1:nrow(mydata) # construct id variable

# Create counterfactual version of exposure (income); repeated 4 times because there are 4 mediators
levelsOfINCOME <- unique(mydata$A.income)
myData1 <- mydata
myData2 <- mydata
myData3 <- mydata
myData4 <- mydata
myData1$incom_M1.alc <- levelsOfINCOME[1]
myData2$incom_M1.alc <- levelsOfINCOME[2]
myData3$incom_M1.alc <- levelsOfINCOME[3]
myData4$incom_M1.alc <- levelsOfINCOME[4]
tempMyData <- rbind(myData1, myData2, myData3, myData4)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData4 <- tempMyData
myData1$incom_M2.smk <- levelsOfINCOME[1]
myData2$incom_M2.smk <- levelsOfINCOME[2]
myData3$incom_M2.smk <- levelsOfINCOME[3]
myData4$incom_M2.smk <- levelsOfINCOME[4]
tempMyData <- rbind(myData1, myData2, myData3, myData4)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData4 <- tempMyData
myData1$incom_M3.bmi <- levelsOfINCOME[1]
myData2$incom_M3.bmi <- levelsOfINCOME[2]
myData3$incom_M3.bmi <- levelsOfINCOME[3]
myData4$incom_M3.bmi <- levelsOfINCOME[4]
tempMyData <- rbind(myData1, myData2, myData3, myData4)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData4 <- tempMyData
myData1$incom_M4.phy <- levelsOfINCOME[1]
myData2$incom_M4.phy <- levelsOfINCOME[2]
myData3$incom_M4.phy <- levelsOfINCOME[3]
myData4$incom_M4.phy <- levelsOfINCOME[4]
newMyData <- rbind(myData1, myData2, myData3, myData4)



# Split data into 4 parts to allow for step 3 below (since the dataframe is too large)
nrow(newMyData)
nrow(newMyData)/4
nrow(newMyData)/2
nrow(newMyData)/4*3


newMyData_1 <- newMyData[1:14719616, ]
newMyData_2 <- newMyData[14719617:29439232, ]
newMyData_3 <- newMyData[29439233:44158848, ]
newMyData_4 <- newMyData[44158849:nrow(newMyData), ]

### Step 3: Construct weights  *********************************************************************************************************************
# **************************************************************************************************************************************************

# M1: alcohol
# Part 1
newMyData_1$ATemp <- newMyData_1$A.income
tempDir1a <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M1.alc)]

newMyData_1$ATemp <- newMyData_1$incom_M1.alc
tempIndir1a <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M1.alc)]

# Part 2
newMyData_2$ATemp <- newMyData_2$A.income
tempDir1b <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M1.alc)]

newMyData_2$ATemp <- newMyData_2$incom_M1.alc
tempIndir1b <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M1.alc)]

# Part 3
newMyData_3$ATemp <- newMyData_3$A.income
tempDir1c <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M1.alc)]

newMyData_3$ATemp <- newMyData_3$incom_M1.alc
tempIndir1c <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M1.alc)]

# Part 4
newMyData_4$ATemp <- newMyData_4$A.income
tempDir1d <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M1.alc)]

newMyData_4$ATemp <- newMyData_4$incom_M1.alc
tempIndir1d <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M1.alc)]


# Combine
tempDir1 <- c(tempDir1a, tempDir1b, tempDir1c, tempDir1d)
tempIndir1 <- c(tempIndir1a, tempIndir1b, tempIndir1c, tempIndir1d)

newMyData$weight1 <- tempIndir1/tempDir1




#M2: Smoking
# Part 1
newMyData_1$ATemp <- newMyData_1$A.income
tempDir2a <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M2.smk)]

newMyData_1$ATemp <- newMyData_1$incom_M2.smk
tempIndir2a <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M2.smk)]


# Part 2
newMyData_2$ATemp <- newMyData_2$A.income
tempDir2b <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M2.smk)]

newMyData_2$ATemp <- newMyData_2$incom_M2.smk
tempIndir2b <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M2.smk)]


# Part 3
newMyData_3$ATemp <- newMyData_3$A.income
tempDir2c <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M2.smk)]

newMyData_3$ATemp <- newMyData_3$incom_M2.smk
tempIndir2c <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M2.smk)]


# Part 4
newMyData_4$ATemp <- newMyData_4$A.income
tempDir2d <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M2.smk)]

newMyData_4$ATemp <- newMyData_4$incom_M2.smk
tempIndir2d <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M2.smk)]


# Combine
tempDir2 <- c(tempDir2a, tempDir2b, tempDir2c, tempDir2d)
tempIndir2 <- c(tempIndir2a, tempIndir2b, tempIndir2c, tempIndir2d)

newMyData$weight2 <- tempIndir2/tempDir2




#M3: BMI
# Part 1
newMyData_1$ATemp <- newMyData_1$A.income
tempDir3a <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M3.bmi)]

newMyData_1$ATemp <- newMyData_1$incom_M3.bmi
tempIndir3a <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M3.bmi)]


# Part 2
newMyData_2$ATemp <- newMyData_2$A.income
tempDir3b <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M3.bmi)]

newMyData_2$ATemp <- newMyData_2$incom_M3.bmi
tempIndir3b <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M3.bmi)]

# Part 3
newMyData_3$ATemp <- newMyData_3$A.income
tempDir3c <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M3.bmi)]

newMyData_3$ATemp <- newMyData_3$incom_M3.bmi
tempIndir3c <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M3.bmi)]


# Part 4
newMyData_4$ATemp <- newMyData_4$A.income
tempDir3d <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M3.bmi)]

newMyData_4$ATemp <- newMyData_4$incom_M3.bmi
tempIndir3d <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M3.bmi)]



# Combine
tempDir3 <- c(tempDir3a, tempDir3b, tempDir3c, tempDir3d)
tempIndir3 <- c(tempIndir3a, tempIndir3b, tempIndir3c, tempIndir3d)

newMyData$weight3 <- tempIndir3/tempDir3






#M4: Physical activity
# Part 1
newMyData_1$ATemp <- newMyData_1$A.income
tempDir4a <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M4.phy)]

newMyData_1$ATemp <- newMyData_1$incom_M4.phy
tempIndir4a <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M4.phy)]


# Part 2
newMyData_2$ATemp <- newMyData_2$A.income
tempDir4b <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M4.phy)]

newMyData_2$ATemp <- newMyData_2$incom_M4.phy
tempIndir4b <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M4.phy)]


# Part 3
newMyData_3$ATemp <- newMyData_3$A.income
tempDir4c <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M4.phy)]

newMyData_3$ATemp <- newMyData_3$incom_M4.phy
tempIndir4c <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M4.phy)]

# Part 4
newMyData_4$ATemp <- newMyData_4$A.income
tempDir4d <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M4.phy)]

newMyData_4$ATemp <- newMyData_4$incom_M4.phy
tempIndir4d <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M4.phy)]


# Combine
tempDir4 <- c(tempDir4a, tempDir4b, tempDir4c, tempDir4d)
tempIndir4 <- c(tempIndir4a, tempIndir4b, tempIndir4c, tempIndir4d)

newMyData$weight4 <- tempIndir4/tempDir4



# Final weight
newMyData$weightM <- newMyData$weight1 * newMyData$weight2 * newMyData$weight3 * newMyData$weight4
hist(newMyData$weightM)


# Remove temporary items
rm(fitM1, fitM2, fitM3, fitM4,
  tempIndir1, tempIndir2, tempIndir3, tempIndir4, 
  tempDir1, tempDir2, tempDir3, tempDir4)


## save expanded data
saveRDS(newMyData, file.path(output, "expandedData_income_fem.rds"))


# WOMEN - Causal Mediation Analysis (Income as SES) ----------------------------------------------------------------------------------

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


# Get final results NOTE: THE NUMBERS BELOW HAVE TO BE CHANGED*********************
summary(CMed_model)  
getTE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41))  
getIE_NotRobust(CMed_model, c(3,5,7,9,29,33,37,41))    
getTE_IE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) 




# MEN - Causal Mediation Data Preparation (Income as SES) -----------------------------------------------------------------------------------------             

### Step 0: Select data to use **************************************************************************************************************************
# *******************************************************************************************************************************************************
mydata <- nhis_male %>%
  mutate(A.income = income4,
    M1.alc = alcohol5v2,
    M2.smk = smoking4,
    M3.bmi = bmi_cat,
    M4.phy = phy_act3) %>%
  dplyr::select(A.income, M1.alc, M2.smk, M3.bmi, M4.phy, allcause_death, bl_age, end_age, married, ethnicity, srvy_yr)


# specifies the reference category
mydata$A.income <- factor(mydata$A.income, levels=c(1,2,3,4), labels = c("Low","Medium", "High", "Missing"))
mydata$A.income <- relevel(mydata$A.income, ref = "High")

# NOTE: For technical reasons, the mediators should be coded as integers starting with 1


# Select random subset of the sample (if needed to improve speed of analyses)
# set.seed(1234)
# mydata <- sample_frac(mydata, .10) # selects X% of sample at random






### Step 1: Fit a model for each mediator ***************************************************************************************************************
# *******************************************************************************************************************************************************

# Fit model for each mediator, conditioning on exposure (income) and all confounders

mydata$ATemp <- mydata$A.income # first, create and use a copy of the exposure variable (for technical reasons related to R)
fitM1 <- vglm(M1.alc ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 3))
fitM2 <- vglm(M2.smk ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 1))
fitM3 <- vglm(M3.bmi ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 2))
fitM4 <- vglm(M4.phy ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 3))






### Step 2: Construct copies of ID and exposure *********************************************************************************************************
# *******************************************************************************************************************************************************

#Create ID Variable
mydata$ID <- 1:nrow(mydata) # construct id variable

# Create counterfactual version of exposure (income); repeated 4 times because there are 4 mediators
levelsOfINCOME <- unique(mydata$A.income)
myData1 <- mydata
myData2 <- mydata
myData3 <- mydata
myData4 <- mydata
myData1$incom_M1.alc <- levelsOfINCOME[1]
myData2$incom_M1.alc <- levelsOfINCOME[2]
myData3$incom_M1.alc <- levelsOfINCOME[3]
myData4$incom_M1.alc <- levelsOfINCOME[4]
tempMyData <- rbind(myData1, myData2, myData3, myData4)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData4 <- tempMyData
myData1$incom_M2.smk <- levelsOfINCOME[1]
myData2$incom_M2.smk <- levelsOfINCOME[2]
myData3$incom_M2.smk <- levelsOfINCOME[3]
myData4$incom_M2.smk <- levelsOfINCOME[4]
tempMyData <- rbind(myData1, myData2, myData3, myData4)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData4 <- tempMyData
myData1$incom_M3.bmi <- levelsOfINCOME[1]
myData2$incom_M3.bmi <- levelsOfINCOME[2]
myData3$incom_M3.bmi <- levelsOfINCOME[3]
myData4$incom_M3.bmi <- levelsOfINCOME[4]
tempMyData <- rbind(myData1, myData2, myData3, myData4)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData4 <- tempMyData
myData1$incom_M4.phy <- levelsOfINCOME[1]
myData2$incom_M4.phy <- levelsOfINCOME[2]
myData3$incom_M4.phy <- levelsOfINCOME[3]
myData4$incom_M4.phy <- levelsOfINCOME[4]
newMyData <- rbind(myData1, myData2, myData3, myData4)




# Split data into 4 parts to allow for step 3 below (since the dataframe is too large)
nrow(newMyData)/4
nrow(newMyData)/2
nrow(newMyData)/4*3
nrow(newMyData)

newMyData_1 <- newMyData[1:11889280, ]
newMyData_2 <- newMyData[11889281:23778560, ]
newMyData_3 <- newMyData[23778561:35667840, ]
newMyData_4 <- newMyData[35667841:nrow(newMyData), ]

### Step 3: Construct weights  *********************************************************************************************************************
# **************************************************************************************************************************************************

# M1: alcohol
# Part 1
newMyData_1$ATemp <- newMyData_1$A.income
tempDir1a <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M1.alc)]

newMyData_1$ATemp <- newMyData_1$incom_M1.alc
tempIndir1a <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M1.alc)]

# Part 2
newMyData_2$ATemp <- newMyData_2$A.income
tempDir1b <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M1.alc)]

newMyData_2$ATemp <- newMyData_2$incom_M1.alc
tempIndir1b <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M1.alc)]

# Part 3
newMyData_3$ATemp <- newMyData_3$A.income
tempDir1c <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M1.alc)]

newMyData_3$ATemp <- newMyData_3$incom_M1.alc
tempIndir1c <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M1.alc)]

# Part 4
newMyData_4$ATemp <- newMyData_4$A.income
tempDir1d <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M1.alc)]

newMyData_4$ATemp <- newMyData_4$incom_M1.alc
tempIndir1d <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M1.alc)]


# Combine
tempDir1 <- c(tempDir1a, tempDir1b, tempDir1c, tempDir1d)
tempIndir1 <- c(tempIndir1a, tempIndir1b, tempIndir1c, tempIndir1d)

newMyData$weight1 <- tempIndir1/tempDir1






#M2: Smoking
# Part 1
newMyData_1$ATemp <- newMyData_1$A.income
tempDir2a <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M2.smk)]

newMyData_1$ATemp <- newMyData_1$incom_M2.smk
tempIndir2a <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M2.smk)]


# Part 2
newMyData_2$ATemp <- newMyData_2$A.income
tempDir2b <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M2.smk)]

newMyData_2$ATemp <- newMyData_2$incom_M2.smk
tempIndir2b <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M2.smk)]


# Part 3
newMyData_3$ATemp <- newMyData_3$A.income
tempDir2c <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M2.smk)]

newMyData_3$ATemp <- newMyData_3$incom_M2.smk
tempIndir2c <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M2.smk)]


# Part 4
newMyData_4$ATemp <- newMyData_4$A.income
tempDir2d <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M2.smk)]

newMyData_4$ATemp <- newMyData_4$incom_M2.smk
tempIndir2d <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M2.smk)]


# Combine
tempDir2 <- c(tempDir2a, tempDir2b, tempDir2c, tempDir2d)
tempIndir2 <- c(tempIndir2a, tempIndir2b, tempIndir2c, tempIndir2d)

newMyData$weight2 <- tempIndir2/tempDir2






#M3: BMI
# Part 1
newMyData_1$ATemp <- newMyData_1$A.income
tempDir3a <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M3.bmi)]

newMyData_1$ATemp <- newMyData_1$incom_M3.bmi
tempIndir3a <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M3.bmi)]


# Part 2
newMyData_2$ATemp <- newMyData_2$A.income
tempDir3b <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M3.bmi)]

newMyData_2$ATemp <- newMyData_2$incom_M3.bmi
tempIndir3b <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M3.bmi)]

# Part 3
newMyData_3$ATemp <- newMyData_3$A.income
tempDir3c <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M3.bmi)]

newMyData_3$ATemp <- newMyData_3$incom_M3.bmi
tempIndir3c <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M3.bmi)]


# Part 4
newMyData_4$ATemp <- newMyData_4$A.income
tempDir3d <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M3.bmi)]

newMyData_4$ATemp <- newMyData_4$incom_M3.bmi
tempIndir3d <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M3.bmi)]



# Combine
tempDir3 <- c(tempDir3a, tempDir3b, tempDir3c, tempDir3d)
tempIndir3 <- c(tempIndir3a, tempIndir3b, tempIndir3c, tempIndir3d)

newMyData$weight3 <- tempIndir3/tempDir3








#M4: Physical activity
# Part 1
newMyData_1$ATemp <- newMyData_1$A.income
tempDir4a <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M4.phy)]

newMyData_1$ATemp <- newMyData_1$incom_M4.phy
tempIndir4a <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M4.phy)]


# Part 2
newMyData_2$ATemp <- newMyData_2$A.income
tempDir4b <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M4.phy)]

newMyData_2$ATemp <- newMyData_2$incom_M4.phy
tempIndir4b <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M4.phy)]


# Part 3
newMyData_3$ATemp <- newMyData_3$A.income
tempDir4c <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M4.phy)]

newMyData_3$ATemp <- newMyData_3$incom_M4.phy
tempIndir4c <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_3))[cbind(1:nrow(newMyData_3),newMyData_3$M4.phy)]

# Part 4
newMyData_4$ATemp <- newMyData_4$A.income
tempDir4d <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M4.phy)]

newMyData_4$ATemp <- newMyData_4$incom_M4.phy
tempIndir4d <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_4))[cbind(1:nrow(newMyData_4),newMyData_4$M4.phy)]


# Combine
tempDir4 <- c(tempDir4a, tempDir4b, tempDir4c, tempDir4d)
tempIndir4 <- c(tempIndir4a, tempIndir4b, tempIndir4c, tempIndir4d)

newMyData$weight4 <- tempIndir4/tempDir4







# Final weight
newMyData$weightM <- newMyData$weight1 * newMyData$weight2 * newMyData$weight3 * newMyData$weight4
hist(newMyData$weightM)


# Remove temporary items
rm(fitM1, fitM2, fitM3, fitM4,
  tempIndir1, tempIndir2, tempIndir3, tempIndir4, 
  tempDir1, tempDir2, tempDir3, tempDir4)


## save expanded data
saveRDS(newMyData, file.path(output, "expandedData_income_men.rds"))




# MEN - Causal Mediation Analysis (Income as SES) ----------------------------------------------------------------------------------

# Load data
expandedData <-readRDS(file.path(output, "expandedData_income_men.rds"))

# Run model
CMed_incom_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.income) * const(incom_M1.alc) + 
                                                              const(A.income) * const(incom_M2.smk) +
                                                              const(A.income) * const(incom_M3.bmi) +
                                                              const(A.income) * const(incom_M4.phy) +
                                                              const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                            data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
                          saveRDS(CMed_incom_f, file.path(output, "CMed_incom_m.rds"))       # Save model results
                          CMed_model <-readRDS(file.path(output, "CMed_income_m.rds"))  # Load model results


# Get final results NOTE: THE NUMBERS BELOW HAVE TO BE CHANGED*********************
summary(CMed_model)   
getTE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41))  
getIE_NotRobust(CMed_model, c(3,5,7,9,29,33,37,41))   
getTE_IE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) 




