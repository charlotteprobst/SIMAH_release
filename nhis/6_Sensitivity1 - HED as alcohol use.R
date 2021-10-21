
# SES x Lifestyle Differential Vulnerability & Exposure Project
# Sensitivity Analyses 1: HED as the index of alcohol use


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


# Set the working directory and other file locations

# Personal Computer:
setwd("C:/Users/klajd/OneDrive/SIMAH")
data    <- "SIMAH_workspace/nhis/Data/"
output  <- "SIMAH_workspace/nhis/SES x Behavior/Output/Sensitivity/"
source("SIMAH_code/nhis/0_Function_Formatted_results.R")

# Load data
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))



# Desciptives (HED as alcohol use) ------------------------------------------------------------------------------------------------
tab_hed <-CreateTableOne(vars= c("hed.factor"),
  factorVars = c("hed.factor"), 
  strata= c("edu.factor", "female.factor"), addOverall = TRUE, data=nhis)
table_hed <- print(tab_hed, noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE)
kableone(table_hed)



# Aalen Models (HED as alcohol use) ------------------------------------------------------------------------------------------------

# FIRST: Run all models and save results

# WOMEN: Interaction model
model <- "_hed_f"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(hed.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
saveRDS(aalen, paste0(output, "aalen", model, ".rds"))  ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  

      # WOMEN: Joint effect model
      model <- "_hed_f2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.hed) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_female)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()  


      
# MEN: Interaction model
model <- "_hed_m"   # Used to name the files appropriately: specify health behavior and sex strata
aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.factor)*const(hed.factor) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ;  pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    

      # MEN: Joint effect model
      model <- "_hed_m2"   # Used to name the file
      aalen <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(edu.hed) + const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = nhis_male)
      saveRDS(aalen, paste0(output, "aalen", model, ".rds")) ; pdf(paste0(output, "aalen", model, ".pdf")); plot(aalen); dev.off()    




# SECOND: load the models and view the results
model <- "_hed_f" ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))     # Name and load the interaction model 
model <- "_hed_f2" ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))   # Name and load the joint effect model
aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen, 28)      # Print results of interest


model <- "_hed_m"  ; aalen <-readRDS(paste0(output, "aalen", model, ".rds"))        
model <- "_hed_m2"  ; aalen2 <-readRDS(paste0(output, "aalen", model, ".rds"))  
aalen_10000py(aalen, 1); aalen_10000py(aalen, 5); aalen_10000py(aalen, 28)
      
      




# WOMEN - Causal Mediation Data Preparation (HED as alcohol use) -----------------------------------------------------------------------------------------         

### Step 0: Select data to use **************************************************************************************************************************
# *******************************************************************************************************************************************************
mydata <- nhis_female %>%
  mutate(A.edu = edu,
    M1.hed = hed,
    M2.smk = smoking4,
    M3.bmi = bmi_cat,
    M4.phy = phy_act3) %>%
  dplyr::select(A.edu, M1.hed, M2.smk, M3.bmi, M4.phy, allcause_death, bl_age, end_age, married, ethnicity, srvy_yr) %>%
  filter(!is.na(M1.hed)) # those with missing data on other variables had already been removed

# specifies the reference category
mydata$A.edu <- factor(mydata$A.edu, levels=c(1,2,3), labels = c("Low", "Med", "High"))
mydata$A.edu <- relevel(mydata$A.edu, ref = "High")

# NOTE: For technical reasons, the mediators should be coded as integers starting with 1

table(mydata$M1.hed)

# Select random subset of the sample (if needed to improve speed of analyses)
# set.seed(1234)
# mydata <- sample_frac(mydata, .10) # selects X% of sample at random






### Step 1: Fit a model for each mediator ***************************************************************************************************************
# *******************************************************************************************************************************************************

# Fit model for each mediator, conditioning on exposure (education) and all confounders

mydata$ATemp <- mydata$A.edu # first, create and use a copy of the exposure variable (for technical reasons related to R)
fitM1 <- vglm(M1.hed ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 1))
fitM2 <- vglm(M2.smk ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 1))
fitM3 <- vglm(M3.bmi ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 2))
fitM4 <- vglm(M4.phy ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 3))






### Step 2: Construct copies of ID and exposure *********************************************************************************************************
# *******************************************************************************************************************************************************

#Create ID Variable
mydata$ID <- 1:nrow(mydata) # construct id variable

# Create counterfactual version of exposure (education); repeated 4 times because there are 4 mediators
levelsOfEDU <- unique(mydata$A.edu)
myData1 <- mydata
myData2 <- mydata
myData3 <- mydata
myData1$edu_M1.hed <- levelsOfEDU[1]
myData2$edu_M1.hed <- levelsOfEDU[2]
myData3$edu_M1.hed <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$edu_M2.smk <- levelsOfEDU[1]
myData2$edu_M2.smk <- levelsOfEDU[2]
myData3$edu_M2.smk <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$edu_M3.bmi <- levelsOfEDU[1]
myData2$edu_M3.bmi <- levelsOfEDU[2]
myData3$edu_M3.bmi <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$edu_M4.phy <- levelsOfEDU[1]
myData2$edu_M4.phy <- levelsOfEDU[2]
myData3$edu_M4.phy <- levelsOfEDU[3]
newMyData <- rbind(myData1, myData2, myData3)






### Step 3: Construct weights  *********************************************************************************************************************
# **************************************************************************************************************************************************

# M1: alcohol
newMyData$ATemp <- newMyData$A.edu
tempDir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.hed)]

newMyData$ATemp <- newMyData$edu_M1.hed
tempIndir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.hed)]

newMyData$weight1 <- tempIndir1/tempDir1


#M2: Smoking
newMyData$ATemp <- newMyData$A.edu
tempDir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]

newMyData$ATemp <- newMyData$edu_M2.smk
tempIndir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]

newMyData$weight2 <- tempIndir2/tempDir2


#M3: BMI
newMyData$ATemp <- newMyData$A.edu
tempDir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]

newMyData$ATemp <- newMyData$edu_M3.bmi
tempIndir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]

newMyData$weight3 <- tempIndir3/tempDir3


#M4: Physical activity
newMyData$ATemp <- newMyData$A.edu
tempDir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]

newMyData$ATemp <- newMyData$edu_M4.phy
tempIndir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]

newMyData$weight4 <- tempIndir4/tempDir4



# Final weight
newMyData$weightM <- newMyData$weight1 * newMyData$weight2 * newMyData$weight3 * newMyData$weight4
hist(newMyData$weightM)


# Remove temporary items
rm(fitM1, fitM2, fitM3, fitM4, tempIndir1, tempIndir2, tempIndir3, tempIndir4, tempDir1, tempDir2, tempDir3, tempDir4)


## save expanded data

saveRDS(newMyData, file.path(output, "expandedData_hed_fem.rds"))




# WOMEN - Causal Mediation Analysis (HED as alcohol use) ----------------------------------------------------------------------------------

# Load data
expandedData <-readRDS(file.path(output, "expandedData_hed_fem.rds"))

# Run model
CMed_hed_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.hed) + 
                                                            const(A.edu) * const(edu_M2.smk) +
                                                            const(A.edu) * const(edu_M3.bmi) +
                                                            const(A.edu) * const(edu_M4.phy) +
                                                            const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                                                          data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
saveRDS(CMed_hed_f, file.path(output, "CMed_hed_f.rds"))       # Save model results
CMed_model <-readRDS(file.path(output, "CMed_hed_f.rds"))  # Load model results


# Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
summary(CMed_model)   #Estimates and SE
getTE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41))  # Simulated estimate and SE for total effect and mediated proportions for other effects
getIE_NotRobust(CMed_model, c(3,5,7,9,29,33,37,41))    # Estimate and simulated SE for indirect combined effect
getTE_IE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) # Mediated proportion and simulated 95% CI for mediated proportion of indirect combined effect





# MEN - Causal Mediation Data Preparation (HED as alcohol use) -----------------------------------------------------------------------------------------         

### Step 0: Select data to use **************************************************************************************************************************
# *******************************************************************************************************************************************************
mydata <- nhis_male %>%
  mutate(A.edu = edu,
    M1.hed = hed,
    M2.smk = smoking4,
    M3.bmi = bmi_cat,
    M4.phy = phy_act3) %>%
  dplyr::select(A.edu, M1.hed, M2.smk, M3.bmi, M4.phy, allcause_death, bl_age, end_age, married, ethnicity, srvy_yr)%>%
  filter(!is.na(M1.hed)) # those with missing data on other variables had already been removed

# specifies the reference category
mydata$A.edu <- factor(mydata$A.edu, levels=c(1,2,3), labels = c("Low", "Med", "High"))
mydata$A.edu <- relevel(mydata$A.edu, ref = "High")

# NOTE: For technical reasons, the mediators should be coded as integers starting with 1


# Select random subset of the sample (if needed to improve speed of analyses)
# set.seed(1234)
# mydata <- sample_frac(mydata, .10) # selects X% of sample at random






### Step 1: Fit a model for each mediator ***************************************************************************************************************
# *******************************************************************************************************************************************************

# Fit model for each mediator, conditioning on exposure (education) and all confounders

mydata$ATemp <- mydata$A.edu # first, create and use a copy of the exposure variable (for technical reasons related to R)
fitM1 <- vglm(M1.hed ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 1))
fitM2 <- vglm(M2.smk ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 1))
fitM3 <- vglm(M3.bmi ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 2))
fitM4 <- vglm(M4.phy ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 3))






### Step 2: Construct copies of ID and exposure *********************************************************************************************************
# *******************************************************************************************************************************************************

#Create ID Variable
mydata$ID <- 1:nrow(mydata) # construct id variable

# Create counterfactual version of exposure (education); repeated 4 times because there are 4 mediators
levelsOfEDU <- unique(mydata$A.edu)
myData1 <- mydata
myData2 <- mydata
myData3 <- mydata
myData1$edu_M1.hed <- levelsOfEDU[1]
myData2$edu_M1.hed <- levelsOfEDU[2]
myData3$edu_M1.hed <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$edu_M2.smk <- levelsOfEDU[1]
myData2$edu_M2.smk <- levelsOfEDU[2]
myData3$edu_M2.smk <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$edu_M3.bmi <- levelsOfEDU[1]
myData2$edu_M3.bmi <- levelsOfEDU[2]
myData3$edu_M3.bmi <- levelsOfEDU[3]
tempMyData <- rbind(myData1, myData2, myData3)

myData1 <- tempMyData
myData2 <- tempMyData
myData3 <- tempMyData
myData1$edu_M4.phy <- levelsOfEDU[1]
myData2$edu_M4.phy <- levelsOfEDU[2]
myData3$edu_M4.phy <- levelsOfEDU[3]
newMyData <- rbind(myData1, myData2, myData3)






### Step 3: Construct weights  *********************************************************************************************************************
# **************************************************************************************************************************************************

# M1: alcohol
newMyData$ATemp <- newMyData$A.edu
tempDir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.hed)]

newMyData$ATemp <- newMyData$edu_M1.hed
tempIndir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.hed)]

newMyData$weight1 <- tempIndir1/tempDir1


#M2: Smoking
newMyData$ATemp <- newMyData$A.edu
tempDir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]

newMyData$ATemp <- newMyData$edu_M2.smk
tempIndir2 <- as.matrix(predict(fitM2,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M2.smk)]

newMyData$weight2 <- tempIndir2/tempDir2


#M3: BMI
newMyData$ATemp <- newMyData$A.edu
tempDir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]

newMyData$ATemp <- newMyData$edu_M3.bmi
tempIndir3 <- as.matrix(predict(fitM3,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M3.bmi)]

newMyData$weight3 <- tempIndir3/tempDir3


#M4: Physical activity
newMyData$ATemp <- newMyData$A.edu
tempDir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]

newMyData$ATemp <- newMyData$edu_M4.phy
tempIndir4 <- as.matrix(predict(fitM4,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M4.phy)]

newMyData$weight4 <- tempIndir4/tempDir4



# Final weight
newMyData$weightM <- newMyData$weight1 * newMyData$weight2 * newMyData$weight3 * newMyData$weight4
hist(newMyData$weightM)


# Remove temporary items
rm(fitM1, fitM2, fitM3, fitM4, tempIndir1, tempIndir2, tempIndir3, tempIndir4, tempDir1, tempDir2, tempDir3, tempDir4)


## save expanded data

saveRDS(newMyData, file.path(output, "expandedData_hed_male.rds"))




# WOMEN - Causal Mediation Analysis (HED as alcohol use)  ----------------------------------------------------------------------------------

# Load data
expandedData <-readRDS(file.path(output, "expandedData_hed_male.rds"))

# Run model
CMed_hed_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.edu) * const(edu_M1.hed) + 
                                                            const(A.edu) * const(edu_M2.smk) +
                                                            const(A.edu) * const(edu_M3.bmi) +
                                                            const(A.edu) * const(edu_M4.phy) +
                                                            const(married) + factor(ethnicity) + const(factor(srvy_yr)),
                                                          data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
saveRDS(CMed_hed_m, file.path(output, "CMed_hed_m.rds"))       # Save model results
CMed_model <-readRDS(file.path(output, "CMed_hed_m.rds"))  # Load model results



# Get final results. NOTE: THE NUMBERS BELOW MAY HAVE TO BE CHANGED IF A DIFFERENT MODEL IS USED
summary(CMed_model)   #Estimates and SE
getTE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41))  # Simulated estimate and SE for total effect and mediated proportions for other effects
getIE_NotRobust(CMed_model, c(3,5,7,9,29,33,37,41))    # Estimate and simulated SE for indirect combined effect
getTE_IE_NotRobust(CMed_model, c(1,3,5,7,9,29,33,37,41), c(3,5,7,9,29,33,37,41)) # Mediated proportion and simulated 95% CI for mediated proportion of indirect combined effect








