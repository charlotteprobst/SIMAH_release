### SIMAH - NHIS Data
### SES x Health Behavior interaction and mediation


# SENSITIVITY ANALYSES 4: All Participants (not sex stratified)



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
data    <- "Data"
output  <- "Output/"
source("0_Function_CausalMed_Results.R")

    
# Load data
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))



# Create Expanded Data for Causal Mediation -----------------------------------------------------------------------------------------

### Step 0: Select data and load functions **************************************************************************************************************
# *******************************************************************************************************************************************************
mydata <- nhis %>%
  mutate(A.edu = edu,
        M1.alc = alcohol5v2,
        M2.smk = smoking4,
        M3.bmi = bmi_cat,
        M4.phy = phy_act3) %>%
  dplyr::select(A.edu, M1.alc, M2.smk, M3.bmi, M4.phy,
        allcause_death, bl_age, end_age, female, married, ethnicity, srvy_yr)
    
# specifies the reference category
mydata$A.edu <- factor(mydata$A.edu, levels=c(1,2,3), labels = c("Low", "Med", "High"))
mydata$A.edu <- relevel(mydata$A.edu, ref = "High")
    
    # NOTE: For technical reasons, the mediators should be coded as integers starting with 1
    
    
# Select random subset of the sample
# mydata <- sample_frac(mydata, .01) # selects X% of sample at random
    
    
    
    
    
### Step 1: Fit a model for each mediator ***************************************************************************************************************
# *******************************************************************************************************************************************************
    
# Fit model for each mediator, conditioning on exposure (education) and confounders
    
mydata$ATemp <- mydata$A.edu # first, create and use a copy of the exposure variable (for technical reasons related to R)
fitM1 <- vglm(M1.alc ~ ATemp + bl_age + female + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 3))
fitM2 <- vglm(M2.smk ~ ATemp + bl_age + female + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 1))
fitM3 <- vglm(M3.bmi ~ ATemp + bl_age + female + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 2))
fitM4 <- vglm(M4.phy ~ ATemp + bl_age + female + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 3))
    
    
    
    
    
### Step 2: Construct copies of ID and exposure *********************************************************************************************************
# *******************************************************************************************************************************************************
    
#Create ID Variable
mydata$ID <- 1:nrow(mydata) # construct id variable
    
# Create counterfactual version of exposure (education); repeated 4 times because there are 4 mediators
levelsOfEDU <- unique(mydata$A.edu)
myData1 <- mydata
myData2 <- mydata
myData3 <- mydata
myData1$edu_M1.alc <- levelsOfEDU[1]
myData2$edu_M1.alc <- levelsOfEDU[2]
myData3$edu_M1.alc <- levelsOfEDU[3]
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



# Split data into 2 parts to allow for step 3 below (since the dataframe is too large)
nrow(newMyData)
nrow(newMyData)/2

newMyData_1 <- newMyData[1:16838442, ]
newMyData_2 <- newMyData[16838443:nrow(newMyData), ]


    
    
    
### Step 3: Construct weights  *********************************************************************************************************************
# **************************************************************************************************************************************************

# M1: alcohol
      # Part 1
      newMyData_1$ATemp <- newMyData_1$A.edu
      tempDir1a <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M1.alc)]
      
      newMyData_1$ATemp <- newMyData_1$edu_M1.alc
      tempIndir1a <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M1.alc)]
      
      
      # Part 2
      newMyData_2$ATemp <- newMyData_2$A.edu
      tempDir1b <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M1.alc)]
      
      newMyData_2$ATemp <- newMyData_2$edu_M1.alc
      tempIndir1b <- as.matrix(predict(fitM1,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M1.alc)]
      
      # Combine
      tempDir1 <- c(tempDir1a, tempDir1b)
      tempIndir1 <- c(tempIndir1a, tempIndir1b)
      
      newMyData$weight1 <- tempIndir1/tempDir1





#M2: Smoking
      # Part 1
      newMyData_1$ATemp <- newMyData_1$A.edu
      tempDir2a <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M2.smk)]
      
      newMyData_1$ATemp <- newMyData_1$edu_M2.smk
      tempIndir2a <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M2.smk)]
      
      # Part 2
      newMyData_2$ATemp <- newMyData_2$A.edu
      tempDir2b <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M2.smk)]
      
      newMyData_2$ATemp <- newMyData_2$edu_M2.smk
      tempIndir2b <- as.matrix(predict(fitM2,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M2.smk)]
      
      
      # Combine
      tempDir2 <- c(tempDir2a, tempDir2b)
      tempIndir2 <- c(tempIndir2a, tempIndir2b)
      
      newMyData$weight2 <- tempIndir2/tempDir2





#M3: BMI
      # Part 1
      newMyData_1$ATemp <- newMyData_1$A.edu
      tempDir3a <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M3.bmi)]
      
      newMyData_1$ATemp <- newMyData_1$edu_M3.bmi
      tempIndir3a <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M3.bmi)]
      
      # Part 2
      newMyData_2$ATemp <- newMyData_2$A.edu
      tempDir3b <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M3.bmi)]
      
      newMyData_2$ATemp <- newMyData_2$edu_M3.bmi
      tempIndir3b <- as.matrix(predict(fitM3,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M3.bmi)]
      
      # Combine
      tempDir3 <- c(tempDir3a, tempDir3b)
      tempIndir3 <- c(tempIndir3a, tempIndir3b)
      
      newMyData$weight3 <- tempIndir3/tempDir3



#M4: Physical activity
      # Part 1
      newMyData_1$ATemp <- newMyData_1$A.edu
      tempDir4a <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M4.phy)]
      
      newMyData_1$ATemp <- newMyData_1$edu_M4.phy
      tempIndir4a <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_1))[cbind(1:nrow(newMyData_1),newMyData_1$M4.phy)]
      
      # Part 2
      newMyData_2$ATemp <- newMyData_2$A.edu
      tempDir4b <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M4.phy)]
      
      newMyData_2$ATemp <- newMyData_2$edu_M4.phy
      tempIndir4b <- as.matrix(predict(fitM4,type = "response", newdata=newMyData_2))[cbind(1:nrow(newMyData_2),newMyData_2$M4.phy)]
      
      
      # Combine
      tempDir4 <- c(tempDir4a, tempDir4b)
      tempIndir4 <- c(tempIndir4a, tempIndir4b)
      
      newMyData$weight4 <- tempIndir4/tempDir4





# Final weight
newMyData$weightM <- newMyData$weight1 * newMyData$weight2 * newMyData$weight3 * newMyData$weight4
hist(newMyData$weightM)
    

## save expanded data

saveRDS(newMyData, file.path(output, "expandedData_all.rds"))

