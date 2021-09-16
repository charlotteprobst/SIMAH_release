### SIMAH - NHIS Data
### SES x Health Behavior interaction and mediation


# LOAD DATA AND SET FILE LOCATIONS

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
data    <- "Data/"
output  <- "Output/"
source("0_Function_CausalMed_Results.R")

    
# Load data
nhis        <- readRDS (file.path(data, "nhis.rds"))
nhis_male   <- readRDS (file.path(data, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data, "nhis_female.rds"))



# OBJECTIVE 2: Causal Mediation - FEMALES

# For more details and theoretical justification/description see:
    # Lange et al. 2014 https//doi.org/10.1093/aje/kwt270
    # Lange et al. 2012 https//doi.org/10.1093/aje/kwr525
    # Lange et al. 2011 https//doi.org/10.1097/EDE.0b013e31821c680c
             
             
### Step 0: Select data to use **************************************************************************************************************************
# *******************************************************************************************************************************************************
mydata <- nhis_female %>%
  mutate(A.edu = edu,
    M1.alc = alcohol5v2,
    M2.smk = smoking4,
    M3.bmi = bmi_cat,
    M4.phy = phy_act3) %>%
  dplyr::select(A.edu, M1.alc, M2.smk, M3.bmi, M4.phy, allcause_death, bl_age, end_age, married, ethnicity, srvy_yr)

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
fitM1 <- vglm(M1.alc ~ ATemp + bl_age + married + factor(ethnicity) + factor(srvy_yr), data = mydata, family=multinomial(refLevel = 3))
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






### Step 3: Construct weights  *********************************************************************************************************************
# **************************************************************************************************************************************************

# M1: alcohol
newMyData$ATemp <- newMyData$A.edu
tempDir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]

newMyData$ATemp <- newMyData$edu_M1.alc
tempIndir1 <- as.matrix(predict(fitM1,type = "response", newdata=newMyData))[cbind(1:nrow(newMyData),newMyData$M1.alc)]

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
rm(fitM1, fitM2, fitM3, fitM4,
   tempIndir1, tempIndir2, tempIndir3, tempIndir4, 
   tempDir1, tempDir2, tempDir3, tempDir4)


## save expanded data

saveRDS(newMyData, file.path(output, "expandedData_fem.rds"))



