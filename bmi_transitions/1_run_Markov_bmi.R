# script to generate MSM model for BMI transitions
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)

setwd("C:/Users/cmp21seb/Documents/SIMAH")

data <- read_csv("SIMAH_workplace/bmi_transitions/BMI_without_NA.csv")

# BMI states need to be in numeric format for MSM, in the order that they would transition.
# BMI Under 30 = 1, BMI 30 and over = 2
data$BMInum <- ifelse(data$BMI_category =="under 30", 1,
                      ifelse(data$BMI_category=="30 and over",2, NA))
summary(data$BMInum)

data$detailBMIcat <- ifelse(data$detailBMI =="underweight", 1,
                      ifelse(data$detailBMI=="healthy",1,
                             ifelse(data$detailBMI=="overweight",2,
                                    ifelse(data$detailBMI=="class 1", 3,
                                           ifelse(data$detailBMI=="class 2", 4,
                                                  ifelse(data$detailBMI=="class 3", 4, NA))))))
summary(data$detailBMIcat)

#converting age into a categorical variable
data$agegroup <- ifelse(data$age<=34, "18-34",
                        ifelse(data$age>34 &data$age<=64, "35-64", "65+"))

# remove anyone with only one observation (i.e. year of data), as they would give an error in MSM 
data <- data %>% ungroup() %>% group_by(uniqueID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) %>% 
  mutate(sex=factor(sex), sex=ifelse(sex=="female",1,0)) # female=1 male=0.

# Order data by uniqueID, then year.
data <- as.data.frame(lapply(data, unlist))
data <- data[order(data$uniqueID, data$year),]

# recode race
data$racefinal <- ifelse(data$racefinal=="Native","other",
                          ifelse(data$racefinal=="Asian/PI","Asian",data$racefinal))
data$racefinal <- as.factor(data$racefinal)
summary(data$racefinal)
data$racefinal <- relevel(data$racefinal, ref="white") # results are all in reference to white.

# Save the cleaned data
saveRDS(data, "SIMAH_workplace/bmi_transitions/BMI_data_cleaned.rds")

## Markov model

# Generate a frequency table counting the number of times each pair of states were observed in successive observation times 
statetable.msm(BMInum, uniqueID, data) 
# In this instance the most common transition is from 1 to 1 (healthy to healthy), and the least common is from 2 to 1 (obese to healthy)

# Generate a Q matrix, stating the allowed instantaneous transitions - giving a zero for any transition that is allowed. 
# The Q matrix size must match the number of possible transitions in dataset. 
# In this case allowing all transitions (i.e. 1 -> 1, 1 -> 2, 2 -> 2 and 2 -> 2)
Q <- rbind( c(0.5, 0.5),
            c(0.5, 0.5))

# NB. May need to add an E matrix, related to the error that can be in the model, but not currently in use. 
# E <- rbind( c(0, 0.1, 0.1, 0.1, 0.1),
#             c(0.1, 0, 0.1, 0.1, 0.1),
#             c(0.1, 0.1, 0, 0.1, 0.1),
#             c(0.1, 0.1, 0.1, 0, 0.1),
#             c(0.1, 0.1, 0.1, 0.1, 0))


# Update the Q matrix with our data. 
Q <- crudeinits.msm(BMInum~year, uniqueID, data=data, qmatrix=Q)
# NB the output here is the transition intensities (the instantaneous risk of moving from state A to state B)
# The diagonal entries of Q are defined as -(the sum of all the other entries in the row).

# Add constraints for the effects of covariates
# Output is the transition intensities with hazard ratios for each covariate

# model 1 - age group and sex 
BMI.msm1 <- msm(BMInum~year, uniqueID, data=data, qmatrix=Q,
                 center=FALSE,
                 covariates=~agegroup+sex,
                 control=list(trace=1, fnscale=761147, maxit=500))
BMI.msm1 

#need center=FALSE
#trace=1 gives update
#could do age groups 18-34, 35-65, over 65
#can change fnscale to minus2loglink value, if it is not converging. 
#maxit is the number of iterations. 

# model 2 - age group, sex and race
BMI.msm2 <- msm(BMInum~year, uniqueID, data=data, qmatrix=Q,
                 center=FALSE,
                 covariates=~agegroup + sex + racefinal,
                 control=list(trace=1, maxit=500, fnscale=336947))
BMI.msm2

# model 3 - age group, sex, race and education
BMI.msm3 <- msm(BMInum~year, uniqueID, data=data, qmatrix=Q,
                center=FALSE,
                covariates=~agegroup + sex + racefinal+educLAST,
                control=list(trace=1, maxit=500, fnscale=336947))
BMI.msm3

# nb. can gain CIs around these TPs by specifying ci="norm" or ci="boot", but not done by default as computationally intensive

# AICs for model comparisson
AIC(BMI.msm1, BMI.msm2, BMI.msm3) 
# model 3 has lowest AIC therefore continue with model 3

# Convert transition intensities into transition probabilities
pmatrix.msm(BMI.msm3, t=1) # pmatrix gives the probabilities of transitioning over a given time interval (in this case 1 year). Could change to t=2 for two years. 
options(scipen = 999)

# Save selected model which will be used to extract TPs
saveRDS(BMI.msm3, "SIMAH_workplace/bmi_transitions/BMI_model.RDS")


# May repear with more detailed BMI categories - not currently in use
# statetable.msm(detailBMIcat, uniqueID, data)
# 
# Q <- rbind( c(0.5, 0.5, 0.5,0.5),
#             c(0.5, 0.5,0.5,0.5),
#             c(0.5, 0.5,0.5,0.5),
#             c(0.5, 0.5,0.5,0.5)) # why do these add up to > 1 in a row?
# 
# Q <- crudeinits.msm(detailBMIcat~year, uniqueID, data=data, qmatrix=Q)
# 
# detailBMI.msm1 <- msm(detailBMIcat~year, uniqueID, data=data, qmatrix=Q,
#                 center=FALSE,
#                 covariates=~agegroup+sex,
#                 control=list(trace=1, fnscale=197513.6, maxit=500))
# detailBMI.msm1


