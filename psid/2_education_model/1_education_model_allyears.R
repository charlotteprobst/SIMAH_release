# script to generate MSM model for education for education transitions paper
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(readxl)
library(doParallel)
library(foreach)
library(parallel)
library(readxl)


# setwd("/home/cbuckley")
setwd("~/Google Drive/SIMAH Sheffield")

source("SIMAH_code/psid/2_education_model/1_setup_markov_model.R")

#### SCRIPT CAN BE STARTED FROM HERE IF REWEIGHTED DATA WITH IDS EXISTS ####
data <- read_csv("SIMAH_workplace/education_transitions/new_PSID_weighted_IDs_2021.csv")

# do the first analysis on the split time periods 

# # setup the datasets for both time periods
data <- data %>% filter(year>=2005 & year<=2019)

data <- setup_markov_model_formodel(data)

Q <- rbind( c(0.08, 0.08, 0, 0, 0),
            c(0, 0.08, 0.08, 0, 0),
            c(0, 0, 0.08, 0.08, 0),
            c(0, 0, 0, 0.08, 0.08),
            c(0, 0, 0, 0, 0.08))

# data$timevary <- cut(data$year,
#                      breaks=c(0,2005,2011,2018),
#                      labels=c("1999-2005","2006-2011","2012-2018"))

data$agecat <- ifelse(data$age==18, "18",
                        ifelse(data$age==19, "19",
                               ifelse(data$age==20, "20",
                                      ifelse(data$age==21, "21",
                                      ifelse(data$age>=22 & data$age<=25, "22-25","26+")))))

data <- data %>% filter(year<=2019)
data <- data[order(data$newID, data$year),]
length(unique(data$uniqueID))
length(unique(data$newID))

# specify baseline models - just race and ethnicity 
Q <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=data)

# E <- rbind( c(0, 0.1, 0, 0, 0),
#        c(0.1, 0, 0.1, 0, 0),
#        c(0, 0, 0, 0.1, 0),
#        c(0, 0, 0, 0, 0.1),
#        c(0, 0, 0, 0.1, 0))

modelnew <- msm(educNUM~year, newID, data=data, qmatrix=Q,
                                   center=FALSE,
                                   covariates=~agecat + sex + racefinal2,
                        control=list(trace=1, fnscale=2543177, maxit=200))
model
AIC(model, modelnew)

saveRDS(model, "SIMAH_workplace/education_transitions/final_models/formodel_model_alltimes2005_altrace.RDS")


# saveRDS(modelt2, "SIMAH_workplace/education_transitions/final_models/formodel_modelt2_sophie.RDS")
# saveRDS(modelt3, "SIMAH_workplace/education_transitions/final_models/formodel_modelt3_sophie.RDS")

# trying the models for different race and ethnicity groups
hispanic <- data %>% filter(racefinal2=="hispanic")
hispanic <- hispanic[order(hispanic$newID, hispanic$year),]
length(unique(hispanic$uniqueID))
length(unique(hispanic$newID))

model <- msm(educNUM~year, newID, data=hispanic, qmatrix=Q,
             center=FALSE,
             covariates=~agecat + sex,
             control=list(trace=1, fnscale=2543177, maxit=200))
model
saveRDS(model, "SIMAH_workplace/education_transitions/final_models/formodel_model_hispanic.RDS")

white <- data %>% filter(racefinal2=="white")
white <- white[order(white$newID, white$year),]
length(unique(white$uniqueID))
length(unique(white$newID))

model <- msm(educNUM~year, newID, data=white, qmatrix=Q,
             center=FALSE,
             covariates=~agecat + sex,
             control=list(trace=1, fnscale=2543177, maxit=200))
model
saveRDS(model, "SIMAH_workplace/education_transitions/final_models/formodel_model_white.RDS")

black <- data %>% filter(racefinal2=="black")
black <- black[order(black$newID, black$year),]
length(unique(black$uniqueID))
length(unique(black$newID))

model <- msm(educNUM~year, newID, data=black, qmatrix=Q,
             center=FALSE,
             covariates=~agecat + sex,
             control=list(trace=1, fnscale=2543177, maxit=200))
model
saveRDS(model, "SIMAH_workplace/education_transitions/final_models/formodel_model_black.RDS")

other <- data %>% filter(racefinal2=="other")
other <- other[order(other$newID, other$year),]
length(unique(other$uniqueID))
length(unique(other$newID))

model <- msm(educNUM~year, newID, data=other, qmatrix=Q,
             center=FALSE,
             covariates=~agecat + sex,
             control=list(trace=1, fnscale=2543177, maxit=200))
model
saveRDS(model, "SIMAH_workplace/education_transitions/final_models/formodel_model_other.RDS")

data <- data %>% filter(year<=2019)
extractedmodel1 <- extract_coefficients(model, "original","1999-2019",data)

data <- data %>% filter(year>=2005 & year<=2019)
# explore the different models 
extractedmodel2 <- extract_coefficients(model2, "new", "2005-2019", data)

extracted <- rbind(extractedmodel1, extractedmodel2) %>% ungroup() %>% 
  dplyr::select(Variable, Transition, time, Estimate, newLower, newUpper) %>% 
  mutate(Estimate=round(Estimate, digits=2),
         Estimate = paste0(Estimate, " (", newLower, ",",newUpper,")")) %>% 
  dplyr::select(-c(newLower:newUpper)) %>% 
  mutate(Variable = gsub("agecat","",Variable),
         Variable=gsub("racefinal2", "",Variable),
         Variable = ifelse(Variable=="sex1","Female",Variable),
         Variable = str_to_title(Variable)) %>% 
  pivot_wider(names_from=Transition, values_from=Estimate)
getwd()
write.csv(extracted, "SIMAH_workplace/microsim/2_output_data/education_calibration/model_compare.csv",
          row.names=F)
