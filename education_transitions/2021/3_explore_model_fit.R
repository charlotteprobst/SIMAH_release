###### Explore model fit/ internal validity

## Setup
library(tidyverse)  
library(msm)    

# If using laptop
# setwd("C:/Users/cmp21seb/Documents/SIMAH/")
# data <- readRDS("SIMAH_workplace/education_transitions/2021/data_to_model/prepped_data_for_markov_2021.rds")
# modelt6 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/covid_modelt6.RDS")

# If using discomachine
Serverwd <- "/home/sophie"
setwd(Serverwd)
data <- readRDS("/home/sophie/inputs/prepped_data_for_markov_2021.rds")
modelt6 <- readRDS("/home/sophie/inputs/covid_modelt6.RDS")

# Prep data
data$racefinal2 <- data$final_race_using_method_hierarchy
data$racefinal2 <- as.character(data$final_race_using_method_hierarchy) 
data$racefinal2 <- ifelse(data$racefinal2=="Asian/PI","other",data$racefinal2) 
data$racefinal2 <- ifelse(data$racefinal2=="Native","other",data$racefinal2) 
data$racefinal2 <- as.factor(data$racefinal2) 

datat6 <- data %>% filter(year>=2013) 
datat6$timevary <- cut(datat6$year,
                       breaks=c(0,2018, 2021),
                       labels=c("2012-2018", "2019-2021"))

# Provide a rough indication of the goodness of fit of a multi-state model, 
# by estimating the observed numbers of individuals occupying each state at a series of times, 
# and comparing these with forecasts from the fitted model.
output <- prevalence.msm(modelt6,mintime=0,maxtime=20,covariates="population")

# If running this takes too long, three options:

# 1. Use the mean observed values through covariates="mean"
# output <- prevalence.msm(modelt6,times=seq(0,10,1),covariates="mean")
saveRDS(output, "Outputs/prevelance_msm_mean")
# 
# # 2. Loop through each covariate
# 
# combo <- expand.grid(timevary = unique(datat6$timevary), 
#                      agecat = unique(datat6$agecat), 
#                      sex = unique(datat6$sex), 
#                      racefinal2=unique(datat6$racefinal2))
# 
# for(i in 1:nrow(combo)){
#   agecat <- combo$agecat[i]
#   sex <- combo$sex[i]
#   racefinal2 <- combo$racefinal2[i]
#   probs[[paste(i)]] <- prevalence.msm(modelt6,mintime=0,maxtime=5, covariates=list(agecat,sex,racefinal2))}
# 
# # 3. Use the disco machine
# 
# # Equivelant but outputting a plot of observed vs expected prevalences:
# plot.prevalence.msm(modelt6,mintime=0,maxtime=20)