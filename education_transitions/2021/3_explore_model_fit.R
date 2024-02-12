###### Explore model fit/ internal validity

# Setup
setwd("C:/Users/cmp21seb/Documents/SIMAH/")
library(tidyverse)  
library(msm)        

# Load model data
data <- readRDS("SIMAH_workplace/education_transitions/2021/data_to_model/prepped_data_for_markov_2021.rds")

# Load model
modelt6 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/covid_modelt6.RDS")

plot.prevalence.msm(modelt6,mintime=0,maxtime=20)