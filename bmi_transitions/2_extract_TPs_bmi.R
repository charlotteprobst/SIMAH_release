# script to generate MSM model for education for education transitions paper
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)

setwd("C:/Users/cmp21seb/Documents/SIMAH")

data <- read_rds("SIMAH_workplace/bmi_transitions/BMI_data_cleaned.rds")
model_bmi <- read_rds("SIMAH_workplace/bmi_transitions/BMI_model.RDS")

combo_bmi <- expand.grid(agegroup=unique(data$agegroup), sex=unique(data$sex), 
                         racefinal=unique(data$racefinal), educLAST=unique(data$educLAST))

TPs <- extractTP_bmi(model_bmi, combo_bmi)

# Condense and clean the table
TPs <- TPs %>% select(agegroup, sex, racefinal, educLAST, Transition, prob)

write.csv(TPs, "SIMAH_workplace/bmi_transitions/bmi_TPs.csv", row.names=F)