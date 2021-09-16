# forrest plot from MSM model
setwd("~/Desktop/repos/SIMAH/Data/Education_transitions/")
gc()
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(ggplot2)
library(metafor)

educ.msm1 <- readRDS("2_analysis/model1_new.RDS")
educ.msm2 <- readRDS("2_analysis/model2_new.RDS")
educ.msm3 <- readRDS("2_analysis/model3_new.RDS")
educ.msm4 <- readRDS("2_analysis/model4_new.RDS")
source("2_analysis/extractTP.R")

data <- read.csv("2_analysis/reweighted_PSID_2019.csv")

mapping <- data.frame(age=unique(data$age))
age <- sort(unique(data$age))
sex <- c(0,1)
educ.msm1
race <- c("black","hispanic","other","white","Asian") 

prob1 <- extractTP(educ.msm1, age, sex, race, mapping)
prob1$Time <- "1999-2009"
prob1$Type <- "interaction"
prob2 <- extractTP(educ.msm3, age, sex, race, mapping)
prob2$Time <- "2011-2019"
prob2$Type <- "interaction"

probs <- rbind(prob1, prob2)
probs$agenew <- NULL

write.csv(probs, "3_outputs/TP_2019.csv", row.names=F)

educ.msm1 <- readRDS("2_analysis/model1_new_sensitivity.RDS")
educ.msm3 <- readRDS("2_analysis/model3_new_sensitivity.RDS")

mapping <- data.frame(age=unique(data$age))
age <- sort(unique(data$age))
sex <- c(0,1)
race <- unique(data$racefinal)

prob1 <- extractTP(educ.msm1, age, sex, race, mapping)
prob1$Time <- "1999-2009"
prob1$Type <- "interaction"
prob2 <- extractTP(educ.msm3, age, sex, race, mapping)
prob2$Time <- "2011-2019"
prob2$Type <- "interaction"
probs <- rbind(prob1, prob2)
probs$agenew <- NULL

write.csv(probs, "3_outputs/TP_2019_sensitivity.csv", row.names=F)
