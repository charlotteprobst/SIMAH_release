# extracting TPs from the final outputs 
setwd("~/Google Drive/SIMAH Sheffield")
gc()
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(ggplot2)
library(metafor)

educ.msm2 <- readRDS("SIMAH_workplace/education_transitions/model1_2019_final.RDS") 
educ.msm4 <- readRDS("SIMAH_workplace/education_transitions/model3_2019_final.RDS")

educ.msm2$call

data <- read_csv("SIMAH_workplace/education_transitions/PSID_reweighted_2019.csv")
data$agescaled <- scale(data$age, center=T)
data$agesqscaled <- scale((data$age^2), center=T)

mapping <- data %>% select(age, agescaled, agesqscaled) %>% 
  distinct()

age <- sort(unique(data$age))
sex <- c(0,1)
data$racefinal <- ifelse(data$racefinal=="Native","other",
                         ifelse(data$racefinal=="Asian/PI","Asian",data$racefinal))
race <- unique(data$racefinal)
source("SIMAH_code/education_transitions/2_analysis/extractTP_function.R")


prob1 <- extractTP(educ.msm2, age, sex, race, mapping)
prob1$Time <- "1999-2009"
prob1$Type <- "interaction"
prob2 <- extractTP(educ.msm4, age, sex, race, mapping)
prob2$Time <- "2011-2019"
prob2$Type <- "interaction"

probs <- rbind(prob1, prob2)

write.csv(probs, "SIMAH_workplace/education_transitions/TP_2019_final.csv", row.names=F)
