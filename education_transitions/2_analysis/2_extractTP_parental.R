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

data <- read_rds("SIMAH_workplace/reweighted_PSID_imp_list.RDS")
data <- data[[1]]

modelst1_baseline <- read_rds("SIMAH_workplace/education_transitions/final_models/t1_baseline_parent.RDS")
modelst2_baseline <- read_rds("SIMAH_workplace/education_transitions/final_models/t2_baseline_parent.RDS")
modelst1_parent <- read_rds("SIMAH_workplace/education_transitions/final_models/t1_interaction.RDS")
modelst2_parent <- read_rds("SIMAH_workplace/education_transitions/final_models/t2_interaction.RDS")

data1 <- data %>% filter(year<=2009)
data2 <- data %>% filter(year>=2011)

data$agescaled <- scale(data$age, center=T)
data$agesqscaled <- scale(data$age^2, center=T)

mapping <- data %>% ungroup() %>% select(age, agescaled, agesqscaled) %>% 
  distinct()
names(mapping) <- c("age","agescaled","agesqscaled")

age <- sort(unique(data$age))
sex <- c(0,1)
data$racefinal <- ifelse(data$racefinal2=="Native","other",
                         ifelse(data$racefinal2=="Asian/PI","other",data$racefinal2))
unique(data$racefinal2)
racevars <- c("white","black","hispanic","other")

combo <- expand.grid(age=unique(data$age), sex = c(0,1), race = racevars, oneCollegeplus = unique(data$oneCollegeplus))

source("SIMAH_code/education_transitions/2_analysis/extractTP_function.R")

baselineprobst1 <- lapply(modelst1_baseline, extractTP, combo=combo, mapping=mapping) %>% do.call(rbind,.) %>% 
  group_by(Transition, age, sex, racefinal, oneCollegeplus) %>% 
  summarise(meanprob = mean(prob), min = min(prob), max=max(prob)) %>% 
  mutate(Time = "1999-2009", model="Baseline")

baselineprobst2 <- lapply(modelst2_baseline, extractTP, combo=combo, mapping=mapping) %>% do.call(rbind,.) %>% 
  group_by(Transition, age, sex, racefinal, oneCollegeplus) %>% 
  summarise(meanprob = mean(prob), min = min(prob), max=max(prob)) %>% 
  mutate(Time = "2011-2019", model="Baseline")

modelst1_parent = modelst1_parent[-which(sapply(modelst1_parent, is.null))]
modelst2_parent = modelst2_parent[-which(sapply(modelst2_parent, is.null))]


interactionprobst1 <- lapply(modelst1_parent, extractTP, combo=combo, mapping=mapping) %>% do.call(rbind,.) %>% 
  group_by(Transition, age, sex, racefinal, oneCollegeplus) %>% 
  summarise(meanprob = mean(prob), min = min(prob), max=max(prob)) %>% 
  mutate(Time = "1999-2009", model="Interaction")

interactionprobst2 <- lapply(modelst2_parent, extractTP, combo=combo, mapping=mapping) %>% do.call(rbind,.) %>% 
  group_by(Transition, age, sex, racefinal, oneCollegeplus) %>% 
  summarise(meanprob = mean(prob), min = min(prob), max=max(prob)) %>% 
  mutate(Time = "2011-2019", model="Interaction")

probs <- rbind(baselineprobst1, baselineprobst2, interactionprobst1, interactionprobst2)

write.csv(probs, "SIMAH_workplace/education_transitions/final_models/TP_parental_interaction.csv", row.names=F)
