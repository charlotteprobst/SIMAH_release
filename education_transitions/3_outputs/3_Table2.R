# generating Table 2 for education transitions paper

setwd("~/Google Drive/SIMAH Sheffield")
gc()
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(ggplot2)
library(metafor)

educ.msm1 <- readRDS("SIMAH_workplace/education_transitions/model1_2019_final.RDS")
educ.msm2 <- readRDS("SIMAH_workplace/education_transitions/model2_2019_final.RDS")
educ.msm3 <- readRDS("SIMAH_workplace/education_transitions/model3_2019_final.RDS")
educ.msm4 <- readRDS("SIMAH_workplace/education_transitions/model4_2019_final.RDS")
source("SIMAH_code/education_transitions/2_analysis/extractdata.R")
AIC(educ.msm1, educ.msm2, educ.msm3, educ.msm4)

model1 <- extractdata(educ.msm1) %>% mutate(time = "1999-2009", model="sexraceage", num=1)
model2 <- extractdata(educ.msm2) %>% mutate(time = "1999-2009", model="sex*raceage", num=2)
model3 <- extractdata(educ.msm3) %>% mutate(time = "2011-2019", model="sexraceage", num=1)
model4 <- extractdata(educ.msm4) %>% mutate(time = "2011-2019", model="sex*raceage", num=2)

model <- rbind(model1, model2, model3, model4)
names <- names(model)[2:25]
model <- model %>% mutate_at(names, function(x) round(x, digits=2)) %>% select_if(~ !any(is.na(.))) %>% 
  pivot_longer(cols=`LEHS->SomeC1_Estimate`:`SomeC3->College_Upper`) %>% 
  separate(name, into=c("from","to","type")) %>% select_if(~ !any(is.na(.))) %>% 
  filter(Variable!="base") %>% mutate(Transition=paste(from, to, sep="_")) %>% pivot_wider(names_from=type, values_from=value) %>% 
  mutate(SE=(Upper-Estimate)/1.96,
         SD = ifelse(time=="1999-2009",
                     SE*(sqrt(119597)),
                     ifelse(time=="2011-2019",
                            SE*(sqrt(137336)), NA)),
         newSE = ifelse(time=="1999-2009",
                        SD / (sqrt(8665)),
                        ifelse(time=="2011-2019",
                               SD / (sqrt(10491)), NA)),
         newLower = (log(Estimate))-(newSE*1.96),
         newUpper = (log(Estimate))+(newSE*1.96),
         newLower = exp(newLower),
         newUpper = exp(newUpper),
         newLower = round(newLower, digits=2),
         newUpper = round(newUpper, digits=2)) %>% select(-c(Lower, Upper, SE,
                                                             SD, newSE)) %>% 
  mutate(EstimateCI = paste(Estimate, " (", newLower, ",", newUpper, ")", sep="")) %>% select(num, time, Variable, Transition, EstimateCI) %>% 
  pivot_wider(names_from=Transition, values_from=EstimateCI) %>% 
  pivot_wider(names_from=time, values_from=c(LEHS_SomeC1:SomeC3_College)) %>% filter(Variable!="agescaled") %>% filter(Variable!="agesqscaled")

model <- model %>% 
  pivot_longer(cols='LEHS_SomeC1_1999-2009':'SomeC3_College_2011-2019') %>% 
  separate(name, into=c("Transition","Year"), sep=-9) %>% 
  pivot_wider(names_from=Transition, values_from=value) %>% arrange(Year,num) %>% filter(num==2)

write.csv(model, "SIMAH_workplace/education_transitions/msmmodel_output_tunnel_paper_2019.csv", row.names=F)

# sensitivity
educ.msm1 <- readRDS("SIMAH_workplace/education_transitions/model1_new_sensitivity.RDS")
educ.msm2 <- readRDS("SIMAH_workplace/education_transitions/model2_new_sensitivity.RDS")
educ.msm3 <- readRDS("SIMAH_workplace/education_transitions/model3_new_sensitivity.RDS")
educ.msm4 <- readRDS("SIMAH_workplace/education_transitions/model4_new_sensitivity.RDS")

model1 <- extractdata(educ.msm1) %>% mutate(time = "1999-2009", model="sexraceage", num=1)
model2 <- extractdata(educ.msm2) %>% mutate(time = "1999-2009", model="sex*raceage", num=2)
model3 <- extractdata(educ.msm3) %>% mutate(time = "2011-2019", model="sexraceage", num=1)
model4 <- extractdata(educ.msm4) %>% mutate(time = "2011-2019", model="sex*raceage", num=2)

model <- rbind(model1, model2, model3, model4)
names <- names(model)[2:25]
options(scipen=999)
model <- model %>% mutate_at(names, function(x) round(x, digits=2)) %>% select_if(~ !any(is.na(.))) %>% 
  pivot_longer(cols=`LEHS->SomeC1_Estimate`:`SomeC3->College_Upper`) %>% 
  separate(name, into=c("from","to","type", sep="_")) %>% select_if(~ !any(is.na(.))) %>% 
  filter(Variable!="base") %>% mutate(Transition=paste(from, to, sep="_")) %>% pivot_wider(names_from=type, values_from=value) %>% 
  mutate(SE=(Upper-Estimate)/1.96,
         SD = ifelse(time=="1999-2009",
                     SE*(sqrt(119597)),
                     ifelse(time=="2011-2019",
                            SE*(sqrt(137336)), NA)),
         newSE = ifelse(time=="1999-2009",
                        SD / (sqrt(8665)),
                        ifelse(time=="2011-2019",
                               SD / (sqrt(10491)), NA)),
         newLower = (log(Estimate))-(newSE*1.96),
         newUpper = (log(Estimate))+(newSE*1.96),
         newLower = exp(newLower),
         newUpper = exp(newUpper),
         newLower = round(newLower, digits=2),
         newUpper = round(newUpper, digits=2)) %>% select(-c(Lower, Upper, SE,
                                                             SD, newSE)) %>% 
  mutate(newUpper = ifelse(newUpper>=100, 100, newUpper)) %>% 
  mutate(EstimateCI = paste(Estimate, " (", newLower, ",", newUpper, ")", sep="")) %>% select(num, time, Variable, Transition, EstimateCI) %>% 
  pivot_wider(names_from=Transition, values_from=EstimateCI) %>% 
  pivot_wider(names_from=time, values_from=c(LEHS_SomeC1:SomeC3_College)) %>% filter(Variable!="age")

write.csv(model, "SIMAH_workplace/education_transitions/msmmodel_output_tunnel_paper_2019_sensitivity.csv", row.names=F)
