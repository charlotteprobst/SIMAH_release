# script to integrate research attachment work and run distress transitions model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
library(msm)
library(tidyverse)

setwd("~/Google Drive/SIMAH Sheffield")

data <- read_csv("SIMAH_workplace/PSID/alldata_new_1999_2019.csv") %>% 
  filter(relationship!="latino/immigrantsampleunknown")

# Generating three states in numeric format for MSM 
data$distclass <- ifelse(data$kessler_score<5, 1,
                         ifelse(data$kessler_score>=5 & data$kessler_score<13,2,
                                ifelse(data$kessler_score>=13,3, NA)))  
# Dropping under 18
data <- data %>% filter(age>=18)

# Create age categorical variable
data$agegroup <- ifelse(data$age<=34, "18-34",
                        ifelse(data$age>34 & data$age<=64,"35-64","65+"))
data$agegroup<-as.factor(data$agegroup)
summary(data$agegroup)
data$sex<-as.factor(data$sex)
summary(data$sex)
# filter above year 1999 (year kessler started)
# data <- data %>% filter(year>1999)
data <- data %>% ungroup() %>% group_by(uniqueID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) %>% mutate(sex=factor(sex),
                                         sex=ifelse(sex=="female",1,0))
# all data needs to be ordered by ID then year
data <- as.data.frame(lapply(data, unlist))
data <- data[order(data$uniqueID, data$year),]###Change to unique ID

data %>% group_by(year) %>% tally()
data$racefinal <- ifelse(data$individualrace=="Native","other",
                         ifelse(data$individualrace=="Asian/PI","Asian",data$individualrace))
data$racefinal <- as.factor(data$racefinal)
summary(data$racefinal)
data$racefinal <- relevel(data$racefinal, ref="white")

statetable.msm(distclass, uniqueID, data)####First kind of markov model

Q2<-rbind(c(0.5, 0.5, 0),
          c(0.5,0.5,0.5),
          c(0,0.5,0.5))

data$sex <- as.factor(data$sex)
data$education_cat<-as.factor(data$education_cat)
data$racefinal<-as.factor(data$racefinal)

Q2 <- crudeinits.msm(distclass~year, uniqueID, data=data, qmatrix=Q2)# change to unique ID

data$agesq <- data$age^2
data$agescaled <- scale(data$age, center=T)
data$agesqscaled <- scale(data$agesq, center=T)
data$bingebin <- as.factor(ifelse(data$bingedrinkdays>=1, 1,
                        ifelse(data$bingedrinkdays==0, 0, NA)))
data$drinkbin <- as.factor(ifelse(data$AlcCAT=="Medium risk" | data$AlcCAT=="High risk" |
                                    data$AlcCAT=="Very high risk","Medium +", data$AlcCAT))
data$drinkbin <- relevel(data$drinkbin, ref="Non-drinker")
data$employment_stat <- factor(data$employment_stat)

# create income quintiles 
data <- data %>% group_by(year) %>% 
  mutate(income_quintile = ntile(total_fam_income, 5),
         bottom_quintile = as.factor(ifelse(income_quintile==1, 1,0)))

dist.msm0 <- msm(distclass~year, uniqueID, data=data, qmatrix=Q2,
                 center=FALSE,
                 control=list(trace=1, fnscale=761147, maxit=500))###It could work to omit the fnscale
dist.msm0
AIC(dist.msm0)
pmatrix.msm(dist.msm0)

dist.msm1 <- msm(distclass~year, uniqueID, data=data, qmatrix=Q2,
                 center=FALSE,
                 covariates=~education_cat +sex+racefinal + age + drinkbin,
                 control=list(trace=1, fnscale=761147, maxit=500))###It could work to omit the fnscale
dist.msm1
pmatrix.msm(dist.msm1, covariates=list(education_cat="LEHS", sex=0))

dist.msm2 <- msm(distclass~year, uniqueID, data=data, qmatrix=Q2,
                 center=FALSE,
                 covariates=~employment_stat +sex+racefinal + age + drinkbin,
                 control=list(trace=1, fnscale=761147, maxit=500))###It could work to omit the fnscale
dist.msm2

dist.msm3 <- msm(distclass~year, uniqueID, data=data, qmatrix=Q2,
                 center=FALSE,
                 covariates=~bottom_quintile +sex+racefinal + age + drinkbin,
                 control=list(trace=1, fnscale=761147, maxit=500))###It could work to omit the fnscale
dist.msm3

data$homeowner <- ifelse(data$homeowner=="neither", "doesntown",
                         ifelse(data$homeowner=="rents","doesntown",
                                ifelse(data$homeowner=="owns","owns",NA)))

dist.msm4 <- msm(distclass~year, uniqueID, data=data, qmatrix=Q2,
                 center=FALSE,
                 covariates=~homeowner +sex+racefinal + age + drinkbin,
                 control=list(trace=1, fnscale=761147, maxit=500))###It could work to omit the fnscale
dist.msm4


dist.msm5 <- msm(distclass~year, uniqueID, data=data, qmatrix=Q2,
                 center=FALSE,
                 covariates=~homeowner + employment_stat + education_cat + bottom_quintile +
                   sex+racefinal + age + drinkbin,
                 control=list(trace=1, fnscale=761147, maxit=500))###It could work to omit the fnscale
dist.msm5

AIC(dist.msm0, dist.msm1, dist.msm2, dist.msm3, dist.msm4, dist.msm5)

