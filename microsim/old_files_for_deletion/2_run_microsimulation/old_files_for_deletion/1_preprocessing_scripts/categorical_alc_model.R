# code to transform categorical alcohol consumption to continuous

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
suppressPackageStartupMessages(library("dplyr"))
library(dplyr)
library(knitr)
library(ipfp)
library(tidyr)
library(janitor)
library(stringr)
library(reshape2)
library(pbapply)
library(ggplot2)
library(gridExtra)
library(readr)
library(readxl)
library(parallel)
library(foreach)
library(faux)
library(splitstackshape)
library(lhs)
library(truncnorm)
library(doParallel)
library(fitdistrplus)
options(scipen=999)



WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(paste(WorkingDirectory))

selectedregion <- "USA"


brfss <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_1984_2020_final.RDS") %>%
  filter(region==selectedregion) %>%
  filter(age_var<=79) %>% filter(YEAR>=2000) %>%
  rename(microsim.init.education = education_summary,
         microsim.init.drinkingstatus=drinkingstatus,
         microsim.init.alc.gpd=gramsperday,
         microsim.init.BMI = BMI,
         microsim.init.income = household_income,
         microsim.init.age=age_var) %>%
  mutate(microsim.init.race = recode(race_eth,"White"="WHI",
                                     "Black"="BLA", "Hispanic"="SPA", "Other"="OTH"),
         microsim.init.sex = recode(sex_recode,"Male"="m","Female"="f"),
         agecat = cut(microsim.init.age,
                      breaks=c(0,24,34,44,54,64,79),
                      labels=c("18.24","25.34","35.44","45.54","55.64","65.79")),
         formerdrinker = ifelse(drinkingstatus_detailed=="Former drinker", 1,0)) %>%
  dplyr::select(YEAR, State, region, microsim.init.race, microsim.init.age,
                microsim.init.sex, microsim.init.education, microsim.init.drinkingstatus,
                microsim.init.alc.gpd, formerdrinker, microsim.init.BMI, microsim.init.income, agecat)


brfss <- brfss %>% mutate(AlcCAT = ifelse(formerdrinker==1, "Former drinker",
                       ifelse(formerdrinker!=1 & microsim.init.alc.gpd==0, "Lifetime abstainer",
                              ifelse(formerdrinker!=1 & microsim.init.sex=="m" & microsim.init.alc.gpd>0 &
                                       microsim.init.alc.gpd<=40, "Low risk",
                                     ifelse(formerdrinker!=1 & microsim.init.sex=="f" & microsim.init.alc.gpd>0 &
                                              microsim.init.alc.gpd<=20, "Low risk",
                                            ifelse(formerdrinker!=1 & microsim.init.sex=="m" & microsim.init.alc.gpd>40 &
                                                     microsim.init.alc.gpd<=60, "Medium risk",
                                                   ifelse(formerdrinker!=1 & microsim.init.sex=="f" & microsim.init.alc.gpd>20 &
                                                            microsim.init.alc.gpd<=40, "Medium risk",
                                                          ifelse(formerdrinker!=1 & microsim.init.sex=="m" & microsim.init.alc.gpd>60,
                                                                 "High risk",
                                                                 ifelse(formerdrinker!=1 & microsim.init.sex=="f" & microsim.init.alc.gpd>40,
                                                                        "High risk", NA)))))))),
                       agecatnew = cut(microsim.init.age,
                                       breaks=c(0,20,25,29,39,49,64,100),
                                       labels=c("18-20","21-25","26-29","30-39",
                                                "40-49","50-64","65+")))
brfss$AlcCAT <- ifelse(brfss$microsim.init.alc.gpd==0, "Non-drinker", brfss$AlcCAT)
library(MASS)

model1 <- lm(microsim.init.alc.gpd ~ AlcCAT + YEAR + AlcCAT*microsim.init.race + AlcCAT*microsim.init.sex + 
               AlcCAT*microsim.init.education + AlcCAT*agecatnew, data=brfss)
summary(model1)

brfss$newconsumption = predict(model1, brfss)
cor.test(brfss$microsim.init.alc.gpd, brfss$newconsumption)

# save a copy of the model in the SIMAH folder 
saveRDS(model1, "SIMAH_workplace/microsim/1_input_data/catcontmodel.RDS")
