# prep brfss data for simulation 
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
library(fitdistrplus)
library(lhs)
library(doParallel)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

SelectedState <- "USA"

brfss <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_1984_2020_final.RDS") %>% 
  filter(age_var<=80) %>% filter(State==SelectedState) %>% 
  mutate(microsim.init.race = recode(race_eth,"White"="WHI","Black"="BLA", "Hispanic"="SPA", "Other"="OTH"),
         microsim.init.sex = recode(sex_recode,"Male"="m","Female"="f"),
         microsim.init.education = education_summary,
         agecat = cut(age_var,
                      breaks=c(0,24,34,44,54,64,100),
                      labels=c("18.24","25.34","35.44","45.54","55.64","65.79")),
         formerdrinker = ifelse(drinkingstatus_detailed=="Former drinker", 1,0),
         microsim.init.BMI = ifelse(BMI<15, 15,
                                    ifelse(BMI>50, 50, BMI))) %>%
  rename(microsim.init.age = age_var,
         microsim.init.drinkingstatus=drinkingstatus,
         microsim.init.alc.gpd=gramsperday,
         microsim.init.income = household_income) %>%
  dplyr::select(YEAR, State, region, microsim.init.race, microsim.init.age,
                microsim.init.sex, microsim.init.education, microsim.init.drinkingstatus,
                microsim.init.alc.gpd, formerdrinker, microsim.init.income, agecat,
                microsim.init.BMI)

source("SIMAH_code/microsim/2_run_microsimulation/0_functions/formerdrinkers_history.R")
lhsSample <- data.frame("BETA_MALE_MORTALITY"=0.0227414,
                        "BETA_FEMALE_MORTALITY"=0.0396643,
                        "BETA_FEMALE_MORBIDITY"=0.0439704,
                        "BETA_FORMER_DRINKERS_MEN"=2.56,
                        "BETA_FORMER_DRINKERS_WOMEN"=2.56,
                        "METABOLIC_BETA1_MALE"=-1.02011,
                        "METABOLIC_BETA2_MALE"=-0.1274623,
                        "METABOLIC_BETA1_FEMALE"=3.03,
                        "METABOLIC_BETA2_FEMALE"=-4.31,
                        "BETA_HEPATITIS"=0.009854,
                        "THRESHOLD"=100000,
                        "THRESHOLD_MODIFIER"=0.66,
                        "IRR_correlation"=0.72,
                        "DECAY_SPEED"=1,
                        "DECAY_LENGTH"=20
)

brfss <- formerdrinkers_history(brfss, lhsSample)
brfss$microsim.init.id <- 1:nrow(brfss)

agesbrfss <- brfss %>% dplyr::select(microsim.init.id, microsim.init.sex, microsim.init.age, microsim.init.alc.gpd) %>%
  mutate(yearstoadd = microsim.init.age-17)
agesbrfss <- expandRows(agesbrfss, "yearstoadd", drop=FALSE)

AgeFunction <- function(data){
  from <- 18
  to <- unique(data$microsim.init.age)
  age <- from:to
  data$newage <- age
  return(data)
}
# # apply the function to each unique individual
agesbrfss <- agesbrfss %>% group_by(microsim.init.id) %>%
  group_modify(~AgeFunction(.))
# categorise age in same categories as Kerr 2013
agesbrfss <- agesbrfss %>% mutate(agecatnew = cut(newage,
                                                  breaks=c(0,20,25,30,40,50,60,70,100),
                                                  labels=c("18-20","21-25","26-30","31-40",
                                                           "41-50","51-60","61-70","71+")),
                                  agecatorig = cut(microsim.init.age,
                                                   breaks=c(0,20,25,30,40,50,60,70,100),
                                                   labels=c("18-20","21-25","26-30","31-40",
                                                            "41-50","51-60","61-70","71+"))) %>%
  dplyr::select(microsim.init.id, microsim.init.sex, microsim.init.age, microsim.init.alc.gpd, newage, agecatnew, agecatorig)
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/HistoryFunction.R")
history <- HistoryFunction(brfss,agesbrfss, lhsSample)
brfss <- left_join(brfss,history, by=c("microsim.init.id")) %>% 
  mutate(Cirrhosis_risk = ifelse(formerdrinker==0 & microsim.init.sex=="m" & 
                                   grams_10years>= 100000, 1,
                                 ifelse(formerdrinker==0 & microsim.init.sex=="f" & 
                                          grams_10years>=100000*0.66, 1, 
                                        ifelse(formerdrinker==1, Cirrhosis_risk, 0))),
         grams_10years = ifelse(formerdrinker==1, former_history,
                                grams_10years)) %>% dplyr::select(-former_history)