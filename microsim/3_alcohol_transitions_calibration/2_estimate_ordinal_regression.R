
# SIMAH - script for estimating ordinal regression for alcohol transitions 

# Load packages and data
library(tidyverse)  # data management
library(ZIM)

# WorkingDirectory <- "/Users/charlottebuckley/Google Drive/SIMAH Sheffield"

# set wd and install the microsim and calibration packages
setwd(paste(WorkingDirectory))

# Load in deterministic population
deterministic_pop <- read_csv("/Users/charlottebuckley/Google Drive/SIMAH Sheffield/SIMAH_workplace/microsim/alcohol_calibration/deterministic_regression/full_pop_deterministic10000_withID.csv") 
deterministic_pop$alc_gpd <- deterministic_pop$microsim.init.alc.gpd
deterministic_pop$sex <- deterministic_pop$microsim.init.sex

deterministic_pop <- code_alcohol_categories(deterministic_pop)

deterministic_selected <- deterministic_pop %>% 
  mutate(abstainer=ifelse(AlcCAT=="Non-drinker",1,0)) %>% 
  mutate(agecat = cut(microsim.init.age,
                      breaks=c(0,24,64,100),
                      labels=c("18-24","25-64","65+"))) %>% 
  dplyr::select(brfssID, microsim.init.id, year, agecat, 
                microsim.init.race, microsim.init.sex,
                microsim.init.education, microsim.init.alc.gpd,
                AlcCAT, abstainer) %>% 
  mutate(alc_rounded = ceiling(microsim.init.alc.gpd),
         alc_rounded = ifelse(alc_rounded>200, 200, alc_rounded)) %>% 
  arrange(microsim.init.id, year) %>% 
  group_by(microsim.init.id) %>% 
  mutate(lagged_alcohol = lag(microsim.init.alc.gpd, n = 1),
         lagged_abstainer = lag(abstainer, n=1)) %>% 
  ungroup() %>% 
  rename(age3_2 = agecat,
         female.factor_2=microsim.init.sex,
         race.factor_2=microsim.init.race,
         edu3_2 = microsim.init.education,
         alc_daily_g_1 = lagged_alcohol,
         abstainer_1 = lagged_abstainer) %>% 
  mutate(female.factor_2=ifelse(female.factor_2=="m","Men","Women"),
         race.factor_2 = ifelse(race.factor_2=="BLA","Black, non-Hispanic",
                                ifelse(race.factor_2=="WHI","White, non-Hispanic",
                                       ifelse(race.factor_2=="OTH","Other, non-Hispanic",
                                              ifelse(race.factor_2=="SPA","Hispanic", NA)))),
         edu3_2 = ifelse(edu3_2=="LEHS","Low",
                         ifelse(edu3_2=="SomeC","Med","High")),
         edu3_2 = ifelse(edu3_2 =="High" & age3_2=="18-24", "Med", edu3_2),
         race.factor_2 = factor(race.factor_2, levels=c("Black, non-Hispanic",
                                                        "Hispanic","Other, non-Hispanic",
                                                        "White, non-Hispanic")),
         race.factor_2 = relevel(race.factor_2,
                                  ref="White, non-Hispanic"))

# create lagged variables
deterministic_selected <- deterministic_selected %>% 
  group_by(microsim.init.id) %>% 
  arrange(microsim.init.id, year) %>% 
  #create a lagged variable for alcohol category
  mutate(lagged_cat = lag(AlcCAT),
         cat1_lag = ifelse(lagged_cat=="Low risk", 1,0),
         cat2_lag = ifelse(lagged_cat=="Medium risk", 1,0),
         cat3_lag = ifelse(lagged_cat=="High risk", 1,0),
         cat1 = ifelse(AlcCAT=="Low risk", 1,0),
         cat2 = ifelse(AlcCAT=="Medium risk", 1,0),
         cat3 = ifelse(AlcCAT=="High risk", 1,0),
         lagged_age = lag(age3_2),
         lagged_education = lag(edu3_2)) %>% ungroup()

deterministic_selected$AlcCAT <- relevel(factor(deterministic_selected$AlcCAT),
                                         ref='Non-drinker')

deterministic_selected$time <- deterministic_selected$year-2000

deterministic_selected$AlcCAT <- factor(deterministic_selected$AlcCAT,
                                        levels=c("Non-drinker","Low risk",
                                                 "Medium risk","High risk"))

test <- deterministic_selected %>% dplyr::select(microsim.init.id, year, microsim.init.alc.gpd) %>% pivot_wider(names_from=year, values_from=microsim.init.alc.gpd)

library(MASS)
fit <- polr(AlcCAT ~ cat1_lag*lagged_age + cat2_lag*lagged_age + cat3_lag*lagged_age +
              female.factor_2*lagged_age + female.factor_2*lagged_education + 
              female.factor_2*race.factor_2 + lagged_education*cat1_lag + 
              lagged_education*cat2_lag + lagged_education*cat3_lag, data=deterministic_selected, Hess=T,
            control=list(maxit=10000))
summary(fit)

TPmodel <- data.frame(summary(fit)$coefficients)
TPmodel$name <- rownames(TPmodel)
write.csv(TPmodel, "SIMAH_workplace/microsim/2_output_data/alcohol_calibration/alcohol_ordinal_model.csv", row.names=F)
