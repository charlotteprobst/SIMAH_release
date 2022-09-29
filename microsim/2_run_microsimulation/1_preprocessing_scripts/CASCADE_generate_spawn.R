# generate spawning file 
library(tidyverse)

WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

SelectedState <- "USA"

####Size of population 
PopulationSize <- 10000

proportion <- PopulationSize/WholePopSize$total


WholePopSize <- read.csv("SIMAH_workplace/microsim/1_generating_population/constraintsUSA.csv") %>% 
  dplyr::select(marriedF:unmarriedM) %>% mutate(total=marriedF+unmarriedF+marriedM+unmarriedM)

Rates <- readRDS(paste("SIMAH_workplace/microsim/1_input_data/migration_rates/CASCADEfinal_rates",SelectedState,".RDS",sep=""))
Rates$agecat <- as.character(Rates$agecat)
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/projecting_migration_and_deaths.R")

Rates <- Rates %>% mutate(microsim.init.spawn.year=Year,
                          MigrationInN = MigrationInN*proportion) %>% 
  dplyr::select(microsim.init.spawn.year, agecat, microsim.init.sex, microsim.init.race, MigrationInN)

brfss <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_1984_2020_final.RDS") %>% 
  filter(age_var<=80) %>% filter(State==SelectedState) %>% 
  mutate(microsim.init.race = recode(race_eth,"White"="WHI","Black"="BLA", "Hispanic"="SPA", "Other"="OTH"),
         microsim.init.sex = recode(sex_recode,"Male"="m","Female"="f"),
         microsim.init.education = education_summary,
         agecat = cut(age_var,
                      breaks=c(0,19,24,29,34,39,44,49,54,59,64,69,74,80),
                      labels=c("18","19-24","25-29","30-34","35-39","40-44",
                               "45-49","50-54","55-59","60-64","65-69",
                               "70-74","75-79")),
         formerdrinker = ifelse(drinkingstatus_detailed=="Former drinker", 1,0),
         microsim.init.BMI = ifelse(BMI<15, 15,
                                    ifelse(BMI>50, 50, BMI))) %>%
  ungroup() %>% 
  rename(microsim.init.age = age_var,
         microsim.init.drinkingstatus=drinkingstatus,
         microsim.init.alc.gpd=gramsperday,
         microsim.init.income = household_income,
         microsim.roles.employment.status = employment,
         microsim.roles.marital.status = marital_status) %>% 
  mutate(microsim.roles.parenthood.status = microsim.roles.employment.status,
         microsim.init.drinks.per.month = microsim.init.alc.gpd*(1/14)*30,
         microsim.init.drink.frequency = frequency,
         microsim.init.annual.frequency = frequency*12,
         microsim.init.heavy.episodic.drinking = NA) %>%
  rename(microsim.init.spawn.year=YEAR) %>% 
  dplyr::select(microsim.init.sex, microsim.init.age, microsim.init.race, 
                microsim.roles.employment.status,
                microsim.roles.parenthood.status, 
                microsim.roles.marital.status, microsim.init.education, microsim.init.income,
                microsim.init.drinkingstatus , microsim.init.heavy.episodic.drinking,
                microsim.init.alc.gpd, microsim.init.drink.frequency, microsim.init.drinks.per.month,
                microsim.init.annual.frequency, microsim.init.spawn.year, agecat)

brfss <- left_join(brfss, Rates)              

spawning <- brfss %>% drop_na(MigrationInN) %>% group_by(microsim.init.spawn.year, agecat, microsim.init.sex, microsim.init.race) %>% 
  mutate(tosample = round(MigrationInN)) %>% 
  sample_n(unique(tosample), replace=T) %>% ungroup() %>% dplyr::select(-c(agecat, MigrationInN, tosample))

microsim.init.id <- (PopulationSize+1):(nrow(spawning)+PopulationSize)
spawning <- cbind(microsim.init.id, spawning)

spawning$random = sample(1:365, nrow(spawning), replace=T)
spawning$microsim.spawn.tick <- ((spawning$microsim.init.spawn.year - 1985) * 365 ) + spawning$random
spawning$random <- NULL

write.csv(spawning, paste("SIMAH_workplace/microsim/1_input_data/agent_files/",SelectedState, "spawningCASCADE", sep="", PopulationSize, ".csv"), row.names=FALSE)
