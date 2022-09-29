#Wrapper code for generating microsimulation populations from BRFSS data
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()

######packages######
library(dplyr)
library(knitr)
library(ipfp)
library(tidyr)
library(janitor)
library(stringr)
library(readr)
library(sjmisc)
library(readxl)
library(foreign)
library(splitstackshape)

options(scipen=999)

####wrapper code for generating base population 
set.seed(42)

####EDIT ONLY BELOW HERE ### 

###set working directory to the main folder
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)

####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee, Washington

SelectedState <- "USA"

PopulationSize <- 10000

####EDIT ONLY ABOVE HERE ##

tokeep <- c("SelectedState", "PopulationSize", "WorkingDirectory", "microsim", "tokeep",
            "wholepopsize", "percentpop", "adjusting")

###generate a base population - IPF from NSDUH data to PSID / Census constraints 

source("SIMAH_code/microsim/1_generate_baseline_population/scripts/CASCADE_basepop_IPF.R")

microsimon <- 0
if(microsimon==1){
source("SIMAH_code/microsim/1_generate_baseline_population/scripts/CASCADE_process_for_microsim.R")
}else if(microsimon==0){
microsim <- microsim %>% 
  mutate(microsim.init.sex= ifelse(microsim.init.sex=="M",1,0),
         microsim.init.drinks.per.month = microsim.init.alc.gpd*(1/14)*30,
         microsim.init.annual.frequency = alcdays*12,
         microsim.init.heavy.episodic.drinking = NA) %>% 
  dplyr::select(microsim.init.id, microsim.init.sex, microsim.init.age, microsim.init.race,
                microsim.roles.employment.status, microsim.roles.parenthood.status,
                microsim.roles.marital.status, microsim.init.education,
                microsim.init.income, microsim.init.drinkingstatus,
                microsim.init.heavy.episodic.drinking, microsim.init.alc.gpd,
                microsim.init.annual.frequency, microsim.init.drinks.per.month,
                microsim.init.BMI, formerdrinker)
}

write.csv(microsim, paste("SIMAH_workplace/microsim/1_input_data/agent_files/",SelectedState, "basepopCASCADE", sep="", PopulationSize, ".csv"), row.names=FALSE)
