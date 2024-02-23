# summarise implausibility for publication
#####Wrapper code for dynamic microsimulation
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
options(scipen=999)

###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee
SelectedState <- "USA"

####Size of population 
PopulationSize <- 200000
WholePopSize <- read.csv("SIMAH_workplace/microsim/1_generating_population/constraintsUSA.csv") %>% 
  dplyr::select(marriedF:unmarriedM) %>% mutate(total=marriedF+unmarriedF+marriedM+unmarriedM)
proportion <- PopulationSize/WholePopSize$total

source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/process_cirrhosis_1984_2016.R")

files <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agesp.RDS") %>% 
  do.call(rbind,.) %>% group_by(year, samplenum, microsim.init.sex, agegroup) %>% 
  summarise(microsim = ifelse(is.na(rateper100000),0,rateper100000)) %>% 
  rename(sex=microsim.init.sex)

meansim <- files %>% group_by(year, sex, agegroup, samplenum) %>% summarise(microsim=mean(microsim))
variance <- files %>% group_by(year, sex, agegroup,samplenum) %>% summarise(variance=var(microsim,na.rm=T)) %>% ungroup() %>% 
  group_by(year, sex,agegroup) %>% summarise(meanvariance=mean(variance,na.rm=T),
                                             std.err = sqrt(meanvariance/2))
target <- cirrhosismortality %>% filter(Year>=1984) %>% rename(target=rate, year=Year) %>% 
  dplyr::select(year, sex, agegroup, target) %>% mutate(target=ifelse(is.na(target),0,target))
target <- left_join(target,variance) %>% ungroup() %>% dplyr::select(year, sex, agegroup, target, meanvariance,std.err)
meansim <- left_join(meansim, target)
meansim <- meansim %>% mutate(implausibility=(abs(microsim-target)) / std.err)

maximplausibility <- meansim %>% group_by(samplenum) %>% 
  summarise(maximplausibility=max(implausibility,na.rm=T))

summarytable <- meansim %>% group_by(year, sex, agegroup) %>% 
  filter(year!=1984) %>% 
  filter(agegroup!="15-19") %>% filter(agegroup!="20-24") %>% 
  summarise(minImplausibility = round(min(implausibility),digits=2),
            maxImplausibility = round(max(implausibility),digits=2)
            # range = paste(minImplausibility, "-", maxImplausibility)
            ) %>% 
  # dplyr::select(year, sex, agegroup, range) %>%
  pivot_wider(names_from=agegroup, values_from=c(minImplausibility, maxImplausibility))

