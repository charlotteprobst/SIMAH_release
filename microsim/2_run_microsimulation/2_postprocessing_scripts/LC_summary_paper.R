# summary stats for paper 
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
options(scipen=999)


###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee
SelectedState <- "USA"

####Size of population 
PopulationSize <- 200000
WholePopSize <- read.csv("SIMAH_workplace/microsim/1_generating_population/constraintsUSA.csv") %>% 
  dplyr::select(marriedF:unmarriedM) %>% mutate(total=marriedF+unmarriedF+marriedM+unmarriedM)
proportion <- PopulationSize/WholePopSize$total
# first age-standardized 
files <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agest.RDS") %>% 
  do.call(rbind,.)

source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/process_cirrhosis_1984_2016.R")

age2010 <- files %>% filter(year==2010) %>% 
  ungroup() %>% 
  group_by(year, microsim.init.sex, agegroup) %>% 
  summarise(poptotal = mean(populationtotal)) %>% ungroup() %>% 
  group_by(year, microsim.init.sex) %>% 
  mutate(percent = poptotal / sum(poptotal)) %>% ungroup() %>% dplyr::select(microsim.init.sex, agegroup, percent)

files <- files %>% group_by(year, microsim.init.sex, agegroup)

sim <- left_join(files, age2010) %>% 
  mutate(cirrhosistotal = ifelse(is.na(cirrhosistotal),0, cirrhosistotal)) %>% 
  group_by(year, samplenum, seed, microsim.init.sex, agegroup) %>% 
  mutate(weightedrate = (cirrhosistotal/populationtotal*100000)*percent) %>% ungroup() %>% 
  group_by(year, samplenum, seed, microsim.init.sex) %>% 
  summarise(microsim = sum(weightedrate)) %>% rename(sex=microsim.init.sex) %>% 
  group_by(year, samplenum, sex) %>% 
  mutate(variance = var(microsim,na.rm=T),
         variance = ifelse(is.na(variance),0.001,variance))

meansim <- sim %>% group_by(year, sex, samplenum) %>% 
  summarise(microsim=mean(microsim,na.rm=T),
            meanvariance = mean(variance,na.rm=T),
            se = sqrt(meanvariance /2 )) %>% rename(Year=year)

meansim <- left_join(meansim, cirrhosismortality_agest) %>% 
  rename(target=agestrate) %>% 
  mutate(sex=ifelse(sex=="m","Men","Women")) %>% 
  mutate(difference = abs(microsim-target))

difference <- meansim %>% dplyr::select(Year, sex, microsim, target) %>% 
  group_by(Year, sex) %>% summarise(min = round(min(microsim,na.rm=T),digits=2),
                                    max = round(max(microsim, na.rm=T),digits=2),
                                    mean = round(mean(microsim, na.rm=T),digits=2),
                                    target = round(mean(target,na.rm=T),digits=2),
                                    difference = abs(mean-target),
                                    summary = paste(mean, " (", min, " - ", max, ")")) %>% 
  dplyr::select(Year, sex, summary, difference,target) %>% 
  pivot_wider(names_from=sex, values_from=c(summary,difference,target))

# age specific 
files <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agesp.RDS") %>% 
  do.call(rbind,.) %>% group_by(year, samplenum, microsim.init.sex, agegroup) %>% 
  summarise(microsim = mean(rateper100000),
            microsim = ifelse(is.na(microsim),0,microsim)) %>% 
  rename(sex=microsim.init.sex)
cirrhosismortality <- cirrhosismortality %>% 
  rename(year=Year) %>% 
  dplyr::select(year, sex, agegroup, rate) %>% 
  rename(target=rate)

files <- left_join(files,cirrhosismortality) %>% 
  mutate(agegroup = ifelse(agegroup=="75.","75+", agegroup),
         sex = ifelse(sex=="f","Women","Men")) %>% 
  filter(agegroup!="15-19") %>% filter(agegroup!="20-24") %>% 
  filter(agegroup!="25-34") %>% 
  filter(agegroup!="75+") %>% 
  group_by(year, sex, agegroup) %>% 
  summarise(min=round(min(microsim),digits=2),
            max=round(max(microsim),digits=2),
            mean=round(mean(microsim),digits=2),
            target=round(mean(target),digits=2),
            difference = abs(mean-target),
            summary = paste(mean, " (", min, " - ", max, ")")) %>% 
  dplyr::select(year, sex, agegroup, summary, difference,target) %>% 
  pivot_wider(names_from=agegroup, values_from=c(summary,difference,target)) %>% 
  filter(sex=="Men")


diff <- files %>% dplyr::select(year, agegroup, difference_Men, difference_Women) %>% 
  pivot_longer(difference_Men:difference_Women) %>% 
  mutate(window = ifelse(year<=2010, "calibration","validation")) %>% 
  group_by(agegroup, name, window) %>% summarise(mean=mean(value))
