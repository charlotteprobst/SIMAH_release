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

hepatitis_data <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Hepatitis_validation.RDS")[[1]]%>% 
  mutate(year = as.numeric(year), data="Simulated",
         microsim.init.sex= ifelse(microsim.init.sex=="m","Men","Women"),
         agegroup =as.character(agegroup),
         agegroup = ifelse(agegroup=="75.","75+",agegroup)) %>% group_by(year, microsim.init.sex) %>% 
  summarise(totalHepB = sum(totalHepB),
            totalHepC = sum(totalHepC)) %>% 
  pivot_longer(totalHepB:totalHepC)


ggplot(data=hepatitis_data, aes(x=year, y=value, colour=name)) + geom_line() + 
  facet_grid(cols=vars(microsim.init.sex))


HepB <- read.csv("SIMAH_workplace/microsim/1_input_data/Incidence_HepB.csv") %>% filter(location_id==102) %>% 
  dplyr::select(year, sex_name, All.Ages)

ggplot(aes(x=year, y=All.Ages, colour=sex_name))




