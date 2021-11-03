# SIMAH project November 2021 

# code for calibration of MSM model parameters to state-level education outputs 

# first set up for microsimulation 
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
library(doParallel)
options(scipen=999)
# set seed for reproducibility - IMPORTANT - DO NOT CHANGE
# note - this also needs to be ran straight after R has been opened
set.seed(42)

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
# WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee
SelectedState <- "Texas"

####Size of population 
PopulationSize <- 1000000

# what proportion of the population does this represent - change to ifelse with all pop sizes when other states added 
WholePopSize <- read.csv("SIMAH_workplace/microsim/1_input_data/fullpopcounts.csv") %>% 
  filter(STATE==SelectedState)

proportion <- PopulationSize/WholePopSize$total
proportion <- ifelse(proportion>1,1,proportion)

#####first read in and process all the necessary data files 
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/load_files.R")
# load in the education transitions data
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/education_transitions.R")

# load in the alcohol transitions data 
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/alcohol_transitions.R")

# load all functions for running the microsimulation - death rates, migration, transition education
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/apply_death_rates.R")
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/outward_migration.R")
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/inward_migration.R")
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/education_setup.R")
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/transition_ed.R")
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/transition_alcohol.R")

# load the function for running the simulation
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/simulation.R")

# switch on and off migration and deaths
migrationdeaths <- 1

# switch on and off education updates
updatingeducation <- 1

# switch on and off alcohol updates
updatingalcohol <- 0

Rates <- readRDS(paste("SIMAH_workplace/microsim/1_input_data/migration_rates/final_rates",SelectedState,".RDS",sep=""))
Rates$agecat <- as.character(Rates$agecat)

# now sample parameters for the education transitions
source("SIMAH_code/microsim/2_run_microsimulation/education_transitions_calibration/extract_uncertainty.R")

# save samples 
saveRDS(transitionsList, "SIMAH_workplace/microsim/2_output_data/transitionsList.RDS")
# length(transitionsList)

# transitionsList <- transitionsList[1:2]

registerDoParallel(30)
# registerDoSNOW(c1)
# plan(multicore, workers=24)
options(future.rng.onMisuse="ignore")
options(future.globals.maxSize = 10000 * 1024^3)
options(future.fork.multithreading.enable = FALSE)

Output <- foreach(i=1:length(transitionsList), .inorder=FALSE,
                     .packages=c("dplyr","tidyr","foreach")) %dopar% {
                       samplenum <- i
                       seed <- Sys.time()
                       print(i)
                       run_microsim(seed,samplenum,basepop, outwardmigrants, inwardmigrants, deathrates, apply_death_rates,
                       updatingeducation, education_setup, transitionroles,
                       calculate_migration_rates, outward_migration, inward_migration, 
                       brfss,Rates,AlctransitionProbability,
                       transitionsList[[i]], PopPerYear, 2000, 2018)
                     }


# get target data 
source("SIMAH_code/microsim/2_run_microsimulation/2_postprocessing_scripts/process_education_compare.R")

# calculate error from target data 
Output <- do.call(rbind,Output)
Output <- Output %>% group_by(samplenum, year, microsim.init.sex, microsim.init.education) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% 
  group_by(samplenum, year, microsim.init.sex) %>% 
  mutate(microsimpercent = n/sum(n),
         year=as.numeric(year))
Output <- left_join(Output, target)

error <- left_join(Output,target) %>% ungroup() %>% 
  group_by(samplenum) %>% 
  mutate(errorsq = (microsimpercent-targetpercent)^2) %>% 
  group_by(samplenum) %>% 
  summarise(RMSE = sqrt(mean(errorsq)))

write.csv(error, "SIMAH_workplace/microsim/2_output_data/error_RMSE.csv")

# graph <- Output %>% pivot_longer(cols=microsimpercent:targetpercent, values_to="percent") %>% 
#   mutate(samplenum = ifelse(name=="targetpercent", "target", samplenum))

Output <- Output %>% mutate(microsim.init.sex = recode(microsim.init.sex,
                                                       "f"="Women","m"="Men"),
                            microsim.init.education = recode(microsim.init.education,
                                                             "LEHS"="High school or less",
                                                             "SomeC"="Some college",
                                                             "College"="College degree plus"),
                            microsim.init.education = factor(microsim.init.education,
                                                             levels=c("High school or less",
                                                                      "Some college",
                                                                      "College degree plus")))
scaleFUN <- function(x) sprintf("%.2f", x)
ggplot(data=Output, aes(x=year, y=microsimpercent, colour=as.factor(samplenum))) + 
  geom_line(linetype="dashed") + geom_line(aes(x=year, y=targetpercent), colour="black") + 
  facet_grid(cols=vars(microsim.init.sex), rows=vars(microsim.init.education), scales="free") + 
  theme_bw() + scale_y_continuous(labels=scales::percent, limits=c(0,NA)) + 
  theme(legend.position="bottom",
        legend.title=element_blank()) + 
  ylab("percentage in category")
ggsave("SIMAH_workplace/microsim/2_output_data/plots/education_states_compare.png",
       dpi=300, width=33, height=19, units="cm")

