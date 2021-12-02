#####Wrapper code for dynamic microsimulation
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
options(scipen=999)
# set seed for reproducibility - IMPORTANT - DO NOT CHANGE
# note - this also needs to be ran straight after R has been opened
set.seed(42)

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee
SelectedState <- "USA"

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

N_SAMPLES <- 1

sampleseeds <- expand.grid(seed=1:5, SampleNum=1:N_SAMPLES)
sampleseeds$seed <- sample(1:nrow(sampleseeds), nrow(sampleseeds), replace=F)
Output <- list()
Output <- foreach(i=1:nrow(sampleseeds)) %do% {
run_microsim(i,1,basepop, outwardmigrants, inwardmigrants, deathrates, apply_death_rates,
                       updatingeducation, education_setup, transitionroles,
                       calculate_migration_rates, outward_migration, inward_migration, 
                       brfss,Rates,AlctransitionProbability,
                       transitions, PopPerYear, 2000, 2018)
}


# saveRDS(Output, "output_fullpop.RDS")
saveRDS(Output[[1]], "SIMAH_workplace/microsim/2_output_data/output_fullpop.RDS")
saveRDS(Output[[2]], "SIMAH_workplace/microsim/2_output_data/output_deaths.RDS")

source("SIMAH_code/microsim/2_run_microsimulation/1_functions/compare_output_target.R")

compare <- compare_output_target(Output[[1]])
# compare <- compare %>% filter(Year<=2018)
# compare$percentdiff <- (abs(compare$microsim-compare$target) / compare$target)*100
# compare$target <- ifelse(compare$Year>=2020, NA, compare$target)
compare <- compare %>% pivot_longer(cols=c(target,microsim)) %>% 
  mutate(microsim.init.sex = recode(microsim.init.sex, "m"="Men","f"="Women"),
         microsim.init.race = recode(microsim.init.race, "BLA"="Black","WHI"="White","SPA"="Hispanic",
                                     "OTH"="Others"),
         agecat = as.character(agecat),
         agecat = ifelse(agecat=="18","18-24",
                         ifelse(agecat=="19-24","18-24",agecat))) %>% 
  group_by(Year, microsim.init.sex, microsim.init.race, agecat, name) %>% 
  summarise(value=sum(value)) %>% 
  mutate(microsim.init.race = factor(microsim.init.race, levels=c("Black","White","Hispanic","Others")),
         name = recode(name, "microsim"="Microsim","target"="ACS / Census")) %>% 
  filter(Year<=2018)

plot <- ggplot(data=compare, aes(x=Year, y=value, colour=agecat, linetype=name, alpha=name)) + 
  geom_line(size=1.5) + 
  facet_grid(cols=vars(microsim.init.sex), rows=vars(microsim.init.race), scales="free") + ylim(0,NA) + theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text=element_text(size=16),
        legend.key.width=unit(2,"cm")) + 
  scale_linetype_manual(values=c("solid","dotted")) + ylab("Total population") + 
  ggtitle(paste(SelectedState)) + scale_alpha_manual(values=c(0.5,1))
plot

ggsave(paste("SIMAH_workplace/microsim/2_output_data/plots/demographic", SelectedState, ".png", sep=""), plot, dpi=300, width=33, height=19, units="cm")

source("SIMAH_code/microsim/2_run_microsimulation/1_functions/compare_output_target_education.R")

compare <- compare_output_target_education(Output[[1]])

compare <- compare %>% pivot_longer(cols=c(n,microsim))

compare$data <- ifelse(compare$name=="microsim","microsim",compare$data)
compare <- compare %>% dplyr::select(Year, microsim.init.sex, microsim.init.education, data, value) %>% distinct()
compare <- compare %>% group_by(Year, microsim.init.sex, data) %>% 
  mutate(percent = value/sum(value),
         percent = round(percent, digit=2),
         microsim.init.sex = ifelse(microsim.init.sex=="m","Men","Women"),
         microsim.init.education = factor(microsim.init.education,
                                          levels=c("High school or less", "Some college", "College degree +"))) %>% 
  filter(data!="Census")

ggplot(data=compare, aes(x=Year, y=round(value,digits=2), colour=microsim.init.education, linetype=data)) + 
  geom_line(size=1.5) + 
  facet_grid(cols=vars(microsim.init.sex), scales="free") + ylim(0,NA) + theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text=element_text(size=14)) + 
  scale_linetype_manual(values=c("solid","dotted")) + ylab("Percentage")

ggplot(data=compare, aes(x=Year, y=round(percent,digits=2), colour=microsim.init.education, linetype=data)) + 
  geom_line(size=1.5) + 
  facet_grid(cols=vars(microsim.init.sex), scales="free") + theme_bw() + 
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text=element_text(size=14)) + 
  scale_linetype_manual(values=c("solid","dotted")) + ylab("Percentage") + 
  scale_y_continuous(labels=scales::percent, limits=c(0,1)) + ggtitle(paste(SelectedState))

ggsave(paste("3_plots/", SelectedState, "education_compare.png", sep=""), dpi=300, width=33, height=19, units="cm")

percentdifference <- compare %>% dplyr::select(Year, microsim.init.sex, microsim.init.education, data,
                                               value) %>% pivot_wider(names_from=data, values_from=value) %>% 
  mutate(percentdiff = abs(microsim-ACS)/ACS)


percentdifference <- compare %>% dplyr::select(Year, microsim.init.sex, microsim.init.education, data,
                                               percent) %>% pivot_wider(names_from=data, values_from=percent) %>% 
  mutate(diff = abs(microsim - ACS)) %>% 
  group_by(microsim.init.sex, microsim.init.education) %>% 
  filter(Year<=2018)






