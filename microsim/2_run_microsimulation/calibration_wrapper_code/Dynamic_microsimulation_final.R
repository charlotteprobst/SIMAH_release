#####Wrapper code for dynamic microsimulation
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()

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

model <- "SIMAH"

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
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/projecting_migration_and_deaths.R")
Output <- list()

Output <- run_microsim(1,1,basepop, outwardmigrants, inwardmigrants, deathrates, apply_death_rates,
                       updatingeducation, education_setup, transitionroles,
                       calculate_migration_rates, outward_migration, inward_migration, 
                       brfss,Rates,AlctransitionProbability,
                       transitions, PopPerYear, 2000, 2020)

source("SIMAH_code/microsim/2_run_microsimulation/2_postprocessing_scripts/postprocess_alcohol.R")

# calculate error from target data 
Output <- do.call(rbind,Output)
Output <- Output %>% group_by(samplenum, year, microsim.init.sex, microsim.init.education,
                              AlcCAT, .drop=FALSE) %>% 
  summarise(n=sum(n)) %>% ungroup() %>% 
  group_by(samplenum, year, microsim.init.sex, microsim.init.education) %>% 
  mutate(microsimpercent = n/sum(n),
         year=as.numeric(as.character(year))) %>% dplyr::select(-n)
Output <- left_join(Output, target)


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
  summarise(value=sum(value),
            lower_ci = lower_ci,
            upper_ci = upper_ci) %>% 
  mutate(microsim.init.race = factor(microsim.init.race, levels=c("Black","White","Hispanic","Others")),
         name = recode(name, "microsim"="Microsim","target"="ACS / Census"))
compare$Year <- as.numeric(as.character(compare$Year))

men <- compare %>% filter(microsim.init.sex=="Men")
bars <- men %>% 
  filter(name=="ACS / Census")
points <- men %>% 
  filter(name=="Microsim")
p <- ggplot(men, aes(value))
p

p1 <- p + geom_bar(data=bars, aes(fill=name, x=Year, y=value, group=name), colour="darkblue", stat="identity", position="dodge") +
  geom_errorbar(data=bars, aes(x=Year, group=name, ymin=lower_ci, ymax=upper_ci)) + 
  geom_point(data=points, aes(x=Year, y=value, group=name, shape=name), colour="darkblue", size=2) + 
  facet_grid(cols=vars(agecat),rows=vars(microsim.init.race),scales="free") + 
  geom_line(data=points, aes(x=Year, y=value, group=name), colour="darkblue") + 
  scale_fill_manual(name="",values="white") + scale_shape_manual(name="", values=18) +
  theme_classic() + ylab("Total Population") + xlab("Year") +theme(legend.title=NULL, legend.margin = margin(-1,0,0,0, "cm")) + 
  ggtitle("population comparison - men")
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 13000000),
  #                    labels=function(x) 
  #                      format(x, big.mark=",", scientific=FALSE))
p1
ggsave("SIMAH_workplace/microsim/2_output_data/plots/compare_pop_2020_M.png",
       dpi=300, width=33, height=19, units="cm")

women <- compare %>% filter(microsim.init.sex=="Women")
bars <- women %>% 
  filter(name=="ACS / Census")
points <- women %>% 
  filter(name=="Microsim")
p <- ggplot(men, aes(value))
p

p1 <- p + geom_bar(data=bars, aes(fill=name, x=Year, y=value, group=name), colour="darkblue", stat="identity", position="dodge") +
  geom_errorbar(data=bars, aes(x=Year, group=name, ymin=lower_ci, ymax=upper_ci)) + 
  geom_point(data=points, aes(x=Year, y=value, group=name, shape=name), colour="darkblue", size=2) + 
  facet_grid(cols=vars(agecat),rows=vars(microsim.init.race),scales="free") + 
  geom_line(data=points, aes(x=Year, y=value, group=name), colour="darkblue") + 
  scale_fill_manual(name="",values="white") + scale_shape_manual(name="", values=18) +
  theme_classic() + ylab("Total Population") + xlab("Year") +theme(legend.title=NULL, legend.margin = margin(-1,0,0,0, "cm")) + 
  ggtitle("population comparison - women")
# scale_y_continuous(expand = c(0, 0), limits = c(0, 13000000),
#                    labels=function(x) 
#                      format(x, big.mark=",", scientific=FALSE))
p1
ggsave("SIMAH_workplace/microsim/2_output_data/plots/compare_pop_2020_F.png",
       dpi=300, width=33, height=19, units="cm")





percentdifference <- compare %>% dplyr::select(Year, microsim.init.sex, microsim.init.education, data,
                                               value) %>% pivot_wider(names_from=data, values_from=value) %>% 
  mutate(percentdiff = abs(microsim-ACS)/ACS)


percentdifference <- compare %>% dplyr::select(Year, microsim.init.sex, microsim.init.education, data,
                                               percent) %>% pivot_wider(names_from=data, values_from=percent) %>% 
  mutate(diff = abs(microsim - ACS)) %>% 
  group_by(microsim.init.sex, microsim.init.education) %>% 
  filter(Year<=2018)






