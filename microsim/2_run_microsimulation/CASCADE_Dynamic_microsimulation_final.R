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
library(faux)
library(splitstackshape)
options(scipen=999)
# set seed for reproducibility - IMPORTANT - DO NOT CHANGE
# note - this also needs to be ran straight after R has been opened
set.seed(42)

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/SIMAH/Microsimulation"
setwd(paste(WorkingDirectory))

####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee
SelectedState <- "USA"

####Size of population 
PopulationSize <- 200000

# run model for CASCADE (1984 start) or SIMAH (2000 start)?
model <- "CASCADE"

# what proportion of the population does this represent - change to ifelse with all pop sizes when other states added 
if(model=="SIMAH"){
  WholePopSize <- read.csv("SIMAH_workplace/microsim/1_input_data/fullpopcounts.csv") %>% 
    filter(STATE==SelectedState)
}else if(model=="CASCADE"){
  WholePopSize <- read.csv("SIMAH_workplace/microsim/1_generating_population/constraintsUSA.csv") %>% 
    dplyr::select(marriedF:unmarriedM) %>% mutate(total=marriedF+unmarriedF+marriedM+unmarriedM)
}

proportion <- PopulationSize/WholePopSize$total
proportion <- ifelse(proportion>1,1,proportion)

# switch to 1 when adjusting migration scripts
adjusting <- 1

#####first read in and process all the necessary data files 
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/CASCADE_load_files.R")
# load in the education transitions data
# source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/education_transitions.R")

# load all functions for running the microsimulation - death rates, migration, transition education
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/apply_death_rates.R")
if(model=="CASCADE"){
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/CASCADE_apply_death_rates.R")
}
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/outward_migration.R")
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/inward_migration.R")
if(model=="CASCADE"){
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/CASCADE_inward_migration.R")
}
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/education_setup.R")
source("SIMAH_code/microsim/2_run_microsimulation/1_functions/transition_ed.R")

# load the function for running the simulation
if(model=="SIMAH"){
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/simulation.R")
}else if(model=="CASCADE"){
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/CASCADE_simulation.R")  
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/HistoryFunction.R")
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/formerdrinkers_history.R")
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/cirrhosis_functions.R")
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/assign_hepatitis.R")
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/updating_alcohol.R")
  source("SIMAH_code/microsim/2_run_microsimulation/1_functions/updating_BMI.R")

}

# switch on and off migration and deaths
migrationdeaths <- 1

# switch on and off education updates
updatingeducation <- 0
# switch on and off alcohol updates
updatingalcohol <- 0

Rates <- readRDS(paste("SIMAH_workplace/microsim/1_input_data/migration_rates/CASCADEfinal_rates",SelectedState,".RDS",sep=""))
Rates$agecat <- as.character(Rates$agecat)
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/projecting_migration_and_deaths.R")

PE <- 1
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/sampling_parameters_IRR.R")

history <- HistoryFunction(basepop, ages)
basepop <- left_join(basepop, history)
basepop <- formerdrinkers_history(basepop)
basepop <- basepop %>% 
  mutate(Cirrhosis_risk = ifelse(formerdrinker==0 & microsim.init.sex=="m" & 
                                   grams_10years>= 100000, 1,
                                 ifelse(formerdrinker==0 & microsim.init.sex=="f" & 
                                          grams_10years>=100000*0.66, 1, 
                                        ifelse(formerdrinker==1, Cirrhosis_risk, 0))),
         grams_10years = ifelse(formerdrinker==1, former_history,
                                grams_10years)) %>% dplyr::select(-former_history)

Output <- list()
Output <- run_microsim(1,1,basepop, outwardmigrants, inwardmigrants, deathrates, apply_death_rates,
                       updatingeducation, education_setup, transitionroles,
                       calculate_migration_rates, outward_migration, inward_migration, 
                       brfss,Rates,AlctransitionProbability,
                       transitions, PopPerYear, 1984, 2016)

source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/process_target_calibration_age.R")

target <- target %>% rename(microsim.init.sex=sex, agecat=agegroup)
Cirrhosis <- left_join(Cirrhosis, target)
ggplot(data=Cirrhosis, aes(x=Year)) + geom_line(aes(y=n), colour="red") + geom_line(aes(y=count),colour="black") +
  facet_grid(rows=vars(microsim.init.sex), cols=vars(agecat)) + theme_bw() + ylim(0,NA)

# saveRDS(Output, "output_fullpop.RDS")
saveRDS(Output[[1]], "SIMAH_workplace/microsim/2_output_data/output_fullpop.RDS")
saveRDS(Output[[2]], "SIMAH_workplace/microsim/2_output_data/output_deaths.RDS")

source("SIMAH_code/microsim/2_run_microsimulation/1_functions/compare_output_target.R")

compare <- compare_output_target(Output)
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
            # lower_ci = lower_ci,
            # upper_ci = upper_ci) %>% 
  mutate(microsim.init.race = factor(microsim.init.race, levels=c("Black","White","Hispanic","Others")),
         name = recode(name, "microsim"="Microsim","target"="ACS / Census")) %>% filter(agecat!="25-34" | agecat!="35-44" |
                                                                                          agecat!="45-54" | agecat!="55-64" |
                                                                                          agecat!="65-74") %>% 
  mutate(agecat=factor(agecat))
compare$Year <- as.numeric(as.character(compare$Year))

men <- compare %>% filter(microsim.init.sex=="Men")
bars <- men %>% 
  filter(name=="ACS / Census")
points <- men %>% 
  filter(name=="Microsim")
p <- ggplot(men, aes(value))
p

p1 <- p + geom_bar(data=bars, aes(fill=name, x=Year, y=value, group=name), colour="darkblue", stat="identity", position="dodge") +
  # geom_errorbar(data=bars, aes(x=Year, group=name, ymin=lower_ci, ymax=upper_ci)) + 
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
  # geom_errorbar(data=bars, aes(x=Year, group=name, ymin=lower_ci, ymax=upper_ci)) + 
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






