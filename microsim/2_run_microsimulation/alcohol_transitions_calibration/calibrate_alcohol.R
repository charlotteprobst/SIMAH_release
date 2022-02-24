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

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
# WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee
SelectedState <- "USA"

####Size of population 
PopulationSize <- 500000

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
updatingalcohol <- 1

Rates <- readRDS(paste("SIMAH_workplace/microsim/1_input_data/migration_rates/final_rates",SelectedState,".RDS",sep=""))
Rates$agecat <- as.character(Rates$agecat)

# now sample parameters for the alcohol transitions
source("SIMAH_code/microsim/2_run_microsimulation/alcohol_transitions_calibration/extract_uncertainty.R")

# PE <- read.csv("SIMAH_workplace/microsim/1_input_data/Supplement 1 - AlcUse Annual Transition Probabilities.csv") %>% 
#   mutate(sex = ifelse(sex=="Men","m","f"),
#          race = ifelse(race=="Black, non-Hispanic","BLA",
#                        ifelse(race=="White, non-Hispanic","WHI",
#                               ifelse(race=="Other, non-Hispanic", "OTH","SPA"))),
#          edu= ifelse(edu=="Low","LEHS",
#                      ifelse(edu=="Med","SomeC","College")),
#          StateTo=ifelse(To=="Category I", "Low risk",
#                         ifelse(To=="Category II", "Medium risk",
#                                ifelse(To=="Category III","High risk",
#                                       ifelse(To=="Former", "Former drinker",
#                                              ifelse(To=="Abstainer","Lifetime abstainer",To))))),
#          From=ifelse(From=="Category I", "Low risk",
#                         ifelse(From=="Category II", "Medium risk",
#                                ifelse(From=="Category III","High risk",
#                                       ifelse(From=="Former", "Former drinker", 
#                                              ifelse(From=="Abstainer","Lifetime abstainer",From))))),
#          cat = paste(age_cat, sex, race, edu, "STATEFROM", From, sep="_")) %>% 
#   group_by(cat) %>% mutate(cumsum=cumsum(Probability)) %>% 
#   dplyr::select(cat, StateTo, cumsum) %>% mutate(cumsum=ifelse(cumsum>=0.9999, 1, cumsum))
  

# save samples 
saveRDS(transitionsList, "SIMAH_workplace/microsim/2_output_data/transitionsList.RDS")
# length(transitionsList)

# transitionsList <- transitionsList[1:2]

options(future.fork.multithreading.enable = FALSE)
options(future.globals.maxSize = 10000 * 1024^3)
options(future.rng.onMisuse="ignore")

registerDoParallel(15)
# registerDoParallel(2)
mcoptions <- list(set.seed=FALSE)

Output <- foreach(i=1:length(transitionsList), .inorder=FALSE,
                  # .options.multicore=mcoptions,
                  .packages=c("dplyr","tidyr","foreach")) %dopar% {
                    samplenum <- i
                    seed <- Sys.time()
                    print(i)
                    gc()
                    run_microsim(seed,samplenum,basepop, outwardmigrants, inwardmigrants, deathrates, apply_death_rates,
                                 updatingeducation, education_setup, transitionroles,
                                 calculate_migration_rates, outward_migration, inward_migration, 
                                 brfss,Rates,transitionsList[[i]],
                                 transitions, PopPerYear, 2000, 2018)
                    # PE <- run_microsim(seed,samplenum,basepop, outwardmigrants, inwardmigrants, deathrates, apply_death_rates,
                    #              updatingeducation, education_setup, transitionroles,
                    #              calculate_migration_rates, outward_migration, inward_migration, 
                    #              brfss,Rates,PE,
                    #              transitions, PopPerYear, 2000, 2018)
                  }


# get target data 
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

# compare PE 
# PE <- PE %>% group_by(samplenum, year, microsim.init.sex, microsim.init.education, AlcCAT, .drop=FALSE) %>% 
#   summarise(n=sum(n)) %>% ungroup() %>% 
#   group_by(samplenum, year, microsim.init.sex, microsim.init.education) %>% 
#   mutate(microsimpercent=n/sum(n),
#          year=as.numeric(as.character(year)))
# PE <- left_join(PE, target)

error <- Output %>% ungroup() %>% 
  group_by(samplenum) %>% 
  mutate(errorsq = (microsimpercent-targetpercent)^2) %>% 
  group_by(samplenum) %>% 
  summarise(maxerror = max(errorsq),
            RMSE = sqrt(mean(errorsq)))

write.csv(error, "SIMAH_workplace/microsim/2_output_data/error_RMSE.csv")

final <- error[which(error$RMSE==min(error$RMSE)),]$samplenum
transitions <- transitionsList[[final]]
saveRDS(transitions, paste0("SIMAH_workplace/microsim/2_output_data/final_alc_transitions", SelectedState, ".RDS"))

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
                                                                      "College degree plus")),
                            AlcCAT = factor(AlcCAT, levels=c("Non-drinker",
                              # "Lifetime abstainer",
                              #                                "Former drinker",
                                                             "Low risk","Medium risk","High risk")))
scaleFUN <- function(x) sprintf("%.2f", x)
ggplot(data=subset(Output, microsim.init.sex=="Men"), aes(x=year, y=microsimpercent, colour=as.factor(samplenum))) + 
  geom_line(linetype="dashed") + geom_line(aes(x=year, y=targetpercent), colour="black") + 
  facet_grid(rows=vars(AlcCAT), cols=vars(microsim.init.education), scales="free") +
  # facet_wrap(~AlcCAT+microsim.init.sex+microsim.init.education, scales="free") + 
  theme_bw() + scale_y_continuous(labels=scales::percent, limits=c(0,NA)) + 
  theme(legend.position="none",
        legend.title=element_blank(),
        strip.background=element_rect(fill="white")) + 
  ylab("percentage in category") + ggtitle("Men")
ggsave(paste("SIMAH_workplace/microsim/2_output_data/plots/alcohol_states_comparemen",SelectedState,".png",sep=""),
       dpi=300, width=33, height=19, units="cm")
ggplot(data=subset(Output, microsim.init.sex=="Women"), aes(x=year, y=microsimpercent, colour=as.factor(samplenum))) + 
  geom_line(linetype="dashed") + geom_line(aes(x=year, y=targetpercent), colour="black") + 
  facet_grid(rows=vars(AlcCAT), cols=vars(microsim.init.education), scales="free") +
  # facet_wrap(~AlcCAT+microsim.init.sex+microsim.init.education, scales="free") + 
  theme_bw() + scale_y_continuous(labels=scales::percent, limits=c(0,NA)) + 
  theme(legend.position="none",
        legend.title=element_blank(),
        strip.background=element_rect(fill="white")) + 
  ylab("percentage in category") + ggtitle("Women")
ggsave(paste("SIMAH_workplace/microsim/2_output_data/plots/alcohol_states_comparewomen",SelectedState,".png",sep=""),
       dpi=300, width=33, height=19, units="cm")

bestrate <- Output %>% filter(samplenum==final) %>% 
  pivot_longer(cols=microsimpercent:targetpercent)

# PE <- PE %>% pivot_longer(cols=microsimpercent:targetpercent) %>% mutate(AlcCAT = factor(AlcCAT, 
#                                     levels=c("Lifetime abstainer",
#                                              "Former drinker",
#                                              "Low risk",
#                                              "Medium risk",
#                                              "High risk")),
#                     microsim.init.education = ifelse(microsim.init.education=="LEHS",
#                                                      "High school degree or less",
#                                                      ifelse(microsim.init.education=="SomeC","Some college","College degree or more")),
#                     microsim.init.education = factor(microsim.init.education,
#                                                      levels=c("High school degree or less",
#                                                               "Some college",
#                                                               "College degree or more")))

ggplot(data=bestrate, aes(x=year, y=value, colour=microsim.init.sex, linetype=name)) + 
  geom_line() +
  scale_linetype_manual(values=c("dashed","solid")) + 
  facet_grid(rows=vars(AlcCAT), cols=vars(microsim.init.education), scales="free") + 
  theme_bw() + scale_y_continuous(labels=scales::percent, limits=c(0,NA)) + 
  theme(legend.position="bottom",
        legend.title=element_blank()) + 
  ylab("percentage in category")
ggsave(paste("SIMAH_workplace/microsim/2_output_data/plots/alcohol_states_bestrate",SelectedState, ".png",sep=""),
       dpi=300, width=33, height=19, units="cm")



