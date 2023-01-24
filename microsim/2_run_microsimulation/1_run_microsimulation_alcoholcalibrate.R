#####SIMAH project 2022 - script for running SIMAH microsimulation model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(devtools)
library(roxygen2)
library(dplyr)
library(tidyr)
library(readr)
library(fitdistrplus)
library(lhs)
library(truncnorm)
library(foreach)
library(doParallel)
options(dplyr.summarise.inform = FALSE)

# load in microsim R package
# setwd("~/Google Drive/SIMAH Sheffield/SIMAH_code")
setwd("/home/cbuckley/SIMAH_code")
install("microsimpackage")

###set working directory to the main "SIMAH" folder in your directory 
# WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
WorkingDirectory <- "/home/cbuckley/"
# DataDirectory <- "~/Google Drive/SIMAH Sheffield/SIMAH_workplace/microsim/1_input_data/"
DataDirectory <- "/home/cbuckley/SIMAH_workplace/microsim/1_input_data/"


setwd(paste(WorkingDirectory))

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings_alcoholcalibrate.R")

output_type <- "alcohol"
liverinteraction <- 0

n_samples <- as.numeric(length(alcohol_transitions_calibration))
n_samples <- 2

registerDoParallel(15)

Output <- list()
Output <- foreach(i =1:n_samples, .inorder=TRUE, .packages=c("dplyr","tidyr","foreach")) %dopar% {
 samplenum <- i 
 run_microsim(1,samplenum,basepop,brfss,
                       death_rates,
                       updatingeducation, education_setup,
                       migration_rates,
                       updatingalcohol, alcohol_transitions_calibration[[i]],
                       catcontmodel, Hep, drinkingdistributions,
                       base_rates, diseases, lhs[[1]], liverinteraction,
                       policy, percentreduction, year_policy, inflation_factor,
                       2000, 2019, output_type)
}
saveRDS(Output, "SIMAH_workplace/microsim/2_output_data/Alc_calibration_output.RDS")

output <- do.call(rbind, Output)
alcohol_type <- "categorical"

# now compare to find the best fit to the data
target <- read.csv(paste0(WorkingDirectory,"SIMAH_workplace/microsim/1_input_data/brfss_alcohol_summary.csv")) %>%
  group_by(YEAR, microsim.init.sex, education_summary, race_eth, AlcCAT) %>%
  summarise(n=sum(n)) %>% rename(sex=microsim.init.sex, year=YEAR, race=race_eth, education=education_summary) %>%
  group_by(year, sex, education) %>%
  mutate(targetpercent=n/sum(n),
         sepercent = sqrt((targetpercent*(1-targetpercent))/sum(n)),
         lower_ci = targetpercent - (1.96*sepercent),
         upper_ci = targetpercent + (1.96*sepercent)) %>%
  dplyr::select(-c(n))

output <- output %>%
  group_by(year, samplenum, microsim.init.sex, microsim.init.race, microsim.init.education, AlcCAT) %>%
  summarise(n=sum(n)) %>%
  mutate(year = as.integer(as.character(year)),
         microsim.init.race = ifelse(microsim.init.race=="BLA","Black",
                       ifelse(microsim.init.race=="WHI","White",
                              ifelse(microsim.init.race=="SPA","Hispanic",
                                     ifelse(microsim.init.race=="OTH","Other", NA))))) %>%
  rename(sex=microsim.init.sex, education=microsim.init.education, race=microsim.init.race) %>% ungroup() %>%
  group_by(year, samplenum, sex, education, race) %>%
  mutate(simulatedpercent=n/sum(n)) %>%
  dplyr::select(-n)

implausibility <- left_join(output, target) %>% 
  mutate(v_m =0.1,
         v_t = sqrt(sepercent^2),
    implausibility = ifelse(simulatedpercent>=lower_ci & simulatedpercent<=upper_ci, 0,
                                 abs(simulatedpercent - targetpercent)/(v_m + v_t))) %>% 
  group_by(samplenum, sex, education, race, AlcCAT) %>% 
  mutate(implausibility = max(implausibility)) %>% 
  group_by(samplenum) %>% summarise(implausibility = max(implausibility))

write.csv(implausibility, "SIMAH_workplace/microsim/2_output_data/Alc_calibration_implausibility.csv",
          row.names=F)