#####SIMAH project 2022 - script for running SIMAH microsimulation model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(devtools)
library(roxygen2)
library(dplyr)
library(tidyverse)
library(fitdistrplus)
library(lhs)
library(truncnorm)
library(data.table)
library(foreach)
options(dplyr.summarise.inform = FALSE)


###set working directory to the main "SIMAH" folder in your directory 
# WorkingDirectory <- "U:/SIMAH/"
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "home/cbuckley"
DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")


# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")

# alcohol_transitions <- read.csv("SIMAH_workplace/microsim/1_input_data/alcohol_transitions_new.csv")
alcohol_transitions <- readRDS(paste0(DataDirectory, "final_alc_transitionsUSA.RDS"))

output_type <- "mortality"

registerDoParallel(15)

Output <- list()
Output <- foreach(i =1:n_samples, .inorder=TRUE, .packages=c("dplyr","tidyr","foreach")) %dopar% {
samplenum <- i 
run_microsim(1,i,basepop,brfss,
                       death_counts,
                       updatingeducation, education_setup,
                       migration_counts,
                       updatingalcohol, alcohol_transitions,
                       base_counts, diseases, lhs[[i]], liverinteraction,
                       policy, percentreduction, year_policy, inflation_factor,
                       2000, 2002, output_type)
}

summary <- readRDS("SIMAH_workplace/microsim/2_output_data/factor_calibrate/summary_test.RDS")

summary <- summarise_mortality_output_calibration(summary, SelectedState, inflation_factor, "2000")

ggplot(data=summary, aes(x=year, y=simulated, colour=as.factor(samplenum))) + geom_line() + 
  geom_line(aes(x=year, y=observed), colour="black", linewidth=1) + 
  facet_grid(cols=vars(education), rows=vars(sex)) + theme_bw() +
  theme(legend.position="none",
        strip.background = element_rect(fill="white")) + ylim(0,NA)

ggsave("SIMAH_workplace/microsim/2_output_data/factor_calibrate/plot_compare_baserates.png",
       dpi=300, width=33, height=19, units="cm")


bestmodel <- summary %>% 
  group_by(samplenum, year, sex, education) %>% 
  summarise(diff = abs(simulated-observed),
            sqerror = diff^2) %>% 
  ungroup() %>% 
  group_by(samplenum,sex,education) %>% 
  summarise(RMSE=sqrt(mean(sqerror))) %>% ungroup() %>% 
  group_by(sex, education) %>% 
  mutate(min = ifelse(min(RMSE)==RMSE,1,0)) %>% 
  filter(min==1)


ggplot(data=subset(summary, samplenum==17), aes(x=year, y=simulated, colour=as.factor(samplenum))) + geom_line() + 
  geom_line(aes(x=year, y=observed), colour="black", linewidth=1) + 
  facet_grid(cols=vars(education), rows=vars(sex)) + #base rate = 2% increase each year from 2010 for men 
                                                  #base rate = 5% increase each year from 2010 for women 
  theme_bw() +
  theme(legend.position="none",
        strip.background = element_rect(fill="white")) + ylim(0,NA)

ggsave("SIMAH_workplace/microsim/2_output_data/factor_calibrate/bestmodel_men.png",
       dpi=300, width=33, height=19, units="cm")
