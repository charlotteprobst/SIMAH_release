#####SIMAH project 2022 - script for running SIMAH microsimulation model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(devtools)
library(roxygen2)
library(gatbxr)
# if having trouble with loading this package - run the below two lines
# install.packages("remotes")
# remotes::install_github("drizztxx/gatbxr")
library(dplyr)
library(tidyverse)
library(fitdistrplus)
library(lhs)
library(truncnorm)
library(data.table)
library(gridExtra)
options(dplyr.summarise.inform = FALSE)

###set working directory to the main "SIMAH" folder in your directory 
# WorkingDirectory <- "U:/SIMAH/"
WorkingDirectory <- "C:/Users/laura/Documents/CAMH/SIMAH/"
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "C:/Users/marie/Dropbox/NIH2020/"

DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")

# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")

# alcohol_transitions <- read.csv("SIMAH_workplace/microsim/1_input_data/alcohol_transitions_new.csv")
alcohol_transitions <- readRDS(paste0(DataDirectory, "final_alc_transitionsUSA.RDS"))

output_type <- "mortality"

# random number seed - sample random number 
seed <- as.numeric(sample(1:100, 1))

# sample number - set to 1 when just running 1 simulation 
samplenum <- 1

# set lhs to the first element of the lhs list- for testing 
lhs <- lhs[[1]]

migration_counts <- read.csv("SIMAH_workplace/microsim/census_data/migration_in_USA.csv")

# checking how the original migration counts fit the population data
Output <- list()
Output <- run_microsim_alt(seed=1,samplenum=1,basepop,brfss,
                       death_counts,
                       updatingeducation, education_transitions,
                       migration_counts,
                       updatingalcohol, alcohol_transitions,
                       catcontmodel, Hep, drinkingdistributions,
                       base_counts, diseases, lhs, liverinteraction,
                       policy=0, percentreduction=0.1, year_policy, inflation_factors,
                       age_inflated,
                       update_base_rate,
                       minyear=2000, maxyear=2019, output="demographics")
Output

compare <- Output %>% 
  mutate(agecat = cut(microsim.init.age, breaks=c(0,18,24,29,34,39,44,49,54,59,64,69,74,80),
                      labels=c("18","19-24","25-29","30-34","35-39",
                               "40-44","45-49","50-54","55-59",
                               "60-64","65-69","70-74","75-79"))) %>% 
  group_by(year,microsim.init.sex,microsim.init.race,agecat) %>% 
  summarise(n=sum(n)) %>% mutate(data="microsim",
                                 year = as.numeric(as.character(year))) %>% 
  rename(Year=year)
  
  # for comparison
ACS <- read.csv("SIMAH_workplace/microsim/census_data/ACS_population_constraints.csv") %>% 
  mutate(data = "ACS") %>% mutate(n=round(TotalPop*proportion)) %>% 
  dplyr::select(-TotalPop)

compare <- rbind(compare, ACS) %>% distinct() %>% 
  pivot_wider(names_from=data, values_from=n) %>% 
  mutate(pct_diff = (microsim-ACS)/ACS)

ggplot(data=subset(compare,agecat=="25-29"), aes(x=Year, y=n, colour=data)) + geom_line() + 
  facet_grid(cols=vars(microsim.init.race), rows=vars(microsim.init.sex))


if(output_type=="demographics"){
summary <- summarise_education_output(Output, SelectedState, DataDirectory)
}else if(output_type=="alcohol"){
  if(alcohol_type=="categorical"){
summary <- summarise_alcohol_output(Output, SelectedState, DataDirectory)
}else if(alcohol_type=="continuous"){
summary <- summarise_alcohol_output_continuous(Output[[2]], SelectedState, DataDirectory)
}
}else if(output_type=="mortality"){
summary1 <- summarise_mortality_output(Output1, SelectedState, DataDirectory, diseases, 2000)
summary2 <- summarise_mortality_output(Output2, SelectedState, DataDirectory, diseases, 2000)
}
# data frame containing mortality outputs
summary_mortality <- summary[[1]]
# plots for mortality 
summary[[2]]

# save a copy of the plot
ggsave("SIMAH_workplace/microsim/2_output_data/mortality_summary_multiple_calibration_best.png", plot, dpi=300,
       width=33, height=19, units="cm")
