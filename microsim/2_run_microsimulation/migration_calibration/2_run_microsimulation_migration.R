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

# output_type <- "mortality"

# random number seed - sample random number 
seed <- as.numeric(sample(1:100, 1))

# sample number - set to 1 when just running 1 simulation 
samplenum <- 1

# set lhs to the first element of the lhs list- for testing 
lhs <- lhs[[1]]

migration_counts <- read.csv("SIMAH_workplace/microsim/1_input_data/migration_in_USA.csv")

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

birth_rates <- Output[[2]]

ggplot(data=birth_rates, aes(x=year, y=rate, colour=microsim.init.sex)) + 
  geom_line() + 
  facet_grid(rows=vars(microsim.init.race)) + 
  ylim(0,0.05)

model <- lm(rate~year+microsim.init.sex+microsim.init.race, data=subset(birth_rates,year>=2015))

datatopredict <- expand.grid(year=c(2019:2025),
                             agecat="18",
                             microsim.init.sex=unique(birth_rates$microsim.init.sex),
                             microsim.init.race=unique(birth_rates$microsim.init.race)) %>%
  mutate(toadd = NA,
         n = NA,
         rate=NA)
birth_rates <- rbind(birth_rates,datatopredict)

birth_rates$rate_predicted <- predict(model, birth_rates)

ggplot(data=birth_rates, aes(x=year, y=rate)) +
  geom_line() + 
  geom_line(aes(x=year, y=rate_predicted), colour="red") + 
  facet_grid(cols=vars(microsim.init.sex),
             rows=vars(microsim.init.race))

migration_rates <- Output[[3]]
ggplot(data=migration_rates, aes(x=year, y=rate, colour=microsim.init.sex)) + 
  geom_line() +
  facet_grid(cols=vars(agecat), rows=vars(microsim.init.race))




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
