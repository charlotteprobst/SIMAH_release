#####SIMAH project 2024 - script for running SIMAH microsimulation model
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
# WorkingDirectory <- "C:/Users/laura/Documents/CAMH/SIMAH/"
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "C:/Users/marie/Dropbox/NIH2020/"
# WorkingDirectory <- "C:/Users/cmp21seb/Documents/SIMAH/"

DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")

# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)
install("SIMAH_code/calibrationpackage", dep=T)

library(microsimpackage)
library(calibrationpackage)

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")

# read in calibrated education transitions
education_transitions <- read_rds(paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/education_calibration/new_implausibility_se", "/transitionsList-10",".RDS"))
for(i in 1:length(education_transitions)){
  education_transitions[[i]]$cat <- gsub("1999-2019+_","",education_transitions[[i]]$cat)
}
# read in calibrated alcohol transitions 
alcohol_transitions <- read_csv(paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/alcohol_calibration/ordinal_calibration/lhs_regression-4.csv"))

# pick a random education / alcohol model to use (for testing purposes)
# this picks a random model from the calibrated education / alcohol models
samplenum <- sample(1:300, 1, replace=F)

education_transitions <- education_transitions[[samplenum]]
alcohol_transitions <- alcohol_transitions %>% filter(sample==samplenum)

# read in the categorical to continuous distributions
catcontmodel <- read.csv("SIMAH_workplace/microsim/1_input_data/CatContDistr_beta.csv") %>%
  dplyr::select(group, shape1, shape2, min, max)

output_type <- "mortality"

# random number seed - sample random number 
seed <- as.numeric(sample(1:100, 1))

# sample number - set to 1 when just running 1 simulation 
samplenum <- 1

# set lhs to the first element of the lhs list- for testing 
lhs <- lhs[[1]]

# set minyear and maxyear 
minyear <- 2000
maxyear <- 2005
updatingeducation <- 1

Output <- list()
Output <- run_microsim_alt(seed=1,samplenum=1,basepop,brfss,
                           death_counts,
                           updatingeducation, education_transitions,
                           migration_rates,
                           updatingalcohol, alcohol_transitions,
                           catcontmodel, drinkingdistributions,
                           base_counts, diseases, lhs, sesinteraction,
                           policy=0, percentreduction=0.1, year_policy, inflation_factors,
                           age_inflated,
                           update_base_rate,
                           minyear=2000, maxyear=2003, output="mortality")

# postprocessing - not currently working!
# alcohol_type <- "categorical"
# 
# if(output_type=="demographics"){
#   summary <- summarise_education_output(Output, SelectedState, DataDirectory)
# }else if(output_type=="alcohol"){
#   if(alcohol_type=="categorical"){
#     summary <- summarise_alcohol_output(Output, SelectedState, DataDirectory)
#   }else if(alcohol_type=="continuous"){
#     summary <- summarise_alcohol_output_continuous(Output[[2]], SelectedState, DataDirectory)
#   }
# }else if(output_type=="mortality"){
#   summary1 <- summarise_mortality_output(Output, SelectedState, DataDirectory, diseases, 2010)
#   summary2 <- summarise_mortality_output(Output2, SelectedState, DataDirectory, diseases, 2000)
# }
# # data frame containing mortality outputs
# summary_mortality <- summary[[1]]
# # plots for mortality 
# summary[[2]]
# 
# # save a copy of the plot
# ggsave("SIMAH_workplace/microsim/2_output_data/mortality_summary_multiple_calibration_best.png", plot, dpi=300,
#        width=33, height=19, units="cm")

