# Script to process PSID education data until 2021

library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
setwd("C:/Users/cmp21seb/Documents/SIMAH")

# Source existing PSID processing functions
source("SIMAH_code/PSID/1_process_data/PSID_processing_functions.R")

# read in the data for 1999-2019 (based on selected individual and family level variables)
data <- read_excel("SIMAH_workplace/PSID/J315522/J315522.xlsx")

# read in the data for 2021
data_2021_fam <-
data_2021_ind <- 


data$familyID <- data$ER30001
data$ID <- data$ER30002
data$uniqueID <- (data$familyID*1000) + data$ID