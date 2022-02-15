# Analyse LE decomposition for 2020 weighted data
# Mortality data NCHS
# Population data: ACS
# Project: SIMAH


# libraries required:
library("tidyverse")
library("DemoDecomp")
library("dplyr")
library("reshape")
library("data.table")


## Set the working directory
setwd("C:/Users/marie/Dropbox/NIH2020/")
#setwd("~/Documents/Promotion/Mortality US")
setwd("~/Google Drive/SIMAH Sheffield/")

Results_SES <- read.csv(paste0("SIMAH_workplace/life_expectancy/2_out_data/dResults_contrib_", 2018, "_", 2020, "ACS_2020weights.csv") )

Results_race <- 

