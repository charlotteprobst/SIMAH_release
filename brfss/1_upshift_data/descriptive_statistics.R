# SIMAH October 2021 - code to generate plots for publication
library(readr)
library(dplyr)
library(tidyr)
library(gtsummary)
library(gt)
library(writexl)

# CB laptop directory
wd <- "~/Google Drive/SIMAH Sheffield/"
# CB desktop directory
# wd <- "G:/My Drive/SIMAH Sheffield/"
setwd(wd)

####read in the joined up data files 
data <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_example.RDS")

# create summary table - selecting Year, state, race, sex, age, education and drinking status vars 
summary <- data %>% dplyr::select(YEAR,State,race_eth, sex_recode, age_var, education_summary,
                       drinkingstatus, drinkingstatus_detailed) %>% 
  filter(State=="USA") %>% filter(YEAR>=2010 & YEAR<=2019) %>% #select USA and years 2010-2019
  dplyr::select(-State) %>% #remove the variable "state" 
  mutate_at(vars(c(race_eth, sex_recode, 
                   education_summary, drinkingstatus_detailed)), 
            as.factor) %>% #convert categorical vars to factors 
  gtsummary::tbl_summary(by=c(YEAR)) #get a summary table by year 
summary <- summary %>% as_gt(.) #convert to correct format for saving output

# save output as a .html (table can be copied into word from here)
gtsave(summary, "SIMAH_workplace/brfss/paper/summary_stats.html")

