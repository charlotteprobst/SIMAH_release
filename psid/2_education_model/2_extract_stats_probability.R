# script to extract summary tables for education for education transitions paper
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(readxl)
library(doParallel)
library(foreach)
library(parallel)
library(ggplot2)

# setwd("/home/cbuckley")
setwd("~/Google Drive/SIMAH Sheffield")

source("SIMAH_code/psid/2_education_model/1_setup_markov_model.R")

data <- read_csv("SIMAH_workplace/education_transitions/new_PSID_weighted_IDs.csv")

# do the first analysis on the split time periods 

# # setup the datasets for both time periods
# 
datat1 <- setup_markov_model(data, y=2009)
datat2 <- setup_markov_model(data, y=2011)

source("SIMAH_code/psid/2_education_model/0_extractTP_function.R")

modelt1_income_int <- readRDS("SIMAH_workplace/education_transitions/final_models/modelt1_income_int_6cat_16_new.RDS")
modelt2_income_int <- readRDS("SIMAH_workplace/education_transitions/final_models/modelt2_income_int_6cat_16_new.RDS")

mappingt1 <- datat1 %>% ungroup() %>% select(age, agescaled, agesqscaled) %>% 
  distinct()
names(mappingt1) <- c("age","agescaled","agesqscaled")

incomemapt1 <- datat1 %>% ungroup() %>% select(total_fam_income, incomescaled) %>% 
  distinct() %>% 
  mutate(ntile=ntile(total_fam_income,5)) %>% group_by(ntile) %>% 
  mutate(medincome = median(total_fam_income)) %>% 
  filter(abs(total_fam_income-medincome)==min(abs(total_fam_income-medincome))) %>% 
  sample_n(1)

mappingt2 <- datat2 %>% ungroup() %>% select(age, agescaled, agesqscaled) %>% 
  distinct()
names(mappingt2) <- c("age","agescaled","agesqscaled")

incomemapt2 <- datat2 %>% ungroup() %>% select(total_fam_income, incomescaled) %>% 
  distinct() %>% 
  mutate(ntile=ntile(total_fam_income,5)) %>% group_by(ntile) %>% 
  mutate(medincome = median(total_fam_income)) %>% 
  filter(abs(total_fam_income-medincome)==min(abs(total_fam_income-medincome))) %>% 
  sample_n(1)

combo1 <- expand.grid(age=unique(datat1$age), sex=c(0,1), race=unique(datat1$racefinal2),
                                 incomescaled=unique(incomemapt1$incomescaled))

combo2 <- expand.grid(age=unique(datat2$age), sex=c(0,1), race=unique(datat2$racefinal2),
                      incomescaled=unique(incomemapt2$incomescaled))

TPt1 <- extractTP(modelt1_income_int, combo1, mappingt1, incomemapt1)

TPt2 <- extractTP(modelt2_income_int, combo2, mappingt2, incomemapt2)

TPt1$time <- "1999 - 2009"
TPt2$time <- "2009 - 2019"

TPs <- rbind(TPt1, TPt2) 

write.csv(TPs, "SIMAH_workplace/education_transitions/final_models/income_model_TP_6cat_16_new.csv", row.names=F)


