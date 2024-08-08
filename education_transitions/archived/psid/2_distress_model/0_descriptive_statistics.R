# script to integrate research attachment work and run distress transitions model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
library(msm)
library(tidyverse)
library(naniar)


setwd("~/Google Drive/SIMAH Sheffield")

data <- read_csv("SIMAH_workplace/PSID/alldata_new_1999_2019.csv") %>% 
  filter(year>1999)

# remove all latino and immigrant sample (not asked questions)
data <- data %>% filter(relationship!="latino/immigrantsampleunknown")

summarystats <- data %>% 
  group_by(year,distress_class) %>% 
  tally()

summary <- data %>% group_by(year) %>% miss_var_summary(.)


