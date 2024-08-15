rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
library(msm)
library(tidyverse)
library(lcmm)
library(lavaan)


setwd("~/Google Drive/SIMAH Sheffield")

data <- read_csv("SIMAH_workplace/PSID/alldata_new_1999_2019.csv") %>% 
  filter(relationship!="latino/immigrantsampleunknown") %>% distinct()

kessler_wide <- data %>% drop_na(kessler_score) %>% 
  dplyr::select(uniqueID, year, kessler_score) %>% 
  distinct() %>% 
  data.frame() %>% 
  pivot_wider(names_from=year, values_from=kessler_score,
              names_prefix="year")

data <- left_join(data, kessler_wide)

model <- ' i =~ 1*year2001 + 1*year2003 + 1*year2007 + 1*year2009 + 1*year2011 + 1*year2013 + 1*year2015 + 1*year2017 + 1*year2019
           s =~ 0*year2001 + 1*year2003 + 2*year2007 + 3*year2009 + 4*year2011 + 5*year2013 + 6*year2015 + 7*year2017 + 8*year2019 
              # regressions
    i ~ sex + age + individualrace + education_cat
    s ~ sex + age + individualrace + education_cat'

fit <- growth(model, data=data)
summary(fit)

