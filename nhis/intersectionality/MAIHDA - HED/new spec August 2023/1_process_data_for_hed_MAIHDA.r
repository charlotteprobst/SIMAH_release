# Script to process nhis data for HED MAIHDA model

# Setup
library(tidyr)
library(dplyr)
library(sjstats)
library(haven)
library(performance)
library(memisc)
library(gt)
library(R2MLwiN)
library(xlsx)
options(MLwiN_path="C:/Program Files/MLwiN v3.05/")

setwd("C:/Users/cmp21seb/Documents/SIMAH/")

options(scipen=10)

# Read in data (full sample):
data <- readRDS("SIMAH_workplace/nhis/intersectionality/cleaned_data/full_sample_new_spec.RDS")

##### PREP DATA FOR FULL SAMPLE

# Generate a binary HED variable
data <- data %>%
  mutate(HED =
           case_when(ALC5UPYR >= 1 ~ 1,
                     ALC5UPYR == 0 ~ 0)) %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_3_cats) %>% 
  mutate(intersections = cur_group_id()) %>%
  group_by(intersections) %>%
  mutate(denominator=n()) %>%
  group_by(HED, intersections) %>%
  mutate(numerator=n(),
         proportion=numerator/denominator,
         percentage=proportion*100) %>%
  ungroup()

# Generate dummy variables for each year
data <- dummy_cols(data, select_columns = "YEAR")

# Subset data to keep only the variables of interest
data <- data %>%
  dplyr::select(intersections, NHISPID, ALCSTAT1, HED, numerator, denominator, proportion, 
                age_3_cats, SEX, race_6_cats, education_3_cats, 
                YEAR_2000, YEAR_2001, YEAR_2002, YEAR_2003, YEAR_2004,
                YEAR_2005, YEAR_2006, YEAR_2007, YEAR_2008, YEAR_2009,
                YEAR_2010, YEAR_2011, YEAR_2012, YEAR_2013, YEAR_2014,
                YEAR_2015, YEAR_2016, YEAR_2017, YEAR_2018)

# Generate a summary table showing the proportion of HEDs by intersection
summary_table <- data %>%
  filter(HED==1) %>%
  dplyr::select(-c(ALCSTAT1, YEAR_2000, YEAR_2001, YEAR_2002, YEAR_2003, YEAR_2004,
                YEAR_2005, YEAR_2006, YEAR_2007, YEAR_2008, YEAR_2009,
                YEAR_2010, YEAR_2011, YEAR_2012, YEAR_2013, YEAR_2014,
                YEAR_2015, YEAR_2016, YEAR_2017, YEAR_2018)) %>%
  distinct(intersections, .keep_all = TRUE)

# Check if any groups with zero HEDs
summary_table %>% 
  ungroup() %>%
  filter(proportion==1)%>%
  distinct() # Nil	

# Save results
saveRDS(data, "SIMAH_workplace/nhis/intersectionality/cleaned_data/new spec August 2023/hed_data_full_sample.rds")

##### PREP DATA FOR DRINKERS ONLY

# subset drinkers only
data_drinkers <- data %>% filter(ALCSTAT1=="Current drinker")

# Generate a summary table showing the proportion of HEDs by intersection
summary_table_drinkers <- data_drinkers %>%
  filter(HED==1) %>%
  dplyr::select(-c(ALCSTAT1, YEAR_2000, YEAR_2001, YEAR_2002, YEAR_2003, YEAR_2004,
                   YEAR_2005, YEAR_2006, YEAR_2007, YEAR_2008, YEAR_2009,
                   YEAR_2010, YEAR_2011, YEAR_2012, YEAR_2013, YEAR_2014,
                   YEAR_2015, YEAR_2016, YEAR_2017, YEAR_2018)) %>%
  distinct(intersections, .keep_all = TRUE)

# Check if any groups with zero HEDs
summary_table_drinkers %>% 
  ungroup() %>%
  filter(proportion==1)%>%
  distinct() # Nil	

# Save results
saveRDS(data_drinkers, "SIMAH_workplace/nhis/intersectionality/cleaned_data/new spec August 2023/hed_data_drinkers_only.rds")