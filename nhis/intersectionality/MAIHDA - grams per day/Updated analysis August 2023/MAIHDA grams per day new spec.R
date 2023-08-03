# Set wd
setwd("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_code/nhis/intersectionality")

# Read in data:
nhis_alc_clean <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/nhis_alc_clean_full_sample.RDS")

# Read in necessary R packages & functions
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ragg)

nhis_alc_clean <- nhis_alc_clean %>% select(YEAR, NHISPID, SEX, education_3_cats, age_3_cats, HISPYN, RACENEW, race_5_cats, alc_daily_g_capped_200)

# recode race ethnicity
nhis_alc_clean <- recode_race_ethnicity_all(nhis_alc_clean)

# review group sizes and raw consumption estimates by race and ethnicity
review_groups <- nhis_alc_clean %>% group_by(race_ethnicity) %>% (count)
review_grams <- nhis_alc_clean %>% group_by(race_ethnicity) %>% summarise(median=median(alc_daily_g_capped_200), IQR=IQR(alc_daily_g_capped_200)) %>% arrange(desc(median))
review <- inner_join(review_groups, review_grams)

# nb. important to highlight that consumption high in non-Hispanic, other race, but sample size small

# Largest groups:
# 1 Non-hispanic, White only
# 8 Hispanic, White only
# 2 Non-hispanic, Black/African American only
# 4 Non-hispanic, Asian only
# 7 Non-hispanic, Multiple race
# 12 Hispanic, Other race 
#	3 American Indian/Alaska Native only

nhis_new_spec <- nhis_alc_clean %>% select(-HISPYN, -RACENEW, -race_5_cats)


