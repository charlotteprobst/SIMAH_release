# Run MAIHDA grams per day with outliers removed

# load packages
library(tidyverse)

# Set working directory
setwd("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_code/nhis/intersectionality")

# Read in data (drinkers only)
data <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/nhis_alc_clean_drinkers_only.RDS")

# Identify outliers based on z-score
z_scores <- scale(data$alc_daily_g_capped_200)
outliers <- which(abs(z_scores) > 3)
outliers_NHISPID <- data$NHISPID[outliers]
outliers_data <- data %>% filter(NHISPID%in%outliers_NHISPID) %>% dplyr::select(NHISPID, alc_daily_g_capped_200)

# NB. All of the outliers have high rather than low levels of consumption