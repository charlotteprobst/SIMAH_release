# Run MAIHDA grams per day with outliers removed

# load packages
library(tidyverse)

# Set working directory
setwd("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_code/nhis/intersectionality")

# Read in data (drinkers only)
data <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/nhis_alc_clean_drinkers_only.RDS")

# View the mean alc daily grams
mean(data$alc_daily_g_capped_200) # 7.88

#### Identify outliers based on z-score

# Set the z-score threshold to 3 (as is common practice)
threshold <- 3

## Method 1. Using the non-transformed capped daily grams

# Calculate Z-scores
z_scores <- scale(data$alc_daily_g_capped_200)

# Identifying outliers' indices based on the threshold (+3 and -3)
outliers_indices <- which(abs(z_scores) > threshold | abs(z_scores) < -threshold)

# Getting NHISPID of the outliers
outliers_NHISPID <- data$NHISPID[outliers_indices]

# Filtering the dataset for outliers and selecting specific columns
outliers_data <- data %>%
  filter(NHISPID %in% outliers_NHISPID) %>%
  select(NHISPID, alc_daily_g_capped_200)

# All of the outliers have high rather than low levels of consumption (> 50 gpd)

## Method 2: Using the log-transformed daily grams

z_scores_2 <- scale(data$capped_daily_grams_log)
outliers_indices_2 <- which(abs(z_scores_2) > threshold | abs(z_scores_2) < -threshold)

# No outliers based on the transformed distribution 

## Method 3: Using modified z-scores (Median Absolute Deviation)
# MAD is a robust measure that can handle skewed data better than z-scores. 
# Instead of using the mean and standard deviation, MAD uses the median and median absolute deviation to identify outliers.

# Calculate Median Absolute Deviation (MAD)
mad_value <- mad(data$alc_daily_g_capped_200, constant = 1.4826)  # constant adjusts MAD to be consistent with the standard deviation

# Define a threshold 
threshold_MAD <- 3 * mad_value

# Identify outliers and non-outliers
outliers_positive <- which(data$alc_daily_g_capped_200 - median(data$alc_daily_g_capped_200) > threshold)
outliers_negative <- which(data$alc_daily_g_capped_200 - median(data$alc_daily_g_capped_200) < -threshold)

# Combine both positive and negative outliers indices
all_outliers_indices_MAD <- c(outliers_positive, outliers_negative)

# Extract NHISPID and values of outliers
outliers_data_MAD <- data[all_outliers_indices_MAD, c("NHISPID", "alc_daily_g_capped_200")]

### All methods identify high rather than low outliers