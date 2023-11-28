# Run MAIHDA grams per day with outliers removed

# load packages
library(tidyverse)
library("R2MLwiN")

# Set working directory
setwd("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_code/nhis/intersectionality")

# Read in data (drinkers only)
data <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/new spec August 2023/grams/grams_data_pre_maihda_drinkers.RDS")

# View the mean alc daily grams
mean(data$alc_daily_g_capped_200) # 7.89
median(data$alc_daily_g_capped_200) # 2.30

#### Identify outliers based on z-score

# Set the z-score threshold to 2 (as is common practice)
threshold <- 2

## Method 1. Using the non-transformed capped daily grams

# Calculate Z-scores
z_scores <- scale(data$alc_daily_g_capped_200)

# Identifying outliers' indices based on the threshold (+2 and -2)
outliers_indices <- which(abs(z_scores) > threshold | abs(z_scores) < -threshold)

# Getting NHISPID of the outliers
outliers_NHISPID <- data$NHISPID[outliers_indices]

# Filtering the dataset for outliers and selecting specific columns
outliers_data <- data %>%
  filter(NHISPID %in% outliers_NHISPID) %>%
  select(NHISPID, alc_daily_g_capped_200)

# Delete individuals with outliers from the data
data_no_outliers <- data %>% filter(!(NHISPID%in%outliers_data$NHISPID))

# Review distribution of data with no outliers
data_no_outliers %>% ungroup() %>%
  summarise("min" = min(alc_daily_g_capped_200), 
                               "max"=max(alc_daily_g_capped_200), 
                               "mean" = mean(alc_daily_g_capped_200))
# min     max   mean
# 0.0384  36.6  5.81

## Method 2: Using the log-transformed daily grams
z_scores_2 <- scale(data$capped_daily_grams_log)
outliers_indices_2 <- which(abs(z_scores_2) > threshold | abs(z_scores_2) < -threshold)

# Getting NHISPID of the outliers
outliers_NHISPID_2 <- data$NHISPID[outliers_indices_2]

# Filtering the dataset for outliers and selecting specific columns
outliers_data_2 <- data %>%
  filter(NHISPID %in% outliers_NHISPID_2) %>%
  select(NHISPID, alc_daily_g_capped_200)

# Delete individuals with outliers from the data
data_no_outliers_2 <- data %>% filter(!(NHISPID%in%outliers_data_2$NHISPID))

# Review distribution of data with no outliers
data_no_outliers_2 %>% ungroup() %>%
  summarise("min" = min(alc_daily_g_capped_200), 
            "max"=max(alc_daily_g_capped_200), 
            "mean" = mean(alc_daily_g_capped_200))
# min     max   mean
# 0.0384  84.4  7.41

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

# Delete individuals with outliers from the data
data_no_outliers_MAD <- data %>% filter(!(NHISPID%in%outliers_data_MAD$NHISPID))

# Review distribution of data with no outliers
data_no_outliers_MAD %>% ungroup() %>%
  summarise("min" = min(alc_daily_g_capped_200), 
            "max"=max(alc_daily_g_capped_200), 
            "mean" = mean(alc_daily_g_capped_200))
#  min    max   mean
# 0.0384  4.3  1.76


#### Run model using data_no_outliers_2

# Prep data for use with Mlwin
model_data <- data_no_outliers_2 %>%
  mutate(cons=1) %>% 
  arrange(intersections, NHISPID)

model_data$age_diaz <- droplevels(model_data$age_diaz)
model_data$YEAR <- as.factor(model_data$YEAR)

# Generate reference table with intersectional names & mean observed grams
intersections_reference <- model_data %>%
  group_by(intersectional_names) %>% 
  distinct(intersections, intersectional_names, mean_observed_grams)

# Null model
(null_grams <- runMLwiN(capped_daily_grams_log ~ 1 + YEAR +
                          (1 | intersections) + 
                          (1 | NHISPID), 
                        data = model_data, 
                        estoptions = list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                          mcmcMeth = list(burnin = 5000,
                                                          thinning = 50,
                                                          resi.store=TRUE))))

# Full model
(full_grams <- runMLwiN(capped_daily_grams_log ~ 1 + YEAR +
                          SEX + age_diaz + race_6_cats + education_3_cats +
                          (1 | intersections) + 
                          (1 | NHISPID), 
                        data = model_data, 
                        estoptions = list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                          mcmcMeth = list(burnin = 5000,
                                                          thinning = 50,
                                                          resi.store=TRUE))))

# save the model objects
saveRDS(null_grams, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/grams/null_grams_drinkers_no_outliers.rds")
saveRDS(full_grams, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/grams/full_grams_drinkers_no_outliers.rds")

# Check convergence achieved
summary(full_grams@chains[, "FP_Intercept"])
mcmc_trace(full_grams@chains)