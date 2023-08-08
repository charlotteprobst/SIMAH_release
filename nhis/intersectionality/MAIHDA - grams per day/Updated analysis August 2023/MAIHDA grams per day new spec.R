# ESTIMATING GRAMS USING MAIHDA, FULL SAMPLE (INCLUDING NON DRINKERS)

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
library(bayesplot)
library(coda)
library("R2MLwiN")
options(MLwiN_path="C:/Program Files/MLwiN v3.05/")
source("functions/recode_race_ethnicity.R")

nhis_alc_clean <- nhis_alc_clean %>% select(YEAR, NHISPID, SEX, ALCSTAT1, education_3_cats, age_3_cats, HISPYN, RACENEW, race_5_cats, alc_daily_g_capped_200)

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

# Keep 7 most populous race only and drop redundant race/ethnicity variables
nhis_new_spec <- nhis_alc_clean %>% filter(race_ethnicity==1|race_ethnicity==8|race_ethnicity==2|race_ethnicity==4|
                                            race_ethnicity==7|race_ethnicity==12|race_ethnicity==3) %>%
                                            select(-HISPYN, -RACENEW, -race_5_cats)

# Check group sizes by intersections based on 7 race categories
full_sample_intersections <- nhis_new_spec %>% 
  group_by(SEX, race_ethnicity, education_3_cats, age_3_cats) %>% 
  mutate(intersections = cur_group_id())%>%
  group_by(intersections) %>%
  mutate(count=n())

group_sizes <- full_sample_intersections %>% distinct(intersections, .keep_all = TRUE)
sum(group_sizes$count >= 20) 
# 9 groups with n<20.  6 of these are Hispanic, other, therefore drop this group and regenerate intersections.

nhis_new_spec <- nhis_new_spec %>% filter(race_ethnicity!=12)

# Convert race and ethnicity from numeric to categorical variable
nhis_new_spec$race_6_cats <- factor(nhis_new_spec$race_ethnicity,
                                       levels = c(1,8,2,4,7,3),
                                       labels = c("Non-hispanic, White only", "Hispanic, White only", 
                                                  "Non-hispanic, Black/African American only", "Non-hispanic, Asian only", 
                                                  "Non-hispanic, Multiple race", "American Indian/Alaska Native only"))

# Check group sizes by intersections based on 6 race categories
full_sample_intersections <- nhis_new_spec %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_3_cats) %>% 
  mutate(intersections = cur_group_id())%>%
  group_by(intersections) %>%
  mutate(count=n())

group_sizes <- full_sample_intersections %>% distinct(intersections, .keep_all = TRUE)
sum(group_sizes$count >= 20) # 105 out of 108 groups with n>20

# Consider data transformation for alc daily grams
full_sample <- full_sample_intersections %>% mutate(new_grams = alc_daily_g_capped_200 + 0.02)# add half of the smallest grams value (for drinkers) to zero values
# Check recommended lambda with boxcox
b <- MASS::boxcox(lm(full_sample$new_grams ~ 1))
lambda <- b$x[which.max(b$y)] # -0.06
lambda2 <- forecast::BoxCox.lambda(full_sample$new_grams)  # -0.07
# As both suggested lambda are close to 0, log transformation is appropriate
full_sample$capped_daily_grams_log <- log(full_sample$new_grams)
# Distribution plot 
ggplot(full_sample, aes(x=capped_daily_grams_log), y) + geom_histogram(bins=200) + 
  ggtitle("Distribution of estimated daily grams post transformation, full sample")+ 
  xlab("Daily grams of alcohol, post transformation") +
  ylab("Frequency")

saveRDS(full_sample, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/full_sample_new_spec.RDS")


# Prep data for use with R2mlwin package:
# Generate a constant
full_sample_MAIHDA <- full_sample %>%
  mutate(cons=1)
# Sort by intersections
full_sample_MAIHDA <- full_sample_MAIHDA %>% arrange(intersections)

# Specify model formulae

# null model
F1 <- alc_daily_g_capped_200 ~ 1 + YEAR + (1 | intersections) + (1 | NHISPID)
# full model
F2 <- alc_daily_g_capped_200 ~ 1 + YEAR + SEX + age_3_cats + race_5_cats + education_3_cats + decade + (1 | intersections) + (1 | NHISPID)

#### Run MAIHDA on full sample, non-transformed data, MCMC

## Null model
VarCompModelMCMC <- runMLwiN(Formula = F1, 
                             data = full_sample_MAIHDA, 
                             estoptions = list(resi.store = TRUE, EstM = 1,
                                               burnin = 5000,
                                               iter = 50000))
# Calculate the VPC
print(VPC_MCMC_null <- VarCompModelMCMC["RP"][["RP2_var_Intercept"]]/(VarCompModelMCMC["RP"][["RP1_var_Intercept"]] + VarCompModelMCMC["RP"][["RP2_var_Intercept"]]))

# Check convergence achieved
summary(VarCompModelMCMC@chains)
mcmc_trace(VarCompModelMCMC@chains)
autocorr(VarCompModelMCMC@chains)

VarCompModel <- runMLwiN(Formula = F1, 
                         data = full_sample_MAIHDA,
                         estoptions = list(resi.store = TRUE))

# Save the model object
saveRDS(VarCompModelMCMC, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/grams_null_new_spec.RDS")


# 2.1.2 Main effects model
VarCompResidMCMC <- runMLwiN(Formula = F2, 
                             data = data_intersections_MAIHDA_MCMC, 
                             estoptions = list(resi.store = TRUE, EstM = 1,
                                               burnin = 500,
                                               iter = 5000))
# Default: burn in = 500, iterations = 5000, a thinning factor of 1, a random number seed of 1, 1 chain 

# Recalculate VPC
print(VPC_MCMC_full <- VarCompResidMCMC["RP"][["RP2_var_Intercept"]]/
        (VarCompResidMCMC["RP"][["RP1_var_Intercept"]] + VarCompResidMCMC["RP"][["RP2_var_Intercept"]]))
