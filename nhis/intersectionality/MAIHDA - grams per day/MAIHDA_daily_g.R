# This scripts runs MAIHDA on the transformed grams per day data for drinkers 

setwd("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_code/nhis/intersectionality/")

# Load in data
transformed_drinkers <- readRDS("U:/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/6_nhis_alc_clean_transformed_drinkers_log.RDS")

# Load necessary packages
library("R2MLwiN")
library(tidyr)
library(dplyr)
library(bayesplot)
library(coda)
library(ggplot2)

# Bias toward non-scientific notation
options(scipen=10)

# State file path for MLwiN
options(MLwiN_path="C:\\Program Files\\MLwiN v3.05")

# Generate intersectional groups
data_intersections_MAIHDA <- transformed_drinkers %>% 
  group_by(SEX, race_5_cats, education_3_cats, age_3_cats, decade) %>% 
  mutate(intersections = cur_group_id()) %>%
  group_by(intersections) %>%
  mutate(count=n())

# Add a column of the observed mean grams per day for each intersection
data_intersections_MAIHDA <- data_intersections_MAIHDA %>%
  group_by(intersections) %>%
  mutate(mean_observed_grams = mean(alc_daily_g_capped_200))

# Create a reference table of the intersectional groups - numbers and names
temp <- data_intersections_MAIHDA %>%
  mutate(sex = dplyr::recode(SEX, Male = "M", Female = "F"),
         race_5_cats = dplyr::recode(race_5_cats, 
                                     "Black/African American" = "Black", 
                                     "Hispanic, White" = "Hispanic"),
         education_3_cats = dplyr::recode(education_3_cats, 
                                          "high school or less" = "low edu.", 
                                          "some college" = "med. edu.", 
                                          "4+ years college" = "high edu."))

temp2 <- temp %>% 
  mutate(intersectional_names = as.character(paste(sex, age_3_cats, race_5_cats, education_3_cats, decade)))

MAIHDA_intersections_reference <- distinct(temp2, intersections, .keep_all = TRUE) %>% dplyr::select(intersections, intersectional_names)

# Prep data for use with R2mlwin package:
# Generate a constant
data_intersections_MAIHDA <- data_intersections_MAIHDA %>%
  mutate(cons=1)
# Sort by intersections
data_intersections_MAIHDA <- data_intersections_MAIHDA %>% arrange(intersections)

# Specify model formulae

# null model
F1 <- capped_daily_grams_log ~ 1 + (1 | intersections) + (1 | NHISPID)
# full model
F2 <- capped_daily_grams_log ~ 1 + SEX + age_3_cats + race_5_cats + education_3_cats + decade + (1 | intersections) + (1 | NHISPID)

### 1.1 Run MAIHDA with IGLS:

## 1.1.1 Null model
VarCompModel <- runMLwiN(Formula = F1, 
                          data = data_intersections_MAIHDA,
                          estoptions = list(resi.store = TRUE))
# Calculate the VPC
print(VPC_IGLS_null <- VarCompModel["RP"][["RP2_var_Intercept"]]/
        (VarCompModel["RP"][["RP1_var_Intercept"]] + VarCompModel["RP"][["RP2_var_Intercept"]]))

# View level 1 residuals
hist(VarCompModel@residual$lev_1_resi_est_Intercept)
qqnorm(VarCompModel@residual$lev_1_resi_est_Intercept)
qqline(VarCompModel@residual$lev_1_resi_est_Intercept, col = "steelblue", lwd = 2)

# View level 2 residuals
hist(VarCompModel@residual$lev_2_resi_est_Intercept)
qqnorm(VarCompModel@residual$lev_2_resi_est_Intercept)
qqline(VarCompModel@residual$lev_2_resi_est_Intercept, col = "steelblue", lwd = 2)

# Save the model object
saveRDS(VarCompModel, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/grams_IGLS_null.RDS")

# OR read in the model object
VarCompModel <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/grams_IGLS_null.RDS")

## 1.1.2 Main-effects model
VarCompResid <- runMLwiN(Formula = F2, 
                         data = data_intersections_MAIHDA,
                         estoptions = list(resi.store = TRUE))
# Re-calculate VPC
print(VPC_IGLS_full <- VarCompResid["RP"][["RP2_var_Intercept"]]/(VarCompResid["RP"][["RP1_var_Intercep"]] + VarCompResid["RP"][["RP2_var_Intercept"]]))

# View level 1 residuals
hist(VarCompResid@residual$lev_1_resi_est_Intercept)
qqnorm(VarCompResid@residual$lev_1_resi_est_Intercept)
qqline(VarCompResid@residual$lev_1_resi_est_Intercept, col = "steelblue", lwd = 2)
plot(VarCompResid@residual$lev_1_resi_est_Intercept)
abline(h = 0, lty = 2, col = "red")

# View level 2 residuals
hist(VarCompResid@residual$lev_2_resi_est_Intercept)
qqnorm(VarCompResid@residual$lev_2_resi_est_Intercept)
qqline(VarCompResid@residual$lev_2_resi_est_Intercept, col = "steelblue", lwd = 2)
plot(VarCompResid@residual$lev_2_resi_est_Intercept)
abline(h = 0, lty = 2, col = "red")

# Save the model object
saveRDS(VarCompResid, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/grams_IGLS_full.RDS")

# OR read in model object
VarCompResid <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/grams_IGLS_full.RDS")

# Estimate yhat (and SEs) using predict function
data_intersections_MAIHDA$yhat <- predict(VarCompResid) # the predicted expected value
data_intersections_MAIHDA$yhat_se <- predict(VarCompResid, se.fit=TRUE)$se.fit # the standard error of the predicted expected value

# Create table with results for just one intersection
processed_intersections_MAIHDA <- distinct(data_intersections_MAIHDA, intersections, .keep_all = TRUE)

# Extract residuals from the Mlwin output object
processed_intersections_MAIHDA$residuals <- VarCompResid@residual$lev_2_resi_est_Intercept 

# Extract the estimate of variance around the residuals, and take the sqrt of it to get the standard error around the residuals
processed_intersections_MAIHDA$residualsSE <- sqrt(VarCompResid@residual$lev_2_resi_var_Intercept) %>% sqrt() # standard error around residuals

# Estimate the overall mean combining additive effects (mean) and multiplicative effects (residuals)
processed_intersections_MAIHDA <- processed_intersections_MAIHDA %>% 
  mutate(estimate = sum(yhat, residuals))

# Estimate the total standard error by combining the error around the mean plus the error around the residuals:
processed_intersections_MAIHDA <- processed_intersections_MAIHDA %>% 
  mutate(SE =(sqrt((yhat_se*yhat_se)+(residualsSE*residualsSE)))/2)

# Generate IGLS results table
Results_table_IGLS <- processed_intersections_MAIHDA %>%
  dplyr::select(intersections, count, birth_year, YEAR, decade, AGE, SEX, race_5_cats, education_3_cats, age_3_cats, yhat, yhat_se, residuals, residualsSE, estimate, SE)

Results_table_IGLS <- inner_join(Results_table_IGLS, MAIHDA_intersections_reference, by = "intersections")

### 2. Run MAIHDA with MCMC

# Duplicate data for use with MCMC
data_intersections_MAIHDA_MCMC <- data_intersections_MAIHDA

## 2.1.1 Null model
VarCompModelMCMC <- runMLwiN(Formula = F1, 
                              data = data_intersections_MAIHDA_MCMC, 
                              estoptions = list(resi.store = TRUE, EstM = 1,
                              burnin = 5000,
                              iter = 50000))
# Calculate the VPC
print(VPC_MCMC_null <- VarCompModelMCMC["RP"][["RP2_var_Intercept"]]/(VarCompModelMCMC["RP"][["RP1_var_Intercept"]] + VarCompModelMCMC["RP"][["RP2_var_Intercept"]]))

# Check convergence achieved
summary(VarCompModelMCMC@chains)
mcmc_trace(VarCompModelMCMC@chains)
autocorr(VarCompModelMCMC@chains)

# Save the model object
saveRDS(VarCompModelMCMC, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/grams_MCMC_null.RDS")

# OR read in the model object
VarCompModelMCMC <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/grams_MCMC_null.RDS")

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

# Check convergence achieved
summary(VarCompResidMCMC@chains[, "FP_Intercept"])
mcmc_trace(VarCompResidMCMC@chains)
autocorr(VarCompResidMCMC@chains)

# Save model object
saveRDS(VarCompResidMCMC, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/grams_MCMC_full.RDS")

# OR read in model object
VarCompResidMCMC <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/grams_MCMC_full.RDS")

# Generate a table of model coefficients & variance estimates, comparing the null and full models

temp_null <- getSummary(VarCompModelMCMC) 
temp_null <- as.data.frame(Coefs_null[["coef"]])
temp_null <- round(temp_null, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(temp_null) <- c("fixed intercept","between strata variance, null","within strata variance, null")

# seperate out coefs and variance
coefs_null <- temp_null['fixed intercept',]
variance_null <- temp_null[c('between strata variance, null','within strata variance, null'),]

temp_main_effects <- getSummary(VarCompResidMCMC)
temp_main_effects <- as.data.frame(temp_main_effects[["coef"]])
temp_main_effects <- round(temp_main_effects, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(temp_main_effects) <- c("fixed intercept","female","age 25-69", "age 70+", 
                                  "Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Other", 
                                  "Hispanic", "Some college", "4+ years college","2010-2018", 
                                  "between strata variance, main effects", "within strata variance, main effects")

# seperate out coefs and variance
coefs_main_effects <- temp_main_effects[c("fixed intercept","female","age 25-69", "age 70+", 
                                  "Non-Hispanic Black", "Non-Hispanic Asian", "Non-Hispanic Other", 
                                  "Hispanic", "Some college", "4+ years college","2010-2018"),]
variance_main_effects <- temp_main_effects[c("between strata variance, main effects", "within strata variance, main effects"),]

coefs_table <- rbind(coefs_null, coefs_main_effects)
variance_table <- rbind(variance_null, variance_main_effects)
coef_variance_table <- rbind(coefs_table, variance_table)

# Create a gt object to enable addition of headers
gt_table <- gt(coef_variance_table, rownames_to_stub = TRUE) %>%
 tab_row_group(
    label = "Variance",
    rows = 13:16
  ) %>%
  tab_row_group(
    label = "Main effects model",
    rows = 2:12
  ) %>%
  tab_row_group(
    label = "Null model",
    rows = 1:1
  )
  
gt_table

# Estimate yhat (and SEs) using predict function
data_intersections_MAIHDA_MCMC$yhat <- predict(VarCompResidMCMC) # the predicted expected value
data_intersections_MAIHDA_MCMC$yhat_se <- predict(VarCompResidMCMC, se.fit=TRUE)$se.fit # the standard error of the predicted expected value

# Create table with results for just one intersection
processed_intersections_MAIHDA_MCMC <- distinct(data_intersections_MAIHDA_MCMC, intersections, .keep_all = TRUE)

# Extract residuals from the Mlwin output object
processed_intersections_MAIHDA_MCMC$residuals <- VarCompResidMCMC@residual$lev_2_resi_est_Intercept

# Extract the estimate of variance around the residuals, and take the sqrt of it to get the standard error around the residuals
processed_intersections_MAIHDA_MCMC$residualsSE <- sqrt(VarCompResidMCMC@residual$lev_2_resi_variance_Intercept) # standard error around residuals

# Estimate the overall mean combining additive effects (main effects) and multiplicative effects (residuals)
processed_intersections_MAIHDA_MCMC <- processed_intersections_MAIHDA_MCMC %>% 
  mutate(estimate = sum(yhat, residuals))

# Estimate the total standard error by combining the error around the mean plus the error around the residuals:
processed_intersections_MAIHDA_MCMC <- processed_intersections_MAIHDA_MCMC %>% 
  mutate(SE =(sqrt((yhat_se*yhat_se)+(residualsSE*residualsSE)))/2)

# Generate MCMC results table
Results_table_MCMC <- processed_intersections_MAIHDA_MCMC %>%
  dplyr::select(intersections, count, birth_year, YEAR, decade, AGE, SEX, race_5_cats, education_3_cats, age_3_cats, yhat, yhat_se, residuals, residualsSE, estimate, SE, mean_observed_grams)

Results_table_MCMC <- inner_join(Results_table_MCMC, MAIHDA_intersections_reference, by = "intersections")

### Back transform the estimates

back_transform_log <- function(x) (exp(x))

# Back transform estimates
transformed_estimates_IGLS <- Results_table_IGLS %>% 
  mutate(
    back_transformed_estimate = back_transform_log(estimate),
    back_transformed_CI_lower = back_transform_log(estimate - 1.96*SE),
    back_transformed_CI_upper = back_transform_log(estimate + 1.96*SE))

transformed_estimates_MCMC <- Results_table_MCMC %>% 
  mutate(
    back_transformed_estimate = back_transform_log(estimate),
    back_transformed_CI_lower = back_transform_log(estimate - 1.96*SE),
    back_transformed_CI_upper = back_transform_log(estimate + 1.96*SE))

# Save back transformed estimates
saveRDS(transformed_estimates_IGLS, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/MAIHDA_transformed_estimates_grams_IGLS.RDS")
saveRDS(transformed_estimates_MCMC, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/MAIHDA_transformed_estimates_grams_MCMC.RDS")

# Compare the results from IGLS and MCMC
transformed_estimates_IGLS %>% group_by(SEX) %>% summarise(mean(back_transformed_estimate), mean(back_transformed_CI_lower), mean(back_transformed_CI_upper))
transformed_estimates_MCMC %>% group_by(SEX) %>% summarise(mean(back_transformed_estimate), mean(back_transformed_CI_lower), mean(back_transformed_CI_upper))
# Estimates the same but confidence intervals more narrow with MCMC

# Explore face validity of back-transformed MCMC data

# Compare overall mean
mean(transformed_estimates_MCMC$mean_observed_grams) # 7.56
mean(transformed_estimates_MCMC$back_transformed_estimate) # 2.14

# Compare mean grams per day of each intersectional group with the estimated grams per day
temp <- transformed_estimates_MCMC %>% dplyr::select(intersectional_names, count, mean_observed_grams, back_transformed_estimate)
# Visually review the correlation between the observed and estimated values
ggplot(temp, aes(x=mean_observed_grams, y=back_transformed_estimate)) + geom_point() +
  ggtitle("Comparisson of observed and estimated daily grams, 180 intersectional groups")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/comparisson of observed and estimated grams.png", 
dpi=300, width=33, height=19, units="cm")
# Interpretation: Positive correlation but generally estimates are lower than observed.  

# Compare the ranking of intersectional groups based on observed and estimated grams
temp$rank_observed_grams <-rank(temp$mean_observed_grams)
temp$rank_estimated_grams <-rank(temp$back_transformed_estimate)
# Visually review the correlation between the observed and estimated rankings
ggplot(temp, aes(x=rank_observed_grams, y=rank_estimated_grams)) + geom_point() +
  ggtitle("Comparisson of observed vs estimated drinking 'rank', 180 intersectional groups")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/comparisson of observed and estimated drinking ranks.png", 
       dpi=300, width=33, height=19, units="cm")
# Interpretation: Positive correlation but some groups are estimated to be in a higher ranking drinking category than the observed grams may suggest.

# The discrepancies between observed and estimated are expected to be related to by shrinkage.

# Save the comparison of results table
saveRDS(temp, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/comparissons of observed and estimated daily grams.RDS")
