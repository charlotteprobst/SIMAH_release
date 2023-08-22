# MAIHDA of grams of alcohol per day

# Spec 1: Original age cats, new race cats.

# Set-up

# Set wd
setwd("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_code/nhis/intersectionality")

# Read in necessary R packages & functions
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ragg)
library(bayesplot)
library(coda)
library(memisc)
library("R2MLwiN")
source("functions/recode_race_ethnicity.R")

options(MLwiN_path="C:/Program Files/MLwiN v3.05/")

setwd("C:/Users/cmp21seb/Documents/SIMAH/")

options(scipen=10)


##### PRE PROCESSING

## FULL SAMPLE

# Read in data (full sample):
data <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/nhis_alc_clean_full_sample.RDS")

# Drop individuals age <21
data_0 <- data %>% filter(age_diaz!="18-20")

# Generate new race category variable

# Keep 6 selected race and ethnicity groups
data_1 <- data_0 %>% filter(race_ethnicity==1|race_ethnicity==8|race_ethnicity==2|race_ethnicity==4|
                            race_ethnicity==7|race_ethnicity==3) 

# Convert race and ethnicity from numeric to categorical variable
data_1$race_6_cats <- factor(data_1$race_ethnicity,
                             levels = c(1,8,2,4,7,3),
                             labels = c("White", "Hispanic White", 
                                        "Black", "Asian", 
                                        "Multiple race", "AI/AN"))

# Generate intersections
data_2 <- data_1 %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_diaz) %>% 
  mutate(intersections = cur_group_id()) %>%
  mutate(intersectional_names = as.character(paste(SEX, age_diaz, race_6_cats, education_3_cats)))
  
# Check intersectional group sizes
temp <- data_2 %>% 
  group_by(intersections) %>%
  mutate(count=n())

group_sizes <- temp %>% distinct(intersections, .keep_all = TRUE)
sum(group_sizes$count <= 20) # 2 groups with n<=20

# Add a column of the observed mean grams per day for each intersection
data_3 <- data_2 %>%
  group_by(intersections) %>%
  mutate(mean_observed_grams = mean(alc_daily_g_capped_200))

##### RUN THE MAIHDA MODELS WITH FULL SAMPLE #####

# Prep data for use with Mlwin
model_data <- data_3 %>%
  mutate(cons=1) %>% 
  arrange(intersections, NHISPID)

model_data$age_diaz <- droplevels(model_data$age_diaz)

model_data$YEAR <- as.factor(model_data$YEAR)

# Generate reference table with intersectional names
intersections_reference <- model_data %>%
  distinct(intersections, intersectional_names)

# Null model
(null_grams <- runMLwiN(capped_daily_grams_log ~ 1 + 
                          (1 | intersections) + 
                          (1 | NHISPID), 
                             data = model_data, 
                             estoptions = list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                               mcmcMeth = list(burnin = 5000,
                                                               thinning = 50,
                                                               resi.store=TRUE))))
# Full model
(full_grams <- runMLwiN(capped_daily_grams_log ~ 1 + SEX + age_diaz + race_6_cats + education_3_cats +
                          (1 | intersections) + 
                          (1 | NHISPID), 
                        data = model_data, 
                        estoptions = list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                          mcmcMeth = list(burnin = 5000,
                                                          thinning = 50,
                                                          resi.store=TRUE))))

# save the model objects
saveRDS(null_grams, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/grams/null_grams_spec_3.rds")
saveRDS(full_grams, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/grams/full_grams_spec_3.rds")

# Check convergence achieved
summary(full_grams@chains[, "FP_Intercept"])
mcmc_trace(full_grams@chains)


##### PRODUCE A TABLE OF MODEL COEFFICIENTS 
# comparing the null and full models

coefs_null <- getSummary(null_grams)
coefs_null <- as.data.frame(coefs_null[["coef"]])
coefs_null <- round(coefs_null, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_null) <- c("intercept_FE_1","strata_RE_1","individuals_RE_1")

coefs_full <- getSummary(full_grams)
coefs_full <- as.data.frame(coefs_full[["coef"]])
coefs_full <- round(coefs_full, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_full) <- c("intercept_FE_2","female","age 25-59", "age 60+",
                          "Hispanic", "Black", "Asian", "Multiple race", "AI/AN",
                          "Some college", "4+ years college", "RP2_var_intercept", "RP1_var_intercept")

coefs_table <- rbind(coefs_null, coefs_full)
saveRDS(coefs_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/grams model coefficients and variance_spec_3.rds")
write.csv(coefs_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/grams model coefficients and variance_spec_3.csv")


##### CALCULATE VPC AND PCV (from the parameter point estimates)
VPC_grams_null <- print(VPC <- null_grams["RP"][["RP2_var_Intercept"]]/(null_grams["RP"][["RP1_var_Intercept"]] + null_grams["RP"][["RP2_var_Intercept"]]))
VPC_grams_full <- print(VPC <- full_grams["RP"][["RP2_var_Intercept"]]/(full_grams["RP"][["RP1_var_Intercept"]] + full_grams["RP"][["RP2_var_Intercept"]]))
VPC_table <- data.frame(Model = c("null", "main effects"),
                        VPC = c(VPC_grams_null, VPC_grams_full))
write.csv(VPC_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/VPC_table_grams_spec_3.csv")

##### EXTRACT MODEL ESTIMATES

# Add intersectional group sizes as important indicator of expected level of shrinkage
model_data <- model_data %>% 
  group_by(intersections) %>%
  mutate(count=n())

# Estimate yhat (and SEs) using predict function
model_data$yhat <- predict(full_grams) # the predicted expected value
model_data$yhat_se <- predict(full_grams, se.fit=TRUE)$se.fit # the standard error of the predicted expected value

# Create table with results for just one intersection
mdata_results <- distinct(model_data, intersections, .keep_all = TRUE)

# Extract residuals from the Mlwin output object
mdata_results$residuals <- full_grams@residual$lev_2_resi_est_Intercept

# Extract the estimate of variance around the residuals, and take the sqrt of it to get the standard error around the residuals
mdata_results$residualsSE <- sqrt(full_grams@residual$lev_2_resi_variance_Intercept) # standard error around residuals

# Estimate the overall mean combining additive effects (main effects) and multiplicative effects (residuals)
mdata_results <- mdata_results %>% 
  mutate(estimate = sum(yhat, residuals))

# Estimate the total standard error by combining the error around the mean plus the error around the residuals:
mdata_results <- mdata_results %>% 
  mutate(SE =(sqrt((yhat_se*yhat_se)+(residualsSE*residualsSE)))) # For now, assuming that division by 2 not required

# Generate MCMC results table
mdata_results <- mdata_results %>%
  dplyr::select(intersections, intersectional_names, count, SEX, race_6_cats, education_3_cats, 
                age_diaz, yhat, yhat_se, residuals, residualsSE, estimate, SE, mean_observed_grams)

##### Back transform the estimates
back_transform_log <- function(x) (exp(x))

mdata_results_2 <- mdata_results %>% 
  mutate(
    back_transformed_estimate = back_transform_log(estimate),
    back_transformed_CI_lower = back_transform_log(estimate - 1.96*SE),
    back_transformed_CI_upper = back_transform_log(estimate + 1.96*SE))

##### Save estimates
saveRDS(mdata_results_2, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/estimated_grams_spec_3.RDS")
write.csv(mdata_results_2, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/estimated_grams_spec_3.csv")

##### Explore face validity of back-transformed data
temp <- mdata_results_2 %>% dplyr::select(intersectional_names, count, mean_observed_grams, back_transformed_estimate)
ggplot(temp, aes(x=mean_observed_grams, y=back_transformed_estimate)) + geom_point() +
  ggtitle("Comparisson of observed and estimated daily grams, 180 intersectional groups")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/new spec August 2023/grams/observed vs estimated grams_spec_3.png", 
       dpi=300, width=33, height=19, units="cm")
# Interpretation: Positive correlation but generally estimates are lower than observed.  

# Compare the ranking of intersectional groups based on observed and estimated grams
temp$rank_observed_grams <-rank(temp$mean_observed_grams)
temp$rank_estimated_grams <-rank(temp$back_transformed_estimate)
ggplot(temp, aes(x=rank_observed_grams, y=rank_estimated_grams)) + geom_point() +
  ggtitle("Comparisson of observed vs estimated drinking 'rank', 180 intersectional groups")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/new spec August 2023/grams/observed vs estimated grams ranks_spec_3.png", 
       dpi=300, width=33, height=19, units="cm")
# Interpretation: Positive correlation but some groups are estimated to be in a higher ranking drinking category than the observed grams may suggest.
# The discrepancies between observed and estimated are expected to be related to by shrinkage.


##### RUN THE MAIHDA MODELS WITH DRINKERS ONLY #####

# Subset drinkers
data_4 <- data_3 %>% filter(ALCSTAT1=="Current drinker")

# Prep data for use with Mlwin
model_data_drinkers <- data_4 %>%
  mutate(cons=1) %>% 
  arrange(intersections, NHISPID)

model_data_drinkers$age_diaz <- droplevels(model_data_drinkers$age_diaz)

model_data_drinkers$YEAR <- as.factor(model_data_drinkers$YEAR)

# Generate reference table with intersectional names
intersections_reference_drinkers <- model_data_drinkers %>%
  distinct(intersections, intersectional_names)

# Null model
(null_grams_drinkers <- runMLwiN(capped_daily_grams_log ~ 1 + 
                          (1 | intersections) + 
                          (1 | NHISPID), 
                        data = model_data_drinkers, 
                        estoptions = list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                          mcmcMeth = list(burnin = 5000,
                                                          thinning = 50,
                                                          resi.store=TRUE))))
# Full model
(full_grams_drinkers <- runMLwiN(capped_daily_grams_log ~ 1 + SEX + age_diaz + race_6_cats + education_3_cats +
                          (1 | intersections) + 
                          (1 | NHISPID), 
                        data = model_data_drinkers, 
                        estoptions = list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                          mcmcMeth = list(burnin = 5000,
                                                          thinning = 50,
                                                          resi.store=TRUE))))

# save the model objects
saveRDS(null_grams_drinkers, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/grams/null_grams_drinkers_spec_3.rds")
saveRDS(full_grams_drinkers, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/grams/full_grams_drinkers_spec_3.rds")

# Check convergence achieved
summary(full_grams@chains[, "FP_Intercept"])
mcmc_trace(full_grams@chains)


##### PRODUCE A TABLE OF MODEL COEFFICIENTS 
# comparing the null and full models

coefs_null <- getSummary(null_grams)
coefs_null <- as.data.frame(coefs_null[["coef"]])
coefs_null <- round(coefs_null, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_null) <- c("intercept_FE_1","strata_RE_1","individuals_RE_1")

coefs_full <- getSummary(full_grams)
coefs_full <- as.data.frame(coefs_full[["coef"]])
coefs_full <- round(coefs_full, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_full) <- c("intercept_FE_2","female","age 25-59", "age 60+",
                          "Hispanic", "Black", "Asian", "Multiple race", "AI/AN",
                          "Some college", "4+ years college", "RP2_var_intercept", "RP1_var_intercept")

coefs_table <- rbind(coefs_null, coefs_full)
saveRDS(coefs_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/grams model coefficients and variance_spec_3.rds")
write.csv(coefs_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/grams model coefficients and variance_spec_3.csv")


##### CALCULATE VPC AND PCV (from the parameter point estimates)
VPC_grams_null <- print(VPC <- null_grams["RP"][["RP2_var_Intercept"]]/(null_grams["RP"][["RP1_var_Intercept"]] + null_grams["RP"][["RP2_var_Intercept"]]))
VPC_grams_full <- print(VPC <- full_grams["RP"][["RP2_var_Intercept"]]/(full_grams["RP"][["RP1_var_Intercept"]] + full_grams["RP"][["RP2_var_Intercept"]]))
VPC_table <- data.frame(Model = c("null", "main effects"),
                        VPC = c(VPC_grams_null, VPC_grams_full))
write.csv(VPC_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/VPC_table_grams_spec_3.csv")

##### EXTRACT MODEL ESTIMATES

# Add intersectional group sizes as important indicator of expected level of shrinkage
model_data <- model_data %>% 
  group_by(intersections) %>%
  mutate(count=n())

# Estimate yhat (and SEs) using predict function
model_data$yhat <- predict(full_grams) # the predicted expected value
model_data$yhat_se <- predict(full_grams, se.fit=TRUE)$se.fit # the standard error of the predicted expected value

# Create table with results for just one intersection
mdata_results <- distinct(model_data, intersections, .keep_all = TRUE)

# Extract residuals from the Mlwin output object
mdata_results$residuals <- full_grams@residual$lev_2_resi_est_Intercept

# Extract the estimate of variance around the residuals, and take the sqrt of it to get the standard error around the residuals
mdata_results$residualsSE <- sqrt(full_grams@residual$lev_2_resi_variance_Intercept) # standard error around residuals

# Estimate the overall mean combining additive effects (main effects) and multiplicative effects (residuals)
mdata_results <- mdata_results %>% 
  mutate(estimate = sum(yhat, residuals))

# Estimate the total standard error by combining the error around the mean plus the error around the residuals:
mdata_results <- mdata_results %>% 
  mutate(SE =(sqrt((yhat_se*yhat_se)+(residualsSE*residualsSE)))) # For now, assuming that division by 2 not required

# Generate MCMC results table
mdata_results <- mdata_results %>%
  dplyr::select(intersections, intersectional_names, count, SEX, race_6_cats, education_3_cats, 
                age_diaz, yhat, yhat_se, residuals, residualsSE, estimate, SE, mean_observed_grams)

##### Back transform the estimates
back_transform_log <- function(x) (exp(x))

mdata_results_2 <- mdata_results %>% 
  mutate(
    back_transformed_estimate = back_transform_log(estimate),
    back_transformed_CI_lower = back_transform_log(estimate - 1.96*SE),
    back_transformed_CI_upper = back_transform_log(estimate + 1.96*SE))

##### Save estimates
saveRDS(mdata_results_2, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/estimated_grams_spec_3.RDS")
write.csv(mdata_results_2, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/estimated_grams_spec_3.csv")

##### Explore face validity of back-transformed data
temp <- mdata_results_2 %>% dplyr::select(intersectional_names, count, mean_observed_grams, back_transformed_estimate)
ggplot(temp, aes(x=mean_observed_grams, y=back_transformed_estimate)) + geom_point() +
  ggtitle("Comparisson of observed and estimated daily grams, 180 intersectional groups")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/new spec August 2023/grams/observed vs estimated grams_spec_3.png", 
       dpi=300, width=33, height=19, units="cm")
# Interpretation: Positive correlation but generally estimates are lower than observed.  

# Compare the ranking of intersectional groups based on observed and estimated grams
temp$rank_observed_grams <-rank(temp$mean_observed_grams)
temp$rank_estimated_grams <-rank(temp$back_transformed_estimate)
ggplot(temp, aes(x=rank_observed_grams, y=rank_estimated_grams)) + geom_point() +
  ggtitle("Comparisson of observed vs estimated drinking 'rank', 180 intersectional groups")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/new spec August 2023/grams/observed vs estimated grams ranks_spec_3.png", 
       dpi=300, width=33, height=19, units="cm")
# Interpretation: Positive correlation but some groups are estimated to be in a higher ranking drinking category than the observed grams may suggest.
# The discrepancies between observed and estimated are expected to be related to by shrinkage.