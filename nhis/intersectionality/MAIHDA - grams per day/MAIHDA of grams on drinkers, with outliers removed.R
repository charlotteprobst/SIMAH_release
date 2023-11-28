# Run MAIHDA grams per day with outliers removed

# load packages
library(tidyverse)
library(ragg)
library(bayesplot)
library(coda)
library(memisc)
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

# re-estimate the mean_observed_grams for each intersection based on the non-outlier data
model_data <- data_no_outliers_2 %>% group_by(intersectional_names) %>%
  mutate(mean_observed_grams=mean(alc_daily_g_capped_200))

# Prep data for use with Mlwin
model_data <- model_data %>%
  mutate(cons=1) %>% 
  arrange(intersections, NHISPID)

model_data$age_diaz <- droplevels(model_data$age_diaz)
model_data$YEAR <- as.factor(model_data$YEAR)

# Generate reference table with intersectional names 
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


##################################################################### ANALYSIS

# Read in the model objects
null_grams <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/grams/null_grams_drinkers_no_outliers.rds")
full_grams <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/grams/full_grams_drinkers_no_outliers.rds")


##### CHECK MODELLING ASSUMPTIONS

## Null model
# # Histogram of residuals
# hist(residuals(null_grams))
# # Heteroskedasticity of residuals
# plot(fitted(null_grams), resid(null_grams))
# abline(h = 0, lty = 2, col = "red")
# # QQ plot
# qqnorm(residuals(null_grams))
# qqline(residuals(null_grams), col = "steelblue", lwd = 2)
# 
# ## Full model
# hist(residuals(full_grams))
# # Heteroskedasticity of residuals
# plot(fitted(full_grams), resid(full_grams))
# abline(h = 0, lty = 2, col = "red")
# # QQ plot
# qqnorm(residuals(full_grams))
# qqline(residuals(full_grams), col = "steelblue", lwd = 2)


## Null model
## Level 1 residuals
hist(null_grams["residual"][["lev_1_resi_est_Intercept"]])
# Heteroskedasticity of residuals
plot(null_grams["residual"][["lev_1_resi_est_Intercept"]])
abline(h = 0, lty = 2, col = "red")
# QQ plot
qqnorm(null_grams["residual"][["lev_1_resi_est_Intercept"]])
qqline(null_grams["residual"][["lev_1_resi_est_Intercept"]], col = "steelblue", lwd = 2)
## Level 2 residuals
hist(null_grams["residual"][["lev_2_resi_est_Intercept"]])
# Heteroskedasticity of residuals
plot(null_grams["residual"][["lev_2_resi_est_Intercept"]])
abline(h = 0, lty = 2, col = "red")
# QQ plot
qqnorm(null_grams["residual"][["lev_2_resi_est_Intercept"]])
qqline(null_grams["residual"][["lev_2_resi_est_Intercept"]], col = "steelblue", lwd = 2)

### Full model
## Level 1 residuals
hist(full_grams["residual"][["lev_1_resi_est_Intercept"]])
# Heteroskedasticity of residuals
plot(full_grams["residual"][["lev_1_resi_est_Intercept"]])
abline(h = 0, lty = 2, col = "red")
# QQ plot
qqnorm(full_grams["residual"][["lev_1_resi_est_Intercept"]])
qqline(full_grams["residual"][["lev_1_resi_est_Intercept"]], col = "steelblue", lwd = 2)
## Level 2 residuals
hist(full_grams["residual"][["lev_2_resi_est_Intercept"]])
# Heteroskedasticity of residuals
plot(full_grams["residual"][["lev_2_resi_est_Intercept"]])
abline(h = 0, lty = 2, col = "red")
# QQ plot
qqnorm(full_grams["residual"][["lev_2_resi_est_Intercept"]])
qqline(full_grams["residual"][["lev_2_resi_est_Intercept"]], col = "steelblue", lwd = 2)


##### PRODUCE A TABLE OF MODEL COEFFICIENTS 
# comparing the null and full models

coefs_null <- getSummary(null_grams)
coefs_null <- as.data.frame(coefs_null[["coef"]])
coefs_null <- round(coefs_null, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_null) <- c("intercept_FE_1","Year 2001", "Year 2002", "Year 2003", "Year 2004",
                          "Year 2005", "Year 2006", "Year 2007", "Year 2008", "Year 2009",
                          "Year 2010", "Year 2011", "Year 2012", "Year 2013", "Year 2014",
                          "Year 2015", "Year 2016", "Year 2017", "Year 2018",
                          "strata_RE_1","individuals_RE_1")

coefs_full <- getSummary(full_grams)
coefs_full <- as.data.frame(coefs_full[["coef"]])
coefs_full <- round(coefs_full, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_full) <- c("intercept_FE_2","Year 2001", "Year 2002", "Year 2003", 
                          "Year 2004", "Year 2005", "Year 2006", "Year 2007", "Year 2008", 
                          "Year 2009","Year 2010", "Year 2011", "Year 2012", "Year 2013", 
                          "Year 2014", "Year 2015", "Year 2016", "Year 2017", "Year 2018",
                          "female","age 25-59", "age 60+",
                          "Hispanic", "Black", "Asian", "Multiple race", "AI/AN",
                          "Some college", "4+ years college", 
                          "RP2_var_intercept", "RP1_var_intercept")

coefs_table <- rbind(coefs_null, coefs_full)
saveRDS(coefs_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/model coefficients and variance_grams_drinkers_no_outliers.rds")
write.csv(coefs_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/model coefficients and variance_grams_drinkers_no_outliers.csv")

##### CALCULATE VPC AND PCV (from the parameter point estimates)
VPC_grams_null <- null_grams["RP"][["RP2_var_Intercept"]]/(null_grams["RP"][["RP1_var_Intercept"]] + null_grams["RP"][["RP2_var_Intercept"]])
VPC_grams_full <- full_grams["RP"][["RP2_var_Intercept"]]/(full_grams["RP"][["RP1_var_Intercept"]] + full_grams["RP"][["RP2_var_Intercept"]])
VPC_table <- data.frame(Model = c("null", "main effects"),
                        VPC = c(VPC_grams_null, VPC_grams_full))
write.csv(VPC_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/VPC_table_grams_drinkers_no_outliers.csv")

##### Extract data from relevant slots of s4 object (based upon full model)

# Add intersectional group sizes as important indicator of expected level of shrinkage
model_data <- model_data %>% 
  group_by(intersections) %>%
  mutate(count=n())

# data frame
data <- full_grams@data
intersections <- distinct(data, intersections, .keep_all = TRUE)

# Estimates of fixed effects
fixed_effects <- full_grams@FP
fixed_effects <- as.data.frame(fixed_effects)

# Estimates of random effects
random_effects <- full_grams@RP
random_effects <- as.data.frame(random_effects)


##### PREPARE FIXED-PART PAREMETER CHAINS 
# Store the constant and estimated coef for each variable, for each iteration (100 iterations)

chains <- full_grams@chains
chains <- as.data.frame(chains)
mb_prepped <- chains %>% dplyr::select(-c(deviance, RP2_var_Intercept, RP1_var_Intercept))

mb_prepped <- dplyr::rename(mb_prepped,
                            b_cons = "FP_Intercept",
                            b_female = "FP_SEXFemale",
                            b_adult = "FP_age_diaz25-59",
                            b_older_adult = "FP_age_diaz60+",
                            b_Black = "FP_race_6_catsBlack",
                            b_Asian = "FP_race_6_catsAsian",
                            b_AI_AN = "FP_race_6_catsAI/AN",
                            b_Hispanic = "FP_race_6_catsHispanic White",
                            b_Multiple_race = "FP_race_6_catsMultiple race",
                            b_med = "FP_education_3_catssome college",
                            b_high = "FP_education_3_cats4+ years college",
                            b_2001 = "FP_YEAR2001",
                            b_2002 = "FP_YEAR2002",
                            b_2003 = "FP_YEAR2003",
                            b_2004 = "FP_YEAR2004",
                            b_2005 = "FP_YEAR2005",
                            b_2006 = "FP_YEAR2006",
                            b_2007 = "FP_YEAR2007",
                            b_2008 = "FP_YEAR2008",
                            b_2009 = "FP_YEAR2009",
                            b_2010 = "FP_YEAR2010",
                            b_2011 = "FP_YEAR2011",
                            b_2012 = "FP_YEAR2012",
                            b_2013 = "FP_YEAR2013",
                            b_2014 = "FP_YEAR2014",
                            b_2015 = "FP_YEAR2015",
                            b_2016 = "FP_YEAR2016",
                            b_2017 = "FP_YEAR2017",
                            b_2018 = "FP_YEAR2018")

mb_prepped$iteration <- rep(c(1:100))

##### PREPARE intersections RANDOM EFFECTS CHAINS
# Store the value of the random effect, for each intersectional group, for each iteration

# extract the residual chains
resi_chains_lev_2 <- full_grams@resi.chains$resi_lev2
resi_chains_lev_2 <- as.data.frame(resi_chains_lev_2)

# reformat
mu_prepped <- resi_chains_lev_2
mu_prepped$iteration <- 1:nrow(mu_prepped)
mu_prepped <- pivot_longer(resi_chains_lev_2, u_0_1:u_0_108)
mu_prepped$iteration <- rep(c(1:100), each = 108)

# Generate a table with the intersectional groups to estimate for (i.e., year set to 2009 for all)
# Convert all years to 0s, except for the "YEAR2009" column
intersections_2009 <- intersections %>%
  mutate_at(vars(starts_with("YEAR")), ~ 0) %>%
  mutate(YEAR2009 = 1)

##### MERGE DATA, FIXED-PART PARAMETER AND RANDOM EFFECT CHAINS TOGETHER
mdata_prepped <- inner_join(mb_prepped, mu_prepped, by = 'iteration')
mdata_prepped$name <- str_sub(mdata_prepped$name, 5)
mdata_prepped$name <- as.numeric(mdata_prepped$name)
mdata_prepped <- dplyr::rename(mdata_prepped, intersections = name, u = value)
mdata_prepped <- inner_join(mdata_prepped, intersections_2009, by = 'intersections')


##### CALCULATE VALUES OF INTEREST (est = estA + estI)

mdata_prepped <- mdata_prepped %>% mutate(
  est = exp(b_cons*Intercept
            + b_female*SEXFemale
            + b_adult*`age_diaz25-59`
            + b_older_adult*`age_diaz60+`  
            + b_Hispanic*`race_6_catsHispanic White`
            + b_Asian*`race_6_catsAsian`
            + b_AI_AN*`race_6_catsAI/AN`
            + b_Black*`race_6_catsBlack`
            + b_Multiple_race*`race_6_catsMultiple race`
            + b_med*`education_3_catssome college`
            + b_high*`education_3_cats4+ years college`
            + b_2009*`YEAR2009`
            + u)
)

mdata_prepped <- mdata_prepped %>% mutate(
  estA = exp(b_cons*Intercept
             + b_female*SEXFemale
             + b_adult*`age_diaz25-59`
             + b_older_adult*`age_diaz60+`  
             + b_Hispanic*`race_6_catsHispanic White`
             + b_Asian*`race_6_catsAsian`
             + b_AI_AN*`race_6_catsAI/AN`
             + b_Black*`race_6_catsBlack`
             + b_Multiple_race*`race_6_catsMultiple race`
             + b_med*`education_3_catssome college`
             + b_high*`education_3_cats4+ years college`
             + b_2009*`YEAR2009`
  )
)

# Grams attributable to interaction calculated as the difference between est and estA
mdata_prepped <- mdata_prepped %>% 
  mutate(estI = est - estA)

# Calculate the mean, 2.5th and 97.5th percentiles of the MCMC chains
mdata_prepped <- mdata_prepped %>% 
  group_by(intersections) %>%
  mutate(estmn = mean(est),
         estlo = quantile(est,.25),
         esthi = quantile(est,.75),
         estAmn = mean(estA),
         estAlo = quantile(estA,.25),
         estAhi = quantile(estA,.75),
         estImn = mean(estI),
         estIlo = quantile(estI,.25),
         estIhi = quantile(estI,.75))

# Drop chains and just keep their summaries (mean, 2.5th and 97.5th)
mdata_results <- mdata_prepped %>%
  dplyr::select(-"iteration", -"est",  -"estA", -"estI", -contains(c("b_", "u_" ))) %>%
  distinct(intersections, .keep_all=TRUE)

# Merge with intersectional names reference table
mdata_results <- inner_join(mdata_results, intersections_reference)

# save results
saveRDS(mdata_results, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/mdata_results_grams_drinkers_2009_no_outliers.rds")
write.csv(mdata_results, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/mdata_results_grams_drinkers_2009_no_outliers.csv")


##### SUMMARY RESULTS TABLES
mdata_results <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/mdata_results_grams_drinkers_2009_no_outliers.rds")

# Summarise intersectional groups with the highest and lowest estimated grams
mdata_max_5_overall <- mdata_results %>% ungroup %>% slice_max(estmn, n = 5) %>% 
  dplyr::select(intersectional_names, estmn, estlo, esthi, estAmn, estAlo, estAhi, estImn, estIlo, estIhi, mean_observed_grams)
mdata_min_5_overall <- mdata_results %>% ungroup %>% slice_min(estmn, n = 5) %>% 
  dplyr::select(intersectional_names, estmn, estlo, esthi, estAmn, estAlo, estAhi, estImn, estIlo, estIhi, mean_observed_grams)
mdata_overall <- rbind(mdata_max_5_overall, mdata_min_5_overall)

write.csv(mdata_overall, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/mdata_5_estimates_drinkers_2009_no_outliers.csv")

# Summarise which intersectional groups have the largest differences in grams estimates,
# when comparing additive only estimates vs estimates which include interaction effects
mdata_max_5_interactions <- mdata_results %>% ungroup %>% slice_max(estImn, n = 5) %>% 
  dplyr::select(intersectional_names, estmn, estlo, esthi, estAmn, estAlo, estAhi, estImn, estIlo, estIhi)
mdata_min_5_interactions <- mdata_results %>% ungroup %>% slice_min(estImn, n = 5) %>% 
  dplyr::select(intersectional_names, estmn, estlo, esthi, estAmn, estAlo, estAhi, estImn, estIlo, estIhi)
mdata_interactions <- rbind(mdata_max_5_interactions, mdata_min_5_interactions)

write.csv(mdata_interactions, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/mdata_5_interactions_drinkers_2009_no_outliers_2.csv")

##### Explore face validity of estimates
temp <- mdata_results %>% dplyr::select(intersectional_names, mean_observed_grams, estmn) 
write.csv(temp, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/drinkers clean/Table of mean observed vs estimated grams - drinkers only.csv")
ggplot(temp, aes(x=mean_observed_grams, y=estmn)) + geom_point() + xlim(0,40) + ylim(0,40) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Comparisson of observed and estimated daily grams, 180 intersectional groups")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/new spec August 2023/grams/observed vs estimated grams_drinkers_2009_no_outliers_2.png", 
       dpi=300, width=33, height=19, units="cm")
# Interpretation: All but one estimates are lower than observed. 

# Compare the ranking of intersectional groups based on observed and estimated grams
temp$rank_observed_grams <-rank(temp$mean_observed_grams)
temp$rank_estimated_grams <-rank(temp$estmn)
ggplot(temp, aes(x=rank_observed_grams, y=rank_estimated_grams)) + geom_point() + xlim(0,40) + ylim(0,40) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Comparisson of observed vs estimated drinking 'rank', 180 intersectional groups")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/new spec August 2023/grams/observed rank vs estimated rank grams_drinkers_2009_no_outliers.png", 
       dpi=300, width=33, height=19, units="cm")

# Interpretation: Fairly linear positive correlation between the observed and the estimated rankings