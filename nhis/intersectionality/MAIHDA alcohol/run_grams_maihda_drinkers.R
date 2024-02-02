##############################################################################
# MAIHDA of grams of alcohol per day - DRINKERS
##############################################################################

######################################################################## Set-up
setwd("C:/Users/cmp21seb/Documents/SIMAH/")
code <- "SIMAH_code/nhis/intersectionality/MAIHDA alcohol/"
inputs <- "SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/inputs/"
models <- "SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/models/"
outputs <- "SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/outputs/"

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
source(paste0(code, "functions/recode_race_ethnicity.R"))
options(MLwiN_path="C:/Program Files/MLwiN v3.05/")
options(scipen=10)

################################################################# PRE PROCESSING

# Read in data (full sample):
data <- readRDS(paste0(inputs,"nhis_alc_clean_full_sample.RDS"))

# subset drinkers
data_drinkers <- data %>% filter(ALCSTAT1=="Current drinker")

# Drop individuals age <21
data_0 <- data_drinkers %>% filter(age_diaz!="18-20")

# Keep only the 6 selected race and ethnicity groups
data_1 <- data_0 %>% filter(!is.na(race_6_cats))

# Generate intersections
data_2 <- data_1 %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_diaz) %>% 
  mutate(intersections = cur_group_id()) %>%
  mutate(intersectional_names = as.character(paste(SEX, age_diaz, race_6_cats, education_3_cats)))
  
# Check intersectional group sizes
temp <- data_2 %>% 
  group_by(intersections) %>%
  mutate(count=n())
group_sizes <- temp %>% distinct(intersections, count)
sum(group_sizes$count <= 20) # 3 groups with n<=20

# Add a column of the observed mean grams per day for each intersection
data_3 <- data_2 %>%
  group_by(intersections) %>%
  mutate(mean_observed_grams = mean(alc_daily_g_capped_200))

# Calculate overall mean grams for the sample of drinkers
mean(data_3$alc_daily_g_capped_200)
sd(data_3$alc_daily_g_capped_200)

# Generate reference table with intersectional names & mean observed grams per intersection
intersections_reference <- data_3 %>%
  group_by(intersectional_names) %>% 
  distinct(intersections, intersectional_names, mean_observed_grams)

# Generate reference table with intersectional names & mean observed grams for the year 2009 only
intersections_reference_2009 <- data_3 %>%
  filter(YEAR==2009) %>%
  group_by(intersections) %>%
  mutate(count_2009=n(),
         mean_observed_grams_2009 = mean(alc_daily_g_capped_200))%>% 
  distinct(intersections, intersectional_names, count_2009, mean_observed_grams_2009)

# Prep data for use with Mlwin
model_data <- data_3 %>%
  mutate(cons=1) %>% 
  arrange(intersections, NHISPID)

model_data$age_diaz <- droplevels(model_data$age_diaz)
model_data$YEAR <- as.factor(model_data$YEAR)

# Save
saveRDS(model_data, paste0(inputs, "grams_data_pre_maihda_drinkers.rds"))

#################################################################### MODELLING

# Read in prepped data
model_data <- readRDS(paste0(inputs, "grams_data_pre_maihda_drinkers.rds"))

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
saveRDS(null_grams, paste0(models, "null_grams_drinkers.rds"))
saveRDS(full_grams, paste0(models, "full_grams_drinkers.rds"))

# Check convergence achieved
summary(full_grams@chains[, "FP_Intercept"])
mcmc_trace(full_grams@chains)

##################################################################### ANALYSIS

# Read in the model objects
null_grams <- readRDS(paste0(models, "null_grams_drinkers.rds"))
full_grams <- readRDS(paste0(models, "full_grams_drinkers.rds"))

##### CHECK MODELLING ASSUMPTIONS

## Null model, level 1 residuals
hist(null_grams["residual"][["lev_1_resi_est_Intercept"]])
# Heteroskedasticity of residuals
plot(null_grams["residual"][["lev_1_resi_est_Intercept"]])
abline(h = 0, lty = 2, col = "red")
# QQ plot
qqnorm(null_grams["residual"][["lev_1_resi_est_Intercept"]])
qqline(null_grams["residual"][["lev_1_resi_est_Intercept"]], col = "steelblue", lwd = 2)
## Null model, level 2 residuals
hist(null_grams["residual"][["lev_2_resi_est_Intercept"]])
# Heteroskedasticity of residuals
plot(null_grams["residual"][["lev_2_resi_est_Intercept"]])
abline(h = 0, lty = 2, col = "red")
# QQ plot
qqnorm(null_grams["residual"][["lev_2_resi_est_Intercept"]])
qqline(null_grams["residual"][["lev_2_resi_est_Intercept"]], col = "steelblue", lwd = 2)

### Full model, level 1 residuals
hist(full_grams["residual"][["lev_1_resi_est_Intercept"]])
# Heteroskedasticity of residuals
plot(full_grams["residual"][["lev_1_resi_est_Intercept"]])
abline(h = 0, lty = 2, col = "red")
# QQ plot
qqnorm(full_grams["residual"][["lev_1_resi_est_Intercept"]])
qqline(full_grams["residual"][["lev_1_resi_est_Intercept"]], col = "steelblue", lwd = 2)
## Full model, level 2 residuals
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
saveRDS(coefs_table, paste0(outputs, "grams drinkers/model coefficients and variance_grams_drinkers.rds"))
write.csv(coefs_table, paste0(outputs, "grams drinkers/model coefficients and variance_grams_drinkers.csv"))

##### CALCULATE VPC AND PCV (from the parameter point estimates)
VPC_grams_null <- null_grams["RP"][["RP2_var_Intercept"]]/(null_grams["RP"][["RP1_var_Intercept"]] + null_grams["RP"][["RP2_var_Intercept"]])
VPC_grams_full <- full_grams["RP"][["RP2_var_Intercept"]]/(full_grams["RP"][["RP1_var_Intercept"]] + full_grams["RP"][["RP2_var_Intercept"]])
VPC_table <- data.frame(Model = c("null", "main effects"),
                        VPC = c(VPC_grams_null, VPC_grams_full))
write.csv(VPC_table, paste0(outputs, "grams drinkers/VPC_table_grams_drinkers.csv"))

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
                            b_Black = "FP_race_6_catsNH Black",
                            b_Asian = "FP_race_6_catsNH Asian",
                            b_AI_AN = "FP_race_6_catsNH AI/AN",
                            b_Hispanic = "FP_race_6_catsHispanic",
                            b_Multiple_race = "FP_race_6_catsNH Multiple race",
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

RP1_var_Intercept <- random_effects[rownames(random_effects) == "RP1_var_Intercept", "random_effects"]
constant <- exp(0.5*RP1_var_Intercept)

# Estimates including both additive and interaction effects:
mdata_prepped <- mdata_prepped %>% mutate(
  est = exp(b_cons*Intercept
                    + b_female*SEXFemale
                    + b_adult*`age_diaz25-59`
                    + b_older_adult*`age_diaz60+`  
                    + b_Hispanic*`race_6_catsHispanic`
                    + b_Asian*`race_6_catsNH Asian`
                    + b_AI_AN*`race_6_catsNH AI/AN`
                    + b_Black*`race_6_catsNH Black`
                    + b_Multiple_race*`race_6_catsNH Multiple race`
                    + b_med*`education_3_catssome college`
                    + b_high*`education_3_cats4+ years college`
                    + b_2009*`YEAR2009`
                    + u)*constant
)

# Estimates including additive effects only:
mdata_prepped <- mdata_prepped %>% mutate(
  estA = exp(b_cons*Intercept
                           + b_female*SEXFemale
                           + b_adult*`age_diaz25-59`
                           + b_older_adult*`age_diaz60+`  
                           + b_Hispanic*`race_6_catsHispanic`
                           + b_Asian*`race_6_catsNH Asian`
                           + b_AI_AN*`race_6_catsNH AI/AN`
                           + b_Black*`race_6_catsNH Black`
                           + b_Multiple_race*`race_6_catsNH Multiple race`
                           + b_med*`education_3_catssome college`
                           + b_high*`education_3_cats4+ years college`
                           + b_2009*`YEAR2009`)*constant
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

# Merge with intersectional names reference table & group sizes
mdata_results <- inner_join(mdata_results, intersections_reference)
mdata_results <- inner_join(mdata_results, group_sizes)

# Select only the important variables
mdata_results <- mdata_results %>% 
  dplyr::select(intersectional_names, count, mean_observed_grams, 
                estmn, estlo, esthi,
                estAmn, estAlo, estAhi,
                estImn, estIlo, estIhi) %>% 
  mutate_if(is.numeric, round, 1)

# Merge with intersectional names reference table
mdata_results <- inner_join(mdata_results, intersections_reference)

# save results
saveRDS(mdata_results, paste0(outputs, "grams drinkers/results_grams_drinkers.rds"))
write.csv(mdata_results, paste0(outputs, "grams drinkers/results_grams_drinkers.csv"))

##### SUMMARY RESULTS TABLES
mdata_results <- readRDS(paste0(outputs, "grams drinkers/results_grams_drinkers.rds"))

# Summarise intersectional groups with the highest and lowest estimated grams
mdata_max_5_overall <- mdata_results %>% ungroup %>% slice_max(estmn, n = 5) 
mdata_min_5_overall <- mdata_results %>% ungroup %>% slice_min(estmn, n = 5)
mdata_overall <- rbind(mdata_max_5_overall, mdata_min_5_overall)

write.csv(mdata_overall, paste0(outputs, "grams drinkers/mdata_5_estimates_drinkers.csv"))

# Summarise which intersectional groups have the largest differences in grams estimates,
# when comparing additive only estimates vs estimates which include interaction effects
mdata_max_5_interactions <- mdata_results %>% ungroup %>% slice_max(estImn, n = 5) 
mdata_min_5_interactions <- mdata_results %>% ungroup %>% slice_min(estImn, n = 5)  
mdata_interactions <- rbind(mdata_max_5_interactions, mdata_min_5_interactions)
write.csv(mdata_interactions, paste0(outputs, "grams drinkers/mdata_5_interactions_drinkers.csv"))

##### Explore face validity of estimates

# Compare mean observed (overall) and estimated in a table
temp <- mdata_results %>% dplyr::select(intersectional_names, mean_observed_grams, estmn) %>%
  mutate(difference = estmn - mean_observed_grams,
         abs_difference = abs(difference),
         percent_difference = abs(difference/estmn*100))
write.csv(temp, paste0(outputs, "grams drinkers/mean observed vs estimated grams - drinkers.csv"))

# Compare mean observed (2009 observed only) and estimated in a table
temp_2009 <- mdata_results %>% left_join(., intersections_reference_2009) %>%
  dplyr::select(intersectional_names, count_2009, mean_observed_grams_2009, estmn) %>%
  mutate(difference = estmn - mean_observed_grams_2009,
         abs_difference = abs(difference))

# Compare observed vs estimated grams:  
# 1a) on log scale, observed data from all years
temp_log <- mdata_results %>% left_join(., intersections_reference_2009) %>%
  mutate(log_observed_mean_all_years = log(mean_observed_grams),
         log_observed_mean_2009 = log(mean_observed_grams_2009),
         log_estimated_mean_2009 = log(estmn),
         difference = log_estimated_mean_2009 - log_observed_mean_all_years,
         abs_difference = abs(difference),
         difference_2009 = log_estimated_mean_2009 - log_observed_mean_2009,
         abs_difference_2009 = abs(difference_2009))

ggplot(temp_log, aes(x=log_observed_mean_all_years, y=log_estimated_mean_2009)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Comparisson of log observed (all years) and estimated (2009) daily grams, 108 intersectional groups") +
  xlim(range(c(temp_log$log_observed_mean_all_years, temp_log$log_estimated_mean_2009))) +
  ylim(range(c(temp_log$log_observed_mean_all_years, temp_log$log_estimated_mean_2009)))

# 1a) on log scale, observed data from 2009
ggplot(temp_log, aes(x=log_observed_mean_2009, y=log_estimated_mean_2009)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  ggtitle("Comparisson of log observed (2009) and estimated (2009) daily grams, 108 intersectional groups") +
  xlim(c(-4, 4)) +
  ylim(c(-4, 4))

# 2a) as residuals plot (plot of difference between observed & estimate)
ggplot(temp, aes(x=intersectional_names, y=difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = NULL, y = "Estimated minus observed grams") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylim(-30,30) +
  ggtitle("Difference between mean observed (all years) and estimated (2009) grams - corrected eij")

# 2b) as residuals plot (plot of difference between observed & estimate, 2009 only)
ggplot(temp_2009, aes(x=intersectional_names, y=difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = NULL, y = "Estimated minus observed grams") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylim(-30,30) +
  ggtitle("Difference between mean observed (2009) and estimated (2009) grams - corrected eij")

# 2c) as residuals plot on log scale (plot of difference between observed & estimate)
ggplot(temp_log, aes(x=intersectional_names, y=difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = NULL, y = "Estimated minus observed grams") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylim(-30,30) +
  ggtitle("Difference between mean observed (all years) and estimated (2009) grams - corrected eij - on log scale")
ggsave(paste0(outputs, "grams drinkers/observed(all years) vs estimated(2009), drinkers, log scale.png"), 
       dpi=300, width=33, height=19, units="cm")

# 2d) as residuals plot on log scale (plot of difference between observed & estimate, 2009 v 2009)
ggplot(temp_log, aes(x=intersectional_names, y=difference_2009)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = NULL, y = "Estimated minus observed grams") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylim(-30,30) +
  ggtitle("Difference between mean observed (2009) and estimated (2009) grams - corrected eij - on log scale")

# 3a) difference versus group size
ggplot(temp, aes(x=count, y=abs_difference)) + 
  geom_point() + 
  labs(x = "group size", y = "Difference between observed and estimated (absolute)") +
  ggtitle("Absolute difference between mean observed (all years) and estimated (2009) grams")

# 3b) difference versus group size on log scale
ggplot(temp_log, aes(x=count, y=abs_difference)) + 
  geom_point() + 
  labs(x = "group size", y = "Absolute difference") +
  ggtitle("Difference between mean observed (all years) and estimated (2009) grams, log scale")

# 3c) difference versus group size on log scale, 2009
ggplot(temp_log, aes(x=count, y=abs_difference_2009)) + 
  geom_point() + 
  labs(x = "group size", y = "Absolute difference") +
  ggtitle("Difference between mean observed (2009) and estimated (2009) grams, log scale")

# 3d) percent difference (percentage of total estimated grams) versus group size
ggplot(temp, aes(x=count, y=percent_difference)) + 
  geom_point() + 
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "group size", y = "percent difference") +
  ggtitle("Difference between mean observed and estimated grams, as a percentage of estimate")

# Test correlation between group size and differences
cor(temp$count, temp$abs_difference) # 0.04 i.e., no correlation
cor(temp_2009$count_2009, temp_2009$abs_difference, use = "pairwise.complete.obs") # -0.1, some slight negative correlation
cor(temp_log$count, temp_log$abs_difference_2009, use = "pairwise.complete.obs") # - 0.1, some slight negative correlation

# 4) Histogram of the differences - would expect this to be positively skewed as the observed and predicted means are also positively skewed
hist(temp$mean_observed_grams)
moments::skewness(temp$mean_observed_grams) # 2.2 (skewness > 0 indicates positive skew)
hist(temp$estmn)
moments::skewness(temp$estmn) # 1.2
hist(temp$difference)
moments::skewness(temp$difference) # 1.5
mean(temp$difference) # 3
hist(temp_log$difference)
moments::skewness(temp_log$difference) # 2
mean(temp_log$difference) # 0.3



