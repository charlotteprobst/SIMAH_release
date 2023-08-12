##### Script to run the logistic MAIHDA and separate results into additive and interaction effects 
# Replicating the method of analysis undertaken in:
# Axelsson Fisk, S., Mulinari, S., Wemrell, M., Leckie, G., Perez Vicente, R., Merlo, J. Chronic Obstructive Pulmonary Disease in Sweden: an intersectional multilevel analysis of individual heterogeneity and discriminatory accuracy

# nb. currently not controlling for YEAR.  Plan to do this by introducing YEAR dummy variables.
# trial changing age category

library(tidyverse)
library(stringr)
library(R2MLwiN)
library(boot)
library(memisc)

# Read in the processed data ready for modelling

# Full sample
model_data <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/new spec August 2023/hed_data_full_sample.RDS")

##### RUN THE MAIHDA MODELS

# Prep data for use with Mlwin
model_data <- model_data %>%
  mutate(cons=1) %>% 
  arrange(intersections, NHISPID)

# null model
(null_HED <- runMLwiN(logit(HED) ~ 1 + (1|intersections), 
                      D = "Binomial", data = model_data,
                      estoptions=list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                      mcmcMeth = list(burnin = 5000,
                                                      thinning = 50,
                                                      resi.store=TRUE))))                                           


# full model
(full_HED <- runMLwiN(logit(HED) ~ 1 + SEX + age_3_cats + race_6_cats + education_3_cats 
                             + (1|intersections), 
                             D = "Binomial", data = model_data,
                             estoptions=list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                        mcmcMeth = list(burnin = 5000,
                                                        thinning = 50,
                                                        resi.store=TRUE))))                                           


# save the model objects
saveRDS(null_HED, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/null_HED.rds")
saveRDS(full_HED, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/full_HED.rds")

##### PRODUCE A TABLE OF MODEL COEFFICIENTS 
# comparing the null and full models
 
coefs_null <- getSummary(null_HED)
coefs_null <- as.data.frame(coefs_null[["coef"]])
coefs_null <- round(coefs_null, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_null) <- c("intercept_FE_1","strata_RE_1","individuals_RE_1")
 
coefs_full <- getSummary(full_HED)
coefs_full <- as.data.frame(coefs_full[["coef"]])
coefs_full <- round(coefs_full, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_full) <- c("intercept_FE_2","female","age 25-69", "age 70+",
                                 "Hispanic", "Black", "Asian", "Multiple race", "AI/AN",
                                 "Some college", "4+ years college",
                                 "strata_RE_2", "RP1_var_bcons_1")

coefs_table <- rbind(coefs_null, coefs_full)
saveRDS(coefs_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/HED model coefficients and variance.rds")


##### CALCULATE VPC AND PCV (from the parameter point estimates)
VPC_HED_null <- print(VPC <- null_HED["RP"][["RP2_var_Intercept"]]/(pi^2/3 + null_HED["RP"][["RP2_var_Intercept"]]))
VPC_full_HED <- print(VPC <- full_HED["RP"][["RP2_var_Intercept"]]/(pi^2/3 + full_HED["RP"][["RP2_var_Intercept"]]))
VPC_table <- data.frame(Model = c("null", "main effects"),
                         VPC = c(VPC_HED_null, VPC_full_HED))
saveRDS(VPC_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/hed_VPC_table_drinkers.Rmd")

##### Extract data from relevant slots of s4 object (based upon full model)

# data frame
data <- full_HED@data
intersections <- distinct(data, intersections, .keep_all = TRUE)
intersections <- intersections %>% dplyr::select(-c(HED, "l1id", "_denom"))

# Estimates of fixed effects
fixed_effects <- full_HED@FP
fixed_effects <- as.data.frame(fixed_effects)

# Estimates of random effects
random_effects <- full_HED@RP
random_effects <- as.data.frame(random_effects)

##### PREPARE FIXED-PART PAREMETER CHAINS 
# Store the constant and estimated coef for each variable, for each iteration (100 iterations)

chains <- full_HED@chains
chains <- as.data.frame(chains)
mb_prepped <- chains %>% dplyr::select(-c(deviance, RP2_var_Intercept, RP1_var_bcons_1))

mb_prepped <- dplyr::rename(mb_prepped,
                      b_cons = "FP_Intercept",
                      b_female = "FP_SEXFemale",
                      b_adult = "FP_age_3_cats25-69",
                      b_older_adult = "FP_age_3_cats70+",
                      b_Black = "FP_race_6_catsBlack",
                      b_Asian = "FP_race_6_catsAsian",
                      b_AI_AN = "FP_race_6_catsAI/AN",
                      b_Hispanic = "FP_race_6_catsHispanic White",
                      b_Multiple_race = "FP_race_6_catsMultiple race",
                      b_med = "FP_education_3_catssome college",
                      b_high = "FP_education_3_cats4+ years college")

mb_prepped$iteration <- rep(c(1:100))

##### PREPARE intersections RANDOM EFFECTS CHAINS
# Store the value of the random effect, for each intersectional group, for each iteration

# extract the residual chains
resi_chains_lev_2 <- full_HED@resi.chains$resi_lev2
resi_chains_lev_2 <- as.data.frame(resi_chains_lev_2)

# reformat
mu_prepped <- resi_chains_lev_2
mu_prepped$iteration <- 1:nrow(mu_prepped)
mu_prepped <- pivot_longer(resi_chains_lev_2, u_0_1:u_0_108)
mu_prepped$iteration <- rep(c(1:100), each = 108)

##### MERGE DATA, FIXED-PART PARAMETER AND RANDOM EFFECT CHAINS TOGETHER
mdata_prepped <- inner_join(mb_prepped, mu_prepped, by = 'iteration')
mdata_prepped$name <- str_sub(mdata_prepped$name, 5)
mdata_prepped$name <- as.numeric(mdata_prepped$name)
mdata_prepped <- dplyr::rename(mdata_prepped, intersections = name, u = value)
mdata_prepped <- inner_join(mdata_prepped, intersections, by = 'intersections')

##### CALCULATE PERCENTAGES OF INTEREST (p = pA + pB)

# Percentage p based on fixed AND random part

mdata_prepped <- mdata_prepped %>% mutate(
  p = 100*inv.logit(b_cons*Intercept
                    + b_female*SEXFemale
                    + b_adult*`age_3_cats25-69`
                    + b_older_adult*`age_3_cats70+`  
                    + b_Hispanic*`race_6_catsHispanic White`
                    + b_Asian*`race_6_catsAsian`
                    + b_AI_AN*`race_6_catsAI/AN`
                    + b_Black*`race_6_catsBlack`
                    + b_Multiple_race*`race_6_catsMultiple race`
                    + b_med*`education_3_catssome college`
                    + b_high*`education_3_cats4+ years college`
                    + u)
)

# # Percentage pA based only on the fixed-part
mdata_prepped <- mdata_prepped %>% mutate(
   pA = 100*inv.logit(b_cons*Intercept
                    + b_female*SEXFemale
                    + b_adult*`age_3_cats25-69`
                    + b_older_adult*`age_3_cats70+`  
                    + b_Hispanic*`race_6_catsHispanic White`
                    + b_Asian*`race_6_catsAsian`
                    + b_AI_AN*`race_6_catsAI/AN`
                    + b_Black*`race_6_catsBlack`
                    + b_Multiple_race*`race_6_catsMultiple race`
                    + b_med*`education_3_catssome college`
                    + b_high*`education_3_cats4+ years college`)
)

# # Percentage calculated as the difference between p and pA
mdata_prepped <- mdata_prepped %>% mutate(
   pB = p - pA)

# Calculate the mean, 2.5th and 97.5th percentiles of the MCMC chains
 mdata_prepped <- mdata_prepped %>% 
   group_by(intersections) %>%
   mutate(pmn = mean(p),
          plo = quantile(p,.25),
          phi = quantile(p,.75),
          pAmn = mean(pA),
          pAlo = quantile(pA,.25),
          pAhi = quantile(pA,.75),
          pBmn = mean(pB),
          pBlo = quantile(pB,.25),
          pBhi = quantile(pB,.75))

# Drop chains and just keep their summaries (mean, 2.5th and 97.5th)
mdata_results <- mdata_prepped %>%
  dplyr::select(-"iteration", -"p",  -"pA", -contains(c("b_", "u_" ))) %>%
  distinct(intersections, .keep_all=TRUE)

# save results
saveRDS(mdata_results, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/mdata_results.rds")

################################################################################
# SENSITIVTY ANALYSIS 1 - REPEAT ANALYSIS WITH DRINKERS ONLY

# Drinkers only
model_data_drinkers <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/new spec August 2023/hed_data_drinkers_only.RDS")

# Prep data for use with Mlwin
model_data_drinkers <- model_data_drinkers %>%
  mutate(cons=1) %>%
  arrange(intersections, NHISPID)

# null model
(null_HED_drinkers <- runMLwiN(logit(HED) ~ 1 + (1|intersections),
                      D = "Binomial", data = model_data_drinkers,
                      estoptions=list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                      mcmcMeth = list(burnin = 5000,
                                                      thinning = 50,
                                                      resi.store=TRUE))))


# full model
(full_HED_drinkers <- runMLwiN(logit(HED) ~ 1 + SEX + age_3_cats + race_6_cats + education_3_cats
                      + (1|intersections),
                      D = "Binomial", data = model_data_drinkers,
                      estoptions=list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                      mcmcMeth = list(burnin = 5000,
                                                      thinning = 50,
                                                      resi.store=TRUE))))


# save the model objects
saveRDS(null_HED_drinkers, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/null_HED_drinkers.rds")
saveRDS(full_HED_drinkers, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/full_HED_drinkers.rds")

coefs_null_drinkers <- getSummary(null_HED_drinkers)
coefs_null_drinkers <- as.data.frame(coefs_null_drinkers[["coef"]])
coefs_null_drinkers <- round(coefs_null_drinkers, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_null_drinkers) <- c("intercept_FE_1","strata_RE_1","individuals_RE_1")

coefs_full_drinkers <- getSummary(full_HED_drinkers)
coefs_full_drinkers <- as.data.frame(coefs_full_drinkers[["coef"]])
coefs_full_drinkers <- round(coefs_full_drinkers, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_full_drinkers) <- c("intercept_FE_2","female","age 25-69", "age 70+",
                          "Hispanic", "Black", "Asian", "Multiple race", "AI/AN",
                          "Some college", "4+ years college",
                          "strata_RE_2", "RP1_var_bcons_1")

coefs_table_drinkers <- rbind(coefs_null_drinkers, coefs_full_drinkers)
saveRDS(coefs_table_drinkers, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/HED model coefficients and variance_drinkers.rds")

##### CALCULATE VPC AND PCV (from the parameter point estimates)
VPC_HED_null_drinkers <- print(VPC <- null_HED_drinkers["RP"][["RP2_var_Intercept"]]/(pi^2/3 + null_HED_drinkers["RP"][["RP2_var_Intercept"]]))
VPC_full_HED_drinkers <- print(VPC <- full_HED_drinkers["RP"][["RP2_var_Intercept"]]/(pi^2/3 + full_HED_drinkers["RP"][["RP2_var_Intercept"]]))
VPC_table_drinkers <- data.frame(Model = c("null", "main effects"),
                         VPC = c(VPC_HED_null_drinkers, VPC_full_HED_drinkers))
saveRDS(VPC_table_drinkers, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/hed_VPC_table_drinkers.Rmd")

##### Extract data from relevant slots of s4 object (based upon full model)

# data frame
data_drinkers <- full_HED_drinkers@data
intersections_drinkers <- distinct(data_drinkers, intersections, .keep_all = TRUE)
intersections_drinkers <- intersections_drinkers %>% dplyr::select(-c(HED, "l1id", "_denom"))

# Estimates of fixed effects
fixed_effects_drinkers <- full_HED_drinkers@FP
fixed_effects_drinkers <- as.data.frame(fixed_effects_drinkers)

# Estimates of random effects
random_effects_drinkers <- full_HED_drinkers@RP
random_effects_drinkers <- as.data.frame(random_effects_drinkers)

##### PREPARE FIXED-PART PAREMETER CHAINS
# Store the constant and estimated coef for each variable, for each iteration (100 iterations)

chains_drinkers <- full_HED_drinkers@chains
chains_drinkers <- as.data.frame(chains_drinkers)
mb_prepped_drinkers <- chains_drinkers %>% dplyr::select(-c(deviance, RP2_var_Intercept, RP1_var_bcons_1))

mb_prepped_drinkers <- dplyr::rename(mb_prepped_drinkers,
                      b_cons = "FP_Intercept",
                      b_female = "FP_SEXFemale",
                      b_adult = "FP_age_3_cats25-69",
                      b_older_adult = "FP_age_3_cats70+",
                      b_Black = "FP_race_6_catsBlack",
                      b_Asian = "FP_race_6_catsAsian",
                      b_AI_AN = "FP_race_6_catsAI/AN",
                      b_Hispanic = "FP_race_6_catsHispanic White",
                      b_Multiple_race = "FP_race_6_catsMultiple race",
                      b_med = "FP_education_3_catssome college",
                      b_high = "FP_education_3_cats4+ years college")

mb_prepped_drinkers$iteration <- rep(c(1:100))

##### PREPARE intersections RANDOM EFFECTS CHAINS
# Store the value of the random effect, for each intersectional group, for each iteration 

# extract the residual chains
resi_chains_lev_2_drinkers <- full_HED_drinkers@resi.chains$resi_lev2
resi_chains_lev_2_drinkers <- as.data.frame(resi_chains_lev_2_drinkers)

# reformat
mu_prepped_drinkers <- resi_chains_lev_2_drinkers
mu_prepped_drinkers$iteration <- 1:nrow(mu_prepped_drinkers)
mu_prepped_drinkers <- pivot_longer(resi_chains_lev_2_drinkers, u_0_1:u_0_108)
mu_prepped_drinkers$iteration <- rep(c(1:100), each = 108)

##### MERGE DATA, FIXED-PART PARAMETER AND RANDOM EFFECT CHAINS TOGETHER
mdata_prepped_drinkers <- inner_join(mb_prepped_drinkers, mu_prepped_drinkers, by = 'iteration')
mdata_prepped_drinkers$name <- str_sub(mdata_prepped_drinkers$name, 5)
mdata_prepped_drinkers$name <- as.numeric(mdata_prepped_drinkers$name)
mdata_prepped_drinkers <- dplyr::rename(mdata_prepped_drinkers, intersections = name, u = value)
mdata_prepped_drinkers <- inner_join(mdata_prepped_drinkers, intersections, by = 'intersections')

##### CALCULATE PERCENTAGES OF INTEREST (p = pA + pB)

# Percentage p based on fixed and random part

mdata_prepped_drinkers <- mdata_prepped_drinkers %>% mutate(
  p = 100*inv.logit(b_cons*Intercept
                    + b_female*SEXFemale
                    + b_adult*`age_3_cats25-69`
                    + b_older_adult*`age_3_cats70+`  
                    + b_Hispanic*`race_6_catsHispanic White`
                    + b_Asian*`race_6_catsAsian`
                    + b_AI_AN*`race_6_catsAI/AN`
                    + b_Black*`race_6_catsBlack`
                    + b_Multiple_race*`race_6_catsMultiple race`
                    + b_med*`education_3_catssome college`
                    + b_high*`education_3_cats4+ years college`
                    + u)
)

# Percentage pA based only on the fixed-part
mdata_prepped_drinkers <- mdata_prepped_drinkers %>% mutate(
  pA = 100*inv.logit(b_cons*Intercept
                     + b_female*SEXFemale
                     + b_adult*`age_3_cats25-69`
                     + b_older_adult*`age_3_cats70+`  
                     + b_Hispanic*`race_6_catsHispanic White`
                     + b_Asian*`race_6_catsAsian`
                     + b_AI_AN*`race_6_catsAI/AN`
                     + b_Black*`race_6_catsBlack`
                     + b_Multiple_race*`race_6_catsMultiple race`
                     + b_med*`education_3_catssome college`
                     + b_high*`education_3_cats4+ years college`)
)

# Percentage calculated as the difference between p and pA
mdata_prepped_drinkers <- mdata_prepped_drinkers %>% mutate(
  pB = p - pA)

# Calculate the mean, 2.5th and 97.5th percentiles of the MCMC chains

mdata_prepped_drinkers <- mdata_prepped_drinkers %>%
  group_by(intersections) %>%
  mutate(pmn = mean(p),
         plo = quantile(p,.25),
         phi = quantile(p,.75),
         pAmn = mean(pA),
         pAlo = quantile(pA,.25),
         pAhi = quantile(pA,.75),
         pBmn = mean(pB),
         pBlo = quantile(pB,.25),
         pBhi = quantile(pB,.75))

# Drop chains and just keep their summaries (mean, 2.5th and 97.5th)
mdata_results_drinkers <- mdata_prepped_drinkers %>%
  dplyr::select(-"iteration", -"p",  -"pA", -contains(c("b_", "u_" ))) %>%
  distinct(intersections, .keep_all=TRUE)

# save results
saveRDS(mdata_results_drinkers, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/mdata_results_drinkers.rds")

################################################################################
# SENSITIVITY ANALYSIS 2 - RUN INCLUDING FIXED EFFECTS FOR YEAR
