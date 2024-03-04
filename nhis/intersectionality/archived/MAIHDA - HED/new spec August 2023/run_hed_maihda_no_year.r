##### Script to run the logistic MAIHDA and separate results into additive and interaction effects 
# Replicating the method of analysis undertaken in:
# Axelsson Fisk, S., Mulinari, S., Wemrell, M., Leckie, G., Perez Vicente, R., Merlo, J. Chronic Obstructive Pulmonary Disease in Sweden: an intersectional multilevel analysis of individual heterogeneity and discriminatory accuracy

# NOT CONTROLLING FOR SURVEY YEAR

# Setup
library(tidyverse)
library(tidyr)
library(dplyr)
library(sjstats)
library(haven)
library(performance)
library(memisc)
library(gt)
library(R2MLwiN)
library(xlsx)
library(stringr)
library(boot)
library(memisc)
library(fastDummies)

options(MLwiN_path="C:/Program Files/MLwiN v3.05/")

setwd("C:/Users/cmp21seb/Documents/SIMAH/")

options(scipen=10)

################################################################# PRE PROCESSING

# Read in the data (same as for main analysis)
model_data <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/new spec August 2023/HED/hed_data_pre_maihda_main.rds")

# Prep data for use with Mlwin
model_data <- model_data %>%
  mutate(cons=1) %>% 
  arrange(intersections, NHISPID)

model_data$age_diaz <- droplevels(model_data$age_diaz)

# Generate reference table with intersectional names
intersections_reference <- model_data %>%
  distinct(intersections, intersectional_names)

##### RUN THE MAIHDA MODELS

# null model
(null_HED <- runMLwiN(logit(HED) ~ 1 + 
                        (1|intersections), 
                      D = "Binomial", data = model_data,
                      estoptions=list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                      mcmcMeth = list(burnin = 5000,
                                                      thinning = 50,
                                                      resi.store=TRUE))))                                           

# full model
(full_HED <- runMLwiN(logit(HED) ~ 1 + SEX + age_diaz + race_6_cats + education_3_cats +
                      (1|intersections), 
                      D = "Binomial", data = model_data,
                      estoptions=list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                      mcmcMeth = list(burnin = 5000,
                                                      thinning = 50,
                                                      resi.store=TRUE))))                                           

# save the model objects
saveRDS(null_HED, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/HED/null_HED_no_year.rds")
saveRDS(full_HED, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/HED/full_HED_no_year.rds")

####################################################################### ANALYSIS
# read in the model objects
null_HED <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/HED/null_HED_no_year.rds")
full_HED <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/HED/full_HED_no_year.rds")

##### PRODUCE A TABLE OF MODEL COEFFICIENTS 
# comparing the null and full models

coefs_null <- getSummary(null_HED)
coefs_null <- as.data.frame(coefs_null[["coef"]])
coefs_null <- round(coefs_null, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_null) <- c("intercept_FE_1","strata_RE_1","individuals_RE_1")

coefs_full <- getSummary(full_HED)
coefs_full <- as.data.frame(coefs_full[["coef"]])
coefs_full <- round(coefs_full, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_full) <- c("intercept_FE_2","female","age 25-59", "age 60+",
                          "Hispanic", "Black", "Asian", "Multiple race", "AI/AN",
                          "Some college", "4+ years college","strata_RE_2", "RP1_var_bcons_1")

coefs_table <- rbind(coefs_null, coefs_full)
saveRDS(coefs_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/HED/HED model coefficients and variance_no_year.rds")

##### CALCULATE VPC AND PCV (from the parameter point estimates)
VPC_HED_null <- print(VPC <- null_HED["RP"][["RP2_var_Intercept"]]/(pi^2/3 + null_HED["RP"][["RP2_var_Intercept"]]))
VPC_full_HED <- print(VPC <- full_HED["RP"][["RP2_var_Intercept"]]/(pi^2/3 + full_HED["RP"][["RP2_var_Intercept"]]))
VPC_table <- data.frame(Model = c("null", "main effects"),
                        VPC = c(VPC_HED_null, VPC_full_HED))
write.csv(VPC_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/HED/hed_VPC_table_no_year.csv")

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
                            b_adult = "FP_age_diaz25-59",
                            b_older_adult = "FP_age_diaz60+",
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
                    + b_adult*`age_diaz25-59`
                    + b_older_adult*`age_diaz60+`  
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
mdata_prepped <- mdata_prepped %>% mutate(
  pA = 100*inv.logit(b_cons*Intercept
                     + b_female*SEXFemale
                     + b_adult*`age_diaz25-59`
                     + b_older_adult*`age_diaz60+`  
                     + b_Hispanic*`race_6_catsHispanic White`
                     + b_Asian*`race_6_catsAsian`
                     + b_AI_AN*`race_6_catsAI/AN`
                     + b_Black*`race_6_catsBlack`
                     + b_Multiple_race*`race_6_catsMultiple race`
                     + b_med*`education_3_catssome college`
                     + b_high*`education_3_cats4+ years college`)
)

# Percentage calculated as the difference between p and pA
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

# Merge with intersectional names reference table
mdata_results <- inner_join(mdata_results, intersections_reference)

# save results
saveRDS(mdata_results, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/HED/mdata_results_no_year.rds")

##### SUMMARY RESULTS TABLES
mdata_results <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/HED/mdata_results_no_year.rds")

# Summarise intersectional groups with the highest and lowest proportions of HEDs
mdata_max_5_overall <- mdata_results %>% ungroup %>% slice_max(pmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_min_5_overall <- mdata_results %>% ungroup %>% slice_min(pmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_overall <- rbind(mdata_max_5_overall, mdata_min_5_overall)

write.csv(mdata_overall, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/HED/mdata_5_estimates_no_year.csv")

# Summarise which intersectional groups have the largest differences in proportions,
# when comparing additive only estimates vs estimates which include interaction effects
mdata_max_5_interactions <- mdata_results %>% ungroup %>% slice_max(pBmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_min_5_interactions <- mdata_results %>% ungroup %>% slice_min(pBmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_interactions <- rbind(mdata_max_5_interactions, mdata_min_5_interactions)

write.csv(mdata_interactions, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/HED/mdata_5_interactions_no_year.csv")