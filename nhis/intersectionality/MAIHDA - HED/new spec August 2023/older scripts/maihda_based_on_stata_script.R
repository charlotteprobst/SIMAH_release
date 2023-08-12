## Script to run the logistic MAIHDA and disagregate results into additive and interaction effects
# (replicating results generated in STATA)

library(tidyverse)
library(stringr)
library(R2MLwiN)
library(boot)

# STATA data for reference
# modeled data
m3data_prepped <- read_dta("U:/STATA HED/logit model/m3data_prepped.dta")
# results table
stata_results <- read_dta("U:/STATA HED/logit model/m3results.dta")

# Read in the processed data ready for modelling
model_data <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/prepped_HED_model_data.RDS")

##### RUN THE MAIHDA MODELS
(full_HED_direct <- runMLwiN(logit(HED) ~ 1 + SEX + age_3_cats + race_5_cats + education_3_cats + decade 
                             + (1|intersections), 
                             D = "Binomial", data = model_data,
                             estoptions=list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                        mcmcMeth = list(burnin = 5000,
                                                        thinning = 50,
                                                        resi.store=TRUE))))                                           


# save the model output
saveRDS(full_HED_direct, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/full_HED_direct_100_its.rds")


# Extract data from relevant slots of s4 object

# data frame
data <- full_HED_direct@data
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

chains <- full_HED_direct@chains
chains <- as.data.frame(chains) 
mb_prepped <- chains %>% dplyr::select(-c(deviance, RP2_var_Intercept, RP1_var_bcons_1))

mb_prepped <- rename(mb_prepped,
                      "FP_Intercept" = b_cons,
                      "FP_SEXFemale" = b_female,
                      "FP_age_3_cats25-69" = b_adult,
                      "FP_age_3_cats70+" = b_older_adult,
                      "FP_race_5_catsNon-Hispanic Black/African American" = b_Black, 
                      "FP_race_5_catsNon-Hispanic Asian"  = b_Asian,
                      "FP_race_5_catsNon-Hispanic Other" = b_Other,
                      "FP_race_5_catsHispanic" = b_Hispanic,
                      "FP_education_3_catssome college" = b_med,
                      "FP_education_3_cats4+ years college" = b_high,
                      "FP_decade2010-2018" = b_second_decade)

mb_prepped$iteration <- rep(c(1:100))

##### PREPARE intersections RANDOM EFFECTS CHAINS
# Store the value of the random effect, for each intersectional group, for each iteration 

# extract the residual chains
resi_chains_lev_2 <- full_HED_direct@resi.chains$resi_lev2
resi_chains_lev_2 <- as.data.frame(resi_chains_lev_2)

# reformat
mu_prepped <- resi_chains_lev_2
mu_prepped$iteration <- 1:nrow(mu_prepped)
mu_prepped <- pivot_longer(resi_chains_lev_2, u_0_1:u_0_180)
mu_prepped$iteration <- rep(c(1:100), each = 180)

##### MERGE DATA, FIXED-PART PARAMETER AND RANDOM EFFECT CHAINS TOGETHER
mdata_prepped <- inner_join(mb_prepped, mu_prepped, by = 'iteration')
mdata_prepped$name <- str_sub(mdata_prepped$name, 5)
mdata_prepped$name <- as.numeric(mdata_prepped$name)
mdata_prepped <- dplyr::rename(mdata_prepped, intersections = name, u = value)
mdata_prepped <- inner_join(mdata_prepped, intersections, by = 'intersections')

##### CALCULATE PERCENTAGES OF INTEREST (p = pA + pB)
  
# Percentage p based on fixed and random part

mdata_prepped <- mdata_prepped %>% mutate(
  p = 100*inv.logit(b_cons*Intercept 
                      + b_female*SEXFemale 
                      + b_adult*`age_3_cats25-69`
                      + b_older_adult*`age_3_cats70+`
                      + b_Black*`race_5_catsNon-Hispanic Black/African American`
                      + b_Asian*`race_5_catsNon-Hispanic Asian`
                      + b_Other*`race_5_catsNon-Hispanic Other` 
                      + b_Hispanic*race_5_catsHispanic 
                      + b_med*`education_3_catssome college` 
                      + b_high*`education_3_cats4+ years college`
                      + b_second_decade*`decade2010-2018` 
                      + u)
)

# Percentage pA based only on the fixed-part
mdata_prepped <- mdata_prepped %>% mutate(
  pA = 100*inv.logit(b_cons*Intercept 
                     + b_female*SEXFemale 
                     + b_adult*`age_3_cats25-69`
                     + b_older_adult*`age_3_cats70+`
                     + b_Black*`race_5_catsNon-Hispanic Black/African American`
                     + b_Asian*`race_5_catsNon-Hispanic Asian`
                     + b_Other*`race_5_catsNon-Hispanic Other` 
                     + b_Hispanic*race_5_catsHispanic 
                     + b_med*`education_3_catssome college` 
                     + b_high*`education_3_cats4+ years college`
                     + b_second_decade*`decade2010-2018`)
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

# save results
saveRDS(mdata_results, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/mdata_results_100_its.rds")