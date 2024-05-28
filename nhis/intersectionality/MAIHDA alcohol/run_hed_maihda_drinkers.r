##############################################################################
# MAIHDA of HED - DRINKERS
# Script to run the logistic MAIHDA and separate results into additive and interaction effects 
# Replicating the method of analysis undertaken in:
# Axelsson Fisk, S., Mulinari, S., Wemrell, M., Leckie, G., Perez Vicente, R., Merlo, J. Chronic Obstructive Pulmonary Disease in Sweden: an intersectional multilevel analysis of individual heterogeneity and discriminatory accuracy
##############################################################################

######################################################################## Set-up
setwd("C:/Users/cmp21seb/Documents/SIMAH/")
code <- "SIMAH_code/nhis/intersectionality/MAIHDA alcohol/"
inputs <- "SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/inputs/"
models <- "SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/models/"
outputs <- "SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/outputs/"

# Read in necessary R packages
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

# Read in data (full sample):
data <- readRDS(paste0(inputs,"nhis_alc_clean_full_sample.RDS"))

# subset drinkers
data_drinkers <- data %>% filter(ALCSTAT1=="Current drinker")

# Drop individuals age <21
data_0 <- data_drinkers %>% filter(age_diaz!="18-20")

# Keep only the 6 selected race and ethnicity groups
data_1 <- data_0 %>% filter(!is.na(race_6_cats))

# Check intersectional group sizes
group_sizes <- data_1 %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_diaz) %>% 
  mutate(intersections = cur_group_id())%>%
  group_by(intersections) %>%
  mutate(count=n()) %>% distinct(intersections, .keep_all = TRUE)
sum(group_sizes$count <= 20) # 3 groups with n<=20

# Generate a binary HED variable
data_2 <- data_1 %>%
  mutate(HED =
           case_when(ALC5UPYR >= 1 ~ 1,
                     ALC5UPYR == 0 ~ 0)) 

# Generate intersections
data_3 <- data_2 %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_diaz) %>% 
  mutate(intersections = cur_group_id(),
         intersectional_names = as.character(paste(SEX, age_diaz, race_6_cats, education_3_cats)), 
         denominator=n()) %>%
  group_by(HED, intersections) %>%
  mutate(numerator=n(),
         proportion=numerator/denominator,
         percentage=proportion*100) %>%
  ungroup() 

# Subset data to keep only the variables of interest
data_4 <- data_3 %>%
  dplyr::select(intersections, intersectional_names, NHISPID, ALCSTAT1, HED, numerator, denominator, proportion, 
                age_diaz, SEX, race_6_cats, education_3_cats, YEAR)

# count number of HEDs and calculate proportion of sample that are HEDs
data_4 %>% count(HED==1)
data_4 %>%
  summarise(proportion_hed = mean(HED, na.rm = TRUE)*100) # 33.3%

# Generate reference table with intersectional names & proportion of observed HED per intersection
intersections_reference <- data_4 %>%
  group_by(intersectional_names) %>%
  mutate(Observed_prop_HED = mean(HED, na.rm = TRUE),
         count=n()) %>%
  distinct(intersections, intersectional_names, count, Observed_prop_HED)

# Generate reference table with intersectional names & proportion of observed HED for the year 2009 only
intersections_reference_2009 <- data_4 %>%
  filter(YEAR==2009) %>%
  group_by(intersectional_names) %>%
  mutate(Observed_prop_HED_2009 = mean(HED, na.rm = TRUE),
         count_2009=n()) %>%
  distinct(intersections, intersectional_names, count_2009, Observed_prop_HED_2009)

# Prep data for use with Mlwin
model_data_drinkers <- data_4 %>%
  mutate(cons=1) %>%
  arrange(intersections, NHISPID)

model_data_drinkers$age_diaz <- droplevels(model_data_drinkers$age_diaz)
model_data_drinkers$YEAR <- as.factor(model_data_drinkers$YEAR)

# Save
saveRDS(model_data_drinkers, paste0(inputs, "hed_data_pre_maihda_drinkers.rds"))

###################################################################### MODELLING

##### RUN THE MAIHDA MODELS WITH DRINKERS ONLY

# Read in the data
model_data_drinkers <- readRDS(paste0(inputs, "hed_data_pre_maihda_drinkers.rds"))

# null model
(null_HED_drinkers <- runMLwiN(logit(HED) ~ 1 + 
                                 YEAR + 
                                 (1|intersections),
                      D = "Binomial", data = model_data_drinkers,
                      estoptions=list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                      mcmcMeth = list(burnin = 5000,
                                                      thinning = 50,
                                                      resi.store=TRUE))))

# full model
(full_HED_drinkers <- runMLwiN(logit(HED) ~ 1 + 
                                 SEX + age_diaz + race_6_cats + education_3_cats +
                                 YEAR + 
                                 (1|intersections),
                      D = "Binomial", data = model_data_drinkers,
                      estoptions=list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                      mcmcMeth = list(burnin = 5000,
                                                      thinning = 50,
                                                      resi.store=TRUE))))

# save the model objects
saveRDS(null_HED_drinkers, paste0(models, "null_HED_drinkers.rds"))
saveRDS(full_HED_drinkers, paste0(models, "full_HED_drinkers.rds"))

## read in the model objects
null_HED_drinkers <- readRDS(paste0(models, "null_HED_drinkers.rds"))
full_HED_drinkers <- readRDS(paste0(models, "full_HED_drinkers.rds"))

coefs_null_drinkers <- getSummary(null_HED_drinkers)
coefs_null_drinkers <- as.data.frame(coefs_null_drinkers[["coef"]])
coefs_null_drinkers <- round(coefs_null_drinkers, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_null_drinkers) <- c("intercept","Year 2001", "Year 2002", "Year 2003", "Year 2004",
                                   "Year 2005", "Year 2006", "Year 2007", "Year 2008", "Year 2009",
                                   "Year 2010", "Year 2011", "Year 2012", "Year 2013", "Year 2014",
                                   "Year 2015", "Year 2016", "Year 2017", "Year 2018","strata_RE_1","individuals_RE_1")

coefs_full_drinkers <- getSummary(full_HED_drinkers)
coefs_full_drinkers <- as.data.frame(coefs_full_drinkers[["coef"]])
coefs_full_drinkers <- round(coefs_full_drinkers, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_full_drinkers) <- c("intercept_full","female","age 25-69", "age 70+",
                          "Hispanic", "Black", "Asian", "Multiple race", "AI/AN",
                          "Some college", "4+ years college", "Year 2001", 
                          "Year 2002", "Year 2003", "Year 2004", "Year 2005", "Year 2006", 
                          "Year 2007", "Year 2008", "Year 2009","Year 2010", "Year 2011", 
                          "Year 2012", "Year 2013", "Year 2014","Year 2015", "Year 2016", 
                          "Year 2017", "Year 2018", "strata_RE_2", "RP1_var_bcons_1")

coefs_table_drinkers <- rbind(coefs_null_drinkers, coefs_full_drinkers)
saveRDS(coefs_table_drinkers, paste0(outputs, "HED drinkers/HED model coefficients and variance_drinkers.rds"))
write.csv(coefs_table_drinkers, paste0(outputs, "HED drinkers/HED model coefficients and variance_drinkers.csv"))

##### CALCULATE VPC AND PCV (from the parameter point estimates)
VPC_HED_null_drinkers <- print(VPC <- null_HED_drinkers["RP"][["RP2_var_Intercept"]]/(pi^2/3 + null_HED_drinkers["RP"][["RP2_var_Intercept"]]))
VPC_full_HED_drinkers <- print(VPC <- full_HED_drinkers["RP"][["RP2_var_Intercept"]]/(pi^2/3 + full_HED_drinkers["RP"][["RP2_var_Intercept"]]))
VPC_table_drinkers <- data.frame(Model = c("null", "main effects"),
                         VPC = c(VPC_HED_null_drinkers, VPC_full_HED_drinkers))
write.csv(VPC_table_drinkers, paste0(outputs, "HED drinkers/hed_VPC_table_drinkers.csv"))

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
mdata_prepped_drinkers <- inner_join(mdata_prepped_drinkers, intersections_drinkers, by = 'intersections')

##### CALCULATE PERCENTAGES OF INTEREST (p = pA + pB)

# Percentage p based on fixed and random part

mdata_prepped_drinkers <- mdata_prepped_drinkers %>% mutate(
  p = 100*inv.logit(b_cons*Intercept
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
                    + u)
)

# Percentage pA based only on the fixed-part
mdata_prepped_drinkers <- mdata_prepped_drinkers %>% mutate(
  pA = 100*inv.logit(b_cons*Intercept
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
                     + b_2009*`YEAR2009`)
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

# Merge with intersectional names reference table
mdata_results_drinkers <- inner_join(mdata_results_drinkers, intersections_reference)

# save results
saveRDS(mdata_results_drinkers, paste0(outputs, "HED drinkers/results_drinkers_HED.rds"))

##### SUMMARY RESULTS TABLES
mdata_results_drinkers <- readRDS(paste0(outputs, "HED drinkers/results_drinkers_HED.rds"))

# Summarise intersectional groups with the highest and lowest proportions of HEDs
mdata_max_5_overall <- mdata_results_drinkers %>% ungroup %>% slice_max(pmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_min_5_overall <- mdata_results_drinkers %>% ungroup %>% slice_min(pmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_overall <- rbind(mdata_max_5_overall, mdata_min_5_overall)

write.csv(mdata_overall, paste0(outputs, "HED drinkers/mdata_5_estimates_drinkers_HED.csv"))

# Summarise which intersectional groups have the largest differences in proportions,
# when comparing additive only estimates vs estimates which include interaction effects
mdata_max_5_interactions <- mdata_results_drinkers %>% ungroup %>% slice_max(pBmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_min_5_interactions <- mdata_results_drinkers %>% ungroup %>% slice_min(pBmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_interactions <- rbind(mdata_max_5_interactions, mdata_min_5_interactions)

write.csv(mdata_interactions, paste0(outputs, "HED drinkers/mdata_5_interactions_drinkers_HED.csv"))

##### Explore face validity of estimates

# Compare observed HED (overall) and estimated HED in a table
temp <- mdata_results_drinkers %>% dplyr::select(intersectional_names, Observed_prop_HED, pmn) %>%
  mutate(Observed_prop_HED = Observed_prop_HED*100,
         difference = pmn - Observed_prop_HED,
         abs_difference = abs(difference),
         percent_difference = abs(difference/pmn*100))
write.csv(temp, paste0(outputs, "HED drinkers/Mean observed vs estimated grams - drinkers HED.csv"))

# Compare mean observed (2009 observed only) and estimated in a table
temp_2009 <- mdata_results_drinkers %>% inner_join(., intersections_reference_2009) %>%
  dplyr::select(intersectional_names, Observed_prop_HED_2009, pmn) %>%
  mutate(Observed_prop_HED_2009 = Observed_prop_HED_2009*100,
         difference = pmn - Observed_prop_HED_2009,
         abs_difference = abs(difference))
