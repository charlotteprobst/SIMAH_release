# MAIHDA of grams of alcohol per day - FULL SAMPLE

# Spec 1: Original age cats, new race cats.

######################################################################## Set-up

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

# set new wd
setwd("C:/Users/cmp21seb/Documents/SIMAH/")

options(scipen=10)


################################################################# PRE PROCESSING

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

# Save
saveRDS(data_3, "SIMAH_workplace/nhis/intersectionality/cleaned_data/new spec August 2023/grams/grams_data_pre_maihda_main.rds")

#################################################################### MODELLING

# Read in model data
data_3 <- readRDS("SIMAH_workplace/nhis/intersectionality/cleaned_data/new spec August 2023/grams/grams_data_pre_maihda_main.rds")

# Prep data for use with Mlwin
model_data <- data_3 %>%
  mutate(cons=1) %>% 
  arrange(intersections, NHISPID)

model_data$age_diaz <- droplevels(model_data$age_diaz)

model_data$YEAR <- as.factor(model_data$YEAR)

# Generate reference table with intersectional names & mean observed grams
intersections_reference <- model_data %>%
  group_by(intersectional_names) %>% 
  distinct(intersections, intersectional_names, mean_observed_grams)

# Null model
(null_grams <- runMLwiN(alc_daily_g_capped_200 ~ 1 + YEAR +
                          (1 | intersections) + 
                          (1 | NHISPID), 
                             data = model_data, 
                             estoptions = list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                               mcmcMeth = list(burnin = 5000,
                                                               thinning = 50,
                                                               resi.store=TRUE))))
# Full model
(full_grams <- runMLwiN(alc_daily_g_capped_200 ~ 1 + YEAR +
                          SEX + age_diaz + race_6_cats + education_3_cats +
                          (1 | intersections) + 
                          (1 | NHISPID), 
                        data = model_data, 
                        estoptions = list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                          mcmcMeth = list(burnin = 5000,
                                                          thinning = 50,
                                                          resi.store=TRUE))))

# save the model objects
saveRDS(null_grams, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/grams/null_grams_not_transformed.rds")
saveRDS(full_grams, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/grams/full_grams_not_transformed.rds")

# Check convergence achieved
summary(full_grams@chains[, "FP_Intercept"])
mcmc_trace(full_grams@chains)

##################################################################### ANALYSIS

# Read in the model objects
null_grams <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/grams/null_grams_not_transformed.rds")
full_grams <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/grams/full_grams_not_transformed.rds")


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
saveRDS(coefs_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/model coefficients and variance_grams_not_transformed.rds")
write.csv(coefs_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/model coefficients and variance_grams_not_transformed.csv")

##### CALCULATE VPC AND PCV (from the parameter point estimates)
VPC_grams_null <- null_grams["RP"][["RP2_var_Intercept"]]/(null_grams["RP"][["RP1_var_Intercept"]] + null_grams["RP"][["RP2_var_Intercept"]])
VPC_grams_full <- full_grams["RP"][["RP2_var_Intercept"]]/(full_grams["RP"][["RP1_var_Intercept"]] + full_grams["RP"][["RP2_var_Intercept"]])
VPC_table <- data.frame(Model = c("null", "main effects"),
                        VPC = c(VPC_grams_null, VPC_grams_full))
write.csv(VPC_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/VPC_table_grams_not_transformed.csv")

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


##### MERGE DATA, FIXED-PART PARAMETER AND RANDOM EFFECT CHAINS TOGETHER
mdata_prepped <- inner_join(mb_prepped, mu_prepped, by = 'iteration')
mdata_prepped$name <- str_sub(mdata_prepped$name, 5)
mdata_prepped$name <- as.numeric(mdata_prepped$name)
mdata_prepped <- dplyr::rename(mdata_prepped, intersections = name, u = value)
mdata_prepped <- inner_join(mdata_prepped, intersections, by = 'intersections')


##### CALCULATE VALUES OF INTEREST (est = estA + estI)

mdata_prepped <- mdata_prepped %>% mutate(
  est = (b_cons*Intercept
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
                    + b_2001*`YEAR2001`
                    + b_2002*`YEAR2002`
                    + b_2003*`YEAR2003`
                    + b_2004*`YEAR2004`
                    + b_2005*`YEAR2005`
                    + b_2006*`YEAR2006`
                    + b_2007*`YEAR2007`
                    + b_2008*`YEAR2008`
                    + b_2009*`YEAR2009`
                    + b_2010*`YEAR2010`
                    + b_2011*`YEAR2011`
                    + b_2012*`YEAR2012`
                    + b_2013*`YEAR2013`
                    + b_2014*`YEAR2014`
                    + b_2015*`YEAR2015`
                    + b_2016*`YEAR2016`
                    + b_2017*`YEAR2017`
                    + b_2018*`YEAR2018`
                    + u)
)

mdata_prepped <- mdata_prepped %>% mutate(
  estA = (b_cons*Intercept
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
                           + b_2001*`YEAR2001`
                           + b_2002*`YEAR2002`
                           + b_2003*`YEAR2003`
                           + b_2004*`YEAR2004`
                           + b_2005*`YEAR2005`
                           + b_2006*`YEAR2006`
                           + b_2007*`YEAR2007`
                           + b_2008*`YEAR2008`
                           + b_2009*`YEAR2009`
                           + b_2010*`YEAR2010`
                           + b_2011*`YEAR2011`
                           + b_2012*`YEAR2012`
                           + b_2013*`YEAR2013`
                           + b_2014*`YEAR2014`
                           + b_2015*`YEAR2015`
                           + b_2016*`YEAR2016`
                           + b_2017*`YEAR2017`
                           + b_2018*`YEAR2018`
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
saveRDS(mdata_results, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/mdata_results_grams_not_transformed.rds")
write.csv(mdata_results, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/mdata_results_grams_not_transformed.csv")


##### SUMMARY RESULTS TABLES
mdata_results <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/mdata_results_grams_not_transformed.rds")

# Summarise intersectional groups with the highest and lowest estimated grams
mdata_max_5_overall <- mdata_results %>% ungroup %>% slice_max(estmn, n = 5) %>% 
  dplyr::select(intersectional_names, estmn, estlo, esthi, estAmn, estAlo, estAhi, estImn, estIlo, estIhi)
mdata_min_5_overall <- mdata_results %>% ungroup %>% slice_min(estmn, n = 5) %>% 
  dplyr::select(intersectional_names, estmn, estlo, esthi, estAmn, estAlo, estAhi, estImn, estIlo, estIhi)
mdata_overall <- rbind(mdata_max_5_overall, mdata_min_5_overall)

write.csv(mdata_overall, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/mdata_5_estimates_not_transformed.csv")

# Summarise which intersectional groups have the largest differences in grams estimates,
# when comparing additive only estimates vs estimates which include interaction effects
mdata_max_5_interactions <- mdata_results %>% ungroup %>% slice_max(estImn, n = 5) %>% 
  dplyr::select(intersectional_names, estmn, estlo, esthi, estAmn, estAlo, estAhi, estImn, estIlo, estIhi)
mdata_min_5_interactions <- mdata_results %>% ungroup %>% slice_min(estImn, n = 5) %>% 
  dplyr::select(intersectional_names, estmn, estlo, esthi, estAmn, estAlo, estAhi, estImn, estIlo, estIhi)
mdata_interactions <- rbind(mdata_max_5_interactions, mdata_min_5_interactions)

write.csv(mdata_interactions, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/grams/mdata_5_interactions_not_transformed.csv")

##### Explore face validity of estimates
temp <- mdata_results %>% dplyr::select(intersectional_names, mean_observed_grams, estmn) 
ggplot(temp, aes(x=mean_observed_grams, y=estmn)) + geom_point() + 
  ggtitle("Comparisson of observed and estimated daily grams, 180 intersectional groups")
 ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/new spec August 2023/grams/observed vs estimated grams_not_transformed.png", 
       dpi=300, width=33, height=19, units="cm")
# Interpretation: Positive correlation but generally estimates are lower than observed. 
 
# Compare the ranking of intersectional groups based on observed and estimated grams
temp$rank_observed_grams <-rank(temp$mean_observed_grams)
temp$rank_estimated_grams <-rank(temp$estmn)
ggplot(temp, aes(x=rank_observed_grams, y=rank_estimated_grams)) + geom_point() + 
ggtitle("Comparisson of observed vs estimated drinking 'rank', 180 intersectional groups")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/new spec August 2023/grams/observed rank vs estimated rank grams_not_transformed.png", 
       dpi=300, width=33, height=19, units="cm")