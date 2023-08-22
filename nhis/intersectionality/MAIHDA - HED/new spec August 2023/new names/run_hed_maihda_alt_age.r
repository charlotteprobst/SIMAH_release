##### Script to run the logistic MAIHDA and separate results into additive and interaction effects 
# Replicating the method of analysis undertaken in:
# Axelsson Fisk, S., Mulinari, S., Wemrell, M., Leckie, G., Perez Vicente, R., Merlo, J. Chronic Obstructive Pulmonary Disease in Sweden: an intersectional multilevel analysis of individual heterogeneity and discriminatory accuracy

# ALTERNATIVE AGE CATS ANALYSIS
# 6 race cats (White, Black, Hispanic White, AI/AN, Asian, Multiple race)
# 3 age cats (18-24; 25-69; 70+)
# 2 gender cats 
# 3 education cats
# Controlling for survey year
# 108 intersectional groups total 

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

##### PRE PROCESSING

# Read in data (full sample):
data <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/nhis_alc_clean_full_sample.RDS")

# Generate new race category variable

# review group sizes and raw consumption estimates by race and ethnicity
review_groups <- data %>% group_by(race_ethnicity) %>% (count)
review_grams <- data %>% group_by(race_ethnicity) %>% summarise(median=median(alc_daily_g_capped_200), IQR=IQR(alc_daily_g_capped_200)) %>% arrange(desc(median))
review <- inner_join(review_groups, review_grams)

# Largest groups:
# 1 Non-hispanic, White only
# 8 Hispanic, White only
# 2 Non-hispanic, Black/African American only
# 4 Non-hispanic, Asian only
# 7 Non-hispanic, Multiple race
# 12 Hispanic, Other race 
#	3 American Indian/Alaska Native only

# Keep 7 most populous race only
data_1 <- data %>% filter(race_ethnicity==1|race_ethnicity==8|race_ethnicity==2|race_ethnicity==4|
                                             race_ethnicity==7|race_ethnicity==12|race_ethnicity==3) 

# Check group sizes by intersections based on 7 race categories
group_sizes <- data_1 %>% 
  group_by(SEX, race_ethnicity, education_3_cats, age_3_cats) %>% 
  mutate(intersections = cur_group_id())%>%
  group_by(intersections) %>%
  mutate(count=n()) %>% 
  distinct(intersections, .keep_all = TRUE)
sum(group_sizes$count <= 20) 
# 9 groups with n<20.  6 of these are Hispanic, other, therefore drop this group and regenerate intersections.

data_2 <- data_1 %>% filter(race_ethnicity!=12)

# Convert race and ethnicity from numeric to categorical variable
data_2$race_6_cats <- factor(data_2$race_ethnicity,
                                    levels = c(1,8,2,4,7,3),
                                    labels = c("White", "Hispanic White", 
                                               "Black", "Asian", 
                                               "Multiple race", "AI/AN"))

# Check group sizes by intersections based on 6 race categories
group_sizes_2 <- data_2 %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_3_cats) %>% 
  mutate(intersections = cur_group_id())%>%
  group_by(intersections) %>%
  mutate(count=n())%>% distinct(intersections, .keep_all = TRUE)
sum(group_sizes_2$count <= 20) # 3 groups with n<=20

# Generate a binary HED variable
data_3 <- data_2 %>%
  mutate(HED =
           case_when(ALC5UPYR >= 1 ~ 1,
                     ALC5UPYR == 0 ~ 0)) 

# Generate intersections
data_4 <- data_3 %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_3_cats) %>% 
  mutate(intersections = cur_group_id()) %>%
  mutate(intersectional_names = as.character(paste(SEX, age_3_cats, race_6_cats, education_3_cats))) %>%
  group_by(intersections) %>%
  mutate(denominator=n()) %>%
  group_by(HED, intersections) %>%
  mutate(numerator=n(),
         proportion=numerator/denominator,
         percentage=proportion*100) %>%
  ungroup() 

# Subset data to keep only the variables of interest
data_5 <- data_4 %>%
  dplyr::select(intersections, intersectional_names, NHISPID, YEAR, ALCSTAT1, HED, numerator, denominator, proportion, 
                age_3_cats, SEX, race_6_cats, education_3_cats)

# Generate a summary table showing the proportion of HEDs by intersection
summary_table <- data_5 %>%
  filter(HED==1) %>%
  dplyr::select(-c(ALCSTAT1)) %>%
  distinct(intersections, .keep_all = TRUE)

# Check if any groups with zero HEDs
summary_table %>% 
  ungroup() %>%
  filter(proportion==1)%>%
  distinct() # Nil	

# Save
saveRDS(data_5, "SIMAH_workplace/nhis/intersectionality/cleaned_data/new spec August 2023/HED/hed_data_pre_MAIHDA_alt_age.rds")

##### RUN THE MAIHDA MODELS WITH FULL SAMPLE

# Read in the data
model_data <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/new spec August 2023/HED/hed_data_pre_MAIHDA_alt_age.RDS")

# Prep data for use with Mlwin
model_data <- model_data %>%
  mutate(cons=1) %>% 
  arrange(intersections, NHISPID)

model_data$YEAR <- as.factor(model_data$YEAR)

# Generate reference table with intersectional names
intersections_reference <- model_data %>%
  distinct(intersections, intersectional_names)

# null model
(null_HED <- runMLwiN(logit(HED) ~ 1 + YEAR +
                        (1|intersections), 
                      D = "Binomial", data = model_data,
                      estoptions=list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                      mcmcMeth = list(burnin = 5000,
                                                      thinning = 50,
                                                      resi.store=TRUE))))                                           


# full model
(full_HED <- runMLwiN(logit(HED) ~ 1 + YEAR +
                        SEX + age_3_cats + race_6_cats + education_3_cats +
                             (1|intersections), 
                             D = "Binomial", data = model_data,
                             estoptions=list(EstM=1, resi.store=TRUE, resi.store.levs=c(1,2),
                                        mcmcMeth = list(burnin = 5000,
                                                        thinning = 50,
                                                        resi.store=TRUE))))                                           

# save the model objects
saveRDS(null_HED, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/HED/null_HED_alt_age.rds")
saveRDS(full_HED, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/HED/full_HED_alt_age.rds")

# read in the model objects
null_HED <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/HED/null_HED_alt_age.rds")
full_HED <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/models/new spec August 2023/HED/full_HED_alt_age.rds")

##### PRODUCE A TABLE OF MODEL COEFFICIENTS 
# comparing the null and full models
 
coefs_null <- getSummary(null_HED)
coefs_null <- as.data.frame(coefs_null[["coef"]])
coefs_null <- round(coefs_null, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_null) <- c("intercept_FE_1","Year 2001", "Year 2002", "Year 2003", "Year 2004",
                          "Year 2005", "Year 2006", "Year 2007", "Year 2008", "Year 2009",
                          "Year 2010", "Year 2011", "Year 2012", "Year 2013", "Year 2014",
                          "Year 2015", "Year 2016", "Year 2017", "Year 2018","strata_RE_1","individuals_RE_1")
 
coefs_full <- getSummary(full_HED)
coefs_full <- as.data.frame(coefs_full[["coef"]])
coefs_full <- round(coefs_full, 3) %>% dplyr::select(est,lwr,upr,p)
rownames(coefs_full) <- c("intercept_FE_2","female","age 25-69", "age 70+",
                                 "Hispanic", "Black", "Asian", "Multiple race", "AI/AN",
                                 "Some college", "4+ years college",
                          "Year 2001", "Year 2002", "Year 2003", "Year 2004",
                          "Year 2005", "Year 2006", "Year 2007", "Year 2008", "Year 2009",
                          "Year 2010", "Year 2011", "Year 2012", "Year 2013", "Year 2014",
                          "Year 2015", "Year 2016", "Year 2017", "Year 2018",
                                 "strata_RE_2", "RP1_var_bcons_1")

coefs_table <- rbind(coefs_null, coefs_full)
saveRDS(coefs_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/HED/HED model coefficients and variance_alt_age.rds")


##### CALCULATE VPC AND PCV (from the parameter point estimates)
VPC_HED_null <- print(VPC <- null_HED["RP"][["RP2_var_Intercept"]]/(pi^2/3 + null_HED["RP"][["RP2_var_Intercept"]]))
VPC_full_HED <- print(VPC <- full_HED["RP"][["RP2_var_Intercept"]]/(pi^2/3 + full_HED["RP"][["RP2_var_Intercept"]]))
VPC_table <- data.frame(Model = c("null", "main effects"),
                         VPC = c(VPC_HED_null, VPC_full_HED))
write.csv(VPC_table, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/HED/hed_VPC_table_alt_age.csv")

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
                    + b_2018*`YEAR2018`)
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

# Merge with intersectional names reference table
mdata_results <- inner_join(mdata_results, intersections_reference)

# save results
saveRDS(mdata_results, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/HED/mdata_results_alt_age.rds")

##### SUMMARY RESULTS TABLES
mdata_results <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/HED/mdata_results_alt_age.rds")

# Summarise intersectional groups with the highest and lowest proportions of HEDs
mdata_max_5_overall <- mdata_results %>% ungroup %>% slice_max(pmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_min_5_overall <- mdata_results %>% ungroup %>% slice_min(pmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_overall <- rbind(mdata_max_5_overall, mdata_min_5_overall)

write.csv(mdata_overall, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/HED/mdata_5_estimates_alt_age.csv")

# Summarise which intersectional groups have the largest differences in proportions,
# when comparing additive only estimates vs estimates which include interaction effects
mdata_max_5_interactions <- mdata_results %>% ungroup %>% slice_max(pBmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_min_5_interactions <- mdata_results %>% ungroup %>% slice_min(pBmn, n = 5) %>% 
  dplyr::select(intersectional_names, pmn, plo, phi, pAmn, pAlo, pAhi, pBmn, pBlo, pBhi)
mdata_interactions <- rbind(mdata_max_5_interactions, mdata_min_5_interactions)

write.csv(mdata_interactions, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/new spec August 2023/HED/mdata_5_interactions_alt_age.csv")
