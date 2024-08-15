##################### ##########################################################
# Run MAIHDA on grams per day using BRFSS data
################################################################################


################################################################################# Setup
# read in data
BRFSS_data <- readRDS("U:\\SIMAH\\SIMAH_workplace\\BRFSS\\BRFSS_upshifted_2000_2020_USA.RDS")

# Load necessary packages
library("R2MLwiN")
library(tidyr)
library(dplyr)

# Bias toward non-scientific notation
options(scipen=10)

################################################################################ Data prep

# subset drinkers
drinkers_brfss <- BRFSS_data %>% filter(drinkingstatus_detailed == "Monthly drinker" | drinkingstatus_detailed == "Yearly drinker")

# remove years 2019 and 2020 to make comparable to NHIS analysis
drinkers_brfss <- drinkers_brfss %>% filter(YEAR != 2019 & YEAR != 2020)

# drop individuals with estimated 0 grams per day on average
drinkers_brfss <- drinkers_brfss %>% subset(gramsperday_orig !=0)

# Assign all individuals an ID number
drinkers_brfss <- drinkers_brfss %>% mutate(ID = row_number())

# Process and review demographics

# Sex
drinkers_brfss <- rename(drinkers_brfss, SEX = sex_recode)

drinkers_brfss %>% 
  group_by(SEX) %>% 
  count %>% 
  ungroup() %>% 
  mutate(percent=n/sum(n)*100)

# Education
drinkers_brfss <- rename(drinkers_brfss, education_3_cats = education_summary)

drinkers_brfss %>% 
  group_by(education_3_cats) %>% 
  count %>% 
  ungroup() %>% 
  mutate(percent=n/sum(n)*100)

# Age 3 cats
drinkers_brfss <- drinkers_brfss %>% mutate(
  age_3_cats = dplyr::case_when(
    age_var <= 24 ~ 1, # Adolesents and young adults (18-24)
    age_var > 24 & age_var <=69 ~ 2, # Adults (25-69)
    age_var > 69 ~ 3), # Older adults (70-99)
)

drinkers_brfss$age_3_cats <- factor(drinkers_brfss$age_3_cats,
                                     levels = c(1,2,3),
                                     labels = c("18-24", "25-69", "70+"))

mean(drinkers_brfss$age_var)
drinkers_brfss %>% 
  group_by(age_3_cats) %>% 
  count() %>%
  ungroup() %>% 
  mutate(percent=n/sum(n)*100)

# Decade
drinkers_brfss <- drinkers_brfss %>% mutate(
  decade = case_when(
    YEAR == 2000 | YEAR == 2001 | YEAR == 2002 | YEAR == 2003 | YEAR == 2004 |
    YEAR == 2005 | YEAR == 2006 | YEAR == 2007 | YEAR == 2008 | YEAR == 2009 ~ 1, 
    YEAR == 2010 | YEAR == 2011 | YEAR == 2012 | YEAR == 2013 | YEAR == 2014 | 
    YEAR == 2015 | YEAR == 2016 | YEAR == 2017 | YEAR == 2018  ~ 2))

drinkers_brfss %>% 
  group_by(decade) %>% 
  count() %>%
  ungroup() %>% 
  mutate(percent=n/sum(n)*100)

# Label new categories:
drinkers_brfss$decade <- factor(drinkers_brfss$decade,
                                levels = c(1,2),
                                labels = c("2000-2009","2010-2018"))

# Race 
# nb. only in 4 cats as no breakdown of other possible, and education already in 3 cats
drinkers_brfss %>% 
  group_by(race_eth) %>% 
  count() %>%
  ungroup() %>% 
  mutate(percent=n/sum(n)*100)

# Appears NAs already dropped from data

#### ANALYSIS 1: USING ORIGINAL DATA (NOT UPSHIFTED), NO SURVEY WEIGHTS, DRINKERS ONLY (i.e. comparable to nhis analysis)

# check distribution of grams per day amongst drinkers
min(drinkers_brfss$gramsperday_orig)
max(drinkers_brfss$gramsperday_orig)
hist(drinkers_brfss$gramsperday_orig)

# apply cap of 200g
drinkers_brfss <- drinkers_brfss %>% 
  mutate(gramsperday_orig_cap = if_else(gramsperday_orig > 200, 200, gramsperday_orig))

## Review average daily grams

# Raw:
mean(drinkers_brfss$gramsperday_orig) # 10.4

# Raw capped:
mean(drinkers_brfss$gramsperday_orig_cap) # 10.2

## Review distribution
hist(drinkers_brfss$gramsperday_orig_cap)

# Transform alcohol data to fit normal distribution
drinkers_brfss <- drinkers_brfss %>% 
  mutate(gramsperday_orig_cap_log = log(gramsperday_orig_cap))

# Test transformation by back transforming and comparing to original data
drinkers_brfss <- drinkers_brfss %>% 
  mutate(gramsperday_test = exp(gramsperday_orig_cap_log))

# Review distribution
hist(drinkers_brfss$gramsperday_orig_cap_log) # poor transformation

#### ANALYSIS 2: USING UPSHIFTED DATA, NO SURVEY WEIGHTS, DRINKERS ONLY 

### Data prep

# replicated dataset for use in analysis 2 to avoid confusion
drinker_brfss_2 <- drinkers_brfss

# drop individuals with estimated 0 grams per day on average
drinkers_brfss_2 <- drinkers_brfss %>% subset(gramsperday_upshifted !=0)

# check distribution of grams per day amongst drinkers
min(drinkers_brfss_2$gramsperday_upshifted)
max(drinkers_brfss_2$gramsperday_upshifted)
hist(drinkers_brfss_2$gramsperday_upshifted)

# apply cap of 200g
drinkers_brfss_2 <- drinkers_brfss_2 %>% 
  mutate(gramsperday_upshifted_cap = if_else(gramsperday_upshifted > 200, 200, gramsperday_upshifted))

## Review average daily grams

# Upshifted:
mean(drinkers_brfss_2$gramsperday_upshifted) # 16.7

# Upshifted then capped:
mean(drinkers_brfss_2$gramsperday_upshifted_cap) # 16.0

## Review distribution
hist(drinkers_brfss_2$gramsperday_upshifted_cap)

# Transform alcohol data to fit normal distribution
drinkers_brfss_2 <- drinkers_brfss_2 %>% 
  mutate(gramsperday_upshifted_cap_log = log(gramsperday_upshifted_cap))

# Test transformation by back transforming and comparing to original data
drinkers_brfss_2 <- drinkers_brfss_2 %>% 
  mutate(gramsperday_test = exp(gramsperday_upshifted_cap_log))

# Review distribution
hist(drinkers_brfss_2$gramsperday_upshifted_cap_log) # slightly better transformation

# trial alternative transformation
# b <- MASS::boxcox(lm(drinkers_brfss_2$gramsperday_upshifted_cap ~ 1))
# lambda <- b$x[which.max(b$y)] # -0.02 (also suggestive that log transformation best).  However, note very wide CIs around lambda value

# Generate intersectional groups
drinkers_brfss_2 <- drinkers_brfss_2 %>% 
  group_by(SEX, race_eth, education_3_cats, age_3_cats, decade) %>% 
  mutate(intersections = cur_group_id()) %>%
  group_by(intersections) %>%
  mutate(count=n())

# Intersections
intersectional_group_sizes <- drinkers_brfss_2 %>% 
  group_by(intersections) %>% 
  mutate(percent=count/sum(count)*100) %>%
  select(intersections, age_3_cats, race_eth, SEX, decade, education_3_cats, count, percent) %>%
  distinct()
# Smallest group size is 215 people

# Add a column of the observed mean grams per day for each intersection
drinkers_brfss_2 <- drinkers_brfss_2 %>%
  group_by(intersections) %>%
  mutate(mean_observed_grams = mean(gramsperday_upshifted_cap))

# Create a reference table of the intersectional groups - numbers and names
temp <- drinkers_brfss_2 %>%
  mutate(sex = dplyr::recode(SEX, Male = "M", Female = "F"))

temp2 <- temp %>% 
  mutate(intersectional_names = as.character(paste(sex, age_3_cats, race_eth, education_3_cats, decade)))

MAIHDA_intersections_reference <- distinct(temp2, intersections, .keep_all = TRUE) %>% 
  dplyr::select(intersections, intersectional_names)

# Prep data for use with R2mlwin package:
# Generate a constant
drinkers_brfss_2 <- drinkers_brfss_2 %>%
  mutate(cons=1)

# Sort by intersections
drinkers_brfss_2 <- drinkers_brfss_2 %>% arrange(intersections, ID)

# Save data used for modelling
saveRDS(drinkers_brfss_2, "U:/SIMAH/SIMAH_workplace/BRFSS/Intersectionality/drinkers_brfss.RDS")

############################################################################### Run models
# Change file path for mlwin based on computer being used
# options(MLwiN_path="C:/Program Files/MLwiN v3.01/") # Uni
options(MLwiN_path="C:/Program Files/MLwiN v3.05") # home

# Read in processed data
drinkers_brfss_2 <- readRDS("U:/SIMAH/SIMAH_workplace/BRFSS/Intersectionality/drinkers_brfss.RDS")

### Specify MAIHDA model formula
#Note for normal response models the level 1 ID needs to be explicitly included in the random part of the model formula; this is not the case for discrete response models.

# Null model
F1 <- gramsperday_upshifted_cap_log ~ 1 + (1 | intersections) + (1 | ID)

# Main-effects model
F2 <- gramsperday_upshifted_cap_log ~ 1 + SEX + age_3_cats + race_eth + education_3_cats + decade + (1 | intersections) + (1 | ID)

#################### Run the null model and calculate VPC
VarCompModelMCMC <- runMLwiN(Formula = F1, 
                             data = drinkers_brfss_2)

VPC_null <- VarCompModelMCMC["RP"][["RP2_var_Intercept"]]/
        (VarCompModelMCMC["RP"][["RP1_var_Intercept"]] + VarCompModelMCMC["RP"][["RP2_var_Intercept"]])*100

# Warning messages:
# 1: In reset < 0 || reset > 2 :
#   'length(x) = 2 > 1' in coercion to 'logical(1)'
# 2: In reset < 0 || reset > 2 :
#   'length(x) = 2 > 1' in coercion to 'logical(1)'

within_group_variance_null <- 100-VPC_null

################### Run the main-effects two-level model (storing residuals) and recalculate VPC
VarCompResidMCMC <- runMLwiN(Formula = F2, 
                             data = drinkers_brfss_2, 
                             estoptions = list(resi.store = TRUE, EstM = 1))
# Default: burn in = 500, iterations = 5000, a thinning factor of 1, a random number seed of 1, 1 chain 
# For fixed parameters MLwiN employs, by default, an improper uniform prior p(β) ∝ 1

# Error in foreign::read.dta(chainfile[i]) : 
#   unable to open file: 'No such file or directory'

# Trial running with IGLS instead
VarCompResidIGLS <- runMLwiN(Formula = F2, 
                             data = drinkers_brfss_2, 
                             estoptions = list(resi.store = TRUE))

VPC_full_IGLS <- VarCompResidIGLS["RP"][["RP2_var_Intercept"]]/
        (VarCompResidIGLS["RP"][["RP1_var_Intercept"]] + VarCompResidIGLS["RP"][["RP2_var_Intercept"]])*100 # 0.8%

within_group_variance_full_IGLS <- 100-VPC_full_IGLS # 99.2%

PCV <- (VPC_null-VPC_full_IGLS)/VPC_null*100 # 90.5%

saveRDS(VarCompResidIGLS, "U:/SIMAH/SIMAH_workplace/BRFSS/Intersectionality/VarCompResidIGLS.RDS")