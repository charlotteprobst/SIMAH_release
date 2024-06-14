#### Set up

######################################################################## Set-up
setwd("C:/Users/cmp21seb/Documents/SIMAH/")
code <- "SIMAH_code/nhis/intersectionality/MAIHDA alcohol/"
inputs <- "SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/inputs/"
models <- "SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/models/"
outputs <- "SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/outputs/"

# Read in necessary R packages
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(ipumsr)     # load in data extracted from IPUMS website
library(labelled)
library(mice)

# Source required functions
source(paste0(code,"functions/recode_to_NA_all.R"))
source(paste0(code,"functions/remove_na.R"))
source(paste0(code,"functions/generate_ALCSTAT.R"))
source(paste0(code,"functions/assign_grams_alcohol.R"))
source(paste0(code,"functions/recode_alc_cats.R"))
source(paste0(code,"functions/recode_race_ethnicity.R"))
source(paste0(code,"functions/recode_income.R"))
source(paste0(code,"functions/recode_education.R"))
source(paste0(code,"functions/recode_age.R"))
source(paste0(code,"functions/recode_sexorien.R"))
source(paste0(code,"functions/recode_cohort.R"))

# Set default theme for plots:
theme_set(theme_bw(base_size = 12))

# Bias towards non scientific notation
options(scipen = 100)

#### Initial read in of raw data from IPUMS website
#(Lynn A. Blewett, Julia A. Rivera Drew, Miriam L. King, Kari C.W. Williams, Natalie Del Ponte and Pat Convey. IPUMS Health Surveys: National Health Interview Survey, Version 7.1 [dataset]. Minneapolis, MN: IPUMS, 2021). Available at: https://doi.org/10.18128/D070.V7.1):
ddi <- read_ipums_ddi(paste0(inputs, "/raw_data/nhis_00005.xml"))
data <- read_ipums_micro(ddi)

# Exclude years after 2018 as Qs not asked in 2019 and drinking likely to have changed in 2020 due to pandemic
# No alc Qs in 2019; No ALC5UPYR in 2020)
nhis_subset <- data %>%
      dplyr::select(YEAR, INTERVWMO, INTERVWYR, NHISPID, PERWEIGHT, SAMPWEIGHT, AGE, BIRTHYR, SEX, SEXORIEN, EDUCREC2, RACENEW, HISPYN, USBORN, CITIZEN, INCFAM97ON2, starts_with("ALC"), starts_with("MORT"), SMOKFREQNOW) %>%
 filter(YEAR <=2018)  # n= 1762659

# Exclude individuals with alc questions Not In Universe (Universe = Sample adults aged 18+)
 nhis_subset <- nhis_subset %>%
    filter(ALCSTAT1 != 0) # 572339
 
# Create data dictionary for reference
dictionary <- labelled::generate_dictionary(nhis_subset)
 
# Zap labels from the dataframe to facilitate data manipulation 
nhis_subset <- nhis_subset %>% zap_formats() %>% zap_labels()
 
# Save the subset data
saveRDS(nhis_subset, paste0(inputs, "nhis_subset.RDS"))

######### START FROM HERE IF PREVIOUSLY DOWNLOADED AND SAVED RAW IPUMS DATA

# Read in the subset data
nhis_subset <- readRDS(paste0(inputs,"nhis_subset.RDS"))

# Convert all types of NA to be consistent i.e. convert 'refused', 'not ascertained', 'don't know', 'inconsistent' and 'NIU' to NA
nhis_subset_converted_NA <- recode_to_NA_all(nhis_subset)

# Drop individuals aged <21
nhis_21 <- nhis_subset_converted_NA %>% filter(AGE>=21) # 550,979

# Recode race into 6 categories
nhis_subset_race <- recode_race_ethnicity_all(nhis_21)

# Keep only the 6 selected race and ethnicity groups
nhis_subset_race_6 <- nhis_subset_race %>% filter(!is.na(race_6_cats)) # 550,183

#Recode age
nhis_subset_age <- recode_age(nhis_subset_race_6)
nhis_subset_age$age_3_cats <- factor(nhis_subset_age$age_3_cats,
                                                 levels = c(1,2,3),
                                                 labels = c("18-24", "25-69", "70+"))
nhis_subset_age$age_diaz <- factor(nhis_subset_age$age_diaz,
                                                   levels = c(0,1,2,3),
                                                   labels = c("18-20", "21-24", "25-59","60+"))
# Drop individuals aged <21
nhis_21 <- nhis_subset_age %>% filter(AGE>=21) # 550,183

## Recategorise age, sexual orientation, income, education and birth cohort

nhis_subset_sexorien <- recode_sexorien(nhis_subset_age)
nhis_subset_sexorien$SEXORIEN <- factor(nhis_subset_sexorien$SEXORIEN,
                              levels = c(1,2),
                              labels = c("Heterosexual", "Homosexual/bisexual/something else"))

nhis_subset_income <- recode_income(nhis_subset_sexorien)
nhis_subset_income$income <- factor(nhis_subset_income$income,
                    levels = c(1,2,3,4),
                    labels = c("$0 - $34,999", "$35,000-$74,999", "$75,000-$99,999","$100,000 and over"))

nhis_subset_education <- recode_education(nhis_subset_income)
nhis_subset_education$education_3_cats <- factor(nhis_subset_education$education_3_cats,
                                    levels = c(1,2,3),
                                    labels = c("high school or less", "some college", "4+ years college"))
nhis_subset_education$education_4_cats <- factor(nhis_subset_education$education_4_cats,
                                    levels = c(1,2,3,4),
                                    labels = c("no high school (<= grade 8)", "some high school or high school graduate", "some college", "4+ years college"))
nhis_subset_education$education_5_cats <- factor(nhis_subset_education$education_5_cats,
                                    levels = c(1,2,3,4,5),
                                    labels = c("no high school (<= grade 8)", "Some high school (grades 9-11)", "Finished high school (grade 12)", "Some college", "4+ years college"))

nhis_subset_cohort <- dplyr::rename(nhis_subset_education, birth_year = BIRTHYR)
sum(is.na(nhis_subset_cohort$birth_year)) # 77,046 individuals missing birth year, therefore generate a birth year estimate for those missing birth year
nhis_subset_cohort <- nhis_subset_cohort %>% 
  mutate(birth_year_est = ifelse(is.na(birth_year), YEAR-AGE, birth_year))
  # Group into cohorts based on estimated year of birth
nhis_subset_cohort <- recode_cohort(nhis_subset_cohort)
nhis_subset_cohort$birth_cohort <- factor(nhis_subset_cohort$birth_cohort,
                                    levels = c(1,2,3,4,5),
                                    labels = c("silent", "baby_boomers", "gen_x", "millenials", "gen_z"))

nhis_subset_decade <- nhis_subset_cohort %>% mutate(
      decade = case_when(
      YEAR == 2000 | YEAR == 2001 | YEAR == 2002 | YEAR == 2003 | YEAR == 2004 |
      YEAR == 2005 | YEAR == 2006 | YEAR == 2007 | YEAR == 2008 | YEAR == 2009 ~ 1, 
      YEAR == 2010 | YEAR == 2011 | YEAR == 2012 | YEAR == 2013 | YEAR == 2014 | 
      YEAR == 2015 | YEAR == 2016 | YEAR == 2017 | YEAR == 2018  ~ 2))
nhis_subset_decade$decade <- factor(nhis_subset_decade$decade,
                                          levels = c(1,2),
                                          labels = c("2000-2009","2010-2018"))

nhis_subset_recoded <- nhis_subset_decade

# Review demographics of the sample adult subset (so can compare to overall population demographics)
sample_adults_by_sex_race_age <- nhis_subset_recoded %>% 
  count(SEX, age_3_cats, race_6_cats) %>%
  mutate(percent = n/sum(n)*100) %>%
  group_by(SEX) %>%
  arrange(desc(percent), .by_group = TRUE)

## Review missing data

# Overall
nhis_subset_recoded %>%
  dplyr::select(ALCSTAT1, age_3_cats, race_6_cats, SEX, education_3_cats, income) %>%
  md.pattern(rotate.names = TRUE)

# BY SEX
missing_data_sex <- nhis_subset_recoded %>% 
  group_by(SEX) %>%
  mutate(n=1) %>%
  mutate(total_pop = sum(n), 
            Age_NA = sum(is.na(AGE)), 
            Educ_NA = sum(is.na(EDUCREC2)),
            Sex_orien_NA = sum(is.na(SEXORIEN)),
            Income_NA = sum(is.na(income)),
            Alc_status_NA = sum(is.na(ALCSTAT1)),
            birth_year_NA = sum(is.na(birth_year)),
            race_NA = sum(is.na(race_6_cats)),
            # Calculate percentages for variables with some missing data:
            perc_missing_age = (Age_NA/total_pop)*100,
            perc_missing_Alc_status = (Alc_status_NA/total_pop)*100,
            perc_missing_edu = (Educ_NA/total_pop)*100,
            perc_missing_income = (Income_NA/total_pop)*100,
            perc_missing_sex_orien = (Sex_orien_NA/total_pop)*100,
            perc_missing_birth_year = (birth_year_NA/total_pop)*100,
            perc_missing_race = (race_NA/total_pop*100))%>% 
  dplyr::select(SEX, perc_missing_birth_year, perc_missing_income, perc_missing_edu, perc_missing_sex_orien, perc_missing_race, perc_missing_Alc_status) %>%
  unique()

# BY AGE
missing_data_age <- nhis_subset_recoded %>% 
  group_by(age_3_cats) %>%
  mutate(n=1) %>%
  summarise(total_pop = sum(n), 
            Educ_NA = sum(is.na(EDUCREC2)),
            Sex_orien_NA = sum(is.na(SEXORIEN)),
            Income_NA = sum(is.na(income)),
            Alc_status_NA = sum(is.na(ALCSTAT1)),
            birth_year_NA = sum(is.na(birth_year)),
            race_NA = sum(is.na(race_6_cats)),
            # Calculate percentages for variables with some missing data:
            perc_missing_edu = (Educ_NA/total_pop)*100, 
            perc_missing_sex_orien = (Sex_orien_NA/total_pop)*100,
            perc_missing_income = (Income_NA/total_pop)*100,
            perc_missing_Alc_status = (Alc_status_NA/total_pop)*100,
            perc_missing_birth_year = (birth_year_NA/total_pop)*100,
            perc_missing_race = (race_NA/total_pop*100)) %>% 
  dplyr::select(age_3_cats, perc_missing_birth_year, perc_missing_income, perc_missing_edu, perc_missing_sex_orien, perc_missing_race, perc_missing_Alc_status) %>%
  unique()

# BY RACE
missing_data_race <- nhis_subset_recoded %>% 
  group_by(race_6_cats) %>%
  mutate(n=1) %>%
  summarise(total_pop = sum(n), 
            Age_NA = sum(is.na(AGE)), 
            Educ_NA = sum(is.na(EDUCREC2)),
            Sex_orien_NA = sum(is.na(SEXORIEN)),
            Income_NA = sum(is.na(income)),
            Alc_status_NA = sum(is.na(ALCSTAT1)),
            birth_year_NA = sum(is.na(birth_year)),
            # Calculate percentages for variables with some missing data:
            perc_missing_age = (Age_NA/total_pop)*100,
            perc_missing_edu = (Educ_NA/total_pop)*100, 
            perc_missing_sex_orien = (Sex_orien_NA/total_pop)*100,
            perc_missing_income = (Income_NA/total_pop)*100,
            perc_missing_Alc_status = (Alc_status_NA/total_pop)*100,
            perc_missing_birth_year = (birth_year_NA/total_pop)*100) %>%
  dplyr::select(race_6_cats, perc_missing_age, perc_missing_birth_year, perc_missing_income, perc_missing_edu, perc_missing_sex_orien, perc_missing_Alc_status) %>%
  unique()

# BY SES
missing_data_ses <- nhis_subset_recoded %>% 
  group_by(education_5_cats) %>%
  mutate(n=1) %>%
  summarise(total_pop = sum(n), 
            Age_NA = sum(is.na(AGE)), 
            Sex_orien_NA = sum(is.na(SEXORIEN)),
            Income_NA = sum(is.na(income)),
            Alc_status_NA = sum(is.na(ALCSTAT1)),
            birth_year_NA = sum(is.na(birth_year)),
            race_NA = sum(is.na(race_6_cats)),
            # Calculate percentages for variables with some missing data:
            perc_missing_age = (Age_NA/total_pop)*100,
            perc_missing_sex_orien = (Sex_orien_NA/total_pop)*100,
            perc_missing_income = (Income_NA/total_pop)*100,
            perc_missing_Alc_status = (Alc_status_NA/total_pop)*100,
            perc_missing_birth_year = (birth_year_NA/total_pop)*100,
            perc_missing_race = (race_NA/total_pop*100)) %>% 
  dplyr::select(education_5_cats, perc_missing_age, perc_missing_birth_year, perc_missing_income, perc_missing_sex_orien, perc_missing_race, perc_missing_Alc_status) %>%
  unique()

# BY YEAR
missing_data_year <- nhis_subset_recoded %>% 
  group_by(YEAR) %>%
  mutate(n=1) %>%
  summarise(total_pop = sum(n), 
            Age_NA = sum(is.na(AGE)), 
            Educ_NA = sum(is.na(EDUCREC2)),
            Sex_orien_NA = sum(is.na(SEXORIEN)),
            Income_NA = sum(is.na(income)),
            Alc_status_NA = sum(is.na(ALCSTAT1)),
            birth_year_NA = sum(is.na(birth_year)),
            race_NA = sum(is.na(race_6_cats)),
            # Calculate percentages for variables with some missing data:
            perc_missing_edu = (Educ_NA/total_pop)*100, 
            perc_missing_age = (Age_NA/total_pop)*100,
            perc_missing_sex_orien = (Sex_orien_NA/total_pop)*100,
            perc_missing_income = (Income_NA/total_pop)*100,
            perc_missing_Alc_status = (Alc_status_NA/total_pop)*100,
            perc_missing_birth_year = (birth_year_NA/total_pop)*100,
            perc_missing_race = (race_NA/total_pop*100)) %>% 
  dplyr::select(YEAR, perc_missing_age, perc_missing_birth_year, perc_missing_income, perc_missing_sex_orien, perc_missing_race, perc_missing_edu, perc_missing_Alc_status) %>%
  unique()

## Review missing consumption patterns data for drinkers:
drinkers_missing_consumption <- nhis_subset_recoded %>% 
  filter(ALCSTAT1==3) %>% 
  group_by(SEX, age_3_cats, race_6_cats, education_3_cats) %>%
  mutate(n=1) %>%
  summarise(total_pop = sum(n), 
            ALC_AMT_NA = sum(is.na(ALCAMT)),
            ALC_over5_NA = sum(is.na(ALC5UPYR)),
            ALC_DAYS_NA = sum(is.na(ALCDAYSYR)),
            perc_missing_ALC_amt = (ALC_AMT_NA/total_pop)*100,
            perc_missing_ALC_over5 = (ALC_over5_NA/total_pop)*100,
            perc_missing_ALC_days_yr = (ALC_DAYS_NA/total_pop)*100) %>%
    arrange(perc_missing_ALC_amt)

## Drop individuals missing essential data (education & alc status)
edu_variables <- c("education_3_cats", "education_4_cats", "education_5_cats")
nhis_subset_dropped_edu_na <- remove_na(nhis_subset_recoded, all_of(edu_variables)) # 545,938
nhis_subset_dropped_alcstat_na <- remove_na(nhis_subset_dropped_edu_na, "ALCSTAT1")

## Drop individuals who drink, but who are missing information on consumption patterns
drinkers <- subset(nhis_subset_dropped_alcstat_na, ALCSTAT1==3)
non_drinkers <- subset(nhis_subset_dropped_alcstat_na, ALCSTAT1==1| ALCSTAT1==2)
variables <- c("ALCAMT","ALC5UPYR","ALCDAYSYR")
drinkers_dropped_na <- remove_na(drinkers, variables)
nhis_subset_dropped_alc_na <- rbind(drinkers_dropped_na, non_drinkers)

## Estimate average grams of alcohol use per person and assign as a new variable ("alc_daily_g")
#(Expanded Quantity/Frequency (QF) Approach & assuming standard drink size)
nhis_subset_grams_alc <- assign_grams_alcohol(nhis_subset_dropped_alc_na)

# Create sub-categories of alcohol use based on average alcohol consumption (grams):
nhis_subset_alc_cats <- recode_alc_cats(nhis_subset_grams_alc)

nhis_subset_alc_cats$alc_4_cats <- factor(nhis_subset_alc_cats$alc_4_cats,
                                          levels = c(1,2,3,4),
                                          labels = c("Abstainers (lifetime abstainers & former drinkers)", 
                                                     "Category I", "Category II", "Category III"))

nhis_subset_alc_cats$alc_5_cats <- factor(nhis_subset_alc_cats$alc_5_cats,
                                          levels = c(1,2,3,4,5),
                                          labels = c("lifetime abstainers","former drinkers",
                                                     "Category I", "Category II", "Category III"))

nhis_subset_alc_cats$alc_6_cats <- factor(nhis_subset_alc_cats$alc_6_cats,
                                          levels = c(1,2,3,4,5,6),
                                          labels = c("lifetime abstainers","former drinkers",
                                                     "Category I", "Category II", "Category III","Category IV"))

nhis_subset_alc_cats$ALCSTAT1 <- factor(nhis_subset_alc_cats$ALCSTAT1,
                                      levels = c(1,2,3),
                                      labels = c("Lifetime abstainer", "Former drinker", "Current drinker"))

## Convert sex to a factor
nhis_subset_alc_cats$SEX <- factor(nhis_subset_alc_cats$SEX,
                                      levels = c(1,2),
                                      labels = c("Male", "Female"))

# Check face validity of new alcohol variables:
nhis_subset_alc_cats %>%
  group_by(alc_5_cats) %>% 
  summarise(mean(alc_daily_g), mean(ALCDAYSYR), mean(ALCAMT), mean(ALC5UPYR))

## Review consistency of alcohol data
# Classified as inconsistent if:
# a) individuals reporting number of days drinking 5 units+ as more than the total number of days drinking
# b) individuals report drinking > 5 drinks on average, but also report never drinking more than 5 drinks

# Add column with a binary variable for whether alc. information is missing or not
data_with_inconsistancies <- nhis_subset_alc_cats %>%
  mutate(inconsistent_alc = if_else(
    ALC5UPYR > ALCDAYSYR | ALC5UPYR ==0 & ALCAMT >= 5, 1, 0))

# Check no abstainers classified as inconsistent
data_with_inconsistancies %>% 
  filter(inconsistent_alc==1) %>% 
  group_by(alc_4_cats) %>% 
  summarise(n())

# Calculate number of drinkers with inconsistent alcohol information and calculate as a % of total drinkers
inconsistent_drinkers_n <- data_with_inconsistancies %>% 
  filter(ALCSTAT1=="Current drinker" & inconsistent_alc==1) %>%
  nrow()

consistent_drinkers_n <- data_with_inconsistancies %>% 
  filter(ALCSTAT1=="Current drinker" & inconsistent_alc==0) %>%
  nrow()

inconsistent_drinkers_n/(inconsistent_drinkers_n + consistent_drinkers_n)*100 # 0.8%

# Drop people with inconsistent alcohol data
nhis_alc_clean <- data_with_inconsistancies %>% filter(inconsistent_alc==0)

## Cap consumption at 200grams
nhis_alc_clean <- nhis_alc_clean %>%
    mutate(alc_daily_g_capped_200 = if_else(alc_daily_g > 200, 200, alc_daily_g))

# Check face validity of estimate:
mean(nhis_alc_clean$alc_daily_g) # 5
median(nhis_alc_clean$alc_daily_g) # 0.3
mean(nhis_alc_clean$alc_daily_g_capped_200) # 4.9
median(nhis_alc_clean$alc_daily_g_capped_200) # 0.3

# Check raw means by intersections
raw_means_per_group <- nhis_alc_clean %>% 
  group_by(SEX, age_diaz, race_6_cats, education_3_cats) %>% 
  summarise(mean = mean(alc_daily_g)) # max 26 grams

raw_means_per_group_capped <- nhis_alc_clean %>% 
  group_by(SEX, age_diaz, race_6_cats, education_3_cats) %>% 
  summarise(mean = mean(alc_daily_g_capped_200)) # max 26 grams

ggplot(nhis_alc_clean, aes(x=alc_daily_g_capped_200), y) + 
  geom_histogram() + 
  xlab("Daily grams of alcohol") +
  ylab("Frequency") +
ggtitle("Raw distribution of daily grams alcohol, capped, all sample adults")
ggsave(paste0(outputs,"analytic sample/raw_distribution_daily_grams_full_sample.png"), dpi=300, width=33, height=19, units="cm")

## TRANSFORM ALC DAILY GRAMS

# Consider data transformation for alc daily grams
nhis_alc_clean <- nhis_alc_clean %>% 
  mutate(new_grams = alc_daily_g_capped_200 + 0.02)# add half of the smallest grams value (for drinkers) to zero values
# Check new variable
mean_new_grams <- nhis_alc_clean %>% 
  group_by(SEX, age_diaz, race_6_cats, education_3_cats) %>% 
  summarise(mean = mean(new_grams)) # maximum mean is 26.07 grams

# Check recommended lambda with boxcox
b <- MASS::boxcox(lm(nhis_alc_clean$new_grams ~ 1))
lambda <- b$x[which.max(b$y)] # -0.06
lambda2 <- forecast::BoxCox.lambda(nhis_alc_clean$new_grams)  # -0.07

# As both suggested lambda are close to 0, log transform:
nhis_alc_clean$capped_daily_grams_log <- log(nhis_alc_clean$new_grams)
# Check new variable
mean_log_grams <- nhis_alc_clean %>% 
  group_by(SEX, age_diaz, race_6_cats, education_3_cats) %>% 
  summarise(mean = mean(capped_daily_grams_log))

# Distribution plot 
ggplot(nhis_alc_clean, aes(x=capped_daily_grams_log), y) + geom_histogram() + 
  ggtitle("Distribution of estimated daily grams post transformation, full sample")+ 
  xlab("Daily grams of alcohol, post transformation") +
  ylab("Frequency")
ggsave(paste0(outputs,"analytic sample/transformed_distribution_daily_grams_full_sample.png"), dpi=300, width=33, height=19, units="cm")

## SAVE CLEANED DATA

# Save full cleaned data
saveRDS(nhis_alc_clean, paste0(inputs,"nhis_alc_clean_full_sample.RDS"))

# Save subset of drinkers only 
nhis_alc_clean_drinkers <- nhis_alc_clean %>%
  filter(ALCSTAT1=="Current drinker") 

saveRDS(nhis_alc_clean_drinkers, paste0(inputs,"nhis_alc_clean_drinkers_only.RDS"))

# Distribution plot drinkers only
ggplot(nhis_alc_clean_drinkers, aes(x=capped_daily_grams_log), y) + geom_histogram() + 
  ggtitle("Distribution of estimated daily grams post transformation, drinkers only")+ 
  xlab("Daily grams of alcohol, post transformation") +
  ylab("Frequency")
ggsave(paste0(outputs,"analytic sample/transformed_distribution_daily_grams_drinkersonly.png"), dpi=300, width=33, height=19, units="cm")



## READ IN CLEANED DATA
nhis_alc_clean <- readRDS(paste0(inputs,"nhis_alc_clean_full_sample.RDS"))
nhis_alc_clean_drinkers <- readRDS(paste0(inputs,"nhis_alc_clean_drinkers_only.RDS"))

# View the analytic sample size if sample weights were used
sum(nhis_alc_clean_drinkers$SAMPWEIGHT) # v large sample if using weights

# View raw and transformed distributions for drinkers only

ggplot(nhis_alc_clean_drinkers, aes(x=alc_daily_g_capped_200), y) + 
  geom_histogram() + 
  ylim(0, 100000)+
  xlab("Daily grams of alcohol") +
  ylab("Frequency") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))
ggsave(paste0(outputs,"Cleaned tables for grams manuscript/Supplementary tables & figures/SF1. Raw distribution of daily grams.png"), dpi=300, width=33, height=19, units="cm")

ggplot(nhis_alc_clean_drinkers, aes(x=capped_daily_grams_log), y) + geom_histogram() + 
  xlab("Daily grams of alcohol, post transformation") +
  ylab("Frequency")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))
ggsave(paste0(outputs,"Cleaned tables for grams manuscript/Supplementary tables & figures/SF2. Log-transformed distribution of daily grams.png"), dpi=300, width=33, height=19, units="cm")
