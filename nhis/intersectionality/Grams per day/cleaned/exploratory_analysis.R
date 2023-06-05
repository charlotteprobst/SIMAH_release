# Set wd
setwd("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_code/nhis/intersectionality")

# Read in data:
nhis_alc_clean <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/nhis_alc_clean_full_sample.RDS")

# Read in necessary R packages & functions
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ragg)

# Read in necessary functions
source("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_code/nhis/intersectionality/functions/remove_na.R")

# Set default theme for plots:
theme_set(theme_bw(base_size = 12))

# Summary stats by unitary demographic factors

#Full sample
age <- nhis_alc_clean %>% group_by(age_3_cats) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste("age",age_3_cats))
sex <- nhis_alc_clean %>% group_by(SEX) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(SEX))
race <- nhis_alc_clean %>% group_by(race_5_cats) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(race_5_cats))
edu <- nhis_alc_clean %>% group_by(education_3_cats) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(education_3_cats))
decade <- nhis_alc_clean %>% group_by(decade) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(decade))

full_sample_demographics <- bind_rows(age,sex, race, edu, decade) %>% select(Characteristic, n, percent)
saveRDS(full_sample_demographics, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/full_sample_demographics.RDS")

# Drinkers only
nhis_alc_clean %>% filter(ALCSTAT1 == "Current drinker") %>% count()
drinkers_age <- nhis_alc_clean %>% filter(ALCSTAT1 == "Current drinker") %>% group_by(age_3_cats) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste("age",age_3_cats))
drinkers_sex <- nhis_alc_clean %>% filter(ALCSTAT1 == "Current drinker") %>% group_by(SEX) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(SEX))
drinkers_race <- nhis_alc_clean %>% filter(ALCSTAT1 == "Current drinker") %>% group_by(race_5_cats) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(race_5_cats))
drinkers_edu <- nhis_alc_clean %>% filter(ALCSTAT1 == "Current drinker") %>% group_by(education_3_cats) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(education_3_cats))
drinkers_decade <- nhis_alc_clean %>% filter(ALCSTAT1 == "Current drinker") %>% group_by(decade) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(decade))

drinkers_demographics <- bind_rows(drinkers_age,drinkers_sex, drinkers_race, drinkers_edu, drinkers_decade) %>% select(Characteristic, n, percent)
saveRDS(drinkers_demographics, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/results tables/drinkers_demographics.RDS")

# Compare median & IQR of average consumption for full sample and drinkers only (median as data skewed)
nhis_alc_clean %>% summarise(median(alc_daily_g), IQR(alc_daily_g))
nhis_alc_clean %>% filter(ALCSTAT1 == "Current drinker") %>% summarise(median(alc_daily_g), IQR(alc_daily_g)) 

# Comparison of education status by age category  - to check that each age category contains all educational status 
nhis_alc_clean %>% filter(age_3_cats=="18-24") %>% 
  group_by(education_3_cats) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percent=n/sum(n)*100)

nhis_alc_clean %>% filter(age_3_cats=="25-69") %>% 
  group_by(education_3_cats) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percent=n/sum(n)*100)

nhis_alc_clean %>% filter(age_3_cats=="70+") %>% 
  group_by(education_3_cats) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percent=n/sum(n)*100)

## Demographics by birth cohort

nhis_alc_clean %>% filter(birth_cohort == "silent") %>% summarise(min(AGE), max(AGE)) # 55-85
nhis_alc_clean %>% filter(birth_cohort == "baby_boomers") %>% summarise(min(AGE), max(AGE)) # 36-70
nhis_alc_clean %>% filter(birth_cohort == "gen_x") %>% summarise(min(AGE), max(AGE)) # 20-51
nhis_alc_clean %>% filter(birth_cohort == "millenials") %>% summarise(min(AGE), max(AGE))# 18-35
nhis_alc_clean %>% filter(birth_cohort == "gen_z") %>% summarise(min(AGE), max(AGE)) # 18-19

nhis_alc_clean %>%
  group_by(birth_cohort) %>%
  count(education_5_cats) %>%
  group_by(birth_cohort) %>%
  mutate(percent = n/sum(n)*100) %>%
  ggplot(aes(x = birth_cohort, y = percent, fill = education_5_cats)) +
  geom_col(position = position_dodge())+
  xlab("birth_cohort") +
  ylab("% of birth cohort") +
  ylim(0,100) +
  ggtitle("Educational group prevelances by birth cohort")
# no people of legal drinking age (21) in gen-z & v. few people with college education in gen_z,
# therefore may not be appropriate to group by cohort

# Summary of drinkers versus non-drinkers by unitary factors
nhis_alc_clean %>%
  group_by(SEX) %>%
  count(ALCSTAT1) %>%
  mutate(percent = n/sum(n)*100) %>%
  ggplot(aes(x = ALCSTAT1, y = percent, fill = SEX)) +
  geom_col(position = position_dodge())+
  xlab("Drinking status") +
  ylab("% of total sex") +
  ylim(0,100) +
  ggtitle("Differences in drinking status by sex")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/drinking_status_by_sex.png", dpi=300, width=33, height=19, units="cm")

nhis_alc_clean %>%
  group_by(race_5_cats) %>%
  count(ALCSTAT1) %>%
  mutate(percent = n/sum(n)*100) %>%
  ggplot(aes(x = ALCSTAT1, y = percent, fill = race_5_cats)) +
  geom_col(position = position_dodge())+
  xlab("Drinking status") +
  ylab("% of total racial/ethnic group") +
  ylim(0,100) +
  theme(axis.text.x = element_text(size=10)) +
  ggtitle("Differences in drinking status by race/ethnicity")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/drinking_status_by_race.png", dpi=300, width=33, height=19, units="cm")

nhis_alc_clean %>%
  group_by(education_5_cats) %>%
  count(ALCSTAT1) %>%
  mutate(percent = n/sum(n)*100) %>%
  ggplot(aes(x = ALCSTAT1, y = percent, fill = education_5_cats)) +
  geom_col(position = position_dodge())+
  xlab("Drinking status") +
  ylab("% of total SES group") +
  theme(axis.text.x = element_text(size=10)) +
  ylim(0,100) +
  ggtitle("Differences in drinking status by SES")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/drinking_status_by_education.png", dpi=300, width=33, height=19, units="cm")

nhis_alc_clean %>%
  group_by(age_3_cats) %>%
  count(ALCSTAT1) %>%
  mutate(percent = n/sum(n)*100) %>%
  ggplot(aes(x = ALCSTAT1, y = percent, fill = age_3_cats)) +
  geom_col(position = position_dodge())+
  xlab("Drinking status") +
  ylab("% of total age group") +
  theme(axis.text.x = element_text(size=10), title=element_text(size=14)) +
  ylim(0,100) +
  ggtitle("Differences in drinking status by age group")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/drinking_status_by_age.png", dpi=300, width=33, height=19, units="cm")

nhis_alc_clean %>%
  group_by(birth_cohort) %>%
  count(ALCSTAT1) %>%
  mutate(percent = n/sum(n)*100) %>%
  ggplot(aes(x = ALCSTAT1, y = percent, fill = birth_cohort)) +
  geom_col(position = position_dodge())+
  xlab("Drinking status") +
  ylab("% of total birth cohort") +
  ylim(0,100) +
  ggtitle("Differences in drinking status by birth cohort")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/drinking_status_by_birth_cohort.png", dpi=300, width=33, height=19, units="cm")

# Distribution of responses to alcohol questions for drinkers

# 1. Number of drinks per occasion
nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  ggplot(aes(x = ALCAMT)) +
  geom_histogram(binwidth=1)+
  xlab("Average drinks per occassion") +
  ylab("Count") +
  xlim(0,20) +
  theme(axis.text.x = element_text(size=10)) +
  ggtitle("Distribution of drinks per occassion, drinkers only")

# 2. Drinking days per year
nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  ggplot(aes(x = ALCDAYSYR)) +
  geom_histogram(binwidth=1)+
  xlab("Number of days per year") +
  ylab("Count") +
  xlim(0,365) +
  theme(axis.text.x = element_text(size=10)) +
  ggtitle("Distribution of number of drinking days per year, all drinkers")

# 3. Drinking days per month
nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  ggplot(aes(x = ALCDAYSMO)) +
  geom_histogram(binwidth=1)+
  xlab("Number of days per month") +
  ylab("Count") +
  xlim(0,31) +
  theme(axis.text.x = element_text(size=10)) +
  ggtitle("Distribution of number of drinking days per month, all drinkers")

# 4. Drinking days per week
nhis_alc_clean %>% 
  filter(ALCSTAT1 == "Current drinker") %>%
  mutate(ALCDAYSWK = dplyr::case_when(
   ALCDAYSWK == 0 ~ 0.1,
   ALCDAYSWK == 10 ~ 1, 
   ALCDAYSWK == 20 ~ 2, 
   ALCDAYSWK == 30 ~ 3, 
   ALCDAYSWK == 40 ~ 4, 
   ALCDAYSWK == 50 ~ 5,
   ALCDAYSWK == 60 ~ 6,
   ALCDAYSWK == 70 ~ 7)) %>%
  ggplot(aes(x = ALCDAYSWK)) +
  geom_bar() +
  scale_x_binned()+
  xlab("Number of days per week") +
  ylab("Count") +
  xlim(0,7.5) +
  theme(axis.text.x = element_text(size=10)) +
  ggtitle("Distribution of number of drinking days per week, drinkers only")

# Number of days drinking > 5 units
nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  ggplot(aes(x = ALC5UPYR)) +
  geom_histogram(binwidth=1)+
  xlab("Number of days per year") +
  ylab("Count") +
  xlim(0,365) +
  ylim(0,20000)+
  theme(axis.text.x = element_text(size=10)) +
  ggtitle("Distribution of number of days a year that drink > 5 drinks (per year), drinkers only")

# Average consumption for drinkers, grouped by sociodemographic factors

# AGE
nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  group_by(SEX, AGE) %>%
  summarise(mean_alc_daily_g = mean(alc_daily_g)) %>%
  ggplot(aes(x=AGE, y= mean_alc_daily_g, color = SEX)) + geom_point() +
  scale_x_continuous(breaks=seq(0,85,5)) +
  scale_y_continuous(breaks=seq(0,12,1)) +
  geom_line() +
  ggtitle("mean daily grams of alcohol for drinkers, by age")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/alc_daily_g_by_age_drinkers_only.png", dpi=300, width=33, height=19, units="cm")

nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  mutate(age_group =
    case_when(
    AGE >= 18 & AGE <=20 ~ "18-20",
    AGE >= 21 & AGE <=23 ~ "21-23",
    AGE >= 24 & AGE <=30 ~ "24-30",
    AGE >= 31 & AGE <=40 ~ "31-40",
    AGE >= 41 & AGE <=50 ~ "41-50",
    AGE >= 51 & AGE <=60 ~ "51-60",
    AGE >= 61 & AGE <=70 ~ "61-70",
    AGE >= 71 & AGE <=80 ~ "71-80",
    AGE >= 81  ~ "81+")) %>%
  group_by(SEX, age_group) %>%
  summarise(mean_alc_daily_g = mean(alc_daily_g)) %>%
ggplot(aes(x=age_group, y=mean_alc_daily_g, colour=SEX)) +
  facet_grid(. ~ SEX) +
  geom_point() + 
  geom_segment( aes(x=age_group, xend=age_group, y=0, yend=mean_alc_daily_g))+
  theme(axis.text.x = element_text(size = 8), legend.position = "none") +
  ggtitle("mean daily grams of alcohol for drinkers, by age group") 
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/alc_daily_g_by_age__groups_drinkers_only.png", dpi=300, width=33, height=19, units="cm")

nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
   group_by(SEX, age_4_cats) %>%
  summarise(mean_alc_daily_g = mean(alc_daily_g)) %>%
  ggplot(aes(x=age_4_cats, y=mean_alc_daily_g, colour=SEX)) +
  facet_grid(. ~ SEX) +
  geom_point() + 
  geom_segment( aes(x=age_4_cats, xend=age_4_cats, y=0, yend=mean_alc_daily_g))+
  theme(axis.text.x = element_text(size = 8), legend.position = "none") +
  ggtitle("mean daily grams of alcohol for drinkers, by age group 4 cats") 
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/alc_daily_g_by_age_4_cats_drinkers_only.png", dpi=300, width=33, height=19, units="cm")


nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  group_by(SEX, age_3_cats) %>%
  summarise(mean_alc_daily_g = mean(alc_daily_g)) %>%
  ggplot(aes(x=age_3_cats, y=mean_alc_daily_g, colour=SEX)) +
  facet_grid(. ~ SEX) +
  geom_point() + 
  geom_segment( aes(x=age_3_cats, xend=age_3_cats, y=0, yend=mean_alc_daily_g))+
  theme(axis.text.x = element_text(size = 8), legend.position = "none") +
  ggtitle("mean daily grams of alcohol for drinkers, by age group 3 cats") 
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/alc_daily_g_by_age_3_cats_drinkers_only.png", dpi=300, width=33, height=19, units="cm")



# Sex
nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  group_by(SEX) %>%
  ggplot(aes(x = SEX, y = alc_daily_g, colour=SEX)) +            
  geom_boxplot()+
  ylim(0,200)
  ggtitle("mean grams of alcohol per day, split by sex - drinkers only")
# NB. removed 200 rows due to y-limit set to 200grams
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/alc_daily_g_by_sex_drinkers_only.png", dpi=300, width=33, height=19, units="cm")

# Race
nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  group_by(race_5_cats) %>%
  ggplot(aes(x = race_5_cats, y = alc_daily_g, colour = race_5_cats)) +            
  geom_boxplot()+
  ylim(0,200) +
  theme(axis.text.x=element_blank()) +
  ggtitle("mean grams of alcohol per day, split by race")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/alc_daily_g_by_race_drinkers_only.png", dpi=300, width=33, height=19, units="cm")

# Education
nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  group_by(education_5_cats) %>%
  ggplot(aes(x = as.factor(education_5_cats), y = alc_daily_g, colour = education_5_cats)) +            
  geom_boxplot() +
  ylim(0,20) +
  xlab("educational status") +
  theme(axis.text.x=element_blank()) +
  ggtitle("mean grams of alcohol per day, split by education status")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/alc_daily_g_by_SES_5_cats_drinkers_only.png", dpi=300, width=33, height=19, units="cm")

# Three education categories
nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  group_by(education_3_cats) %>%
  ggplot(aes(x = as.factor(education_3_cats), y = alc_daily_g, colour = education_3_cats)) +            
  geom_boxplot()+
  ylim(0,50) +
  ggtitle("mean grams of alcohol per day, split by education status (3 cats) - drinkers only")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/alc_daily_g_by_SES_3_cats_drinkers_only.png", dpi=300, width=33, height=19, units="cm")

# Sexual orientation
nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  remove_na("SEXORIEN") %>%
  group_by(SEXORIEN) %>%
  ggplot(aes(x = SEXORIEN, y = alc_daily_g, colour=SEXORIEN)) +            
  geom_boxplot()+
  ylim(0,200) +
  ggtitle("mean grams of alcohol per day, split by sexual orientation - drinkers only")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/alc_daily_g_by_sex_orien_drinkers_only.png", dpi=300, width=33, height=19, units="cm")

# Year
nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  group_by(SEX, YEAR) %>%
  summarise(mean_alc_daily_g = mean(alc_daily_g)) %>%
  ggplot(aes(x=YEAR, y= mean_alc_daily_g, color = SEX)) + geom_point(size=3) +
  geom_line() +
  ggtitle("mean grams of alcohol per day, split by year and sex (drinkers only)")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/alc_daily_g_by_year_drinkers_only.png", dpi=300, width=33, height=19, units="cm")

par(mfrow=c(1,2))
nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  group_by(decade, SEX) %>%
  summarise(mean_alc_daily_g = mean(alc_daily_g)) %>%
ggplot(aes(x=decade, y=mean_alc_daily_g, colour=SEX)) +
  facet_grid(. ~ SEX) +
  geom_point() + 
  geom_segment( aes(x=decade, xend=decade, y=0, yend=mean_alc_daily_g))+
  theme(axis.text.x = element_text(size = 8)) 
  ggtitle("mean grams of alcohol per day, split by decade and sex - drinkers only")
  ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/alc_daily_g_by_decade_drinkers_only.png", dpi=300, width=33, height=19, units="cm")

# Birth year
nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  group_by(SEX, birth_year) %>%
  summarise(mean_alc_daily_g = mean(alc_daily_g)) %>%
  ggplot(aes(x=birth_year, y= mean_alc_daily_g, color = SEX)) + geom_point() + 
  scale_x_continuous(breaks=seq(1910,2000,5)) +
  scale_y_continuous(breaks=seq(0,14,1)) +
  geom_line() +
  theme(axis.text.x = element_text(size = 8), title = element_text(size=12)) +
  ggtitle("mean daily grams of alcohol for drinkers, by birth year") 
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/alc_daily_g_by_birth_year_drinker_only.png", dpi=300, width=33, height=19, units="cm")

# Birth cohorts
par(mfrow=c(1,2))
nhis_alc_clean %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  group_by(birth_cohort) %>%
  summarise(mean_alc_daily_g = mean(alc_daily_g)) %>%
ggplot(aes(x=birth_cohort, y=mean_alc_daily_g, colour=birth_cohort)) +
  geom_point() + 
  geom_segment( aes(x=birth_cohort, xend=birth_cohort, y=0, yend=mean_alc_daily_g))+
  theme(axis.text.x = element_text(size = 8))+
  ggtitle("mean grams of alcohol per day, split by birth_cohort - drinkers only") 
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/plots/alc_daily_g_by_birth_cohort_drinkers_only.png", dpi=300, width=33, height=19, units="cm")

# Based on the exploratory analysis, the following appear important to include:
# Sex: 2 cats (data availability)
# Race: ideally 5 categories as drinking patterns of Asian people very different from remaining people in 'other' group
# Age: at least 3 categories to capture peak around age 21 and decline after age 75
# Education: 3 education categories appropriate to capture increase with education (nb. number of years of college is not available in nhis)
# Time effects: More appropriate to include period effects than cohort as genz has inadequate age range and numbers of people of drinking age
# NB. Sexual oritentation also appears important but high levels of missing data