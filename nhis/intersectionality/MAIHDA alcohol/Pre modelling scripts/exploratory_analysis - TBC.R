######################################################################## Set-up
setwd("C:/Users/cmp21seb/Documents/SIMAH/")
code <- "SIMAH_code/nhis/intersectionality/MAIHDA alcohol/"
inputs <- "SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/inputs/"
models <- "SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/models/"
outputs <- "SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/outputs/"

# Read in data:
nhis_alc_clean <- readRDS(paste0(inputs,"/nhis_alc_clean_full_sample.RDS"))

# Read in necessary R packages & functions
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ragg)

# Read in necessary functions
source(paste0(code,"functions/remove_na.R"))

# Set default theme for plots:
theme_set(theme_bw(base_size = 12))

# Drop individuals age <21
data_0 <- nhis_alc_clean %>% filter(age_diaz!="18-20")

# Keep only the 6 selected race and ethnicity groups
data_1 <- data_0 %>% filter(!is.na(race_6_cats))

# Generate intersections
data_2 <- data_1 %>% 
  group_by(SEX, race_6_cats, education_3_cats, age_diaz) %>% 
  mutate(intersections = cur_group_id()) %>%
  mutate(intersectional_names = as.character(paste(SEX, age_diaz, race_6_cats, education_3_cats)))

# Summary stats by unitary demographic factors

#Full sample
age <- data_2 %>% group_by(age_3_cats) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste("age",age_3_cats))
age_diaz <- data_2 %>% group_by(age_diaz) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste("age",age_diaz))
sex <- data_2 %>% group_by(SEX) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(SEX))
race_5_cats <- data_2 %>% group_by(race_5_cats) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(race_5_cats))
race_6_cats <- data_2 %>% group_by(race_6_cats) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(race_6_cats))
edu <- data_2 %>% group_by(education_3_cats) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(education_3_cats))

new_spec_demographics <- bind_rows(age_diaz,sex, race_6_cats, edu) %>% dplyr::select(Characteristic, n, percent)
saveRDS(new_spec_demographics, paste0(outputs,"full_sample_demographics.RDS"))
write.csv(new_spec_demographics, paste0(outputs,"full_sample_demographics.csv"))

# Drinkers only
data_2 %>% filter(ALCSTAT1 == "Current drinker") %>% count()
drinkers_age <- data_2 %>% filter(ALCSTAT1 == "Current drinker") %>% group_by(age_3_cats) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste("age",age_3_cats))
drinkers_age_diaz <- data_2 %>% filter(ALCSTAT1 == "Current drinker") %>% group_by(age_diaz) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste("age",age_diaz))
drinkers_sex <- data_2 %>% filter(ALCSTAT1 == "Current drinker") %>% group_by(SEX) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(SEX))
drinkers_race_5_cats <- data_2 %>% filter(ALCSTAT1 == "Current drinker") %>% group_by(race_5_cats) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(race_5_cats))
drinkers_race_6_cats <- data_2 %>% filter(ALCSTAT1 == "Current drinker") %>% group_by(race_6_cats) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(race_6_cats))
drinkers_edu <- data_2 %>% filter(ALCSTAT1 == "Current drinker") %>% group_by(education_3_cats) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(education_3_cats))
drinkers_decade <- data_2 %>% filter(ALCSTAT1 == "Current drinker") %>% group_by(decade) %>% count %>% ungroup() %>% mutate(percent=n/sum(n)*100, Characteristic=paste(decade))

drinkers_demographics <- bind_rows(drinkers_age_diaz,drinkers_sex, drinkers_race_6_cats, drinkers_edu) %>% 
  dplyr::select(Characteristic, n, percent)
saveRDS(drinkers_demographics, paste0(outputs,"drinkers_demographics.RDS"))
write.csv(drinkers_demographics, paste0(outputs,"drinkers_demographics.csv"))

# Compare median & IQR of average consumption for full sample and drinkers only (median as data skewed)
data_2 %>% summarise(median(alc_daily_g), IQR(alc_daily_g))
data_2 %>% filter(ALCSTAT1 == "Current drinker") %>% summarise(median(alc_daily_g), IQR(alc_daily_g)) 

# Summary of drinkers versus non-drinkers by unitary factors
data_2 %>%
  group_by(SEX) %>%
  count(ALCSTAT1) %>%
  mutate(percent = n/sum(n)*100) %>%
  ggplot(aes(x = ALCSTAT1, y = percent, fill = SEX)) +
  geom_col(position = position_dodge())+
  xlab("Drinking status") +
  ylab("% of total sex") +
  ylim(0,100) +
  ggtitle("Differences in drinking status by sex")
ggsave(paste0(outputs,"analytic sample/drinking_status_by_sex.png"), dpi=300, width=33, height=19, units="cm")

data_2 %>%
  group_by(race_6_cats) %>%
  count(ALCSTAT1) %>%
  mutate(percent = n/sum(n)*100) %>%
  ggplot(aes(x = ALCSTAT1, y = percent, fill = race_6_cats)) +
  geom_col(position = position_dodge())+
  xlab("Drinking status") +
  ylab("% of total racial/ethnic group") +
  ylim(0,100) +
  theme(axis.text.x = element_text(size=10)) +
  ggtitle("Differences in drinking status by Race and Ethnicity")
ggsave(paste0(outputs,"analytic sample/drinking_status_by_race_6_cats.png"), dpi=300, width=33, height=19, units="cm")

nhis_alc_clean %>%
  group_by(education_3_cats) %>%
  count(ALCSTAT1) %>%
  mutate(percent = n/sum(n)*100) %>%
  ggplot(aes(x = ALCSTAT1, y = percent, fill = education_3_cats)) +
  geom_col(position = position_dodge())+
  xlab("Drinking status") +
  ylab("% of total SES group") +
  theme(axis.text.x = element_text(size=10)) +
  ylim(0,100) +
  ggtitle("Differences in drinking status by education")
ggsave(paste0(outputs,"analytic sample/drinking_status_by_education_3_cats.png"), dpi=300, width=33, height=19, units="cm")

nhis_alc_clean %>%
  group_by(age_diaz) %>%
  count(ALCSTAT1) %>%
  mutate(percent = n/sum(n)*100) %>%
  ggplot(aes(x = ALCSTAT1, y = percent, fill = age_diaz)) +
  geom_col(position = position_dodge())+
  xlab("Drinking status") +
  ylab("% of total age group") +
  theme(axis.text.x = element_text(size=10), title=element_text(size=14)) +
  ylim(0,100) +
  ggtitle("Differences in drinking status by age group")
ggsave(paste0(outputs,"analytic sample/drinking_status_by_age.png"), dpi=300, width=33, height=19, units="cm")

# Average consumption, grouped by sociodemographic factors

# AGE & SEX
data_2 %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  group_by(SEX, AGE) %>%
  summarise(mean_alc_daily_g = mean(alc_daily_g)) %>%
  ggplot(aes(x=AGE, y= mean_alc_daily_g, color = SEX)) + geom_point() +
  scale_x_continuous(breaks=seq(0,95,5)) +
  scale_y_continuous(breaks=seq(0,20,1)) +
  geom_line() +
  ggtitle("mean daily grams of alcohol for drinkers, by age and sex")
ggsave(paste0(outputs,"analytic sample/alc_daily_g_by_age_and_sex_drinkers_only.png"), dpi=300, width=33, height=19, units="cm")

data_2 %>%
  group_by(SEX, age_3_cats) %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  summarise(mean_alc_daily_g = mean(alc_daily_g)) %>%
  ggplot(aes(x=age_3_cats, y=mean_alc_daily_g, colour=SEX)) +
  facet_grid(. ~ SEX) +
  geom_point() + 
  geom_segment( aes(x=age_3_cats, xend=age_3_cats, y=0, yend=mean_alc_daily_g))+
  theme(axis.text.x = element_text(size = 8), legend.position = "none") +
  ggtitle("mean daily grams of alcohol for drinkers, by age group 3 cats") 
ggsave(paste0(outputs,"analytic sample/alc_daily_g_by_age_3_cats_drinkers_18.png"), dpi=300, width=33, height=19, units="cm")

data_2 %>%
  group_by(SEX, age_diaz) %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  summarise(mean_alc_daily_g = mean(alc_daily_g)) %>%
  ggplot(aes(x=age_diaz, y=mean_alc_daily_g, colour=SEX)) +
  facet_grid(. ~ SEX) +
  geom_point() + 
  geom_segment( aes(x=age_diaz, xend=age_diaz, y=0, yend=mean_alc_daily_g))+
  theme(axis.text.x = element_text(size = 8), legend.position = "none") +
  ggtitle("mean daily grams of alcohol for drinkers, by age") 
ggsave(paste0(outputs,"analytic sample/alc_daily_g_by_age_3_cats_drinkers_only.png"), dpi=300, width=33, height=19, units="cm")

# Race
data_2 %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  group_by(race_6_cats) %>%
  ggplot(aes(x = race_6_cats, y = alc_daily_g_capped_200, colour = race_6_cats)) +            
  geom_boxplot()+
  ylim(0,50) +
  theme(axis.text.x=element_blank()) +
  ggtitle("mean grams of alcohol per day, by Race and Ethnicity, drinkers only")
ggsave(paste0(outputs,"analytic sample/alc_daily_g_by_race_6_cats_drinkers_only.png"), dpi=300, width=33, height=19, units="cm")

# Education
data_2 %>%
  group_by(education_3_cats) %>%
  filter(ALCSTAT1 == "Current drinker") %>%
  ggplot(aes(x = as.factor(education_3_cats), y = alc_daily_g_capped_200, colour = education_3_cats)) +            
  geom_boxplot() +
  xlab("educational status") +
  ylim(0,50)
  theme(axis.text.x=element_blank()) +
  ggtitle("mean grams of alcohol per day, by education status, drinkers only")
ggsave(paste0(outputs,"analytic sample/alc_daily_g_by_SES_3_cats_drinkers_only.png"), dpi=300, width=33, height=19, units="cm")

# Sexual orientation
data_2 %>%
  remove_na("SEXORIEN") %>%
  group_by(SEXORIEN) %>%
  ggplot(aes(x = SEXORIEN, y = alc_daily_g, colour=SEXORIEN)) +            
  geom_boxplot()+
  ylim(0,50) +
  ggtitle("mean grams of alcohol per day, by sexual orientation, full sample")
ggsave(paste0(outputs,"analytic sample/alc_daily_g_by_sex_orien_full_sample.png"), dpi=300, width=33, height=19, units="cm")

# Year
data_2 %>%
  group_by(SEX, YEAR) %>%
  summarise(mean_alc_daily_g = mean(alc_daily_g)) %>%
  ggplot(aes(x=YEAR, y= mean_alc_daily_g, color = SEX)) + 
  geom_point(size=3) +
  geom_line() +
  ggtitle("mean grams of alcohol per day, by year and sex")
ggsave(paste0(outputs,"analytic sample/alc_daily_g_by_year_full_sample.png"), dpi=300, width=33, height=19, units="cm")

# Table of drinking status by intersectional group
drink_status_table <- data_2 %>%
  group_by(intersectional_names, ALCSTAT1) %>%
  count %>% ungroup() %>% 
  group_by(intersectional_names) %>%
  mutate(percent=n/sum(n)*100)

drink_status_wider <- drink_status_table %>% 
  pivot_wider(names_from = ALCSTAT1, values_from = c(n, percent), values_fill = 0) 
write.csv(drink_status_wider, paste0(outputs,"analytic sample/drinking_status_by_intersections.csv"))

drink_status_highest_5 <- drink_status_wider %>% ungroup %>% slice_max(`percent_Current drinker`, n = 5)
drink_status_lowest_5 <- drink_status_wider %>% ungroup %>% slice_min(`percent_Current drinker`, n = 5)
drink_status_fives <- rbind(drink_status_highest_5, drink_status_lowest_5)
write.csv(drink_status_fives, paste0(outputs,"analytic sample/drink_status_five_highest_&_lowest_proportion_drinkers.csv"))
