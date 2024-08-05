# Exploration of education categories by subgroup

library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
library(tidyverse)
setwd("C:/Users/cmp21seb/Documents/SIMAH")

all_data <- read.csv("SIMAH_workplace/PSID/cleaned data/psid_data_1999_2021.csv")

n_individuals_all_data <- n_distinct(all_data$uniqueID) # 43,884 individuals in the data

# Generate columns for education as want them named in the graph
plot_data <- all_data %>%
  mutate(education_cat=ifelse(grepl("College", education_cat), "4+ years of college",
                          ifelse(grepl("SomeC", education_cat), "Some college", 
                                 ifelse(grepl("LEHS", education_cat), "High school or less", NA))),
         education_cat=factor(education_cat,levels=c("4+ years of college", "Some college", "High school or less", NA)))

# Generate columbs for race that are comparable to ACS
plot_data <- all_data %>%
  mutate(race_ACS_cats=case_when(final_race_using_method_hierarchy=="white" ~ "White",    
                                 final_race_using_method_hierarchy=="black" ~ "Black",    
                                 final_race_using_method_hierarchy=="hispanic" ~ "Hispanic", 
                                 final_race_using_method_hierarchy=="Native"| final_race_using_method_hierarchy=="Asian/PI" | final_race_using_method_hierarchy=="other" ~ "Other",   
                                 is.na(final_race_using_method_hierarchy) ~ NA))

# Remove education NA
education_no_na <- plot_data %>% filter(!is.na(education_cat))
n_education_individuals <- n_distinct(education_no_na$uniqueID) # 31,924 individuals with education data
n_education_na_individuals <- n_individuals_all_data - n_education_individuals # 11,960 individuals with missing education data

# Plots of educational attainment over time

# Unweighted
plot_data <- education_no_na %>%
  group_by(year, sex, final_race_using_method_hierarchy, education_cat) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)*100)

ggplot(data=plot_data, aes(x=year, y=prop, colour=education_cat)) + geom_line() + 
  theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + ylim(0,NA) + xlim(1999, 2021)+
  facet_grid(sex~final_race_using_method_hierarchy) + ggtitle("Trends in educational attainment by race and gender - unweighted")

# Using condensed racial groups
plot_data_ACS <- education_no_na %>%
  group_by(year, sex, race_ACS_cats, education_cat) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)*100)

ggplot(data=plot_data_ACS, aes(x=year, y=prop, colour=education_cat)) + geom_line() + 
  theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + ylim(0,NA) + xlim(1999, 2021)+
  facet_grid(sex~race_ACS_cats) + ggtitle("Trends in educational attainment by race (ACS cats) and gender, unweighted")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/PSID/Results/Education trends/education_trends_unweighted.png", width = 40, height = 25, units = "cm")

# Weighted

# Replace any weights of NA with zeros
weights_no_na <- education_no_na %>%
  mutate(individualweight_longitudinal = ifelse(is.na(individualweight_longitudinal), 0, individualweight_longitudinal),
         individualweight_cross.sectional = ifelse(is.na(individualweight_cross.sectional), 0, individualweight_cross.sectional))

# LONGITUDINAL WEIGHTS

# Create a survey design object
survey_design_longitudinal <- weights_no_na %>% 
  as_survey_design(weights = individualweight_longitudinal)

# Calculate weighted proportions by year, sex, race and education category
longitudinal_plot_data <- survey_design_longitudinal %>%
  group_by(year, sex, final_race_using_method_hierarchy, education_cat) %>%
  summarise(proportion= survey_mean()*100)

ggplot(data=longitudinal_plot_data, aes(x=year, y=proportion, colour=education_cat)) + geom_line() + 
  theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + ylim(0,NA) + xlim(1999, 2021)+
  facet_grid(sex~final_race_using_method_hierarchy) + ggtitle("Trends in educational attainment by race and gender, using longitudinal weights")

# Using condensed racial groups & including confidence intervals
longitudinal_plot_data_ACS <- survey_design_longitudinal %>%
  group_by(year, sex, race_ACS_cats, education_cat) %>%
  summarise(proportion= survey_mean()*100)

ggplot(data=longitudinal_plot_data_ACS, aes(x=year, y=proportion, colour=education_cat)) + 
  geom_line() + 
  geom_errorbar(aes(ymin = proportion - 1.96 * proportion_se, ymax = proportion + 1.96 * proportion_se), width = 0.2) +
  theme_bw() + 
  theme(legend.title=element_blank(), legend.position="bottom") + 
  ylim(0,NA) + 
  xlim(1999, 2021)+
  facet_grid(sex~race_ACS_cats) + 
  ggtitle("Trends in educational attainment by race (ACS cats) and gender, using longitudinal weights")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/PSID/Results/Education trends/education_trends_longitudinal_weights.png", width = 40, height = 25, units = "cm")

# CROSS SECTIONAL WEIGHTS

# Create a survey design object
survey_design_crosssectional <- education_no_na %>% 
  as_survey_design(weights = individualweight_cross.sectional)

# Calculate weighted proportions by year, sex, race and education category
crosssectional_plot_data <- survey_design_crosssectional %>%
  group_by(year, sex, final_race_using_method_hierarchy, education_cat) %>%
  summarise(proportion= survey_mean()*100)

ggplot(data=crosssectional_plot_data, aes(x=year, y=proportion, colour=education_cat)) + geom_line() + 
  theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + ylim(0,NA) + xlim(1999, 2021)+
  facet_grid(sex~final_race_using_method_hierarchy) + ggtitle("Trends in educational attainment by race and gender - weighted")

# condensed racial groups & including confidence intervals
crosssectional_plot_data_ACS <- survey_design_crosssectional %>%
  group_by(year, sex, race_ACS_cats, education_cat) %>%
  summarise(proportion= survey_mean()*100)

ggplot(data=crosssectional_plot_data_ACS, aes(x=year, y=proportion, colour=education_cat)) + 
  geom_line() + 
  geom_errorbar(aes(ymin = proportion - 1.96 * proportion_se, ymax = proportion + 1.96 * proportion_se), width = 0.2) +
  theme_bw() + 
  theme(legend.title=element_blank(), legend.position="bottom") + 
  ylim(0,NA) + 
  xlim(1999, 2021)+
  facet_grid(sex~race_ACS_cats) + 
  ggtitle("Trends in educational attainment by race (ACS cats) and gender, using cross-sectional weights")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/PSID/Results/Education trends/education_trends_crosssectional_weights.png", width = 40, height = 25, units = "cm")

