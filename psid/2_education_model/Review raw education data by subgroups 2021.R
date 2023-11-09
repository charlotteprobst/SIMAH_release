# Exploration of education categories by subgroup

library(dplyr)
library(ggplot2)
library(survey)
library(srvyr)
library(tidyverse)
setwd("C:/Users/cmp21seb/Documents/SIMAH")

all_data <- read.csv("SIMAH_workplace/PSID/cleaned data/all_data_1999_2021_excl_non_responders081123.csv")

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

# unweighted
plot_data <- education_no_na %>%
  group_by(year, sex, final_race_using_method_hierarchy, education_cat) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)*100)

ggplot(data=plot_data, aes(x=year, y=prop, colour=education_cat)) + geom_line() + 
  theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + ylim(0,NA) + xlim(1999, 2021)+
  facet_grid(sex~final_race_using_method_hierarchy) + ggtitle("Trends in educational attainment by race and gender - unweighted")

# weighted

# Create a survey design object
survey_design <- education_no_na %>% 
  as_survey_design(weights = individualweight)

# Calculate weighted proportions by year, sex, race and education category
weighted_plot_data <- survey_design %>%
  group_by(year, sex, final_race_using_method_hierarchy, education_cat) %>%
  summarise(proportion= survey_mean()*100)

ggplot(data=weighted_plot_data, aes(x=year, y=proportion, colour=education_cat)) + geom_line() + 
  theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + ylim(0,NA) + xlim(1999, 2021)+
  facet_grid(sex~final_race_using_method_hierarchy) + ggtitle("Trends in educational attainment by race and gender - weighted")

# condensed racial groups & including confidence intervals

weighted_plot_data_ACS <- survey_design %>%
  group_by(year, sex, race_ACS_cats, education_cat) %>%
  summarise(proportion= survey_mean()*100)

ggplot(data=weighted_plot_data_ACS, aes(x=year, y=proportion, colour=education_cat)) + 
  geom_line() + 
  geom_errorbar(aes(ymin = proportion - 1.96 * proportion_se, ymax = proportion + 1.96 * proportion_se), width = 0.2) +
  theme_bw() + 
  theme(legend.title=element_blank(), legend.position="bottom") + 
  ylim(0,NA) + 
  xlim(1999, 2021)+
  facet_grid(sex~race_ACS_cats) + 
  ggtitle("Trends in educational attainment by race (ACS cats) and gender - weighted")

# Explore the strange peak in 2005 by plotting proportions in each racial category 

race_over_time_unweighted <- education_no_na %>%
  group_by(year, race_ACS_cats) %>%
  summarise(n = n()) %>%
  mutate(proportion = n / sum(n)*100)

ggplot(data=race_over_time_unweighted, aes(x=year, y=proportion, colour=race_ACS_cats)) + geom_line() +
  theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + 
  ggtitle("Trends in race over time - unweighted")

race_over_time_weighted <- survey_design %>%
  group_by(year, race_ACS_cats) %>%
  summarise(proportion= survey_mean()*100)

ggplot(data=race_over_time_weighted, aes(x=year, y=proportion, colour=race_ACS_cats)) + geom_line() +
  theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + 
 ggtitle("Trends in race over time - weighted")

