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

# Remove education NA
education_no_na <- plot_data %>% filter(!is.na(education_cat))
n_education_individuals <- n_distinct(education_no_na$uniqueID) # 31,924 individuals with education data
n_education_na_individuals <- n_individuals_all_data - n_education_individuals # 11,960 individuals with missing education data

# Plots - unweighted
plot_data <- education_no_na %>%
  group_by(year, sex, final_race_using_method_hierarchy, education_cat) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)*100)

ggplot(data=plot_data, aes(x=year, y=prop, colour=education_cat)) + geom_line() + facet_grid(cols=vars(education_cat), scales="free") +
  theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + ylim(0,NA) + xlim(1999, 2021)+
  facet_grid(sex~final_race_using_method_hierarchy) + ggtitle("Trends in educational attainment by race and gender - unweighted")

# Plots - weighted

# Create a survey design object
survey_design <- education_no_na %>% 
  as_survey_design(weights = individualweight)

# Calculate weighted proportions by year, sex, race and education category
weighted_plot_data <- survey_design %>%
  group_by(year, sex, final_race_using_method_hierarchy, education_cat) %>%
  summarise(proportion= survey_mean()*100)

ggplot(data=weighted_plot_data, aes(x=year, y=proportion, colour=education_cat)) + geom_line() + facet_grid(cols=vars(education_cat), scales="free") +
  theme_bw() + theme(legend.title=element_blank(), legend.position="bottom") + ylim(0,NA) + xlim(1999, 2021)+
  facet_grid(sex~final_race_using_method_hierarchy) + ggtitle("Trends in educational attainment by race and gender - weighted")

