# Script to view transtions over time
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(readxl)
library(doParallel)
library(foreach)
library(parallel)
library(ggplot2)
library(RColorBrewer)
library(ggalluvial)
library(patchwork)

setwd("C:/Users/cmp21seb/Documents/SIMAH/")

source("SIMAH_code/education_transitions/2021/functions/0_transition_population_2021.R")

TPs <- read.csv("SIMAH_workplace/education_transitions/2021/annual_education_TPs_model_6.csv")

# Generate a column of transition
TPs <- TPs %>% mutate(
  StateFrom = case_when(
  StateFrom == "State 1" ~ "LEHS",
  StateFrom == "State 2" ~ "SomeC1",
  StateFrom == "State 3" ~ "SomeC2",
  StateFrom == "State 4" ~ "SomeC3",
  StateFrom == "State 5" ~ "College"),
  StateTo = case_when(
  StateTo == "State 1" ~ "LEHS",
  StateTo == "State 2" ~ "SomeC1",
  StateTo == "State 3" ~ "SomeC2",
  StateTo == "State 4" ~ "SomeC3",
  StateTo == "State 5" ~ "College")
) %>%
  mutate(Transition = paste(StateFrom, StateTo, sep = "->"))

# Generate a table showing only TPs that are feasible i.e. excluding categories such as College -> HS (for supplementary info)
TPs_allowed <- TPs %>% filter(Transition=="LEHS->LEHS"|Transition=="LEHS->HS"|
                              Transition=="HS->HS"|Transition=="HS->SomeC1"|
                              Transition=="SomeC1->SomeC1"|Transition=="SomeC1->SomeC2"|
                              Transition=="SomeC2->SomeC2"|Transition=="SomeC2->SomeC3"|
                              Transition=="SomeC3->SomeC3"|Transition=="SomeC3->College"|
                              Transition=="College->College")

write.csv(TPs_allowed, "SIMAH_workplace/education_transitions/TPs_allowed_2021_model6.csv")

population <- generate_population(TPs, 1000000) # population starts all aged 18.

# Simulate the population forward based on the model with a time covariate covering all years 
# simulatedpop2005 <- simulate_population(population, TPs, "1999-2005") 
# simulatedpop2011 <- simulate_population(population, TPs, "2006-2011") 
simulatedpop2018 <- simulate_population(population, TPs, "2012-2018")  
simulatedpop2021 <- simulate_population(population, TPs, "2019-2021") 

# simulatedpop_all_years <- rbind(simulatedpop2005, simulatedpop2011) %>%
#   rbind(., simulatedpop2018) %>% 
#   rbind(., simulatedpop2021)
# output <- simulatedpop_all_years

simulatedpop_two_periods <- rbind(simulatedpop2018, simulatedpop2021)
output <- simulatedpop_two_periods

# by age 26 where have people ended up (overall)
education_at_26 <- output %>%
  filter(age == "26") %>%
  group_by(period, education) %>%
  summarise(Nperperiod = n()) %>%
  group_by(period) %>%
  mutate(Percentage = (Nperperiod / sum(Nperperiod)) * 100)

# Define the order of education levels
education_at_26$education <- factor(education_at_26$education,
                                    levels = c("LEHS", "SomeC1", "SomeC2", "SomeC3", "College"))

# by age 26 where have people ended up (by race and sex)
education_at_26_race_sex <- output %>%
  filter(age == "26") %>%
  group_by(period, sex, race, education) %>%
  summarise(Nperperiod = n()) %>%
  group_by(period, sex, race) %>%
  mutate(Percentage = (Nperperiod / sum(Nperperiod)) * 100)

# Define the order of education levels
education_at_26_race_sex$education <- factor(education_at_26_race_sex$education,
                                    levels = c("LEHS", "SomeC1", "SomeC2", "SomeC3", "College"))


# Generate plots

# Generate a colour blind friendly pallete:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Plotting grouped bar chart with reordered education levels
ggplot(education_at_26, aes(x = period, y = Percentage, fill = education)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of simulated individuals, aged 26+, in each education group by model Time Period",
       x = "Time Period",
       y = "Percentage") +
  scale_fill_manual(values=cbPalette) +
  theme_minimal() +
  theme(legend.position = "bottom") 

# Plotting stacked bar chart
ggplot(education_at_26, aes(x = period, y = Percentage, fill = education)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Time period",
       y = "Percentage") +
  theme(
    text = element_text(size = 14),  
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 12),  
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    strip.text = element_text(size = 14),
    plot.caption = element_text(hjust=0)) +
  scale_fill_manual(values=cbPalette) +  
  theme(legend.position = "right") 
ggsave("SIMAH_workplace/education_transitions/2021/Figure 2, stacked bar chart.png", dpi=300, width = 12, height = 7)

# Plotting separate plots for each sex and race combination using facet_grid
ggplot(education_at_26_race_sex, aes(x = period, y = Percentage, fill = education)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Time period",
       y = "Percentage") +
  theme(
    text = element_text(size = 14),  
    axis.title = element_text(size = 16),  
    axis.text = element_text(size = 12),  
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    strip.text = element_text(size = 14)
  ) +
  scale_fill_manual(values=cbPalette) + 
  facet_grid(~race)  # Creating a grid of plots for each combination of sex and race
ggsave("SIMAH_workplace/education_transitions/2021/Figure 3, stacked bar chart, by race.png", dpi=300, width = 12, height = 7)

