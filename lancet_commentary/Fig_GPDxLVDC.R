######################################
## Project: Lancet commentary liver ##
## Author: Carolin Kilian           ##
## Start date: 05/09/2024           ##
## Date last changed: 05/09/2024    ##
######################################

# Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
#library(scales)

# Load settings
rm(list = ls())
setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")

# Load files
brfss.raw <- readRDS("brfss/processed_data/BRFSS_upshifted_2000_2022_final.RDS")
mort.raw <- read.csv("microsim/1_input_data/allethn_sumCOD_0022_SIMAH.csv")
pop.raw <- read.csv("lancet_commentary/ACS_popcounts_2000_2021_updated.csv") 

## ----------------------------------------------------------------
## Prepare data 
## ----------------------------------------------------------------

# get mean drinking per year, sex and education group 

brfss <- brfss.raw %>% filter(State == "USA") %>% 
  # filter(gramsperday_upshifted > 0) %>%
  group_by(YEAR, sex_recode, education_summary) %>% 
  summarise(GPD_upshifted_all = mean(gramsperday_upshifted))

brfss <- brfss.raw %>% filter(State == "USA") %>% 
  filter(gramsperday_upshifted > 0) %>%
  group_by(YEAR, sex_recode, education_summary) %>% 
  summarise(n = n(),
            GPD_upshifted_drinkers = mean(gramsperday_upshifted),
            LCI_GPD_upshifted_drinkers = GPD_upshifted_drinkers - (1.96*sd(gramsperday_upshifted)/sqrt(n)),
            UCI_GPD_upshifted_drinkers = GPD_upshifted_drinkers + (1.96*sd(gramsperday_upshifted)/sqrt(n)),
            SD_day_drinkers = GPD_upshifted_drinkers / 14) %>% ungroup() %>% 
  left_join(., brfss) %>%
  mutate(sex = ifelse(sex_recode == "Female", "Women", ifelse(sex_recode == "Male", "Men", NA))) %>% 
  rename("year" = "YEAR", "edclass" = "education_summary") %>%
  select(c(year, sex, edclass, n, GPD_upshifted_all, GPD_upshifted_drinkers, 
           LCI_GPD_upshifted_drinkers, UCI_GPD_upshifted_drinkers, SD_day_drinkers))

# prepare population data

pop <- pop.raw %>% 
  mutate(sex = ifelse(sex == 2, "Women", ifelse(sex == 1, "Men", NA))) %>%
  group_by(year, sex, age_gp, edclass) %>%
  summarise(pop = sum(TPop)) 

standard.pop <- pop %>% filter(year == 2000) %>%
  group_by(sex, edclass) %>%
  mutate(age_weight = pop/sum(pop)) %>% select(-c(pop,year))

# get age-standarized mortality rate

mort <- mort.raw %>%
  mutate(sex = ifelse(sex == 2, "Women", ifelse(sex == 1, "Men", NA))) %>% 
  group_by(year, age_gp, sex, edclass) %>%
  summarise(LVDC = sum(LVDCmort),
            LVDC_HLVDC = sum(LVDCmort) + sum(HLVDCmort)) %>% 
  left_join(., pop) %>% 
  mutate(LVDCrate = LVDC / pop * 100000,
         LVDC_HLVDCrate = LVDC_HLVDC / pop * 100000) %>%
  left_join(., standard.pop) %>%
  group_by(year, sex, edclass) %>%
  summarise(LVDCasrate = sum(LVDCrate * age_weight), 
            LVDC_HLVDCasrate = sum(LVDC_HLVDCrate * age_weight))

## ----------------------------------------------------------------
## Visualize
## ----------------------------------------------------------------

pdat <- left_join(brfss, mort) %>%
  filter(year <= 2021) %>% 
  mutate(edclass = factor(edclass, levels = c("LEHS", "SomeC", "College"),
                          labels = c("Less than high school", "Some college", "Bachelor's degree or higher")))

scaleFactor <- max(pdat$LVDCasrate) / max(pdat$GPD_upshifted_drinkers)

theme_barplot <- theme_bw() + 
  theme(axis.text = element_text(size=12, color = "black"),
        legend.text = element_text(size=12, color = "black"),
        strip.text = element_text(size=12),
        title = element_text(size=12),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

ggplot(pdat, aes(x = year, group = sex)) +
  geom_line(aes(y = GPD_upshifted_drinkers*scaleFactor, linetype = sex), color = "#782170", linewidth = 1) + 
  geom_ribbon(aes(ymin = LCI_GPD_upshifted_drinkers*scaleFactor, ymax = UCI_GPD_upshifted_drinkers*scaleFactor,
                  linetype = sex), fill = "#782170", alpha = 0.1) + 
  geom_line(aes(y = LVDCasrate, linetype = sex), color = "#61CBF3", linewidth = 1) + 
  scale_y_continuous(name = "Age-standardized mortality rate (per 100,000)",
                     sec.axis = sec_axis(~./scaleFactor, name = "Average grams of pure alcohol per day")) + 
  facet_grid(cols = vars(edclass)) + 
  ggtitle("Alcohol consumption and alcohol-related liver disease in the US (2000-2021)") +
  theme_barplot

ggsave(paste0("lancet_commentary/Fig1_", Sys.Date(), ".jpg"), dpi=300, width = 10, height = 6)
