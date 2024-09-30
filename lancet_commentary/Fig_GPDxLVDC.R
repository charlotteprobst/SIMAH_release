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
library(survey)
library(data.table)
#library(scales)

# Load settings
rm(list = ls())
setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")

# Load files
brfss <- read.csv("lancet_commentary/brfss_0022_summary.csv")
mort.raw <- read.csv("lancet_commentary/alvdcmort_0022_SIMAH.csv")
pop.raw <- read.csv("lancet_commentary/ACS_popcounts_2000_2021_updated.csv") 

## ----------------------------------------------------------------
## Prepare data 
## ----------------------------------------------------------------

#brfss.raw <- readRDS("lancet_commentary/brfss_full_2000_2022.RDS") %>% 
#  do.call(rbind,.) %>% 
#  select(YEAR, final_sample_weight, X_STSTR, X_PSU,
#         sex_recode, education_summary, gramsperday)
#write.csv(brfss.raw, "lancet_commentary/selected_brfss_full_2000_2022.csv", na = ".")

# prepare population data

pop <- pop.raw %>% 
  mutate(sex = ifelse(sex == 2, "Women", ifelse(sex == 1, "Men", NA))) %>%
  group_by(year, sex, age_gp, edclass) %>%
  summarise(pop = sum(TPop)) 

standard.pop <- pop %>% filter(year == 2000) %>%
  group_by(sex, edclass) %>%
  mutate(age_weight = pop/sum(pop)) %>% dplyr::select(-c(pop,year))

# get age-standarized mortality rate

mort <- mort.raw %>%
  filter(!age_gp %like% "17|999") %>% 
  mutate(age_gp = ifelse(age_gp %like% "18", 18,
                         ifelse(age_gp %like% "25", 25,
                                ifelse(age_gp %like% "30", 30,
                                       ifelse(age_gp %like% "35", 35,
                                              ifelse(age_gp %like% "40", 40,
                                                     ifelse(age_gp %like% "45", 45,
                                                            ifelse(age_gp %like% "50", 50,
                                                                   ifelse(age_gp %like% "55", 55,
                                                                          ifelse(age_gp %like% "60", 60,
                                                                                 ifelse(age_gp %like% "65", 65,
                                                                                        ifelse(age_gp %like% "70", 70,
                                                                                               ifelse(age_gp %like% "75", 75,
                                                                                                      ifelse(age_gp %like% "80|85", 80, NA))))))))))))), 
         sex = ifelse(sex == 2, "Women", ifelse(sex == 1, "Men", NA))) %>% 
  group_by(year, age_gp, sex, edclass) %>%
  summarise(ALVDC = sum(ALVDCmort)) %>% 
  left_join(., pop) %>% 
  mutate(ALVDCrate = ALVDC / pop * 100000) %>%
  left_join(., standard.pop) %>%
  group_by(year, sex, edclass) %>%
  summarise(ALVDCasrate = sum(ALVDCrate * age_weight))

## ----------------------------------------------------------------
## Visualize
## ----------------------------------------------------------------

pdat <- left_join(brfss, mort) %>%
  filter(year <= 2021) %>% 
  mutate(alccat3 = alccat3*100, alccat3_lci = alccat3_lci*100, alccat3_uci = alccat3_uci*100,
         edclass = factor(edclass, levels = c("LEHS", "SomeC", "College"),
                          labels = c("Less than high school", "Some college", "Bachelor's degree or higher")))

theme_barplot <- theme_bw() + 
  theme(axis.text = element_text(size=12, color = "black"),
        legend.text = element_text(size=12, color = "black"),
        strip.text = element_text(size=12),
        title = element_text(size=12),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

# GPD and ALVDC

scaleFactor <- max(pdat$ALVDCasrate) / max(pdat$gpd)

ggplot(pdat, aes(x = year, group = sex)) +
  geom_line(aes(y = gpd*scaleFactor, linetype = sex), color = "#729928", linewidth = 1) + 
  geom_ribbon(aes(ymin = gpd_lci*scaleFactor, ymax = gpd_uci*scaleFactor,
                  linetype = sex), fill = "#729928", alpha = 0.1) + 
  geom_line(aes(y = ALVDCasrate, linetype = sex), color = "#4568BA", linewidth = 1) + 
  scale_y_continuous(name = "Age-standardized mortality rate (per 100,000)",
                     sec.axis = sec_axis(~./scaleFactor, name = "Average grams of pure alcohol per day")) + 
  facet_grid(cols = vars(edclass)) + 
  ggtitle("Alcohol consumption and alcohol-related liver cirrhosis in the US (2000-2021)") +
  theme_barplot

ggsave(paste0("lancet_commentary/Fig1_ALVDCxGPD_", Sys.Date(), ".jpg"), dpi=300, width = 10, height = 5)

# ALCCAT3 and ALVDC

scaleFactor <- max(pdat$ALVDCasrate) / max(pdat$alccat3)

ggplot(pdat, aes(x = year)) +
  geom_smooth(aes(y = alccat3*scaleFactor), method = "loess", color = "#729928", fill = "#729928", alpha = 0.1) + 
  geom_smooth(aes(y = ALVDCasrate), method = "loess", color = "#4568BA", fill = "#4568BA", alpha = 0.1) + 
  scale_y_continuous(name = "Age-standardized mortality rate (per 100,000)",
                     sec.axis = sec_axis(~./scaleFactor, name = "Prevalence of high-risk alcohol use (%)"),
                     limits = c(0, 30)) + 
  scale_x_continuous(limits = c(2010,2021), breaks = seq(2010, 2022, 2)) +
  facet_grid(cols = vars(edclass), rows = vars(sex), #scales = "free_y"
             ) + 
  ggtitle("High-risk alcohol use and alcohol-related liver cirrhosis in the US (2000-2021)") +
  theme_barplot

ggsave(paste0("lancet_commentary/Fig1_ALVDCxAlcCat3_", Sys.Date(), ".jpg"), dpi=300, width = 10, height = 6)
