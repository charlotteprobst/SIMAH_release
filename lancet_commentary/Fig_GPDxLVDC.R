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
#library(scales)

# Load settings
rm(list = ls())
setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")

# Load files
brfss.raw <- readRDS("lancet_commentary/20230925_brfss_clean.RDS") %>% 
  filter(gramsperday > 0) %>% mutate(YEAR = is.numeric(YEAR))
mort.raw <- read.csv("lancet_commentary/alvdcmort_0022_SIMAH.csv")
pop.raw <- read.csv("lancet_commentary/ACS_popcounts_2000_2021_updated.csv") 

## ----------------------------------------------------------------
## Prepare data 
## ----------------------------------------------------------------

# get mean drinking per year, sex and education group 

# set survey design
options(survey.lonely.psu = "adjust")
brfss <- svydesign(ids = ~X_PSU, strata = ~interaction(X_STSTR, YEAR), 
                   weights = ~final_sample_weight_adj, nest = T, data = brfss.raw)

gpd <- svyby(~gramsperday, ~sex_recode + education_summary + YEAR, subset(brfss, gramsperday > 0), svymean, multicore=TRUE) %>% 
  select(c("sex_recode", "education_summary", "YEAR", "gramsperday")) %>% 
  mutate(gpd_lci = gpd - (1.96*gpd_se),
         gpd_uci = gpd + (1.96*gpd_se)) %>%
  mutate(sex = ifelse(sex_recode == "Female", "Women", ifelse(sex_recode == "Male", "Men", NA))) %>% 
  rename("year" = "YEAR", "edclass" = "education_summary") %>%
  dplyr::select(c(year, sex, edclass, gpd, gpd_lci, gpd_uci))

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

pdat <- left_join(gpd, mort) %>%
  filter(year <= 2021) %>% 
  mutate(edclass = factor(edclass, levels = c("LEHS", "SomeC", "College"),
                          labels = c("Less than high school", "Some college", "Bachelor's degree or higher")))

scaleFactor <- max(pdat$ALVDCasrate) / max(pdat$gpd)

theme_barplot <- theme_bw() + 
  theme(axis.text = element_text(size=12, color = "black"),
        legend.text = element_text(size=12, color = "black"),
        strip.text = element_text(size=12),
        title = element_text(size=12),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

ggplot(pdat, aes(x = year, group = sex)) +
  geom_line(aes(y = gpd*scaleFactor, linetype = sex), color = "#782170", linewidth = 1) + 
  geom_ribbon(aes(ymin = gpd_lci*scaleFactor, ymax = gpd_uci*scaleFactor,
                  linetype = sex), fill = "#782170", alpha = 0.1) + 
  geom_line(aes(y = ALVDCasrate, linetype = sex), color = "#4568BA", linewidth = 1) + 
  scale_y_continuous(name = "Age-standardized mortality rate (per 100,000)",
                     sec.axis = sec_axis(~./scaleFactor, name = "Average grams of pure alcohol per day")) + 
  facet_grid(cols = vars(edclass)) + 
  ggtitle("Alcohol consumption and alcohol-related liver disease in the US (2000-2021)") +
  theme_barplot

ggsave(paste0("lancet_commentary/Fig1_ALVDCxGPD_", Sys.Date(), ".jpg"), dpi=300, width = 10, height = 6)
