# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Sunday sales ban and BRFSS
## State: all US states
## Author: Carolin Kilian
## Start Date: 07/05/2023
# ----------------------------------------------------------------
# ----------------------------------------------------------------

# --------------------------------------------------------------------------------------

rm(list = ls())

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# LIBARIES
# ----------------------------------------------------------------
# ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(data.table)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230925

# BRFSS 
#datBRFSS_alt <- data.table(readRDS("brfss/processed_data/BRFSS_upshifted_2000_2020_final.RDS"))
datBRFSS <- data.table(readRDS("brfss/processed_data/ACP_brfss_full.RDS"))

# POLICIES
datAP <- read_csv("acp_brfss/data/20230922_ALCPOLICY_2019.csv")

# UNEMPLOYMENT RATE
datUNEMP <- read.xlsx("acp_brfss/data/20230706_state_unemployment.xlsx", sheet = 1, startRow = 7) %>%
  select(c("X2", "X3", "rate")) %>% rename("State" = "X2", "YEAR" = "X3", "unemp.rate" = "rate") %>%
  mutate(YEAR = as.numeric(YEAR))

# MERGE DATA
data <- merge(datBRFSS, datAP, by.x = c("State", "YEAR"), by.y = c("state", "year"), all.x = T)
data <- merge(data, datUNEMP, by = c("State", "YEAR"), all.x = T, all.y = F)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# DEFINE VARIABLES & CLEAN DATA
# ----------------------------------------------------------------

# define sunday sales bans and filter for states
data <- 
  data %>% mutate(sunsalesban_di = ifelse(sunsalesban >= 0.5, 1, ifelse(sunsalesban < 0.5, 0, NA)),
                  sunsalesban = factor(ifelse(sunsalesban == 0, "no ban", ifelse(sunsalesban == 0.5, "partial ban", 
                                                                                 ifelse(sunsalesban == 1, "full ban", NA))), levels = c("no ban", "partial ban", "full ban")),
                  sunsalesban_exloc = ifelse(sunsalesban_exloc >= 0.5, 1, ifelse(sunsalesban_exloc < 0.5, 0, NA))) %>%
  filter(YEAR < 2020, YEAR > 1999)

ggplot(data, aes(sunsalesban)) + geom_histogram(stat = "count")
ggplot(data, aes(sunsalesban_di)) + geom_histogram(stat = "count")
ggplot(data, aes(sunsalesban_exloc)) + geom_histogram(stat = "count")

# clean data
out.missings <- data %>% 
  mutate(sex.mis = ifelse(is.na(sex_recode), 1, 0),
         age.mis = ifelse(is.na(age_var), 1, 0),
         raceth.mis = ifelse(is.na(race_eth), 1, 0),
         ms.mis = ifelse(is.na(marital_status), 1, 0),
         edu.mis = ifelse(is.na(education_summary), 1, 0),
         ds.mis = ifelse(is.na(drinkingstatus), 1, 0),
         freq.mis = ifelse(is.na(alc_frequency), 1, 0),
         quant.mis = ifelse(is.na(quantity_per_occasion), 1, 0)) %>%
  group_by(YEAR) %>% 
  summarise(sex.mis = sum(sex.mis) / n() *100,
            age.mis = sum(age.mis) / n() *100,
            raceth.mis = sum(raceth.mis) / n() *100,
            ms.mis = sum(ms.mis) / n() *100,
            edu.mis = sum(edu.mis) / n() *100,
            ds.mis = sum(ds.mis) / n() *100,
            freq.mis = sum(freq.mis) / n() *100,
            quant.mis = sum(quant.mis) / n() *100)

pdat <- data %>% 
  filter(!is.na(sex_recode),
         !is.na(age_var),
         !is.na(race_eth),
         !is.na(marital_status),
         !is.na(education_summary),
         !is.na(drinkingstatus),
         !is.na(alc_frequency),
         !is.na(quantity_per_occasion),
         !is.na(gramsperday))

# define age groups and factor variables
pdat <- pdat %>% 
  mutate(sex_num = ifelse(sex_recode == "Men", 0, ifelse(sex_recode == "Women", 1, NA)),
         age_gr = as.factor(ifelse(age_var < 35, "18-34", ifelse(age_var >= 35 & age_var < 50, "35-49", 
                                                                 ifelse(age_var >= 50 & age_var < 65, "50-64", ifelse(age_var >= 65, "65+", NA))))),
         education_summary = factor(education_summary, levels = c("College", "SomeC", "LEHS")),
         White = ifelse(race_eth %like% "White", 1, 0), 
         Black = ifelse(race_eth %like% "Black", 1, 0), 
         Hisp = ifelse(race_eth == "Hispanic", 1, 0),
         ROth = ifelse(race_eth %like% "Other", 1, 0),
         LEHS = ifelse(education_summary %like% "LEHS", 1, 0),
         SomeC = ifelse(education_summary %like% "SomeC", 1, 0),
         College = ifelse(education_summary %like% "College", 1, 0))

# cap alcohol use (200 GPD)
pdat %>% filter(!is.na(gramsperday) & drinkingstatus == 1) %>%
  mutate(CAP = ifelse(gramsperday > 200, 1, 0)) %>% 
  summarise(sum(CAP) / n() *100) 

pdat <- pdat %>% filter(!is.na(gramsperday)) %>%
  mutate(gramsperday = ifelse(gramsperday > 200, 200, gramsperday))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ADJUST WEIGHTS
# ----------------------------------------------------------------

pdat <- pdat %>% group_by(State) %>%
  mutate(n_state = n()) %>%
  group_by(State, YEAR) %>%
  mutate(n_year = n(),
         corr.weight = n_year/n_state,
         final_sample_weight_adj = final_sample_weight*corr.weight) %>%
  select(-n_state, -n_year, -corr.weight)
  
# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# EXPORT
# ----------------------------------------------------------------

saveRDS(pdat, file = paste0("acp_brfss/", DATE, "_brfss_clean.RDS"))

write.xlsx(out.missings, file = paste0("acp_brfss/outputs/", DATE, "_BRFSS_Missings.xlsx"), rowNames = FALSE)
