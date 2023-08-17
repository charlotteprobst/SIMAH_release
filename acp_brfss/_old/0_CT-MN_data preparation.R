# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Sunday sales ban  
## State: Connecticut / Minnesota
## Author: Carolin Kilian
## Start Date: 05/07/2023
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
library(data.table)
library(dplyr)

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230705

# mortality data
data <- data.table(read_csv("mortality/3_out data/allethn_rates_1020_STATE_QYEAR_ED.csv"))
#data <- data.table(read_csv("mortality/3_out data/allethn_rates_1020_STATE_QYEAR_SEX.csv"))

# select Connecticut / Minnesota
#datCT <- data[fipsstr == "CT"]
datMN <- data[fipsstr == "MN"]


# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE MORTALITY DATA
# ----------------------------------------------------------------

# QYEAR
datMN <- datMN %>% mutate(QYEAR = ifelse(Q == 1, year + 0.125, ifelse(Q == 2, year + 0.375,
                                        ifelse(Q == 3, year + 0.625, ifelse(Q == 4, year + 0.825, NA)))))

# calculate age-standardized rate per group
# IMPORTANT NOTE: sum of mortality categories != Trate, check Stata code again

# define standard population = 2000
AGEST <- datMN %>% filter(QYEAR == 2000.125) %>% group_by(edclass) %>% mutate(weight = pop_int / sum(pop_int)) %>%
  select(age_gp, edclass, fipsstr, weight)

# combine data with standard population weight
datMN <- left_join(datMN, AGEST)

# age-standardized rates
DATA <- datMN %>% group_by(edclass, year, Q) %>% 
  summarise(Trate = sum(trate * weight), 
            ACUTrate = sum(acutrate * weight),
            CHRONrate = sum(chronrate * weight),
            ALCLIVrate = sum(alclivrate * weight),
            OTHLIVrate = sum(othlivrate * weight),
            UIJrate = sum(uijrate * weight),
            SIJrate = sum(sijrate * weight),
            MVACCrate = sum(mvaccrate * weight),
            RESTrate = sum(restrate * weight)) %>% ungroup() %>%
  mutate(QYEAR = ifelse(Q == 1, year + 0.125, ifelse(Q == 2, year + 0.375,
                        ifelse(Q == 3, year + 0.625, ifelse(Q == 4, year + 0.875, NA))))) %>% as.data.table()

# define factor levels education
DATA[, edclass := factor(edclass, levels = c("LEHS", "SomeC", "College"))]
#DATA[, SEX := factor(ifelse(sex==1, "men", ifelse(sex==2, "women", NA)))]

# reshape to long format
DATA <- reshape(DATA, idvar = c("edclass", "year", "Q", "QYEAR"),
                      varying = c("Trate", "ACUTrate", "CHRONrate", "ALCLIVrate", "OTHLIVrate", "UIJrate", "SIJrate", "MVACCrate"),
                      v.name = "mortrate", 
                      times = c("T", "ACUT", "CHRON", "ALCLIV", "OTHLIV", "UIJ", "SIJ", "MVACC"), 
                      direction = "long")

# ----------------------------------------------------------------
# EXPLORE MORTALITY DATA
# ----------------------------------------------------------------

# 100% AA CONDITIONS: timeseries per group
PDAT <- DATA %>% filter(time %like% "ACUT|CHRON|ALCLIV")

ggplot(data = PDAT, aes(y = mortrate, x = QYEAR)) + 
  geom_line(aes(color = as.factor(time))) + geom_point(aes(color = as.factor(time))) + 
  geom_vline(xintercept = 2017.625) +
  facet_grid(rows = vars(as.factor(edclass)), scales = "free") +
  scale_x_continuous(breaks = seq(2000, 2020, 1), limits = c(2000, 2020.2)) + 
  ggtitle("Age-standardized mortality rate: 100% AA CONDITIONS") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Connecticut/', DATE, '_TS_0020_AAConditions_ED.png'), dpi=300, width = 10, height = 7)

# INJURIES: timeseries per group
PDAT <- DATA %>% filter(time %like% "UIJ|SIJ|MVACC")

ggplot(data = PDAT, aes(y = mortrate, x = QYEAR)) + 
  geom_line(aes(color = as.factor(time))) + geom_point(aes(color = as.factor(time))) + 
  geom_vline(xintercept = 2017.625) +
  facet_grid(rows = vars(as.factor(edclass)), scales = "free") +
  scale_x_continuous(breaks = seq(2000, 2020, 1), limits = c(2000, 2020.2)) + 
  ggtitle("Age-standardized mortality rate: INJURIES (UIJ, SIJ, MVACC)") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Connecticut/', DATE, '_TS_0020_Injuries_ED.png'), dpi=300, width = 10, height = 7)

# --------------------------------------------------------------------------------------

