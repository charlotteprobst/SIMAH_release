# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Sunday sales ban  
## State: Indiana
## Author: Carolin Kilian
## Start Date: 27/06/2023
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
library(openxlsx)

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230711

# mortality data
#data <- data.table(read_csv("mortality/3_out data/allethn_rates_1020_STATE_QYEAR_ED.csv"))
#data <- data.table(read_csv("mortality/3_out data/allethn_rates_1020_STATE_QYEAR_SEX.csv"))
data <- data.table(read_csv("mortality/3_out data/allethn_rates_1020_STATE_QYEAR.csv"))

# select Indiana and years between 2007-2019
datIND <- copy(data[fipsstr == "IN" & year > 2006 & year < 2020])

# Indiana unemployment rates 
datUNEMP <- read.xlsx("acp_brfss/data/20230628_INDIANAunemp.xlsx", sheet = 1, startRow = 12)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE MORTALITY DATA
# ----------------------------------------------------------------

# QYEAR
datIND <- datIND %>% mutate(QYEAR = ifelse(Q == 1, year + 0.125, ifelse(Q == 2, year + 0.375,
                                           ifelse(Q == 3, year + 0.625, ifelse(Q == 4, year + 0.825, NA)))))

# calculate age-standardized rate per group
# IMPORTANT NOTE: sum of mortality categories != Trate, check Stata code again
# calculate 100% alcohol-attributable mortality
datIND <- datIND %>% mutate(aamrate = acutrate + chronrate + alclivrate)

# define standard population = 2010
AGEST <- datIND %>% filter(QYEAR == 2010.125) %>% group_by(sex, edclass) %>% mutate(weight = pop_int / sum(pop_int)) %>%
  select(age_gp, sex, edclass, fipsstr, weight)

# combine data with standard population weight
datIND <- left_join(datIND, AGEST)

# age-standardized rates
DATA <- datIND %>% group_by(sex, edclass, year, Q) %>% 
  select(year, Q, fipsstr, weight, age_gp, sex, edclass, aamrate) %>%
  summarise(AAMrate = sum(aamrate * weight)) %>% 
  ungroup() %>%
  mutate(QYEAR = ifelse(Q == 1, year + 0.125, ifelse(Q == 2, year + 0.375,
                        ifelse(Q == 3, year + 0.625, ifelse(Q == 4, year + 0.875, NA)))),
         SEX = factor(ifelse(sex==1, "men", ifelse(sex==2, "women", NA))),
         EDCLASS = factor(edclass, levels = c("LEHS", "SomeC", "College"))) 
  
# ----------------------------------------------------------------
# EXPLORE MORTALITY DATA
# ----------------------------------------------------------------

# AAMrate: timeseries per group
ggplot(data = DATA, aes(y = AAMrate, x = QYEAR)) + geom_line() + geom_point() + geom_vline(xintercept = 2018.125) +
  facet_grid(rows = vars(EDCLASS), cols = vars(SEX)) +
  scale_x_continuous(breaks = seq(2007, 2019, 1), limits = c(2007, 2020)) + 
  scale_y_continuous(limits = c(0, 10))
  ggtitle("Age-standardized mortality rate: 100% AA CONDITIONS") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(fill="white")) 
#ggsave(paste0('acp_brfss/outputs/figures/Indiana/', DATE, '_TS_0719_AAMrate.png'), dpi=300, width = 10, height = 6)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE UNEMPLOYMENT DATA
# ----------------------------------------------------------------

datUNEMP <- datUNEMP %>% mutate(Q = ifelse(Period %like% "M01|M02|M03", 1, ifelse(Period %like% "M04|M05|M06", 2,
                               ifelse(Period %like% "M07|M08|M09", 3, ifelse(Period %like% "M10|M11|M12", 4, NA)))),
                               year = Year) %>%
                         group_by(year, Q) %>% 
                         summarise(unemp.rate = mean(Observation.Value))

# ----------------------------------------------------------------
# EXPORT
# ----------------------------------------------------------------

write.csv(DATA, paste0('acp_brfss/data/', DATE, "_AGEST_MORTALITY_INDIANA.csv"), row.names=FALSE)
write.csv(datUNEMP, paste0('acp_brfss/data/', DATE, "_UNEMP_INDIANA.csv"), row.names=FALSE)