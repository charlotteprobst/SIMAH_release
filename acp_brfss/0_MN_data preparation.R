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
library(openxlsx)

# --------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230816

# mortality data
data <- data.table(read_csv("mortality/3_out data/allethn_rates_1020_STATE_QYEAR.csv"))

# select Connecticut / Minnesota
datMN <- data[fipsstr == "MN"]

# Indiana unemployment rates 
datUNEMP <- read.xlsx("acp_brfss/data/20230712_MINNESOTAunemp.xlsx", sheet = 1, startRow = 12)

# Figure design
ggdesign <- theme_bw() +
  theme(legend.position = "none", legend.text = element_text(size = 12, color = "black"),
        plot.title = element_text(size = 12), strip.text = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black"), 
        axis.title.y = element_text(size = 12, color = "black"), 
        axis.title.x = element_blank(), axis.ticks = element_blank())

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE MORTALITY DATA
# ----------------------------------------------------------------

# QYEAR
datMN <- datMN %>% mutate(QYEAR = ifelse(Q == 1, year + 0.125, ifelse(Q == 2, year + 0.375,
                                        ifelse(Q == 3, year + 0.625, ifelse(Q == 4, year + 0.825, NA)))))

# calculate age-standardized rate per group
# IMPORTANT NOTE: sum of mortality categories != Trate, check Stata code again
# calculate 100% alcohol-attributable mortality
datMN <- datMN %>% mutate(aamrate = acutrate + chronrate + alclivrate)

# define standard population = 2000
AGEST <- datMN %>% filter(QYEAR == 2000.125) %>% group_by(sex, edclass) %>% mutate(weight = pop_int / sum(pop_int)) %>%
  select(age_gp, sex, edclass, fipsstr, weight)

# combine data with standard population weight
datMN <- left_join(datMN, AGEST)

# age-standardized rates
DATA <- datMN %>% group_by(sex, edclass, year, Q) %>% 
  select(year, Q, fipsstr, weight, age_gp, sex, edclass, aamrate, chronrate, acutrate, alclivrate) %>%
  summarise(AAMrate = sum(aamrate * weight),
            CHRONrate = sum(chronrate * weight),
            ACUTrate = sum(acutrate * weight),
            ALCLIVrate = sum(alclivrate * weight)) %>% 
  ungroup() %>%
  mutate(QYEAR = ifelse(Q == 1, year + 0.125, ifelse(Q == 2, year + 0.375,
                                                     ifelse(Q == 3, year + 0.625, ifelse(Q == 4, year + 0.875, NA)))),
         SEX = factor(ifelse(sex==1, "men", ifelse(sex==2, "women", NA))),
         EDCLASS = factor(edclass, levels = c("LEHS", "SomeC", "College"))) 

# ----------------------------------------------------------------
# EXPLORE MORTALITY DATA
# ----------------------------------------------------------------

# AAMrate: timeseries per group
ggplot(data = DATA, aes(y = AAMrate, x = QYEAR)) + geom_line() + geom_point() + geom_vline(xintercept = 2017.625) +
  facet_grid(rows = vars(EDCLASS), cols = vars(SEX)) +
  scale_x_continuous(breaks = seq(2000, 2019, 1), limits = c(2000, 2020)) + 
  scale_y_continuous(limits = c(0, 10)) + 
ggtitle("Age-standardized mortality rate: 100% AA CONDITIONS") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(fill="white")) 

# 100% AA conditions: 

# change in rate by condition and group using 2017 Q3 as reference
pdat <- DATA %>% group_by(edclass, sex) %>%
  mutate(CHRONchange = (CHRONrate - CHRONrate[QYEAR == 2017.625])/CHRONrate[QYEAR == 2017.625],
         ACUTchange = (ACUTrate - ACUTrate[QYEAR == 2017.625])/ACUTrate[QYEAR == 2017.625],
         ALCLIVchange = (ALCLIVrate - ALCLIVrate[QYEAR == 2017.625])/ALCLIVrate[QYEAR == 2017.625]) %>% ungroup() %>%
  gather(., key = condition, value = change, c("CHRONchange", "ACUTchange", "ALCLIVchange"))

ggplot(data = pdat, aes(y = change, x = QYEAR, colour = condition)) + 
  geom_line() + geom_vline(xintercept = 2017.625) + geom_hline(yintercept = 0) + 
  facet_grid(rows = vars(EDCLASS), cols = vars(SEX), scales = "free_y") +
  scale_x_continuous(breaks = seq(2000, 2019, 1), limits = c(2000, 2020)) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1L),
                     name = "Change in age-standardized mortality rates (reference: 2017 Q3)\n") +
  scale_color_manual(values = c("#7BABAD", "#A90034", "#012D49"),
                     labels = c("acute conditions", "alcoholic liver cirrhosis", "chronic conditions (excl. alcoholic liver cirrhosis)")) + 
  labs(colour = "") + ggdesign + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))
#ggsave(paste0('acp_brfss/outputs/figures/', DATE, '_TS_0019_CONDITIONchange.png'), dpi=300, width = 14, height = 8)

# rate by condition and group
pdat <- DATA %>% gather(., key = condition, value = rate, c("CHRONrate", "ACUTrate", "ALCLIVrate"))

ggplot(data = pdat, aes(y = rate, x = QYEAR, colour = condition)) + 
  geom_line() + geom_vline(xintercept = 2017.625) +
  facet_grid(rows = vars(EDCLASS), cols = vars(SEX)) +
  scale_x_continuous(breaks = seq(2000, 2019, 1), limits = c(2000, 2020)) + 
  scale_y_continuous(limits = c(0, 5)) +
  scale_color_manual(values = c("#7BABAD", "#A90034", "#012D49")) + 
  ggtitle("Age-standardized mortality rate: 100% AA CONDITIONS") +
  ggdesign() + theme(axis.text.x = element_text(angle = 90), strip.background = element_rect(fill="white")) 

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

write.csv(DATA, paste0('acp_brfss/data/', DATE, "_AGEST_MORTALITY_MINNESOTA.csv"), row.names=FALSE)
write.csv(datUNEMP, paste0('acp_brfss/data/', DATE, "_UNEMP_MINNESOTA.csv"), row.names=FALSE)
