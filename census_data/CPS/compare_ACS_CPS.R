# SIMAH project 2023 
# compare ACS and CPS totals 

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
library(tidyverse)
options(scipen=999)

CPS <- read.csv("SIMAH_workplace/CPS/CPS_2000_2020_agegp.csv") %>% mutate(data="CPS")
ACS <- read.csv("SIMAH_workplace/ACS/ACS_popcounts_2000_2021_updated.csv") %>% mutate(data="ACS")

CPS %>% filter(year==2019) %>% summarise(sum(TPop))
ACS %>% filter(year==2019) %>% summarise(sum(TPop))

# compare overall totals (not by age group)
overall <- rbind(CPS, ACS) %>% 
  group_by(year, data, sex, race, edclass) %>% filter(age_gp==80) %>% 
  summarise(TPop = sum(TPop)) %>% 
  mutate(sex = ifelse(sex==1, "Men","Women"), edclass = factor(edclass, levels=c("LEHS","SomeC","College"))) %>% 
  ggplot(aes(x=year, y=TPop, linetype=data, colour=sex)) + geom_line() + 
  facet_grid(cols=vars(edclass), rows=vars(race), scales="free") + 
  theme_bw() + theme(legend.position="bottom", legend.title=element_blank()) + ylim(0,NA) + xlim(2010,2020) + ggtitle("age 80+")
overall

ggsave("SIMAH_workplace/CPS/CPS_ACS_compare80plus.png")
