# SIMAH project 2021 
library(foreign)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
setwd("C:/Users/cmp21seb/Documents/SIMAH/")

# Using raw data

# Read in PSID data
PSID <- read.csv("SIMAH_workplace/PSID/cleaned data/psid_data_1999_2021.csv")
nrow(PSID) # 286,343

# Recategorise PSID cats to align with BRFSS
PSID <- PSID %>% 
  mutate(AlcCAT = ifelse(AlcCAT=="Very high risk", "High risk", AlcCAT)) %>%
  rename(race_eth = final_race_using_method_hierarchy) %>%
  mutate(race_eth = ifelse(race_eth %in% c("AI/AN", "Native", "Asian/PI", "other"), "Other", 
                           ifelse(race_eth=="white", "White",
                                  ifelse(race_eth=="black", "Black",
                                         ifelse(race_eth=="hispanic", "Hispanic", race_eth)))))

# Filter only head of household or alcohol info from TAS (ie only self-reported data)
PSID <- PSID %>% filter(relationship=="head"|!is.na(gpd_TAS))

# Generate a final alcohol category based on TAS and main survey data
PSID <- test %>% mutate(final_alc_cat=if_else(is.na(AlcCAT_TAS), 
                                                AlcCAT, 
                                                AlcCAT_TAS))

# Drop individuals missing alcohol data or missing race data
PSID <- PSID %>% drop_na(final_alc_cat, race_eth)
nrow(PSID) # 80,588

# Read in (upshifted) BRFSS data to compare to
brfssorig <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_2000_2020_final.RDS") %>% 
  filter(age_var<=80) %>% filter(State=="USA")

## Compare gpd (drinkers only)  

summary_psid <- PSID %>% group_by(year, sex) %>% filter(gpd!=0) %>% 
  summarise(meangpd = mean(gpd),
            type="PSID")

summarybrfss <- brfssorig %>%  filter(gramsperday_upshifted!=0) %>% 
  group_by(YEAR, sex_recode) %>% summarise(meangpd = mean(gramsperday_upshifted)) %>% 
  rename(year=YEAR, sex=sex_recode) %>% 
  mutate(sex = ifelse(sex=="Male","male","female"),
         type="BRFSS")

summary <- rbind(summary_psid, summarybrfss)

ggplot(data=summary, aes(x=year, y=meangpd, colour=type)) + geom_line() + ylim(0,NA) + theme_bw() +
  facet_grid(rows=vars(sex)) + ylab("mean gpd (in drinkers)") +
  theme(legend.title=element_blank(),
        strip.background = element_rect(fill="white"))
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/compare_gpd_brfss_non_replicated_main_&_tas_only.png",dpi=300, width=33, height=19, units="cm")

# by sex, year and race
summary_psid_2 <- PSID %>% group_by(year, sex, race_eth) %>% filter(gpd!=0) %>% 
  summarise(meangpd = mean(gpd),
            type="PSID") 

summarybrfss_2 <- brfssorig %>%  filter(gramsperday_upshifted!=0) %>% 
  group_by(YEAR, sex_recode, race_eth) %>% summarise(meangpd = mean(gramsperday_upshifted)) %>% 
  rename(year=YEAR, sex=sex_recode) %>% 
  mutate(sex = ifelse(sex=="Male","male","female"),
         type="BRFSS")

summary_2 <- rbind(summary_psid_2, summarybrfss_2)

ggplot(data=summary_2, aes(x=year, y=meangpd, colour=type)) + geom_line() + ylim(0,NA) + theme_bw() +
  facet_grid(rows=vars(sex), cols=vars(race_eth)) + ylab("mean gpd (in drinkers)") +
  theme(legend.title=element_blank(),
        strip.background = element_rect(fill="white"))
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/compare_gpd_brfss_race_sex_non_replicated_main_&_tas_only.png",dpi=300, width=33, height=19, units="cm")

## Compare prevalence of drinkers
summary_prevalence_psid <- PSID %>% group_by(year, sex, race_eth) %>% 
  summarise(prevalence = mean(drinkingstatus, na.rm=T)) %>% 
  mutate(type="PSID")

summarybrfss_prevalence <- brfssorig %>% 
  group_by(YEAR, sex_recode, race_eth) %>% summarise(prevalence = mean(drinkingstatus)) %>% 
  rename(year=YEAR, sex=sex_recode) %>% 
  mutate(sex = ifelse(sex=="Male","male","female"),
         type="BRFSS")

summary_prevalence <- rbind(summary_prevalence_psid, summarybrfss_prevalence) %>% mutate(prevalence=round(prevalence,digits=2))
scaleFUN <- function(x) sprintf("%.2f", x)

ggplot(data=summary_prevalence, aes(x=year, y=prevalence, colour=type)) + geom_line() + ylim(0,NA) + theme_bw() +
  facet_grid(rows=vars(sex), cols=vars(race_eth)) + ylab("mean prevalence drinkers") +
  theme(legend.title=element_blank(),
        strip.background = element_rect(fill="white")) + 
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA))
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/compare_prevalence_drinkers_brfss_race_sex_non_replicated_main_&_tas_only.png",dpi=300, width=33, height=19, units="cm")

## Compare by alcohol categories
summary_cat_psid <- PSID %>% drop_na(AlcCAT) %>% group_by(year, sex, AlcCAT) %>% tally() %>% ungroup() %>% 
  group_by(year, sex) %>% mutate(percent=n/sum(n), type="PSID")

summarybrfss_cat <- brfssorig %>% 
  mutate(AlcCAT = ifelse(gramsperday_upshifted==0, "Non-drinker",
                         ifelse(sex_recode=="Male" & gramsperday_upshifted>0 & 
                                  gramsperday_upshifted<=40, "Low risk",
                                ifelse(sex_recode=="Female" & gramsperday_upshifted>0 &
                                         gramsperday_upshifted<=20, "Low risk",
                                       ifelse(sex_recode=="Male" & gramsperday_upshifted>40 &
                                                gramsperday_upshifted<=60, "Medium risk",
                                              ifelse(sex_recode=="Female" & gramsperday_upshifted>20 & 
                                                       gramsperday_upshifted<=40, "Medium risk",
                                                     ifelse(sex_recode=="Male" & gramsperday_upshifted>60,
                                                            "High risk",
                                                            ifelse(sex_recode=="Female" & gramsperday_upshifted>40,
                                                                   "High risk", NA)))))))) %>% 
  
  group_by(YEAR, sex_recode, AlcCAT) %>% tally() %>% ungroup() %>% group_by(YEAR, sex_recode) %>% 
  mutate(percent=n/sum(n)) %>% 
  rename(year=YEAR, sex=sex_recode) %>% 
  mutate(sex = ifelse(sex=="Male","male","female"),
         type="BRFSS")

summary_cat <- rbind(summary_cat_psid, summarybrfss_cat) %>% 
  mutate(AlcCAT = factor(AlcCAT, levels=c("Non-drinker","Low risk","Medium risk","High risk")))

ggplot(data=summary_cat, aes(x=year, y=percent, colour=type)) + geom_line() + ylim(0,NA) + 
  theme_bw() +
  theme(legend.title=element_blank(),
        strip.background = element_rect(fill="white")) +
  facet_grid(cols=vars(AlcCAT), rows=vars(sex)) + 
  scale_y_continuous(labels=scaleFUN)

ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/compare_categories_brfss_non_replicated_main_&_tas_only.png",dpi=300, width=33, height=19, units="cm")

           
## Using replicated dataset (CAN'T READ IT IN AS TOO LARGE)
PSID_rep <- readRDS("SIMAH_workplace/alcohol_transitions_psid/psid_data_2005_2021_replicated_for_alc_transitions.rds")

# Generate a final alcohol category based on TAS and main survey data
PSID <- test %>% mutate(final_alc_cat=if_else(is.na(AlcCAT_TAS), 
                                              AlcCAT, 
                                              AlcCAT_TAS))

# Drop individuals missing alcohol data or missing race data
PSID <- PSID %>% drop_na(final_alc_cat, race_eth)
nrow(PSID) # 80,588

# Recategorise PSID cats to align with BRFSS
PSID_rep <- PSID_rep %>%
  mutate(AlcCAT = ifelse(AlcCAT=="Very high risk", "High risk", AlcCAT)) %>%
  rename(race_eth = final_race_using_method_hierarchy) %>%
  mutate(race_eth = ifelse(race_eth %in% c("AI/AN", "Native", "Asian/PI", "other"), "Other",
                           ifelse(race_eth=="white", "White",
                                  ifelse(race_eth=="black", "Black",
                                         ifelse(race_eth=="hispanic", "Hispanic", race_eth)))))

# Compare gpd (drinkers only)  
 
summary_psid_rep <- PSID_rep %>% group_by(year, sex) %>% filter(gpd!=0) %>% 
   summarise(meangpd = mean(gpd),
             type="PSID")

summary_rep <- rbind(summary_psid_rep, summarybrfss)

ggplot(data=summary_rep, aes(x=year, y=meangpd, colour=type)) + geom_line() + ylim(0,NA) + theme_bw() +
  facet_grid(rows=vars(sex)) + ylab("mean gpd (in drinkers)") +
  theme(legend.title=element_blank(),
        strip.background = element_rect(fill="white"))
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/compare_gpd_brfss_psid_replicated.png",dpi=300, width=33, height=19, units="cm")

# by sex, year and race
summary_psid_2_rep <- PSID %>% group_by(year, sex, race_eth) %>% filter(gpd!=0) %>% 
   summarise(meangpd = mean(gpd),
             type="PSID") 
 
summary_2_rep <- rbind(summary_psid_2_rep, summarybrfss_2)

ggplot(data=summary_2_rep, aes(x=year, y=meangpd, colour=type)) + geom_line() + ylim(0,NA) + theme_bw() +
  facet_grid(rows=vars(sex), cols=vars(race_eth)) + ylab("mean gpd (in drinkers)") +
  theme(legend.title=element_blank(),
        strip.background = element_rect(fill="white"))
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/compare_gpd_brfss_race_sex_psid_replicated.png",dpi=300, width=33, height=19, units="cm")

# Compare prevalence of drinkers
summary_prevalence_psid_rep <- PSID %>% group_by(year, sex, race_eth) %>%
  summarise(prevalence = mean(drinkingstatus, na.rm=T)) %>%
  mutate(type="PSID")

summary_prevalence_rep <- rbind(summary_prevalence_psid_rep, summarybrfss_prevalence) %>% mutate(prevalence=round(prevalence,digits=2))
scaleFUN <- function(x) sprintf("%.2f", x)

ggplot(data=summary_prevalence_rep, aes(x=year, y=prevalence, colour=type)) + geom_line() + ylim(0,NA) + theme_bw() +
  facet_grid(rows=vars(sex), cols=vars(race_eth)) + ylab("mean prevalence drinkers") +
  theme(legend.title=element_blank(),
        strip.background = element_rect(fill="white")) +
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA))
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/compare_prevalence_drinkers_brfss_race_sex_psid_replicated.png",dpi=300, width=33, height=19, units="cm")

# Compare by alcohol categories
summary_cat_psid_rep <- PSID %>% drop_na(AlcCAT) %>% group_by(year, sex, AlcCAT) %>% tally() %>% ungroup() %>% 
   group_by(year, sex) %>% mutate(percent=n/sum(n), type="PSID")

summary_cat_rep <- rbind(summary_cat_psid_rep, summarybrfss_cat) %>% 
   mutate(AlcCAT = factor(AlcCAT, levels=c("Non-drinker","Low risk","Medium risk","High risk")))

ggplot(data=summary_cat_psid_rep, aes(x=year, y=percent, colour=type)) + geom_line() + ylim(0,NA) + 
  theme_bw() +
  theme(legend.title=element_blank(),
        strip.background = element_rect(fill="white")) +
  facet_grid(cols=vars(AlcCAT), rows=vars(sex)) +
  scale_y_continuous(labels=scaleFUN)

ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/compare_categories_brfss_psid_rep.png",dpi=300, width=33, height=19, units="cm")
 


                                                              