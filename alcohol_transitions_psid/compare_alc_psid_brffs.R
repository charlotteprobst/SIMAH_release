# SIMAH project 2021 
library(foreign)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
setwd("C:/Users/cmp21seb/Documents/SIMAH/")

### PROCESS DATA

# Read in PSID data
PSID <- read.csv("SIMAH_workplace/PSID/cleaned data/psid_data_1999_2021_050424.csv")

# Filter only survey respondents or people with TAS alc data & year >= 2005 
PSID <- PSID %>% filter((relationship=="head"|!is.na(AlcCAT_TAS)) & year>=2005) 

# Generate a final alcohol category based on TAS and main survey data
PSID <- PSID %>% mutate(final_alc_cat=if_else(is.na(AlcCAT_TAS), 
                                              AlcCAT, 
                                              AlcCAT_TAS))

# Recategorise PSID cats to align with BRFSS
PSID <- PSID %>% 
  mutate(final_alc_cat = ifelse(final_alc_cat=="Very high risk", "High risk", AlcCAT)) %>%
  rename(race_eth = final_race_using_method_hierarchy) %>%
  mutate(race_eth = ifelse(race_eth %in% c("AI/AN", "Native", "Asian/PI", "other"), "Other", 
                           ifelse(race_eth=="white", "White",
                                  ifelse(race_eth=="black", "Black",
                                         ifelse(race_eth=="hispanic", "Hispanic", race_eth)))))

# Drop individuals missing alcohol data, race, age or education data
PSID <- PSID %>% drop_na(final_alc_cat, race_eth, sex, age, education_cat)

# Recategorize age variable into age_cat
PSID <- PSID %>%
  mutate(age_cat = case_when(
    age >= 18 & age <= 24 ~ "18-24",
    age >= 25 & age <= 64 ~ "25-64",
    age >= 65 ~ "65+",
    TRUE ~ NA_character_  # Handle cases not covered by the above conditions
  ))

# Read in (upshifted) BRFSS data to compare to
brfssorig <- read_rds("SIMAH_workplace/brfss/processed_data/BRFSS_upshifted_2000_2022_final.RDS") %>% 
  filter(age_var<=80) %>% filter(State=="USA")

# Recategorize age variable into age_cat
brfssorig <- brfssorig %>%
  mutate(age_cat = case_when(
    age_var >= 18 & age_var <= 24 ~ "18-24",
    age_var >= 25 & age_var <= 64 ~ "25-64",
    age_var >= 65 ~ "65+",
    TRUE ~ NA_character_  # Handle cases not covered by the above conditions
  ))

# Rename education variable 
brfssorig <- brfssorig %>% rename(education_cat=education_summary)

# Add confidence intervals to estimates

# Calcualte CIs
conf_intervals <- brfssorig %>%
  group_by(YEAR, race_eth, sex_recode, age_cat) %>%
  mutate(sample_size=n(),
  mean_gpd = mean(gramsperday_upshifted),
sd = sd(gramsperday_upshifted),
SE = sd/sqrt(sample_size),
lower_gpd = mean_gpd - 1.96*SE,
upper_gpd = mean_gpd + 1.96*SE) %>% ungroup()

### COMPARE GRAMS PER DAY ESTIMATES
summary_psid_gpd <- PSID %>% group_by(year, sex) %>% filter(gpd!=0) %>% 
  summarise(meangpd = mean(gpd),
            type="PSID")

summarybrfss_gpd <- brfssorig %>%  filter(gramsperday_upshifted!=0) %>% 
  group_by(YEAR, sex_recode) %>% 
  summarise(sample_size=n(),
            meangpd = mean(gramsperday_upshifted),
            sd = sd(gramsperday_upshifted),
            SE = sd/sqrt(sample_size),
            lower_gpd = meangpd - 1.96*SE,
            upper_gpd = meangpd + 1.96*SE) %>% 
  rename(year=YEAR, sex=sex_recode) %>% 
  mutate(sex = ifelse(sex=="Male","male","female"),
         type="BRFSS")

summary_gpd <- rbind(summary_psid_gpd, summarybrfss_gpd)

ggplot(data=summary_gpd, aes(x=year, y=meangpd, colour=type)) + 
  geom_line() + 
  geom_ribbon(data = summary_gpd %>% filter(type == "BRFSS"), 
              aes(x = year, ymin = lower_gpd, ymax = upper_gpd, fill = type), 
              alpha = 0.2) +
  scale_x_continuous(limits = c(2005, 2022), breaks = seq(2005, 2022, by = 1), labels = seq(2005, 2022, by = 1)) + 
  ylim(0, NA) + 
  theme_bw() +
  facet_grid(rows = vars(sex)) + 
  ylab("mean gpd (in drinkers)") +
  theme(legend.title = element_blank(),
        strip.background = element_rect(fill = "white")) +
  labs(title = "Mean GPD with Confidence Intervals for BRFSS Data")
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/brfss_vs_psid_gpd_2022.png",dpi=300, width=33, height=19, units="cm")

# by sex, year and race
summary_psid_gpd_sex_year_race <- PSID %>% group_by(year, sex, race_eth) %>% filter(gpd!=0) %>% 
  summarise(meangpd = mean(gpd),
            type="PSID") 

summarybrfss_gpd_sex_year_race <- brfssorig %>%  filter(gramsperday_upshifted!=0) %>% 
  group_by(YEAR, sex_recode, race_eth) %>% summarise(meangpd = mean(gramsperday_upshifted)) %>% 
  rename(year=YEAR, sex=sex_recode) %>% 
  mutate(sex = ifelse(sex=="Male","male","female"),
         type="BRFSS")

summary_gpd_sex_year_race <- rbind(summary_psid_gpd_sex_year_race, summarybrfss_gpd_sex_year_race)

ggplot(data=summary_gpd_sex_year_race, aes(x=year, y=meangpd, colour=type)) + 
  geom_line() + 
  scale_x_continuous(limits = c(2005, 2022), breaks = seq(2005, 2022, by = 2), labels = seq(2005, 2022, by = 2)) + 
  ylim(0,NA) + 
  theme_bw() +
  facet_grid(rows=vars(sex), cols=vars(race_eth)) + ylab("mean gpd (in drinkers)") +
  theme(legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/brfss_vs_psid_gpd_sex_year_race.png",dpi=300, width=33, height=19, units="cm")

### COMPARE DRINKING STATUS (DRINKERS V NON-DRINKERS)
summary_prevalence_psid <- PSID %>% group_by(year, sex, race_eth) %>% 
  summarise(prevalence = mean(drinkingstatus, na.rm=T)) %>% 
  mutate(type="PSID")

summarybrfss_prevalence <- brfssorig %>% 
  group_by(YEAR, sex_recode, race_eth) %>% summarise(prevalence = mean(drinkingstatus)) %>% 
  rename(year=YEAR, sex=sex_recode) %>% 
  mutate(sex = ifelse(sex=="Male","male","female"),
         type="BRFSS")

summary_prevalence <- rbind(summary_prevalence_psid, summarybrfss_prevalence) %>% 
  mutate(prevalence=round(prevalence,digits=2))
scaleFUN <- function(x) sprintf("%.0f", x)

ggplot(data=summary_prevalence, aes(x=year, y=prevalence, colour=type)) + 
  geom_line() +
  scale_x_continuous(limits = c(2005, 2022), breaks = seq(2005, 2022, by = 2), labels = seq(2005, 2022, by = 2)) + 
  ylim(0,NA) + 
  theme_bw() +
  facet_grid(rows=vars(sex), cols=vars(race_eth)) + ylab("mean prevalence drinkers") +
  theme(legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(labels=scaleFUN, limits=c(0,NA))
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/brfss_vs_psid_prevelance_drinkers.png",dpi=300, width=33, height=19, units="cm")

### COMPARE ALCOHOL CATEGORIES (INCLUDING WEIGHTED PSID AND NON-WEIGHTED PSID ESTIMATES)
summary_cat_psid_complex_weighted <- PSID %>%
  drop_na(final_alc_cat, age_cat) %>%
  group_by(uniqueID) %>%
  mutate(meanweightoveryears = mean(individualweight_cross.sectional, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    Nondrinker = as.integer(final_alc_cat == "Non-drinker"),
    Low_risk = as.integer(final_alc_cat == "Low risk"),
    Medium_risk = as.integer(final_alc_cat == "Medium risk"),
    High_risk = as.integer(final_alc_cat == "High risk")
  ) %>%
  group_by(year, sex, age_cat, race_eth) %>%
  summarise(
    prevalence_unweighted_nondrinker = mean(Nondrinker, na.rm = TRUE), 
    prevalence_crosssect_nondrinker = weighted.mean(Nondrinker, individualweight_cross.sectional, na.rm = TRUE),
    prevalence_crosssectmean_nondrinker = weighted.mean(Nondrinker, meanweightoveryears, na.rm = TRUE),
    prevalence_longit_nondrinker = weighted.mean(Nondrinker, individualweight_longitudinal, na.rm = TRUE),
    
    prevalence_unweighted_lowrisk = mean(Low_risk, na.rm = TRUE),
    prevalence_crosssect_lowrisk = weighted.mean(Low_risk, individualweight_cross.sectional, na.rm = TRUE),
    prevalence_crosssectmean_lowrisk = weighted.mean(Low_risk, meanweightoveryears, na.rm = TRUE),
    prevalence_longit_lowrisk = weighted.mean(Low_risk, individualweight_longitudinal, na.rm = TRUE),
    
    prevalence_unweighted_medrisk = mean(Medium_risk, na.rm = TRUE),
    prevalence_crosssect_medrisk = weighted.mean(Medium_risk, individualweight_cross.sectional, na.rm = TRUE),
    prevalence_crosssectmean_medrisk = weighted.mean(Medium_risk, meanweightoveryears, na.rm = TRUE),
    prevalence_longit_medrisk = weighted.mean(Medium_risk, individualweight_longitudinal, na.rm = TRUE),
    
    prevalence_unweighted_highrisk = mean(High_risk, na.rm = TRUE),
    prevalence_crosssect_highrisk = weighted.mean(High_risk, individualweight_cross.sectional, na.rm = TRUE),
    prevalence_crosssectmean_highrisk = weighted.mean(High_risk, meanweightoveryears, na.rm = TRUE),
    prevalence_longit_highrisk = weighted.mean(High_risk, individualweight_longitudinal, na.rm = TRUE)
  )  %>%
  pivot_longer(
    cols = starts_with("prevalence"),
    names_to = c("prevalence", "type", "final_alc_cat"),
    names_sep = "_",
    values_to = "percent"
  )
  
summarybrfss_cat_complex <- brfssorig %>%
    mutate(final_alc_cat = case_when(
    gramsperday_upshifted == 0 ~ "nondrinker",
    sex_recode == "Male" & gramsperday_upshifted > 0 & gramsperday_upshifted <= 40 ~ "lowrisk",
    sex_recode == "Female" & gramsperday_upshifted > 0 & gramsperday_upshifted <= 20 ~ "lowrisk",
    sex_recode == "Male" & gramsperday_upshifted > 40 & gramsperday_upshifted <= 60 ~ "medrisk",
    sex_recode == "Female" & gramsperday_upshifted > 20 & gramsperday_upshifted <= 40 ~ "medrisk",
    sex_recode == "Male" & gramsperday_upshifted > 60 ~ "highrisk",
    sex_recode == "Female" & gramsperday_upshifted > 40 ~ "highrisk",
    TRUE ~ NA_character_
  )) %>%  
  drop_na(final_alc_cat, age_cat) %>%
  group_by(YEAR, age_cat, sex_recode, race_eth, final_alc_cat) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(YEAR, age_cat, sex_recode, race_eth) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup() %>%
  rename(year = YEAR, sex = sex_recode) %>%
  mutate(sex = ifelse(sex == "Male", "male", "female"),
         type = "BRFSS")

summary_cat_complex <- rbind(summary_cat_psid_complex_weighted, summarybrfss_cat_complex) %>% 
  mutate(final_alc_cat = factor(final_alc_cat, levels=c("nondrinker","lowrisk","medrisk","highrisk")),
         percent=percent*100)

cat_complex_plots <- summary_cat_complex %>% 
  filter(type=="unweighted"|type=="crosssectmean"|type=="BRFSS") %>%
  filter(race_eth!="Other")

cat_complex_plots_male <- cat_complex_plots %>% filter (sex=="male")
cat_complex_plots_female <- cat_complex_plots %>% filter (sex=="female")

ggplot(data=cat_complex_plots_male, aes(x=year, y=percent, colour=type)) + geom_line() +
  theme_bw() +
  scale_x_continuous(limits = c(2000, 2022), breaks = seq(2000, 2022, by = 4), labels = seq(2000, 2022, by = 4)) + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) +
  geom_vline(aes(xintercept=2019), linetype="dashed", colour="red")+
  facet_grid(cols=vars(race_eth, age_cat), rows=vars(final_alc_cat), scales = "free_y") +
  ggtitle("Men")
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/brfss_vs_psid_alc_cats_2022_male.png",dpi=300, width=33, height=19, units="cm")

ggplot(data=cat_complex_plots_female, aes(x=year, y=percent, colour=type)) + geom_line() +
  theme_bw() +
  scale_x_continuous(limits = c(2000, 2022), breaks = seq(2000, 2022, by = 4), labels = seq(2000, 2022, by = 4)) + 
  theme(legend.title=element_blank(),
        legend.position="bottom",
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) +
  geom_vline(aes(xintercept=2019), linetype="dashed", colour="red")+
  facet_grid(cols=vars(race_eth, age_cat), rows=vars(final_alc_cat), scales = "free_y") +
  ggtitle("Women")
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/brfss_vs_psid_alc_cats_2022_female.png",dpi=300, width=33, height=19, units="cm")

######## Review BRFSS data only, but including all targets

library(forcats)

# Reorder education_cat using forcats
brfssorig$education_cat <- fct_relevel(brfssorig$education_cat, 
                                                      "LEHS", "SomeC", "College")

# Calculate the confidence intervals
brfss_target <- brfssorig %>%
  mutate(final_alc_cat = case_when(
    gramsperday_upshifted == 0 ~ "nondrinker",
    sex_recode == "Male" & gramsperday_upshifted > 0 & gramsperday_upshifted <= 40 ~ "lowrisk",
    sex_recode == "Female" & gramsperday_upshifted > 0 & gramsperday_upshifted <= 20 ~ "lowrisk",
    sex_recode == "Male" & gramsperday_upshifted > 40 & gramsperday_upshifted <= 60 ~ "medrisk",
    sex_recode == "Female" & gramsperday_upshifted > 20 & gramsperday_upshifted <= 40 ~ "medrisk",
    sex_recode == "Male" & gramsperday_upshifted > 60 ~ "highrisk",
    sex_recode == "Female" & gramsperday_upshifted > 40 ~ "highrisk",
    TRUE ~ NA_character_
  )) %>%  
  drop_na(final_alc_cat, age_cat) %>%
  group_by(YEAR, age_cat, sex_recode, race_eth, education_cat, final_alc_cat) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(YEAR, age_cat, sex_recode, race_eth, education_cat) %>%
  mutate(percent = n / sum(n),
         se = sqrt((percent * (1 - percent)) / sum(n)), # standard error
         lower_ci = pmax(0, percent - 1.96 * se), # lower 95% CI bounded at 0
         upper_ci = pmin(1, percent + 1.96 * se)  # upper 95% CI bounded at 1
  ) %>%
  ungroup() %>%
  rename(year = YEAR, sex = sex_recode) %>%
  mutate(sex = ifelse(sex == "Male", "male", "female"),
         type = "BRFSS")

brfss_target$final_alc_cat <- fct_relevel(brfss_target$final_alc_cat,
"nondrinker", "lowrisk", "medrisk", "highrisk")

# Filter for White males and females 

brfss_targets_male_white <- brfss_target %>% filter (sex=="male", race_eth=="White") %>%
  mutate(percent=percent)
brfss_targets_female_white <- brfss_target %>% filter (sex=="female", race_eth=="White") %>%
  mutate(percent=percent)

ggplot(data = brfss_targets_male_white, aes(x = year, y = percent * 100)) +
  geom_line(aes(color = final_alc_cat)) +
  geom_ribbon(aes(ymin = lower_ci * 100, ymax = upper_ci * 100, fill = final_alc_cat), alpha = 0.2) +
  theme_bw() +
  scale_x_continuous(limits = c(2010, 2022), breaks = seq(2010, 2022, by = 2), labels = seq(2010, 2022, by = 2)) + 
  theme(legend.title = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_vline(aes(xintercept = 2020), linetype = "dashed", colour = "red") +
  facet_grid(cols = vars(age_cat, education_cat), rows = vars(final_alc_cat), scales = "free_y") +
  ggtitle("White Men") +
  labs(y = "Percent (%)")
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/brfss_vs_psid_alc_cats_2022_white_male.png",dpi=300, width=33, height=19, units="cm")

ggplot(data = brfss_targets_female_white, aes(x = year, y = percent * 100)) +
  geom_line(aes(color = final_alc_cat)) +
  geom_ribbon(aes(ymin = lower_ci * 100, ymax = upper_ci * 100, fill = final_alc_cat), alpha = 0.2) +
  theme_bw() +
  scale_x_continuous(limits = c(2010, 2022), breaks = seq(2010, 2022, by = 2), labels = seq(2010, 2022, by = 2)) + 
  theme(legend.title = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_vline(aes(xintercept = 2020), linetype = "dashed", colour = "red") +
  facet_grid(cols = vars(age_cat, education_cat), rows = vars(final_alc_cat), scales = "free_y") +
  ggtitle("White Women") +
  labs(y = "Percent (%)")
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/brfss_vs_psid_alc_cats_2022_white_female.png",dpi=300, width=33, height=19, units="cm")

# Filter for Hispanic males and females 
brfss_targets_male_hispanic <- brfss_target %>%
  filter(sex == "male", race_eth == "Hispanic") %>%
  mutate(percent = percent)

brfss_targets_female_hispanic <- brfss_target %>%
  filter(sex == "female", race_eth == "Hispanic") %>%
  mutate(percent = percent)

# Plot for Hispanic males
ggplot(data = brfss_targets_male_hispanic, aes(x = year, y = percent * 100)) +
  geom_line(aes(color = final_alc_cat)) +
  geom_ribbon(aes(ymin = lower_ci * 100, ymax = upper_ci * 100, fill = final_alc_cat), alpha = 0.2) +
  theme_bw() +
  scale_x_continuous(limits = c(2010, 2022), breaks = seq(2010, 2022, by = 2), labels = seq(2010, 2022, by = 2)) + 
  theme(legend.title = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_vline(aes(xintercept = 2020), linetype = "dashed", colour = "red") +
  facet_grid(cols = vars(age_cat, education_cat), rows = vars(final_alc_cat), scales = "free_y") +
  ggtitle("Hispanic Men") +
  labs(y = "Percent (%)")
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/brfss_vs_psid_alc_cats_2022_hispanic_male.png", dpi = 300, width = 33, height = 19, units = "cm")

# Plot for Hispanic females
ggplot(data = brfss_targets_female_hispanic, aes(x = year, y = percent * 100)) +
  geom_line(aes(color = final_alc_cat)) +
  geom_ribbon(aes(ymin = lower_ci * 100, ymax = upper_ci * 100, fill = final_alc_cat), alpha = 0.2) +
  theme_bw() +
  scale_x_continuous(limits = c(2010, 2022), breaks = seq(2010, 2022, by = 2), labels = seq(2010, 2022, by = 2)) + 
  theme(legend.title = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_vline(aes(xintercept = 2020), linetype = "dashed", colour = "red") +
  facet_grid(cols = vars(age_cat, education_cat), rows = vars(final_alc_cat), scales = "free_y") +
  ggtitle("Hispanic Women") +
  labs(y = "Percent (%)")
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/brfss_vs_psid_alc_cats_2022_hispanic_female.png", dpi = 300, width = 33, height = 19, units = "cm")

# Filter for Black males and females 
brfss_targets_male_black <- brfss_target %>%
  filter(sex == "male", race_eth == "Black") %>%
  mutate(percent = percent)

brfss_targets_female_black <- brfss_target %>%
  filter(sex == "female", race_eth == "Black") %>%
  mutate(percent = percent)

# Plot for Black males
ggplot(data = brfss_targets_male_black, aes(x = year, y = percent * 100)) +
  geom_line(aes(color = final_alc_cat)) +
  geom_ribbon(aes(ymin = lower_ci * 100, ymax = upper_ci * 100, fill = final_alc_cat), alpha = 0.2) +
  theme_bw() +
  scale_x_continuous(limits = c(2010, 2022), breaks = seq(2010, 2022, by = 2), labels = seq(2010, 2022, by = )) + 
  theme(legend.title = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_vline(aes(xintercept = 2020), linetype = "dashed", colour = "red") +
  facet_grid(cols = vars(age_cat, education_cat), rows = vars(final_alc_cat), scales = "free_y") +
  ggtitle("Black Men") +
  labs(y = "Percent (%)")
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/brfss_vs_psid_alc_cats_2022_black_male.png", dpi = 300, width = 33, height = 19, units = "cm")

# Plot for Black females
ggplot(data = brfss_targets_female_black, aes(x = year, y = percent * 100)) +
  geom_line(aes(color = final_alc_cat)) +
  geom_ribbon(aes(ymin = lower_ci * 100, ymax = upper_ci * 100, fill = final_alc_cat), alpha = 0.2) +
  theme_bw() +
  scale_x_continuous(limits = c(2010, 2022), breaks = seq(2010, 2022, by = 2), labels = seq(2010, 2022, by = 2)) + 
  theme(legend.title = element_blank(),
        strip.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_vline(aes(xintercept = 2020), linetype = "dashed", colour = "red") +
  facet_grid(cols = vars(age_cat, education_cat), rows = vars(final_alc_cat), scales = "free_y") +
  ggtitle("Black Women") +
  labs(y = "Percent (%)")
ggsave("SIMAH_workplace/PSID/Results/Alcohol trends/brfss_vs_psid_alc_cats_2022_black_female.png", dpi = 300, width = 33, height = 19, units = "cm")