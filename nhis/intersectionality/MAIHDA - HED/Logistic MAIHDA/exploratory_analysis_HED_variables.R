# Set working directory
setwd("C:/Users/cmp21seb/Documents/SIMAH/")

# Read in necessary R packages & functions
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ragg)

# Read in data:
full_sample <- readRDS("SIMAH_workplace/nhis/intersectionality/cleaned_data/nhis_alc_clean_full_sample.RDS")

# Generate a binary HED variable
full_sample <- full_sample %>%
  mutate(HED =
           case_when(ALC5UPYR >= 1 ~ 1,
                     ALC5UPYR == 0 ~ 0))

# Generate a subset of drinkers
drinkers <- full_sample %>% filter(ALCSTAT1 == "Current drinker")

# Set default theme for plots:
theme_set(theme_bw(base_size = 12))
options(scipen=10)

## Explore proportion of HEDs by intersections

## Full sample
full_sample_binary_intersections <- full_sample %>% 
  group_by(SEX, race_5_cats, education_3_cats, age_3_cats) %>% 
  mutate(intersections = cur_group_id(),
         intersectional_names = as.character(paste(SEX, age_3_cats, race_5_cats, education_3_cats, decade))) %>%
  group_by(intersectional_names) %>%
  count(HED) %>%
  group_by(intersectional_names) %>%
  mutate(group_size = sum(n),
         HEDs = n,
         percent = n/sum(n)*100) %>%
  filter(HED==1) %>%
  arrange(desc(percent))

plot_data_full <- full_sample_binary_intersections %>%
  mutate(sex=ifelse(grepl("F",intersectional_names),"Female","Male"),
         race=ifelse(grepl("Non-Hispanic Asian",intersectional_names),"Asian",
                     ifelse(grepl("Non-Hispanic Black", intersectional_names),"Black",
                            ifelse(grepl("Non-Hispanic Other", intersectional_names), "Other",
                                   ifelse(grepl("White", intersectional_names), "White","Hispanic")))),
         education=ifelse(grepl("high school", intersectional_names), "low edu.",
                          ifelse(grepl("some college", intersectional_names), "medium edu.", "high edu.")),
         education=factor(education,levels=c("low edu.", "medium edu.", "high edu.")),
         age=ifelse(grepl("18-24", intersectional_names), "18-24",
                    ifelse(grepl("25-69", intersectional_names), "25-69", "70+")),
         decade=ifelse(grepl("2000-2009", intersectional_names), "2000-2009", "2010-2018"))

# Plot
HED_percent_males_full <- plot_data_full %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = age, y = percent, fill = education)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  facet_grid(cols = vars(decade), rows = vars(race)) +
  theme(axis.title.x = element_blank(), legend.position = "bottom") +
  ylim(0, 100) +
  ggtitle("Percentage reporting 1+ occasion of HED in the last year, male drinkers") +
  labs(y = "percent")
HED_percent_males_full
ggsave("SIMAH_workplace/nhis/intersectionality/plots/Percent_HEDs_among_male_full_sample.png", dpi=300, width=33, height=19, units="cm")

HED_percent_females_full <- plot_data_full %>%
  filter(sex == "Female") %>%
  ggplot(aes(x = age, y = percent, fill = education)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  facet_grid(cols = vars(decade), rows = vars(race)) +
  theme(axis.title.x = element_blank(), legend.position = "bottom") +
  ylim(0, 100) +
  ggtitle("Percentage reporting 1+ occasion of HED in the last year, female drinkers") +
  labs(y = "percent")
HED_percent_females
ggsave("SIMAH_workplace/nhis/intersectionality/plots/Percent_HEDs_among_female_full_sample.png", dpi=300, width=33, height=19, units="cm")


## Drinkers only
Drinkers_binary_intersections <- drinkers %>% 
  group_by(SEX, race_5_cats, education_3_cats, age_3_cats) %>% 
  mutate(intersections = cur_group_id(),
         intersectional_names = as.character(paste(SEX, age_3_cats, race_5_cats, education_3_cats, decade))) %>%
  group_by(intersectional_names) %>%
  count(HED) %>%
  group_by(intersectional_names) %>%
  mutate(group_size = sum(n),
         HEDs = n,
         percent = n/sum(n)*100) %>%
  filter(HED==1) %>%
  arrange(desc(percent))

plot_data <- Drinkers_binary_intersections %>%
  mutate(sex=ifelse(grepl("F",intersectional_names),"Female","Male"),
         race=ifelse(grepl("Non-Hispanic Asian",intersectional_names),"Asian",
                     ifelse(grepl("Non-Hispanic Black", intersectional_names),"Black",
                            ifelse(grepl("Non-Hispanic Other", intersectional_names), "Other",
                                   ifelse(grepl("White", intersectional_names), "White","Hispanic")))),
         education=ifelse(grepl("high school", intersectional_names), "low edu.",
                          ifelse(grepl("some college", intersectional_names), "medium edu.", "high edu.")),
         education=factor(education,levels=c("low edu.", "medium edu.", "high edu.")),
         age=ifelse(grepl("18-24", intersectional_names), "18-24",
                    ifelse(grepl("25-69", intersectional_names), "25-69", "70+")),
         decade=ifelse(grepl("2000-2009", intersectional_names), "2000-2009", "2010-2018"))

# Plot
HED_percent_males <- plot_data %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = age, y = percent, fill = education)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  facet_grid(cols = vars(decade), rows = vars(race)) +
  theme(axis.title.x = element_blank(), legend.position = "bottom") +
  ylim(0, 100) +
  ggtitle("Percentage reporting 1+ occasion of HED in the last year, male drinkers") +
  labs(y = "percent")
HED_percent_males
ggsave("SIMAH_workplace/nhis/intersectionality/plots/Percent_HEDs_among_male_drinkers.png", dpi=300, width=33, height=19, units="cm")

HED_percent_females <- plot_data %>%
  filter(sex == "Female") %>%
  ggplot(aes(x = age, y = percent, fill = education)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  facet_grid(cols = vars(decade), rows = vars(race)) +
  theme(axis.title.x = element_blank(), legend.position = "bottom") +
  ylim(0, 100) +
  ggtitle("Percentage reporting 1+ occasion of HED in the last year, female drinkers") +
  labs(y = "percent")
HED_percent_females
ggsave("SIMAH_workplace/nhis/intersectionality/plots/Percent_HEDs_among_female_drinkers.png", dpi=300, width=33, height=19, units="cm")

## Explore ALC5UPYR distribution
# Full sample
full_sample %>% 
  count(ALC5UPYR) %>%
  mutate(percent = n/(sum(n))*100)
ggplot(full_sample, aes(x=ALC5UPYR), y) + geom_histogram(bins=100) + 
  ggtitle("Distribution of days HED, full sample")+ 
  xlab("Days of HED") +
  ylab("Frequency")
ggsave("SIMAH_workplace/nhis/intersectionality/plots/raw_distribution_no_days_HED_full_sample.png", dpi=300, width=33, height=19, units="cm")

# Drinkers only
drinkers %>% 
  count(ALC5UPYR) %>%
  mutate(percent = n/(sum(n))*100)
ggplot(full_sample, aes(x=ALC5UPYR), y) + geom_histogram(bins=100) + 
  ggtitle("Distribution of days HED, full sample")+ 
  xlab("Days of HED") +
  ylab("Frequency")
ggsave("SIMAH_workplace/nhis/intersectionality/plots/raw_distribution_no_days_HED_drinkers.png", dpi=300, width=33, height=19, units="cm")

# Majority (80% of full sample and 66% of drinkers) don't ever drink > 5 units therefore many zeros in the dataset.

### Specify null models based on raw data and check the assumptions
# Full sample
model_1 <- lm(ALC5UPYR ~ age_3_cats + decade + race_5_cats + education_3_cats + SEX, data = full_sample)
# Heteroskedasticity of residuals
plot(fitted(model_1), resid(model_1))
abline(h = 0, lty = 2, col = "red") +
title("Heteroskedasticity when running null model, continous HED, full sample")
# QQ plot
qqnorm(residuals(model_1))
qqline(residuals(model_1), col = "steelblue", lwd = 2)
# Assumptions of a linear model not met

# Check if transformation of HED is appropriate as data highly skewed
lambda <- forecast::BoxCox.lambda(drinkers$ALC5UPYR)  # 0.0099
# Value for lambda very close to 0 therefore log transformation appropriate

# generate a constant (half the value of the smallest positive value for ALC5UPYR)
c <- 0.5
drinkers$days_HED_log <- log(drinkers$ALC5UPYR + 0.5)
ggplot(drinkers, aes(x=days_HED_log), y) + geom_histogram(bins=100) + 
  ggtitle("Distribution of days HED log, drinkers")+ 
  xlab("Days of HED") +
  ylab("Frequency")
ggsave("SIMAH_workplace/nhis/intersectionality/plots/distribution_no_days_HED_drinkers_post_transformation.png", dpi=300, width=33, height=19, units="cm")

# Transformation ineffective therefore need to use binary variable