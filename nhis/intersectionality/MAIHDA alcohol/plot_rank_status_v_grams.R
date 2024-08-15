##############################################################################
# Plot of correlation between % drinkers and estiamted grams for each intersection.
##############################################################################

######################################################################## Set-up
setwd("C:/Users/cmp21seb/Documents/SIMAH/")
code <- "SIMAH_code/nhis/intersectionality/MAIHDA alcohol/"
inputs <- "SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/inputs/"
models <- "SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/models/"
outputs <- "SIMAH_workplace/nhis/intersectionality/MAIHDA alcohol/outputs/"

library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(labelled)
library(haven)
library(ggplot2)
library(forcats)

## Bias toward non-scientific notation
options(scipen=10)

# Set default theme for plots:
theme_set(theme_bw(base_size = 12))

# Read in data of estimated grams
plot_data_1 <- readRDS(paste0(outputs,"grams drinkers/results_grams_drinkers.rds")) %>%
# Prep data for plotting
  mutate(sex=ifelse(grepl("Female",intersectional_names),"Women","Men"),
         race=ifelse(grepl("Asian",intersectional_names),"Asian",
                     ifelse(grepl("Black", intersectional_names),"Black",
                            ifelse(grepl("AI/AN", intersectional_names), "AI/AN",
                                   ifelse(grepl("Multiple race", intersectional_names), "Multiple race",
                                          ifelse(grepl("Hispanic", intersectional_names), "Hispanic","White"))))),
         race=factor(race, levels=c("Hispanic", "AI/AN", "Asian","Black", "Multiple race", "White")),
         education=ifelse(grepl("high school", intersectional_names), "high school or less",
                          ifelse(grepl("some college", intersectional_names), "some college", "college+")),
         education=factor(education,levels=c("high school or less", "some college", "college+")),
         age=ifelse(grepl("21-24", intersectional_names), "21-24",
                    ifelse(grepl("25-59", intersectional_names), "25-59", "60+")))

# Read in data of estimated % current drinkers
plot_data_2 <- readRDS(paste0(outputs,"binary drinking status/results binary drinking status.rds")) %>%
# Prep data for plotting
  mutate(sex=ifelse(grepl("Female",intersectional_names),"Women","Men"),
         race=ifelse(grepl("Asian",intersectional_names),"Asian",
                     ifelse(grepl("Black", intersectional_names),"Black",
                            ifelse(grepl("AI/AN", intersectional_names), "AI/AN",
                                   ifelse(grepl("Multiple race", intersectional_names), "Multiple race",
                                          ifelse(grepl("Hispanic", intersectional_names), "Hispanic","White"))))),
         race=factor(race, levels=c("Hispanic", "AI/AN", "Asian","Black", "Multiple race", "White")),
         education=ifelse(grepl("high school", intersectional_names), "high school or less",
                          ifelse(grepl("some college", intersectional_names), "some college", "college+")),
         education=factor(education,levels=c("high school or less", "some college", "college+")),
         age=ifelse(grepl("21-24", intersectional_names), "21-24",
                    ifelse(grepl("25-59", intersectional_names), "25-59", "60+")))

plot_data_combo <- inner_join(plot_data_1, plot_data_2)

# Calculate correlation coefficient
correlation_coefficient <- cor(plot_data_combo$pmn, plot_data_combo$estmn)

# Plotting
ggplot(plot_data_combo, aes(x = pmn, y = estmn)) +
  geom_point() +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE) +  # Adding linear regression line
  labs(x = "Estimated % of current drinkers", y = "Estimated grams per day", 
  title = "Correlation between estimated proportion of current drinkers and alcohol consumption amongst drinkers") +
  theme_minimal()
