##############################################################################
# Plot of correlation between % drinkers and estimated grams for each intersection.
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
corr <- cor(plot_data_combo$pmn, plot_data_combo$estmn)

# Fit a linear regression model
model <- lm(estmn ~ pmn, data = plot_data_combo)

# Predicted values of Y based on the model
plot_data_combo$y_predicted <- predict(model)

# Calculate residuals
plot_data_combo$residuals <- residuals(model)

# Set a threshold for identifying outliers (e.g., consider points with residuals greater than 2 times the standard deviation of residuals as outliers)
threshold <- 2 * sd(plot_data_combo$residuals)

# Identify outliers
outliers <- plot_data_combo[abs(plot_data_combo$residuals) > threshold, ]

# Improve labels for outliers
outliers$intersectional_names <- gsub("Male 21-24", "Young, male,", outliers$intersectional_names)
outliers$intersectional_names <- gsub("NH ", "", outliers$intersectional_names)
outliers$intersectional_names <- gsub(" high school or less", ", low edu.", outliers$intersectional_names)
outliers$intersectional_names <- gsub(" some college", ", med. edu.", outliers$intersectional_names)
outliers$intersectional_names <- gsub("\\s+4\\+\\s*years\\s+college", ", high edu.", outliers$intersectional_names, ignore.case = TRUE)

# Plot data with outliers and correlation coefficient label
ggplot(plot_data_combo, aes(x = pmn, y = estmn)) +
  geom_point() +  
  xlim(0, 100) +
 # ylim(0, 28) +
  geom_smooth(method = "lm", se = FALSE) +  # Adding linear regression line
  labs(x = "Estimated proportion of current drinkers", y = "Estimated GPD", 
       title = "Correlation between estimated proportion of current drinkers and alcohol consumption amongst drinkers") +
 # theme_minimal() +
  geom_point(data = outliers, color = "red") +  # Highlight outliers in red
  geom_text(data = outliers, aes(label = paste(intersectional_names)), colour="black", size=3, vjust = -0.7) + # Add labels to outliers
  geom_text(data = NULL, aes(x = 89, y = 14, label = paste("Correlation coefficient:", round(corr, 2))), color = "black", size = 3, hjust = 0)  # Add label for correlation coefficient

ggsave(paste0(outputs,"Plot of correlation between estimated proportion of current drinkers and alcohol consumption amongst drinkers.png"), dpi=300, width=33, height=19, units="cm")
