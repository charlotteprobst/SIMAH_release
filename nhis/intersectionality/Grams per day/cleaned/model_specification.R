setwd("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_code/nhis/intersectionality/")

# Load relevant packages
library(tidyr)
library(dplyr)
library(haven)
library(lmtest)
library(car)

# Read in drinkers data
drinkers <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/nhis_alc_clean_drinkers_only.RDS")

# Read in full sample data
full_sample <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/nhis_alc_clean_full_sample.RDS")

# pre-transformation, full sample
ggplot(full_sample, aes(x=alc_daily_g_capped_200), y) + geom_histogram(bins=100) + 
  theme(plot.title = element_text(size=12)) +
  ggtitle("Distribution of daily grams of alcohol amongst NHIS sample adults") +
  xlim(0,100) + ylim(0,50000) + 
  xlab("Daily grams of alcohol, pre-transformation") +
  ylab("Frequency")

# pre-transformation, drinkers
ggplot(drinkers, aes(x=alc_daily_g_capped_200), y) + geom_histogram() + 
  ggtitle("Distribution of daily grams pre transformation, drinkers only")+ 
  xlim(0,75) + ylim(0,30000) + 
  xlab("Daily grams of alcohol, pre transformation") +
  ylab("Frequency")

### Specify null models based on raw data and check the assumptions
# NB. The variables included in the model are chosen based on theoretical rationale, data availability, the results of exploratory analysis and feasbile numbers of intersections/intersectional group sizes.

# Full sample
model_1 <- lm(alc_daily_g_capped_200 ~ age_3_cats + decade + race_5_cats + education_3_cats + SEX, data = full_sample)
# Heteroskedasticity of residuals
plot(fitted(model_1), resid(model_1))
abline(h = 0, lty = 2, col = "red")
# QQ plot
qqnorm(residuals(model_1))
qqline(residuals(model_1), col = "steelblue", lwd = 2)

# Drinkers only
model_2 <- lm(alc_daily_g_capped_200 ~ age_3_cats + decade + race_5_cats + education_3_cats + SEX, data = drinkers)
# Heteroskedasticity of residuals
plot(fitted(model_2), resid(model_2))
abline(h = 0, lty = 2, col = "red")
# QQ plot
qqnorm(residuals(model_2))
qqline(residuals(model_2), col = "steelblue", lwd = 2)

# Assumptions not met with raw grams data therefore transform data (either for full sample or drinkers only).  

### Transform data

# Full sample
full_sample <- full_sample %>% mutate(new_grams = alc_daily_g_capped_200 + 0.02)# add half of the smallest grams value (for drinkers) to zero values
# Check recommended lambda with boxcox
b <- MASS::boxcox(lm(full_sample$new_grams ~ 1))
lambda <- b$x[which.max(b$y)] # -0.06
lambda2 <- forecast::BoxCox.lambda(full_sample$new_grams)  # -0.07
# As both suggested lambda are close to 0, log transformation is appropriate
full_sample$capped_daily_grams_log <- log(full_sample$new_grams)
# Distribution plot 
ggplot(full_sample, aes(x=capped_daily_grams_log), y) + geom_histogram(bins=200) + xlim(-4,6) +
  ylim(0,22000) + ggtitle("Distribution of estimated daily grams post transformation, full sample")+ 
  xlab("Daily grams of alcohol, post transformation") +
  ylab("Frequency")

# Drinkers only
# Check recommended lambda with boxcox
b <- MASS::boxcox(lm(drinkers$alc_daily_g_capped_200 ~ 1))
lambda <- b$x[which.max(b$y)] # 0.0606
lambda2 <- forecast::BoxCox.lambda(drinkers$alc_daily_g_capped_200)  # -0.054
# As the suggested lambda are close to 0, log transformation is appropriate
drinkers$capped_daily_grams_log <- log(drinkers$alc_daily_g_capped_200)
# Distribution plot
ggplot(drinkers, aes(x=capped_daily_grams_log), y) + geom_histogram(bins=100) + 
  ggtitle("Distribution of daily grams of alcohol amongst drinkers(log-transformed)")+ 
  xlab("Daily grams of alcohol, post transformation") +
  ylab("Frequency")

### Specify null models based on transformed data and check assumptions

# Full sample
model_3 <- lm(capped_daily_grams_log ~ age_3_cats + decade + race_5_cats + education_3_cats + SEX, data = full_sample)
# Heteroskedasticity of residuals
plot(fitted(model_3), resid(model_3))
abline(h = 0, lty = 2, col = "red")
# QQ plot
qqnorm(residuals(model_3))
qqline(residuals(model_3), col = "steelblue", lwd = 2)

# Drinkers only
model_4 <- lm(capped_daily_grams_log ~ age_3_cats + decade + race_5_cats + education_3_cats + SEX, data = drinkers)
# Heteroskedasticity of residuals
plot(fitted(model_4), resid(model_4))
abline(h = 0, lty = 2, col = "red")
# QQ plot
qqnorm(residuals(model_4))
qqline(residuals(model_4), col = "steelblue", lwd = 2)

### Best homoskedascity and QQplot with model 4

# Compare AIC all models
AIC(model_1, model_2, model_3, model_4) 
# model 4 has lowest AIC
# NB. although noted that models of drinkers & full sample have different numbers of obs)

### Check further assumptions of chosen model (model 4)

# Ramsey's RESET test for functional form. H0: that the model has no omitted variables.
resettest(model_4, power = 2:3, type = c("fitted", "regressor","princomp"), data = transformed_drinkers)
# Interpretation: Can reject the null hypothesis i.e. omitted variables likely.  

# Multicolinearity
car::vif(model_4)
# Interpretation: Very low VIF values, suggesting no issues of multicollinearity

# Compare predicted_transformed alc_daily_g versus actual transformed alc_daily_g
pred_lr_1 <- predict(model_4, interval = 'confidence', type = 'response')
par(mfrow=c(1,2))
hist(pred_lr_1, col = "blue", breaks = 10, xlim=c(-4,8), xlab=("transformed daily grams"), main="modelled data - lr1")
hist(transformed_drinkers$capped_daily_grams_log, col = "blue", breaks = 10, xlim=c(-4,8), xlab=("transformed daily grams"), main="nhis data")

# save transformed data
saveRDS(drinkers, "C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/nhis/intersectionality/cleaned_data/log_transformed_drinkers.RDS")
