#-------------------------------------------------------------------------------
# MULTILEVEL MODELLING - PRACTICAL
#-------------------------------------------------------------------------------
# RANDOM-INTERCEPT MODELS WITH COVARIATES
#-------------------------------------------------------------------------------

# Load required packages
library(foreign)  # Importing Stata datasets
library(ggplot2)  # Graphs
library(lme4)     # Estimating multilevel models 
library(lmerTest) # Printing p-values for the regression coefficients
library(lmtest)   # Conducting likelihood ratio tests
library(tidyverse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ragg)
library(bayesplot)
library(coda)
library(memisc)

# Set the working directory
setwd("C:/Users/cmp21seb/Documents/SIMAH/")

# Load the dataset and assign it to the object mydata
mydata <- readRDS("SIMAH_workplace/nhis/intersectionality/cleaned_data/new spec August 2023/grams/grams_data_pre_maihda_main.rds")

# Return the first part of the data frame
head(mydata)

# Add a constant
mydata <- mydata %>% mutate(cons = 1)

#-------------------------------------------------------------------------------
# DATA STRUCTURE AND DESCRIPTIVE STATISTICS
#-------------------------------------------------------------------------------

# Describe all variables in the data
str(mydata)
# There are 516,439 observations on 69 variables. The level-2 ID is intersection.
# The level-1 ID is NHISPID. 

# The number of observations (516,439) corresponds to the number of individuals (level-1)

# Check the number intersections (level-2 units) there are in the data
length(unique(mydata$intersections))

# There are 516,439 individuals (level-1) nested within 108 schools (level-2)

# Summarise all variables in the data: This includes the mean and 
# five-number summary and frequency tables for categorical variables
summary(mydata)

# Plot age capped_daily_grams_log against intersections

# Using base R
plot(mydata$intersections, mydata$capped_daily_grams_log)

# Using the ggplot2 package
ggplot(mydata, aes(x = intersections, y = capped_daily_grams_log)) +
  geom_point()

# Generate the school means of age 16 scores
mydata$intersectionalmean <- ave(exp(mydata$capped_daily_grams_log), mydata$intersections, FUN = mean, 
                         na.rm = TRUE)
# The ave() function allows us to repeat a command across groups. Here we
# use it to calculate the mean of normexam separately for each school. 
# The function ave() can be used with any function. 
# Here, we use the mean() function to calculate the mean age 16 score 
# (within each school).
# The option na.rm has been set to TRUE to calculate the mean after removing 
# missing values. Otherwise, the mean() function would return NA if there are 
# any missing data.

# Add the school means to the graph
ggplot(mydata,aes(x = intersections)) +
  geom_point(aes(y = exp(capped_daily_grams_log), colour = "NHISPID")) +
  geom_point(aes(y = intersectionalmean, colour = "intersections" )) +
  scale_colour_manual(guide = "legend", 
                      values = c("individual" = "black", "intersections"  = "red"), 
                      labels = c("individual" = "individual grams", 
                                 "intersections" = "intersectional mean"),
                      name=NULL)
# Including the argument "colour" in the aes() function for the geom_point()
# function twice allow us to add labels to the individual and school 
# predictions. We then define the colours and labels using the 
# scale_colour_manual() function, black for individual and red for school 
# predictions.



#-------------------------------------------------------------------------------
# MODEL 1
#-------------------------------------------------------------------------------

# Fit Model 1
m1 <- lm(capped_daily_grams_log ~ 1, data = mydata)
# R would normally include the intercept automatically, but to estimate
# a single-level model with no covariates, we include '1' (an intercept)
# as an independent variable in the formula.
summary(m1)

# Display confidence intervals for the model parameters
confint(m1)

# The log-likelihood statistic can be recovered using the logLik() function
logLik(m1)

# Calculate the deviance for Model 1 automatically and with full precision by
-2*logLik(m1)

# Referring to the stored estimation results in this way is safer than typing
# in values by hand as it avoids making typos. Calculations will also be made
# using full precision (we avoid making rounding errors). Also, if we change 
# the underlying data and rerun the script the values of the log-likelihood 
# will automatically update.



#-------------------------------------------------------------------------------
# MODEL 2
#-------------------------------------------------------------------------------

# Fit Model 2
m2 <- lmer(capped_daily_grams_log ~ 1 + (1 | intersections), data = mydata, REML = FALSE)
summary(m2)
# The lmer() function requires a formula that separates the fixed and random 
# parts of the model. '~ 1' includes the fixed-part intercept, while 
# '(expr|level id)' specifies the random part.
# Note that a fixed-part intercept/constant would be included automatically
# in the fixed part of the model but not in the random part.
# We do not need to explicitly request for the fixed-part intercept,
# but we do need to request for a random intercept by including 1 in the 
# random part specification (1 | school).
# The level-1 random part, that is the usual observation-level residual, is 
# added automatically and so does not need to be specified.
# We fit the model using maximum likelihood (MLE) instead of the restricted
# maximum likelihood (REML) default. REML is preferred when there are few 
# groups.

# Display confidence intervals for the model parameters
confint(m2)

# The deviance of the model is reported by default, but it could also be
# recovered using
-2*logLik(m2)

# or
deviance(m2)

# Calculate the VPC/ICC for Model 2

# There is no command in the lme4 package to recover the VPC/ICC, but we can
# access the estimates of the random parameters using the VarCorr() function
# of the lme4 package. We need to store these estimates as a data frame
# to be able to manipulate them to calculate the VPC/ICC
rpm2 <- as.data.frame(VarCorr(m2))
rpm2

# VPC/ICC = var(u)/[var(u) +var(e)]
rpm2$vcov[rpm2$grp == "intersections"] / sum(rpm2$vcov)
# The ICC is 0.158 


# Perform a LR test of Model 1 vs. Model 2
lrtest(m1, m2)
# Here all we need to do use the lrtest() function from the lmtest package.
# This command reports the LR test comparing the current model to the 
# corresponding single-level version of the same model. 
# The LR test of 499 exceeds the critical value of 3.84 and so the 
# p-value is less than 0.05. The two-level model (Model 2) is therefore 
# preferred to the single-level model (Model 1).

# R warns us about using models of different classes to make sure
# that they are comparable. In this case, we know they can be compared.

#-------------------------------------------------------------------------------
# MODEL 3
#-------------------------------------------------------------------------------

# Fit Model 3
m3 <- lmer(capped_daily_grams_log ~ 1 + SEX + age_diaz + education_3_cats + race_6_cats + (1 | intersections), data = mydata, REML = FALSE)
summary(m3)

# Display confidence intervals for the model parameters
confint(m3)

# Predict the average school line
mydata$xb <- predict(m3, re.form = NA)
# The predict() function with the re.form = NA argument (meaning no random effects) predicts the
# fixed-portion of the linear predictor. That is, that part of the model
# involving the regression coefficients and which describes the relationship 
# in the average school.
# 
# # Plot the average school line on top of a scatterplot of age 16 scores
# # against age 11 scores
# ggplot(mydata, aes(x = standlrt, y = normexam, colour = "student")) +
#   geom_point() +
#   geom_line(aes(y = xb, colour = "school" )) +
#   scale_colour_manual(guide = "legend", 
#                       values = c("student" = "black", "school" = "red"),
#                       labels = c("student" = "Student scores", 
#                                  "school" = "Prediction"),
#                       name=NULL)

# Predict the fitted school lines
mydata$xbu <- predict(m3)
# By default, the predict() function gives the fitted values. That is, the 
# fixed-portion of the linear prediction plus contributions based on 
# predicted random effects (i.e. by default re.form = NULL, meaning all random effects are included)

# # Plot the 65 school lines
# ggplot(mydata,aes(x = standlrt, y = normexam, colour = "student")) +
#   geom_point() +
#   geom_line(aes(y=xbu, colour = "school" , group=school)) +
#   scale_colour_manual(guide = "legend", 
#                       values = c("student" = "black", "school" = "red"),
#                       labels = c("student" = "Student scores", 
#                                  "school" = "Prediction"),
#                       name=NULL)

# The package lme4 provides different functions to recover the estimation
# results. The estimated model parameters can be recovered using the
# fixef() function, and the vcov() function recovers the estimated sampling
# covariance matrix for the fixed part of the model and, as shown before,
# the VarCorr() function recovers the random part estimates
fixef(m3)
vcov(m3)
rpm3 <- as.data.frame(VarCorr(m3))
rpm3

# Calculate the proportion of between-school variance explained by fixed effects
(rpm2$vcov[rpm2$grp == "intersections"] - rpm3$vcov[rpm3$grp == "intersections"]) /
  rpm2$vcov[rpm2$grp == "intersections"] # PCV 94.5%

# # Calculate the proportion of individual-variance explained by fixed effects ??
# (rpm2$vcov[rpm2$grp == "Residual"] - rpm3$vcov[rpm3$grp == "Residual"])/ 
#   rpm2$vcov[rpm2$grp == "Residual"]

# VPC/ICC = var(u)/[var(u) + var(e)]
rpm3$vcov[rpm3$grp == "intersections"] / sum(rpm3$vcov)

# Predict the school random effects and their standard errors
u0 <- data.frame(ranef(m3), condVar = TRUE)
head(u0)
# The variable 'condval' contains the estimated random intercepts, while the
# variable 'condsd' contains the estimated standard errors for the random
# intercepts

# To help understand shrinkage, generate the unshrunken residuals we first 
# need to know the number of individuals in each intersection.
mydata$n <- ave(mydata$cons, mydata$intersections, FUN = sum, na.rm = TRUE)
head(mydata$n)
# This is another use of the function ave(), this time adding 1s to
# count the number of students within each strata.
# Then we add this information to the data frame u0 with the predicted
# random effects using the merge() function. Here we use u0 as the main 
# data frame and add information from the mydata data frame, being careful 
# of only using intersection-level information on the variables we need 
# (intersection and n). The variable that identifies intersections is called 'grp'
# in the u0 data frame and 'intersections' in the mydata data frame. 
u0 <- merge(x = u0,
            y = mydata[!duplicated(mydata$intersections), c("intersections", "n")],
            by.x = "grp", by.y = "intersections")
head(u0)

# Calculate the unshrunken residuals.
# Unshrunken residuals = shrunken residuals/ shrinkage factor
u0$unshrunken <- u0$condval /                    # shrunken residuals
  (rpm3$vcov[rpm3$grp == "intersections"] /             # Var(u)
    (rpm3$vcov[rpm3$grp == "intersections"] +           # Var(u)
      rpm3$vcov[rpm3$grp == "Residual"] / u0$n)) # Var(e)/nj
head(u0)


# Plot the shrunken residuals against the unshrunken residuals
ggplot(u0, aes(x = unshrunken, y = condval, label = grp)) +
  geom_text() +
  geom_abline(slope = 1) 
# Here we have used the function geom_abline() to plot a 45 degree line on
# the top of a scatterplot of the shrunken and unshrunken residuals.
# We have used the geom_text() function and the label aesthetic to plot the
# school ids instead of points. 

# Plot a caterpillar plot of the school effects

# Calculate the lower and upper values of the confidence intervals
u0$lower <- u0$condval - 1.96 * u0$condsd
u0$upper <- u0$condval + 1.96 * u0$condsd

# Calculate the school rank
u0$rank <- rank(u0$condval)
head(u0)

# Plot
ggplot(u0, aes(x = rank, y = condval, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  geom_errorbar() +
  geom_point()
# We added the geom_hline() function to plot a horizontal line at 0


# Predict the grams for Male 21-24 AI/AN 4+years college
# fem5["(Intercept)"] + fem5["girl"]
# 
# # Predict the age 16 score for an average boy in a boys' school
# fem5["(Intercept)"] + fem5["schgendboysch"]
# 
# # Predict the age 16 score for an average girl in a girls' school
# fem5["(Intercept)"] + fem5["girl"] + fem5["schgendgirlsch"]

# Using the predict() function 

# Create a data frame with the values for which we will predict the grams.
# The data frame needs to have the same variables as the model
pdata <- data.frame(SEX = c("Male", "Female"),
                    education_3_cats = c("4+ years college"),
                    race_6_cats = c("AI/AN"),
                    age_diaz = c("21-24"),
                    intersections = c("52", "106"))
pdata

# Predict as estimated in model 3 and for the values in pdata
round(predict(m3, pdata, re.form = NA), 4)

# Compare to estimates from null model
round(predict(m2, pdata, re.form = NA), 4)

## Summary: When just using the intersections, ignoring year and using predict, very similar, as if generated on a middling year