# SIMAH Restricted-access Data
# Causal Mediation  


# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(timereg)    # additive survival models
library(VGAM)       # multinomial regression, needed for causal mediation
library(MASS)       # needed for causal mediation functions
library(knitr)      # formatted table
library(survey)
library(srvyr)
library(broom)

# Specify the data and output file locations

data <- "~/Dropbox/Mac/Desktop/Public Health Institute/Alcohol Research Group/SIMAH/SIMAH_workplace/nhis/Restricted access data/Data/"
output <- "~/Dropbox/Mac/Desktop/Public Health Institute/Alcohol Research Group/SIMAH/SIMAH_workplace/nhis/Restricted access data/Causal mediation/Race x Lifestyle/CausMed/18-85 years old/Extensions/"

source("5_Causal Mediation functions.R")

 
# Load data
nhis18_85 <- readRDS(file.path(data, "nhis18_85.rds"))
nhis18_male <- readRDS(file.path(data, "nhis18_male.rds")) 
nhis18_female <- readRDS(file.path(data, "nhis18_female.rds"))



library(gmodels)
CrossTable(nhis18_85$alcohol5v2.factor, nhis18_85$smk.factor, expected = T, prop.r = T, prop.c = T, prop.chisq = T, sresid = T)


library(nnet)
m1 <- multinom(alcohol5v2.factor ~ smk.factor + female + married.factor + edu.factor + srvy_yr, data = nhis18_85)
m1_results <- tidy(m1, exponentiate = T, conf.int = T) %>% data.frame() 
m1_results %>% mutate_at(c("estimate", "std.error", "statistic", "conf.low", "conf.high"), list(~round(., digits = 2)) ) %>%
  mutate(p.value = case_when(p.value < 0.001 ~ "<0.001",
                             p.value < 0.01 & p.value >= 0.001 ~ "<0.01",
                             p.value < 0.05 & p.value >= 0.01 ~ "<0.05",
                             p.value >= 0.05 ~ "insignificant")) %>%
  select(-std.error, -statistic) %>%
  rename(OR = estimate)



# check the interaction effect between the exposure (ethnicity) and mediator (alcohol)

aalen_int <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(ethnicity)*const(alcohol5v2) + const(female) + const(married.factor) + 
                     const(edu.factor) + const(srvy_yr), data = nhis18_85)

aalen_int_results <- as.data.frame(cbind(aalen_int$gamma, diag(aalen_int$robvar.gamma))) %>%
  mutate (variable = rownames(.),
          var = V2,
          p.value_Deaths = round(2*pnorm(-abs(estimate / sqrt(var))),3),
          p.value_Deaths = ifelse(p.value_Deaths <.001, "<.001", p.value_Deaths),
          lower.ci = round((estimate - (1.96 * sqrt(var)))*100000, 1),
          upper.ci = round((estimate + (1.96 * sqrt(var)))*100000, 1),
          estimate_100000py = round(estimate*100000, 1),
          Deaths_CI_100000py = paste0("(",lower.ci,", ", upper.ci, ")")) %>%
  select (variable, estimate_100000py, Deaths_CI_100000py, p.value_Deaths) %>%
  filter(str_detect(variable, "SES|lifestyle")) %>%
  mutate(variable = str_remove(variable, fixed("const(SES)")),
         variable = str_remove(variable, fixed("const(lifestyle)"))) %>%
  add_row(variable = "INTERACTION MODELS", .before=1)%>%
  remove_rownames()

# OBJECTIVE 2: Causal Mediation

# The causal mediation analyses involves four steps:
# 1) Fit separate multinomial logistic regressions with each mediator (M1, M2, M3, and M4) as the outcome.
# 2) Create copies of the dataset to account for all possible combinations of the exposure and mediators. 
# 3) Using the expanded dataset, calculate weights for each mediator using the predicted probabilities from Step 1. 
# 4) Fit a marginal structural model using Aalen additive hazards with the weight as weight and the id as a cluster level; this ensures thatrobust standard errors are calculated. The model with robust variance and resampling (robust=TRUE) was not used because of computation limiations.  

# For more details and theoretical justification/description see:
# Lange et al. 2014 https//doi.org/10.1093/aje/kwt270
# Lange et al. 2012 https//doi.org/10.1093/aje/kwr525
# Lange et al. 2011 https//doi.org/10.1097/EDE.0b013e31821c680c


# Data Preparation ------------------------------------------------------------------------------------------------------------------
CMed_race3_prep(nhis18_female) %>% saveRDS(paste0(output, "expandedData_fem_weighted.rds"))
CMed_race3_prep(nhis18_male)   %>% saveRDS(paste0(output, "expandedData_male_weighted.rds"))


# Run Analyses, WOMEN ----------------------------------------------------------------------------------------------------------------

# Load data
expandedData <- readRDS(file.path(output, "expandedData_fem_weighted.rds")) %>%
  filter(complete.cases(ID, bl_age, end_age, allcause_death, A.race, race_M1.alc, race_M2.smk, race_M3.bmi, race_M4.phy,
                        married.factor, edu.factor, srvy_yr, weightM))

hist(expandedData$weightM)

# Run model
CMed_f <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.race) + const(race_M1.alc) + const(race_M2.smk) + const(race_M3.bmi) + const(race_M4.phy) +
                                                        const(married.factor) + const(edu.factor) + const(srvy_yr),  # adjust for survey year as continuous variable 
                          data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)  
                
saveRDS(CMed_f, file.path(output, "CMed_f_weighted.rds")) # Save model results      



model_coefficients <- coef(CMed_f) %>%
  row.names() %>% 
  as.data.frame() %>%
  rownames_to_column()

print("model coefficients"); print(model_coefficients)
print("Black"); print(Black)
print("Hispanic"); print(Hispanic)


# Load model and view results
CMed_model <- readRDS(file.path(output, "CMed_f_weighted.rds"))  # load model (if needed)
Black <- c(1, 3, 5, 7, 9)             # List the coefficients of interest 
Hispanic <- c(2, 4, 6, 8, 10)  
format_CMed(CMed_model, Black) %>% kable()    # print formatted results

CMed_women_black <- format_CMed (CMed_model, Black)
CMed_women_hispanic <- format_CMed (CMed_model, Hispanic)
CMed_women <- rbind(CMed_women_black, CMed_women_hispanic)



# Run Analyses, MEN ----------------------------------------------------------------------------------------------------------------

# Load data
expandedData <- readRDS(file.path(output, "expandedData_male_weighted.rds"))


# Run Model
CMed_m <- aalen(Surv(bl_age, end_age, allcause_death) ~ const(A.race) + const(race_M1.alc) + const(race_M2.smk) + const(race_M3.bmi) + const(race_M4.phy) +
                const(married.factor) + const(edu.factor) + const(srvy_yr),  
                data=expandedData, weights=expandedData$weightM, clusters=expandedData$ID, robust=0)

saveRDS(CMed_m, file.path(output, "CMed_m_weighted.rds"))  # Save model results     

# Load model and view results
CMed_model <- readRDS(file.path(output, "CMed_m_weighted.rds"))  # load model (if needed)
Black <- c(1, 3, 5, 7, 9)             # List the coefficients of interest 
Hispanic <- c(2, 4, 6, 8, 10)  
format_CMed (CMed_model, Black) %>% kable()    # print formatted results
format_CMed (CMed_model, Hispanic) %>% kable() 

CMed_men_black <- format_CMed (CMed_model, Black)
CMed_men_hispanic <- format_CMed (CMed_model, Hispanic)
CMed_men <- rbind(CMed_men_black, CMed_men_hispanic)


# COMBINE Results

colnames(CMed_men)   <- paste0("men_", colnames(CMed_men))
colnames(CMed_women) <- paste0("women_", colnames(CMed_women))
CMed_table <- cbind(CMed_men, CMed_women) %>% rename(term = men_label) %>% 
  mutate(race = rep(c("Black", "Hispanic"), each = 11) ) %>%
  relocate(race) %>% dplyr::select(-women_label)
  
CMed_table
write.csv(CMed_table, file=paste0(output, "Table2 Causal Mediation results.csv")) # save results
