# BMI analysis 

# SIMAH October 2021 - code to take the processed BRFSS data file and up-shift to per-capita consumption on a state by state basis
# this code also adjusts the BRFSS monthly alcohol consumption to annual alcohol consumption using data from the NAS 
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(foreign)
library(SASxport)
library(readr)
library(dplyr)
library(tidyr)
library(labelled)
library(sjlabelled)
library(tidyverse)
library(naniar)
library(splitstackshape) 
library(truncnorm)

# CB laptop directory
wd <- "~/Google Drive/SIMAH Sheffield/"
# CB desktop directory
# wd <- "G:/My Drive/SIMAH Sheffield/"
setwd(wd)

####read in the joined up data files 
dataFiles <- readRDS("SIMAH_workplace/brfss/processed_data/brfss_full_selected.RDS")
gc()

# bind years together 
data <- do.call(rbind, dataFiles)

test <- data %>% group_by(YEAR, sex_recode) %>% 
  summarise(meanBMI_calculated = mean(BMI, na.rm=T),
            meanBMI_derived = mean(BMI_derived, na.rm=T)) %>% 
  pivot_longer(cols=meanBMI_calculated:meanBMI_derived)

ggplot(data=test, aes(x=YEAR, y=value, colour=name)) + geom_line() +
  facet_grid(cols=vars(sex_recode))

test <- data %>% group_by(YEAR, sex_recode) %>% 
  mutate(obese_calculated = ifelse(BMI>=30, 1,0),
         obese_derived = ifelse(BMI_derived>=30, 1,0)) %>% 
  summarise(calculated= mean(obese_calculated, na.rm=T),
            derived = mean(obese_derived, na.rm=T)) %>% 
  pivot_longer(cols=calculated:derived) %>% drop_na()

ggplot(data=test, aes(x=YEAR, y=value, colour=name)) + geom_line() +
  facet_grid(cols=vars(sex_recode)) + theme_bw() + ylab("% BMI >=30")

data$BMI_derived = ifelse(data$YEAR<=1986, data$BMI, data$BMI_derived)

data <- data %>%
  dplyr::select(YEAR, State, final_sample_weight, race_eth,
                sex_recode, age_var,
                employment, education_summary,
                household_income,
                marital_status,
                BMI, BMI_derived, drinkingstatus,
                alc_frequency, quantity_per_occasion,
                gramsperday, 
                mentalhealth,physicalhealth,
                # household_income
                hed) %>% drop_na(YEAR, State, race_eth, sex_recode, age_var,
                                 gramsperday, drinkingstatus)


# impute the missing BMI values - so as to not ruin the current alcohol data 
toimpute <- data %>% dplyr::select(YEAR, race_eth, sex_recode, age_var, 
                                   education_summary, employment, marital_status, drinkingstatus,
                                   alc_frequency, gramsperday, BMI_derived) %>% 
  mutate_at(c("race_eth","sex_recode", "education_summary","employment","marital_status","drinkingstatus"), as.factor)

library(mice)
imputed <- mice(toimpute, 1, method=c("","","","","polyreg",
                                      "logreg","logreg","","","",
                                      "pmm"), maxit=5, ridge=0.001, threshold=1)
imputedBMI <- complete(imputed)
summary(imputedBMI$BMI_derived)

write.csv(imputedBMI, "SIMAH_workplace/brfss/processed_data/imputedBMI.csv", row.names=F)

data$BMI_derived_imputed <- imputedBMI$BMI_derived
data$employment_imputed <- imputedBMI$employment
data$education_summary_imputed <- imputedBMI$education_summary
data$marital_status_imputed <- imputedBMI$marital_status

summary(data$BMI_derived_imputed)
summary(data$BMI_derived)

summary(as.factor(data$employment))
summary(data$employment_imputed)

summary(as.factor(data$education_summary))
summary(data$education_summary_imputed)

summary(as.factor(data$marital_status))
summary(data$marital_status_imputed)

imputedBMI$hed <- data$hed

imputedHED <- mice(imputedBMI, m=1, method=c("","","","","","","","","","","","pmm"), maxit=5)

completeHED <- complete(imputedHED)

data$HED_imputed <-  completeHED$hed
  
summary(data$hed)
summary(data$HED_imputed)

summary(data)

data_new <- data %>% 
  dplyr::select(YEAR, State, final_sample_weight, race_eth,
                sex_recode, age_var, employment_imputed, education_summary_imputed,
                household_income, marital_status_imputed, BMI_derived_imputed,
                drinkingstatus, alc_frequency, quantity_per_occasion, gramsperday, 
                mentalhealth, physicalhealth, HED_imputed) %>% 
  rename(employment = employment_imputed,
         education_summary = education_summary_imputed,
         marital_status = marital_status_imputed,
         BMI = BMI_derived_imputed,
         hed = HED_imputed) %>% drop_na(YEAR, State, final_sample_weight, race_eth,
                                        sex_recode, age_var, employment, education_summary, 
                                        BMI, drinkingstatus, alc_frequency, gramsperday, hed)

saveRDS(data_new, "SIMAH_workplace/brfss/processed_data/brfss_selected_imputed.RDS")




