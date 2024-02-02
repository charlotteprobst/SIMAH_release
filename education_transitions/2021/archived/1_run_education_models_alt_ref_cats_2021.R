# script to generate MSM model for education for education transitions paper
library(splitstackshape)
library(dplyr)
library(msm)
library(readr)
library(tidyr)
library(readxl)
library(doParallel)
library(foreach)
library(parallel)
library(readxl)

setwd("C:/Users/cmp21seb/Documents/SIMAH/")

source("SIMAH_code/education_transitions/2021/functions/0_setup_education_model_2021.R")

# Read in prepped model data
data <- read_rds("SIMAH_workplace/education_transitions/2021/data_to_model/prepped_data_for_markov_2021.rds")

### RUN MODELS COVERING WHOLE TIME, WITH A COVARIATE FOR THE TIME PERIOD

### BASED ON FOUR TIME PERIODS
datat5 <- data
datat5$timevary <- cut(data$year,
                     breaks=c(0,2005,2011,2018, 2021),
                     labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))
# Relevel the 'timevary' variable to set "2012-2018" as the reference category
datat5$timevary <- relevel(datat5$timevary, ref = "2012-2018")

# specify models

Q <- rbind( c(0.5, 0.5, 0, 0, 0),
            c(0, 0.5, 0.5, 0, 0),
            c(0, 0, 0.5, 0.5, 0),
            c(0, 0, 0, 0.5, 0.5),
            c(0, 0, 0, 0, 0.5))

Q5 <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat5)
modelt5 <- msm(educNUM~year, newID, data=datat5, qmatrix=Q5,
               center=FALSE,
               covariates=~agecat + sex + racefinal2 + timevary,
               control=list(trace=1, fnscale=271181, maxit=200))

saveRDS(modelt5, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_alt_ref_cats.RDS")

# Run a model with an interaction term for race
modelt5_interaction_race <- msm(educNUM~year, newID, data=datat5, qmatrix=Q5,
               center=FALSE,
               covariates=~agecat + sex + racefinal2 + timevary + racefinal2*timevary,
               control=list(trace=1, fnscale=271181, maxit=200))
saveRDS(modelt5_interaction_race, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_interaction_race.RDS")

# Run a model with an interaction term for sex
modelt5_interaction_sex <- msm(educNUM~year, newID, data=datat5, qmatrix=Q5,
                                center=FALSE,
                                covariates=~agecat + sex + racefinal2 + timevary + sex*timevary,
                                control=list(trace=1, fnscale=271181, maxit=200))
saveRDS(modelt5_interaction_sex, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_interaction_sex.RDS")

# Run separate models for each SEX group to enable interactions (based on model 5)

sex_data <- data
sex_data$timevary <- cut(sex_data$year,
                 breaks=c(0,2005,2011,2018, 2021),
                 labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))
# Relevel the 'timevary' variable to set "2012-2018" as the reference category
sex_data$timevary <- relevel(sex_data$timevary, ref = "2012-2018")


# Men
men <- sex_data %>% filter(sex==0)
Q_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=men)
modelt5_men <- msm(educNUM~year, newID, data=men, qmatrix=Q_men,
               center=FALSE,
               covariates=~agecat + racefinal2 + timevary,
               control=list(trace=1, fnscale=271181, maxit=200))
# Women
women <- sex_data %>% filter(sex==1)
Q_women <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=women)
modelt5_women <- msm(educNUM~year, newID, data=women, qmatrix=Q_women,
                   center=FALSE,
                   covariates=~agecat + racefinal2 + timevary,
                   control=list(trace=1, fnscale=271181, maxit=200))

saveRDS(modelt5_men, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_men_alt_ref_cats.RDS")
saveRDS(modelt5_women, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_women_alt_ref_cats.RDS")

## Run separate models for each RACE group to enable interactions (based on model 5)
race_data <- data
race_data$timevary <- cut(data$year,
                              breaks=c(0,2005,2011,2018, 2021),
                              labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))
# Relevel the 'timevary' variable to set "2012-2018" as the reference category
race_data$timevary <- relevel(race_data$timevary, ref = "2012-2018")

# White
white <- race_data %>% filter(racefinal2=="white")
Q_white <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=white)
modelt5_white <- msm(educNUM~year, newID, data=white, qmatrix=Q_white,
                   center=FALSE,
                   covariates=~agecat + sex + timevary,
                   control=list(trace=1, fnscale=271181, maxit=200))

# Black
black <- race_data %>% filter(racefinal2=="black")
Q_black <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=black)
modelt5_black <- msm(educNUM~year, newID, data=black, qmatrix=Q_black,
                   center=FALSE,
                   covariates=~agecat + sex + timevary,
                   control=list(trace=1, fnscale=271181, maxit=200))

# Hispanic
hispanic <- race_data %>% filter(racefinal2=="hispanic")
Q_hispanic <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=hispanic)
modelt5_hispanic <- msm(educNUM~year, newID, data=hispanic, qmatrix=Q_hispanic,
                     center=FALSE,
                     covariates=~agecat + sex + timevary,
                     control=list(trace=1, fnscale=271181, maxit=200))
# Other
other <- race_data %>% filter(racefinal2=="other")
Q_other <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=other)
modelt5_other <- msm(educNUM~year, newID, data=other, qmatrix=Q_other,
                     center=FALSE,
                     covariates=~agecat + sex + timevary,
                     control=list(trace=1, fnscale=271181, maxit=200))

saveRDS(modelt5_white, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_white_alt_ref_cats.RDS")
saveRDS(modelt5_black, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_black_alt_ref_cats.RDS")
saveRDS(modelt5_hispanic, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_hispanic_alt_ref_cats.RDS")
saveRDS(modelt5_other, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_other_alt_ref_cats.RDS")


# Run separate models for each SEX AND RACE group to enable interactions (based on model 5)

# White men
white_men <- sex_data %>% filter(racefinal2=="white", sex==0)
Q_white_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=white_men)
modelt5_white_men <- msm(educNUM~year, newID, data=white_men, qmatrix=Q_white_men,
                     center=FALSE,
                     covariates=~agecat + timevary,
                     control=list(trace=1, fnscale=271181, maxit=200))

# Black men
black_men <- sex_data %>% filter(racefinal2=="black", sex==0)
Q_black_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=black_men)
modelt5_black_men <- msm(educNUM~year, newID, data=black_men, qmatrix=Q_black_men,
                         center=FALSE,
                         covariates=~agecat + timevary,
                         control=list(trace=1, fnscale=271181, maxit=200))


# Hispanic men
hispanic_men <- sex_data %>% filter(racefinal2=="hispanic", sex==0)
Q_hispanic_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=hispanic_men)
modelt5_hispanic_men <- msm(educNUM~year, newID, data=hispanic_men, qmatrix=Q_hispanic_men,
                         center=FALSE,
                         covariates=~agecat + timevary,
                         control=list(trace=1, fnscale=271181, maxit=200))


# Other race men
other_men <- sex_data %>% filter(racefinal2=="other", sex==0)
Q_other_men <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=other_men)
modelt5_other_men <- msm(educNUM~year, newID, data=other_men, qmatrix=Q_other_men,
                         center=FALSE,
                         covariates=~agecat + timevary,
                         control=list(trace=1, fnscale=271181, maxit=200))



# White women
white_women <- sex_data %>% filter(racefinal2=="white", sex==1)
Q_white_women <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=white_women)
modelt5_white_women <- msm(educNUM~year, newID, data=white_women, qmatrix=Q_white_women,
                         center=FALSE,
                         covariates=~agecat + timevary,
                         control=list(trace=1, fnscale=271181, maxit=200))


# Black women
black_women <- sex_data %>% filter(racefinal2=="black", sex==1)
Q_black_women <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=black_women)
modelt5_black_women <- msm(educNUM~year, newID, data=black_women, qmatrix=Q_black_women,
                         center=FALSE,
                         covariates=~agecat + timevary,
                         control=list(trace=1, fnscale=271181, maxit=200))


# Hispanic women
hispanic_women <- sex_data %>% filter(racefinal2=="hispanic", sex==1)
Q_hispanic_women <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=hispanic_women)
modelt5_hispanic_women <- msm(educNUM~year, newID, data=hispanic_women, qmatrix=Q_hispanic_women,
                            center=FALSE,
                            covariates=~agecat + timevary,
                            control=list(trace=1, fnscale=271181, maxit=200))


# Other race women
other_women <- sex_data %>% filter(racefinal2=="other", sex==1)
Q_other_women <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=other_women)
modelt5_other_women <- msm(educNUM~year, newID, data=other_women, qmatrix=Q_other_women,
                         center=FALSE,
                         covariates=~agecat + timevary,
                         control=list(trace=1, fnscale=271181, maxit=200))

saveRDS(modelt5_white_men, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_white_men_alt_ref_cats.RDS")
saveRDS(modelt5_black_men, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_black_men_alt_ref_cats.RDS")
saveRDS(modelt5_hispanic_men, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_hispanic_men_alt_ref_cats.RDS")
saveRDS(modelt5_other_men, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_other_men_alt_ref_cats.RDS")
saveRDS(modelt5_white_women, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_white_women_alt_ref_cats.RDS")
saveRDS(modelt5_black_women, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_black_women_alt_ref_cats.RDS")
saveRDS(modelt5_hispanic_women, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_hispanic_women_alt_ref_cats.RDS")
saveRDS(modelt5_other_women, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_other_women_alt_ref_cats.RDS")

## Compare model fit across models 
# (unsure if reasonable to compare AICs, given each model is using a different subset of the data)
# Calculate AIC values for each model
AIC_values <- c(AIC(modelt5), AIC(modelt5_men), AIC(modelt5_women), 
                AIC(modelt5_white), AIC(modelt5_black), AIC(modelt5_hispanic),AIC(modelt5_other),
                AIC(modelt5_white_men), AIC(modelt5_black_men), AIC(modelt5_hispanic_men),AIC(modelt5_other_men),
                AIC(modelt5_white_women), AIC(modelt5_black_women), AIC(modelt5_hispanic_women),AIC(modelt5_other_women))

# Create a data frame to store model names and AIC values
model_names <- c("No interactions", "Men", "Women", "White", "Black", "Hispanic", "Other",
                 "White men", "Black men", "Hispanic men", "Other men",
                 "White women", "Black women", "Hispanic women", "Other women")
AIC_comparison_table <- data.frame(Model = model_names, AIC = AIC_values)


#### BASED ON TWO TIME PERIODS ONLY 

# 2007-2018 & 2019-2021
datat5_main <- data %>% filter(year>=2006) %>% ungroup()
datat5_main$timevary <- cut(datat5_main$year,
                            breaks=c(0, 2019, 2021),
                            labels=c("2007-2018", "2019-2021"))
# Relevel the 'timevary' variable to set "2006-2018" as the reference category
datat5_main$timevary <- relevel(datat5_main$timevary, ref = "2007-2018")

# remove anyone with only one year of data- this gives an error in MSM 
datat5_main_observations_over_1 <- datat5_main %>% ungroup() %>% group_by(newID) %>% add_tally(name="totalobservations") %>% 
  filter(totalobservations>1) 

Q5_main <- crudeinits.msm(educNUM~year, newID, qmatrix=Q, data=datat5_main)
modelt5_main <- msm(educNUM~year, newID, data=datat5_main, qmatrix=Q5_main,
                    center=FALSE,
                    covariates=~agecat + sex + racefinal2 + timevary,
                    control=list(trace=1, fnscale=271181, maxit=200))

saveRDS(modelt5_main, "SIMAH_workplace/education_transitions/2021/final_models/modelt5_main.RDS")



