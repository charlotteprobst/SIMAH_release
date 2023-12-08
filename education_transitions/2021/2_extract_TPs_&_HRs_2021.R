## Education transitions analysis to 2021

# Setup
setwd("C:/Users/cmp21seb/Documents/SIMAH/")
library(tidyverse)  # data management
library(skimr)      # descriptive statistics
library(janitor)    # data management
library(msm)        # model transition probabilities
library(tableone)   # create descriptives table
library(knitr)      # create descriptives table
options(scipen = 999)
# 
# # Load model data
data <- readRDS("SIMAH_workplace/education_transitions/2021/data_to_model/prepped_data_for_markov_2021.rds")

# Load MSM Models 
modelt1 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/formodel_modelt1_newn.RDS")
modelt2 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/formodel_modelt2_newn.RDS")
modelt3 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/formodel_modelt3_newn.RDS")
modelt4 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/formodel_modelt4_newn.RDS")
modelt5 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/formodel_modelt5_newn.RDS")
modelt6 <- readRDS("SIMAH_workplace/education_transitions/2021/final_models/formodel_modelt6_newn.RDS")

# Extract TPs for moving between states, overall

predicted_TP <- function(model, year) {

  table <- data.frame(print(pmatrix.msm(model, t=year, ci="norm")))
  rownames(table) <- c("LEHS", "1 year college", "2 years college", "3 years college", "college degree +")
  colnames(table) <- c("LEHS", "1 year college", "2 years college", "3 years college", "college degree +")
  table <- table %>%
    mutate(From = row.names(.)) %>%
    pivot_longer(cols = -From, names_to = "To") %>%
    separate(value, into=c("Estimate","Lower","Upper", NA), sep="\\(|\\,|\\)", convert=TRUE) %>%  # separated based on "(" "," and ")"  convert=TRUE names variables numeric
    mutate(
      newLower = round(Lower, digits=3),
      newUpper = round(Upper, digits=3),
      Estimate = round(Estimate, digits=3),
      EstimateCI = paste0(Estimate, " (", newLower, ", ", newUpper, ")")) %>%
    select (From, To, EstimateCI) %>%
    pivot_wider(names_from = "To", values_from = "EstimateCI")

   return(table)
}

# 1999-2005
modelt1_TPs <- predicted_TP(modelt1, 1)
# 2006-2011
modelt2_TPs <- predicted_TP(modelt2, 1)
# 2012-2018
modelt3_TPs <- predicted_TP(modelt3, 1)
# 2019-2021
modelt4_TPs <- predicted_TP(modelt4, 1)
# all years (covariate for all time periods)
modelt5_TPs <- predicted_TP(modelt5, 1)
# all years (covariate of 2012-2018 vs 2019-201)
modelt6_TPs <- predicted_TP(modelt6, 1)

# Save results
# write_csv(modelt1_TPs, "SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_1999_2005.csv") 
# write_csv(modelt2_TPs, "SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_2006_2011.csv") 
# write_csv(modelt3_TPs, "SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_2012_2018.csv") 
# write_csv(modelt4_TPs, "SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_2019_2021.csv") 
# write_csv(modelt5_TPs, "SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_all_years_a.csv") 
# write_csv(modelt6_TPs, "SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_all_years_b.csv") 

# OR read in results
modelt1_TPs <- read.csv("SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_1999_2005.csv") 
modelt2_TPs <- read.csv("SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_2006_2011.csv") 
modelt3_TPs <- read.csv("SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_2012_2018.csv") 
modelt4_TPs <- read.csv("SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_2019_2021.csv") 
modelt5_TPs <- read.csv("SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_all_years_a.csv") 
modelt6_TPs <- read.csv("SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_all_years_b.csv") 

##################################################################

# Extract TPs for specific groups

extractTP <- function(model,combo){
  probs <- list()
  for(i in 1:nrow(combo)){
  agecat <- combo$agecat[i]
  sex <- combo$sex[i]
  racefinal2 <- combo$racefinal2[i]
  probs[[paste(i)]] <- pmatrix.msm(modelt1, covariates=list(agecat,sex,racefinal2))
    probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
    probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
    probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.5,
                                                            names_to="StateTo", values_to="prob") %>% 
      mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                                 endsWith(StateTo,"2") ~ "State 2",
                                 endsWith(StateTo,"3") ~ "State 3",
                                 endsWith(StateTo,"4") ~ "State 4",
                                 endsWith(StateTo,"5") ~ "State 5")) %>% 
      mutate(age=agecat,
             sex=sex,
             race=racefinal2)
      }
  probs <- do.call(rbind,probs) 
return(probs)
}

combo <- expand.grid(agecat = unique(data$agecat), sex = unique(data$sex), racefinal2=unique(data$racefinal2))

modelt1_TPs_detail <- extractTP(modelt1, combo)
modelt2_TPs_detail <- extractTP(modelt2, combo)
modelt3_TPs_detail <- extractTP(modelt3, combo)
modelt4_TPs_detail <- extractTP(modelt4, combo)

##### include covariates for time period (for models 5 and 6)

data$time_1 <- cut(data$year,
                       breaks=c(0,2005,2011,2018, 2021),
                       labels=c("1999-2005","2006-2011","2012-2018", "2019-2021"))
combo_2 <- expand.grid(timevary = unique(data$time_1), agecat = unique(data$agecat), sex = unique(data$sex), racefinal2=unique(data$racefinal2))

data$time_2 <- cut(data$year,
                       breaks=c(0,2018, 2021),
                       labels=c("2012-2018", "2019-2021"))
combo_3 <- expand.grid(timevary = unique(data$time_2), agecat = unique(data$agecat), sex = unique(data$sex), racefinal2=unique(data$racefinal2))

extractTP_incl_time <- function(model,combo){
  probs <- list()
  for(i in 1:nrow(combo)){
    timevary <- combo$timevary[i]
    agecat <- combo$agecat[i]
    sex <- combo$sex[i]
    racefinal2 <- combo$racefinal2[i]
    probs[[paste(i)]] <- pmatrix.msm(model, covariates=list(timevary,agecat,sex,racefinal2))
    probs[[paste(i)]] <- data.frame(unclass(probs[[paste(i)]]))
    probs[[paste(i)]]$StateFrom <- row.names(probs[[paste(i)]])
    probs[[paste(i)]] <- probs[[paste(i)]] %>% pivot_longer(cols=State.1:State.5,
                                                            names_to="StateTo", values_to="prob") %>%
      mutate(StateTo = case_when(endsWith(StateTo,"1") ~ "State 1",
                                 endsWith(StateTo,"2") ~ "State 2",
                                 endsWith(StateTo,"3") ~ "State 3",
                                 endsWith(StateTo,"4") ~ "State 4",
                                 endsWith(StateTo,"5") ~ "State 5")) %>%
      mutate(time_period=timevary,
             age=agecat,
             sex=sex,
             race=racefinal2)
  }
  probs <- do.call(rbind,probs)
  return(probs)
}

modelt5_TPs_detail <- extractTP_incl_time(modelt5, combo_2)
modelt6_TPs_detail <- extractTP_incl_time(modelt6, combo_3)


# Save results
write_csv(modelt1_TPs_detail, "SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_1999_2005_detail.csv")
write_csv(modelt2_TPs_detail, "SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_2006_2011_detail.csv")
write_csv(modelt3_TPs_detail, "SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_2012_2018_detail.csv")
write_csv(modelt4_TPs_detail, "SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_2019_2021_detail.csv")
write_csv(modelt5_TPs_detail, "SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_all_years_a_detail.csv")
write_csv(modelt6_TPs_detail, "SIMAH_workplace/education_transitions/2021_results/annual_education_TPs_all_years_b_detail.csv")


########################################
# Hazard ratios

predicted_HR <- function(model) {
  
  table <- data.frame(hazard.msm(model))%>% round(2) 
  # Update column names
  colnames(table) <- gsub(x=colnames(table), "agecat", "age",)
  colnames(table) <- gsub(x=colnames(table), "racefinal2", "",)    
  table <- table %>% mutate(transition = row.names(.)) %>%
    pivot_longer(cols=-transition) %>%
    extract(name, into=c("Variable","Type"), regex="(.*)\\.(.*)") %>%   # Separate the string (name) into the variable and type of estimate (HR, Upper, Lower), separate at last occuring period .
    pivot_wider(names_from="Type", values_from = "value") %>%
    mutate(EstimateCI = paste0(HR, " (", L, ", ", U, ")")) %>%
    select(transition, Variable, EstimateCI) %>%
    pivot_wider(names_from = "transition", values_from = EstimateCI)
  colnames(table) <- gsub(x=colnames(table), "State 1", "LEHS",)
  colnames(table) <- gsub(x=colnames(table), "State 2", "1 year",)
  colnames(table) <- gsub(x=colnames(table), "State 3", "2 years",)
  colnames(table) <- gsub(x=colnames(table), "State 4", "3 years",)
  colnames(table) <- gsub(x=colnames(table), "State 5", "college +",)
  
  return(table)
}

modelt1_HRs <- predicted_HR(modelt1)
modelt2_HRs <- predicted_HR(modelt2)
modelt3_HRs <- predicted_HR(modelt3)
modelt4_HRs <- predicted_HR(modelt4)
modelt5_HRs <- predicted_HR(modelt5)
modelt6_HRs <- predicted_HR(modelt6)

# Adjust the CIs to reflect the true (rather than replicated) population size
modelt1_HRs_adjusted <- adjust_CIs_2001(modelt1, "1999-2005", data)
modelt2_HRs_adjusted <- adjust_CIs_2001(modelt1, "2006-2011", data)
modelt3_HRs_adjusted <- adjust_CIs_2001(modelt1, "2012-2018", data)
modelt4_HRs_adjusted <- adjust_CIs_2001(modelt1, "2019-2021", data)
modelt5_HRs_adjusted <- adjust_CIs_2001(modelt5, "1999-2009", data)
modelt6_HRs_adjusted <- adjust_CIs_2001(modelt6, "1999-2009", data)

# write_csv(modelt1_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_1999_2005.csv") 
# write_csv(modelt2_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_2006_2011.csv") 
# write_csv(modelt3_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_2012_2018.csv") 
# write_csv(modelt4_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_2019_2021.csv") 
# write_csv(modelt5_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_all_years_a.csv")
# write_csv(modelt6_HRs_adjusted, "SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_all_years_b.csv")

# OR read in adjusted HR tables
modelt1_HRs_adjusted <- read.csv("SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_1999_2005.csv") 
modelt2_HRs_adjusted <- read.csv("SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_2006_2011.csv") 
modelt3_HRs_adjusted <- read.csv("SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_2012_2018.csv") 
modelt4_HRs_adjusted <- read.csv("SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_2019_2021.csv") 
modelt5_HRs_adjusted <- read.csv("SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_all_years_a.csv") 
modelt6_HRs_adjusted <- read.csv("SIMAH_workplace/education_transitions/2021/annual_education_adjustedHRs_all_years_b.csv") 


