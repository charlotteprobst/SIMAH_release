
# SES x Lifestyle Differential Vulnerability & Exposure Project
# Objective 1: Aalen additive hazard models file


# LOAD DATA AND SET FILE LOCATIONS

# load libraries
library(tidyverse)  # data management
library(broom)
library(skimr)      # descriptive statistics
library(survival)   # surivval analyses
library(survminer)  # surivval analyses
library(timereg)    # additive survival models
library(foreach)    # loops 
memory.limit(size=1e+13)
options(scipen=999)


# Specify the data and output file locations
data_path    <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/Data"
output_tables  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/Restricted NHIS Data/Output/"
output_models  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nhis/Restricted NHIS Data/Output/Models/"

    
# Load data
nhis_all    <- readRDS (file.path(data_path, "nhis.rds"))
nhis_male   <- readRDS (file.path(data_path, "nhis_male.rds"))
nhis_female <- readRDS (file.path(data_path, "nhis_female.rds"))
data_file <- sample_frac(nhis_female, 0.05)


?coxph
?aalen
# OBJECTIVE 1: Joint Effects, Hazard Models - Stratified by Sex

# The effect estimates from the model can be directly interpreted as the number of additional events (deaths) per 1 person year at risk
# Two different versions of the model were ran identify the interaction effect (model with the interaction term) and the joint effect (model with interacting variable)


## Create functions ------------------------------------------------------------------------------------------
# Function to run the Cox and Aalen models
table4to9 <- function(data, deaths_list, SES, lifestyle, table_label){

  foreach (i = deaths_list) %do% {
  
    # 1) Data preparation 
        data <- mutate (data, 
                        cause_of_death = .data[[i]],
                        SES = {{SES}},
                        lifestyle = {{lifestyle}})
        
        # Create an 'interaction' variable, combining the SES and lifestyle
        data$SES_lifestyle <- interaction(data$SES, data$lifestyle)
        
        # Create labels, to be used later
        
        data_name  <- sub(".*_", "", expr(data))
        death_name <- sub("_.*", "", enexpr(i))
        SES_name   <- enexpr(SES)
        lifestyle_name <- enexpr(lifestyle)
        
        cat(death_name, "\n") # progress indicator

    # 2) Run analyses     
        # Cox interaction model
        cox_int <- coxph(Surv(bl_age, end_age, cause_of_death) ~ SES * lifestyle + 
                              married.factor + ethnicity.factor + factor(srvy_yr), data = data)
        cat("    Completed: Cox Interaction model", "\n")  # progress indicator
      
        # Cox joint effect model
        cox_joint <- coxph(Surv(bl_age, end_age, cause_of_death) ~ SES_lifestyle + 
                            married.factor + ethnicity.factor + factor(srvy_yr), data = data)
        cat("    Completed: Cox Joint effects model", "\n")  # progress indicator
      
        # Aalen Interaction model
        aalen_int <- aalen(Surv(bl_age, end_age, cause_of_death) ~ const(SES)*const(lifestyle) +
                            const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = data)
        cat("    Completed: Aalen Interaction model", "\n")  # progress indicator
        
        # Aalen joint effects model
        aalen_joint <- aalen(Surv(bl_age, end_age, cause_of_death) ~ const(SES_lifestyle) +
                              const(married.factor) + ethnicity.factor + const(factor(srvy_yr)),  data = data)
        cat("    Completed: Aalen Joint effects model", "\n")  # progress indicator
        
        # Save model results 
        saveRDS(cox_int,    paste0(output_models, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, "_cox_int.rds"))
        saveRDS(cox_joint,  paste0(output_models, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, "_cox_joint.rds"))
        saveRDS(aalen_int,  paste0(output_models, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, "_aalen_int.rds"))
        saveRDS(aalen_joint,paste0(output_models, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, "_aalen_joint.rds"))

        
  # 3) Format and save results 
  library(broom)
  
   cox_int_results <- cox_int %>% tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
          mutate(variable = term,
            estimate = round(estimate, 2),
            conf.low = round(conf.low, 2),
            conf.high = round(conf.high, 2),
            p.value_HR = round(p.value, 3),
            HR_CI = paste0(estimate, " (",conf.low,", ", conf.high, ")")) %>%
          select(variable, HR_CI, p.value_HR) %>%
          filter(str_detect(variable, "SES|lifestyle")) %>%
          mutate(variable = str_remove(variable, fixed("SES")), 
                 variable = str_remove(variable, fixed("lifestyle"))) %>%
         add_row(variable = "INTERACTION MODELS", .before=1)

   
   cox_joint_results <- cox_joint %>% tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
         mutate(variable = term,
           estimate = round(estimate, 2),
           conf.low = round(conf.low, 2),
           conf.high = round(conf.high, 2),
           p.value_HR = round(p.value, 3),
           HR_CI = paste0(estimate, " (",conf.low,", ", conf.high, ")")) %>%
         select(variable, HR_CI, p.value_HR) %>%
         filter(str_detect(variable, "SES")) %>%
         mutate(variable = str_remove(variable, fixed("SES_lifestyle"))) %>%
         add_row(variable = "JOINT MODELS", .before=1)

   
    aalen_int_results <- as.data.frame(cbind(aalen_int$gamma, diag(aalen_int$robvar.gamma))) %>%
          mutate (variable = rownames(.),
            var = V2,
            p.value_Deaths = round(2*pnorm(-abs(estimate / sqrt(var))),3),
            lower.ci = round((estimate - (1.96 * sqrt(var)))*10000, 1),
            upper.ci = round((estimate + (1.96 * sqrt(var)))*10000, 1),
            estimate_10000py = round(estimate*10000, 1),
            Deaths_CI_10000py = paste0(estimate_10000py, " (",lower.ci,", ", upper.ci, ")")) %>%
          select (variable, Deaths_CI_10000py, p.value_Deaths) %>%
          filter(str_detect(variable, "SES|lifestyle")) %>%
          mutate(variable = str_remove(variable, fixed("const(SES)")),
                variable = str_remove(variable, fixed("const(lifestyle)"))) %>%
          add_row(variable = "INTERACTION MODELS", .before=1)%>%
          remove_rownames()
    
    
    aalen_joint_results <- as.data.frame(cbind(aalen_joint$gamma, diag(aalen_joint$robvar.gamma))) %>%
          mutate (variable = rownames(.),
            var = V2,
            p.value_Deaths = round(2*pnorm(-abs(estimate / sqrt(var))),3),
            lower.ci = round((estimate - (1.96 * sqrt(var)))*10000, 1), 
            upper.ci = round((estimate + (1.96 * sqrt(var)))*10000, 1),
            estimate_10000 = round(estimate*10000, 1),
            Deaths_CI_10000py = paste0(estimate_10000, " (",lower.ci,", ", upper.ci, ")")) %>%
          select (variable, Deaths_CI_10000py, p.value_Deaths) %>%
          filter(str_detect(variable, "SES")) %>% 
          mutate(variable = str_remove(variable, fixed("const(SES_lifestyle)"))) %>%
          add_row(variable = "JOINT MODELS", .before=1) %>%
          remove_rownames() 

    
    cox_results   <- rbind(cox_int_results, cox_joint_results)
    aalen_results <- rbind(aalen_int_results, aalen_joint_results)
    
    results <- full_join(cox_results, aalen_results, by="variable") %>%
      add_row(variable = death_name, .before=1) 
  
    write_csv(results, paste0(output_tables, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, ".csv"), na="")

    cat("    Completed: Results were exported", "\n")  # progress indicator
  }   
}
  

# Causes of death ----------------------------------------------------------------------------------------------
# Specify the causes of death (to be used below)

death_list <- c("allcause_deaths", "alc_deaths", "despair_deaths", "vehicle_deaths", "accident_deaths", 
                "AUD_deaths", "self_harm_deaths", "liver_deaths", "diabetes_deaths", "IHD_deaths", 
                "stroke_deaths", "hyperten_deaths", "poisoning_deaths", "other_deaths")


# Table 4: Alcohol ----------------------------------------------------------------------------------------
## Edu x Alcohol
table4to9(nhis_all,    death_list, edu3, alc5, "table4a") # All participants
table4to9(nhis_female, death_list, edu3, alc5, "table4a") # Females
table4to9(nhis_male,   death_list, edu3, alc5, "table4a") # Males


# **NOTE**: Need to change to code for this so that the start time and end time is specified
table4to9(nhis_fem.age.gp1,  death_list, edu3, alc5, "table4a") # Females, age group 1 # NOTE: Need to change to code for this so that the start time and end time is specified
table4to9(nhis_fem.age.gp2,  death_list, edu3, alc5, "table4a") # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, edu3, alc5, "table4a") # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, edu3, alc5, "table4a") # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, edu3, alc5, "table4a") # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, edu3, alc5, "table4a") # Males, age group 3

table4to9(nhis_fem.white, death_list, edu3, alc5, "table4a") # Females, white
table4to9(nhis_fem.black, death_list, edu3, alc5, "table4a") # Females, black
table4to9(nhis_fem.hisp,  death_list, edu3, alc5, "table4a") # Females, Hispanic
table4to9(nhis_fem.other, death_list, edu3, alc5, "table4a") # Females, Other (non-Hispanic)

table4to9(nhis_male.white, death_list, edu3, alc5, "table4a") # Males, white
table4to9(nhis_male.black, death_list, edu3, alc5, "table4a") # Males, black
table4to9(nhis_male.hisp,  death_list, edu3, alc5, "table4a") # Males, Hispanic
table4to9(nhis_male.other, death_list, edu3, alc5, "table4a") # Males, Other (non-Hispanic)


## Income x Alcohol
table4to9(nhis_all,    death_list, income5, alc5, "table4b") # All participants
table4to9(nhis_female, death_list, income5, alc5, "table4b") # Females
table4to9(nhis_male,   death_list, income5, alc5, "table4b") # Males

table4to9(nhis_fem.age.gp1,  death_list, income5, alc5, "table4b")  # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, income5, alc5, "table4b")  # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, income5, alc5, "table4b")  # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, income5, alc5, "table4b")  # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, income5, alc5, "table4b")  # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, income5, alc5, "table4b")  # Males, age group 3

table4to9(nhis_fem.white, death_list, income5, alc5, "table4b") # Females, white
table4to9(nhis_fem.black, death_list, income5, alc5, "table4b") # Females, black
table4to9(nhis_fem.hisp,  death_list, income5, alc5, "table4b") # Females, Hispanic
table4to9(nhis_fem.other, death_list, income5, alc5, "table4b") # Females, Other (non-Hispanic)

table4to9(nhis_male.white, death_list, income5, alc5, "table4b") # Males, white
table4to9(nhis_male.black, death_list, income5, alc5, "table4b") # Males, black
table4to9(nhis_male.hisp,  death_list, income5, alc5, "table4b") # Males, Hispanic
table4to9(nhis_male.other, death_list, income5, alc5, "table4b") # Males, Other (non-Hispanic)


## Race x Alcohol
table4to9(nhis_all,    death_list, race4, alc5, "table4c") # All participants
table4to9(nhis_female, death_list, race4, alc5, "table4c") # Females
table4to9(nhis_male,   death_list, race4, alc5, "table4c") # Males

table4to9(nhis_fem.age.gp1,  death_list, race4, alc5, "table4c")  # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, race4, alc5, "table4c")  # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, race4, alc5, "table4c")  # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, race4, alc5, "table4c")  # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, race4, alc5, "table4c")  # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, race4, alc5, "table4c")  # Males, age group 3

table4to9(nhis_fem.edu1, death_list, race4, alc5, "table4c") # Female, low edu
table4to9(nhis_fem.edu2, death_list, race4, alc5, "table4c") # Female, medium edu
table4to9(nhis_fen.edu3, death_list, race4, alc5, "table4c") # Female, high edu

table4to9(nhis_male.edu1, death_list, race4, alc5, "table4c") # Males, low edu
table4to9(nhis_male.edu2, death_list, race4, alc5, "table4c") # Males, medium edu
table4to9(nhis_male.edu3, death_list, race4, alc5, "table4c") # Males, high edu






# Table 5: Smoking ----------------------------------------------------------------------------------------
## Edu x Smoking
table4to9(nhis_all,    death_list, edu3, smk4, "table5a") # All participants
table4to9(nhis_female, death_list, edu3, smk4, "table5a") # Females
table4to9(nhis_male,   death_list, edu3, smk4, "table5a") # Males

table4to9(nhis_fem.age.gp1,  death_list, edu3, smk4, "table5a")  # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, edu3, smk4, "table5a")  # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, edu3, smk4, "table5a")  # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, edu3, smk4, "table5a")  # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, edu3, smk4, "table5a")  # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, edu3, smk4, "table5a")  # Males, age group 3

table4to9(nhis_fem.white, death_list, edu3, smk4, "table5a") # Females, white
table4to9(nhis_fem.black, death_list, edu3, smk4, "table5a") # Females, black
table4to9(nhis_fem.hisp,  death_list, edu3, smk4, "table5a") # Females, Hispanic
table4to9(nhis_fem.other, death_list, edu3, smk4, "table5a") # Females, Other (non-Hispanic)

table4to9(nhis_male.white, death_list, edu3, smk4,"table5a")  # Males, white
table4to9(nhis_male.black, death_list, edu3, smk4, "table5a") # Males, black
table4to9(nhis_male.hisp,  death_list, edu3, smk4, "table5a") # Males, Hispanic
table4to9(nhis_male.other, death_list, edu3, smk4, "table5a") # Males, Other (non-Hispanic)


## Income x Smoking
table4to9(nhis_all,    death_list, income5, smk4, "table5b") # All participants
table4to9(nhis_female, death_list, income5, smk4, "table5b") # Females
table4to9(nhis_male,   death_list, income5, smk4, "table5b") # Males

table4to9(nhis_fem.age.gp1,  death_list, income5, smk4, "table5b")  # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, income5, smk4, "table5b")  # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, income5, smk4, "table5b")  # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, income5, smk4, "table5b")  # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, income5, smk4, "table5b")  # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, income5, smk4, "table5b")  # Males, age group 3

table4to9(nhis_fem.white, death_list, income5, smk4, "table5b") # Females, white
table4to9(nhis_fem.black, death_list, income5, smk4, "table5b") # Females, black
table4to9(nhis_fem.hisp,  death_list, income5, smk4, "table5b") # Females, Hispanic
table4to9(nhis_fem.other, death_list, income5, smk4, "table5b") # Females, Other (non-Hispanic)

table4to9(nhis_male.white, death_list, income5, smk4,"table5b")  # Males, white
table4to9(nhis_male.black, death_list, income5, smk4, "table5b") # Males, black
table4to9(nhis_male.hisp,  death_list, income5, smk4, "table5b") # Males, Hispanic
table4to9(nhis_male.other, death_list, income5, smk4, "table5b") # Males, Other (non-Hispanic)


## Race x Smoking
table4to9(nhis_all,    death_list, race4, smk4, "table5c") # All participants
table4to9(nhis_female, death_list, race4, smk4, "table5c") # Females
table4to9(nhis_male,   death_list, race4, smk4, "table5c") # Males

table4to9(nhis_fem.age.gp1,  death_list, race4, smk4, "table5c") # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, race4, smk4, "table5c") # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, race4, smk4, "table5c") # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, race4, smk4, "table5c") # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, race4, smk4, "table5c") # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, race4, smk4, "table5c") # Males, age group 3

table4to9(nhis_fem.edu1, death_list, race4, smk4, "table5c") # Female, low edu
table4to9(nhis_fem.edu2, death_list, race4, smk4, "table5c") # Female, medium edu
table4to9(nhis_fen.edu3, death_list, race4, smk4, "table5c") # Female, high edu

table4to9(nhis_male.edu1, death_list, race4, smk4, "table5c") # Males, low edu
table4to9(nhis_male.edu2, death_list, race4, smk4, "table5c") # Males, medium edu
table4to9(nhis_male.edu3, death_list, race4, smk4, "table5c") # Males, high edu




# Table 6: BMI --------------------------------------------------------------------------------------------
## Edu x BMI
table4to9(nhis_all,    death_list, edu3, bmi4, "table6a") # All participants
table4to9(nhis_female, death_list, edu3, bmi4, "table6a") # Females
table4to9(nhis_male,   death_list, edu3, bmi4, "table6a") # Males

table4to9(nhis_fem.age.gp1,  death_list, edu3, bmi4, "table6a")  # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, edu3, bmi4, "table6a")  # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, edu3, bmi4, "table6a")  # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, edu3, bmi4, "table6a")  # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, edu3, bmi4, "table6a")  # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, edu3, bmi4, "table6a")  # Males, age group 3

table4to9(nhis_fem.white, death_list, edu3, bmi4, "table6a") # Females, white
table4to9(nhis_fem.black, death_list, edu3, bmi4, "table6a") # Females, black
table4to9(nhis_fem.hisp,  death_list, edu3, bmi4, "table6a") # Females, Hispanic
table4to9(nhis_fem.other, death_list, edu3, bmi4, "table6a") # Females, Other (non-Hispanic)

table4to9(nhis_male.white, death_list, edu3, bmi4, "table6a") # Males, white
table4to9(nhis_male.black, death_list, edu3, bmi4, "table6a") # Males, black
table4to9(nhis_male.hisp,  death_list, edu3, bmi4, "table6a") # Males, Hispanic
table4to9(nhis_male.other, death_list, edu3, bmi4, "table6a") # Males, Other (non-Hispanic)


## Income x BMI
table4to9(nhis_all,    death_list, income5, bmi4, "table6b") # All participants
table4to9(nhis_female, death_list, income5, bmi4, "table6b") # Females
table4to9(nhis_male,   death_list, income5, bmi4, "table6b") # Males

table4to9(nhis_fem.age.gp1,  death_list, income5, bmi4, "table6b") # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, income5, bmi4, "table6b") # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, income5, bmi4, "table6b") # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, income5, bmi4, "table6b") # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, income5, bmi4, "table6b") # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, income5, bmi4, "table6b") # Males, age group 3

table4to9(nhis_fem.white, death_list, income5, bmi4, "table6b") # Females, white
table4to9(nhis_fem.black, death_list, income5, bmi4, "table6b") # Females, black
table4to9(nhis_fem.hisp,  death_list, income5, bmi4, "table6b") # Females, Hispanic
table4to9(nhis_fem.other, death_list, income5, bmi4, "table6b") # Females, Other (non-Hispanic)

table4to9(nhis_male.white, death_list, income5, bmi4, "table6b") # Males, white
table4to9(nhis_male.black, death_list, income5, bmi4, "table6b") # Males, black
table4to9(nhis_male.hisp,  death_list, income5, bmi4, "table6b") # Males, Hispanic
table4to9(nhis_male.other, death_list, income5, bmi4, "table6b") # Males, Other (non-Hispanic)


## Race x BMI
table4to9(nhis_all,    death_list, race4, bmi4, "table6c") # All participants
table4to9(nhis_female, death_list, race4, bmi4, "table6c") # Females
table4to9(nhis_male,   death_list, race4, bmi4, "table6c") # Males

table4to9(nhis_fem.age.gp1,  death_list, race4, bmi4, "table6c") # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, race4, bmi4, "table6c") # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, race4, bmi4, "table6c") # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, race4, bmi4, "table6c") # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, race4, bmi4, "table6c") # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, race4, bmi4, "table6c") # Males, age group 3

table4to9(nhis_fem.edu1, death_list, race4, bmi4, "table6c") # Female, low edu
table4to9(nhis_fem.edu2, death_list, race4, bmi4, "table6c") # Female, medium edu
table4to9(nhis_fen.edu3, death_list, race4, bmi4, "table6c") # Female, high edu

table4to9(nhis_male.edu1, death_list, race4, bmi4, "table6c") # Males, low edu
table4to9(nhis_male.edu2, death_list, race4, bmi4, "table6c") # Males, medium edu
table4to9(nhis_male.edu3, death_list, race4, bmi4, "table6c") # Males, high edu





# Table 7: Physical Activity -------------------------------------------------------------------------------
## Edu x Physical Activity
table4to9(nhis_all,    death_list, edu3, phy3, "table7a") # All participants
table4to9(nhis_female, death_list, edu3, phy3, "table7a") # Females
table4to9(nhis_male,   death_list, edu3, phy3, "table7a") # Males

table4to9(nhis_fem.age.gp1,  death_list, edu3, phy3, "table7a") # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, edu3, phy3, "table7a") # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, edu3, phy3, "table7a") # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, edu3, phy3, "table7a") # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, edu3, phy3, "table7a") # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, edu3, phy3, "table7a") # Males, age group 3

table4to9(nhis_fem.white, death_list, edu3, phy3, "table7a") # Females, white
table4to9(nhis_fem.black, death_list, edu3, phy3, "table7a") # Females, black
table4to9(nhis_fem.hisp,  death_list, edu3, phy3, "table7a") # Females, Hispanic
table4to9(nhis_fem.other, death_list, edu3, phy3, "table7a") # Females, Other (non-Hispanic)

table4to9(nhis_male.white, death_list, edu3, phy3, "table7a") # Males, white
table4to9(nhis_male.black, death_list, edu3, phy3, "table7a") # Males, black
table4to9(nhis_male.hisp,  death_list, edu3, phy3, "table7a") # Males, Hispanic
table4to9(nhis_male.other, death_list, edu3, phy3, "table7a") # Males, Other (non-Hispanic)


## Income x Physical Activity
table4to9(nhis_all,    death_list, income5, phy3, "table7b") # All participants
table4to9(nhis_female, death_list, income5, phy3, "table7b") # Females
table4to9(nhis_male,   death_list, income5, phy3, "table7b") # Males

table4to9(nhis_fem.age.gp1,  death_list, income5, phy3, "table7b") # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, income5, phy3, "table7b") # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, income5, phy3, "table7b") # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, income5, phy3, "table7b") # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, income5, phy3, "table7b") # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, income5, phy3, "table7b") # Males, age group 3

table4to9(nhis_fem.white, death_list, income5, phy3, "table7b") # Females, white
table4to9(nhis_fem.black, death_list, income5, phy3, "table7b") # Females, black
table4to9(nhis_fem.hisp,  death_list, income5, phy3, "table7b") # Females, Hispanic
table4to9(nhis_fem.other, death_list, income5, phy3, "table7b") # Females, Other (non-Hispanic)

table4to9(nhis_male.white, death_list, income5, phy3, "table7b") # Males, white
table4to9(nhis_male.black, death_list, income5, phy3, "table7b") # Males, black
table4to9(nhis_male.hisp,  death_list, income5, phy3, "table7b") # Males, Hispanic
table4to9(nhis_male.other, death_list, income5, phy3, "table7b") # Males, Other (non-Hispanic)


## Race x Physical Activity
table4to9(nhis_all,    death_list, race4, phy3, "table7c") # All participants
table4to9(nhis_female, death_list, race4, phy3, "table7c") # Females
table4to9(nhis_male,   death_list, race4, phy3, "table7c") # Males

table4to9(nhis_fem.age.gp1,  death_list, race4, phy3, "table7c") # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, race4, phy3, "table7c") # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, race4, phy3, "table7c") # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, race4, phy3, "table7c") # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, race4, phy3, "table7c") # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, race4, phy3, "table7c") # Males, age group 3

table4to9(nhis_fem.edu1, death_list, race4, phy3, "table7c") # Female, low edu
table4to9(nhis_fem.edu2, death_list, race4, phy3, "table7c") # Female, medium edu
table4to9(nhis_fen.edu3, death_list, race4, phy3, "table7c") # Female, high edu

table4to9(nhis_male.edu1, death_list, race4, phy3, "table7c") # Males, low edu
table4to9(nhis_male.edu2, death_list, race4, phy3, "table7c") # Males, medium edu
table4to9(nhis_male.edu3, death_list, race4, phy3, "table7c") # Males, high edu



# Table 8: PsychDistress -----------------------------------------------------------------------------------
## Edu x PsychDistress
table4to9(nhis_all,    death_list, edu3, PsyDistr3, "table8a") # All participants
table4to9(nhis_female, death_list, edu3, PsyDistr3, "table8a") # Females
table4to9(nhis_male,   death_list, edu3, PsyDistr3, "table8a") # Males

table4to9(nhis_fem.age.gp1,  death_list, edu3, PsyDistr3, "table8a") # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, edu3, PsyDistr3, "table8a") # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, edu3, PsyDistr3, "table8a") # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, edu3, PsyDistr3, "table8a") # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, edu3, PsyDistr3, "table8a") # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, edu3, PsyDistr3, "table8a") # Males, age group 3

table4to9(nhis_fem.white, death_list, edu3, PsyDistr3, "table8a") # Females, white
table4to9(nhis_fem.black, death_list, edu3, PsyDistr3, "table8a") # Females, black
table4to9(nhis_fem.hisp,  death_list, edu3, PsyDistr3, "table8a") # Females, Hispanic
table4to9(nhis_fem.other, death_list, edu3, PsyDistr3, "table8a") # Females, Other (non-Hispanic)

table4to9(nhis_male.white, death_list, edu3, PsyDistr3, "table8a") # Males, white
table4to9(nhis_male.black, death_list, edu3, PsyDistr3, "table8a") # Males, black
table4to9(nhis_male.hisp,  death_list, edu3, PsyDistr3, "table8a") # Males, Hispanic
table4to9(nhis_male.other, death_list, edu3, PsyDistr3, "table8a") # Males, Other (non-Hispanic)


## Income x PsychDistress
table4to9(nhis_all,    death_list, income5, PsyDistr3, "table8b") # All participants
table4to9(nhis_female, death_list, income5, PsyDistr3, "table8b") # Females
table4to9(nhis_male,   death_list, income5, PsyDistr3, "table8b") # Males

table4to9(nhis_fem.age.gp1,  death_list, income5, PsyDistr3, "table8b") # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, income5, PsyDistr3, "table8b") # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, income5, PsyDistr3, "table8b") # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, income5, PsyDistr3, "table8b") # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, income5, PsyDistr3, "table8b") # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, income5, PsyDistr3, "table8b") # Males, age group 3

table4to9(nhis_fem.white, death_list, income5, PsyDistr3, "table8b") # Females, white
table4to9(nhis_fem.black, death_list, income5, PsyDistr3, "table8b") # Females, black
table4to9(nhis_fem.hisp,  death_list, income5, PsyDistr3, "table8b") # Females, Hispanic
table4to9(nhis_fem.other, death_list, income5, PsyDistr3, "table8b") # Females, Other (non-Hispanic)

table4to9(nhis_male.white, death_list, income5, PsyDistr3, "table8b") # Males, white
table4to9(nhis_male.black, death_list, income5, PsyDistr3, "table8b") # Males, black
table4to9(nhis_male.hisp,  death_list, income5, PsyDistr3, "table8b") # Males, Hispanic
table4to9(nhis_male.other, death_list, income5, PsyDistr3, "table8b") # Males, Other (non-Hispanic)


## Race x PsychDistress
table4to9(nhis_all,    death_list, race4, PsyDistr3, "table8c") # All participants
table4to9(nhis_female, death_list, race4, PsyDistr3, "table8c") # Females
table4to9(nhis_male,   death_list, race4, PsyDistr3, "table8c") # Males

table4to9(nhis_fem.age.gp1,  death_list, race4, PsyDistr3, "table8c") # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, race4, PsyDistr3, "table8c") # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, race4, PsyDistr3, "table8c") # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, race4, PsyDistr3, "table8c") # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, race4, PsyDistr3, "table8c") # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, race4, PsyDistr3, "table8c") # Males, age group 3

table4to9(nhis_fem.edu1, death_list, race4, PsyDistr3, "table8c") # Female, low edu
table4to9(nhis_fem.edu2, death_list, race4, PsyDistr3, "table8c") # Female, medium edu
table4to9(nhis_fen.edu3, death_list, race4, PsyDistr3, "table8c") # Female, high edu

table4to9(nhis_male.edu1, death_list, race4, PsyDistr3, "table8c") # Males, low edu
table4to9(nhis_male.edu2, death_list, race4, PsyDistr3, "table8c") # Males, medium edu
table4to9(nhis_male.edu3, death_list, race4, PsyDistr3, "table8c") # Males, high edu



# Table 9: Alcohol x PsychDistress -----------------------------------------------------------------------------------
## Alcohol x PsychDistress
table4to9(nhis_all,    death_list, alc5, PsyDistr3, "table9a") # All participants
table4to9(nhis_female, death_list, alc5, PsyDistr3, "table9a") # Females
table4to9(nhis_male,   death_list, alc5, PsyDistr3, "table9a") # Males

table4to9(nhis_fem.age.gp1,  death_list, alc5, PsyDistr3, "table9a") # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, alc5, PsyDistr3, "table9a") # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, alc5, PsyDistr3, "table9a") # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, alc5, PsyDistr3, "table9a") # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, alc5, PsyDistr3, "table9a") # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, alc5, PsyDistr3, "table9a") # Males, age group 3

table4to9(nhis_fem.white, death_list, alc5, PsyDistr3, "table9a") # Females, white
table4to9(nhis_fem.black, death_list, alc5, PsyDistr3, "table9a") # Females, black
table4to9(nhis_fem.hisp,  death_list, alc5, PsyDistr3, "table9a") # Females, Hispanic
table4to9(nhis_fem.other, death_list, alc5, PsyDistr3, "table9a") # Females, Other (non-Hispanic)

table4to9(nhis_male.white, death_list, alc5, PsyDistr3, "table9a") # Males, white
table4to9(nhis_male.black, death_list, alc5, PsyDistr3, "table9a") # Males, black
table4to9(nhis_male.hisp,  death_list, alc5, PsyDistr3, "table9a") # Males, Hispanic
table4to9(nhis_male.other, death_list, alc5, PsyDistr3, "table9a") # Males, Other (non-Hispanic)


## HED x PsychDistress
table4to9(nhis_all,    death_list, hed4, PsyDistr3, "table9b") # All participants
table4to9(nhis_female, death_list, hed4, PsyDistr3, "table9b") # Females
table4to9(nhis_male,   death_list, hed4, PsyDistr3, "table9b") # Males

table4to9(nhis_fem.age.gp1,  death_list, hed4, PsyDistr3, "table9b") # Females, age group 1
table4to9(nhis_fem.age.gp2,  death_list, hed4, PsyDistr3, "table9b") # Females, age group 2
table4to9(nhis_fem.age.gp3,  death_list, hed4, PsyDistr3, "table9b") # Females, age group 3
table4to9(nhis_male.age.gp1, death_list, hed4, PsyDistr3, "table9b") # Males, age group 1
table4to9(nhis_male.age.gp2, death_list, hed4, PsyDistr3, "table9b") # Males, age group 2
table4to9(nhis_male.age.gp3, death_list, hed4, PsyDistr3, "table9b") # Males, age group 3

table4to9(nhis_fem.white, death_list, hed4, PsyDistr3, "table9b") # Females, white
table4to9(nhis_fem.black, death_list, hed4, PsyDistr3, "table9b") # Females, black
table4to9(nhis_fem.hisp,  death_list, hed4, PsyDistr3, "table9b") # Females, Hispanic
table4to9(nhis_fem.other, death_list, hed4, PsyDistr3, "table9b") # Females, Other (non-Hispanic)

table4to9(nhis_male.white, death_list, hed4, PsyDistr3, "table9b") # Males, white
table4to9(nhis_male.black, death_list, hed4, PsyDistr3, "table9b") # Males, black
table4to9(nhis_male.hisp,  death_list, hed4, PsyDistr3, "table9b") # Males, Hispanic
table4to9(nhis_male.other, death_list, hed4, PsyDistr3, "table9b") # Males, Other (non-Hispanic)



