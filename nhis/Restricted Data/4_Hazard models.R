# # SIMAH Restricted-access Data
# Cox and Aalen Hazard models 

# LOAD DATA AND SET FILE LOCATIONS 

# load libraries
library(tidyverse)  # data management
library(broom)      # To format results
library(skimr)      # descriptive statistics
library(survival)   # survival analyses
library(survminer)  # survival analyses
library(timereg)    # additive survival models
library(survey)     # Survey adjusted results
library(srvyr)
library(foreach)    # loops 
library(tidycmprsk)

memory.limit(size=1e+13)
options(scipen=999)


# Yachen
data_path    <- "C:/Users/yzhu/Desktop/SIMAH project/SIMAH/SIMAH_workplace/nhis/Restricted access data/Data/"
output_tables <- "C:/Users/yzhu/Desktop/SIMAH project/SIMAH/SIMAH_workplace/nhis/Restricted access data/Output/Hazard Models//"
output_models <- "C:/Users/yzhu/Desktop/SIMAH project/SIMAH/SIMAH_workplace/nhis/Restricted access data/Output/Hazard Models/Models/"
output_assump  <- "C:/Users/yzhu/Desktop/SIMAH project/SIMAH/SIMAH_workplace/nhis/Restricted access data/Output/Assumptions/"


# Load all data all at once
load("C:/Users/yzhu/Desktop/SIMAH project/SIMAH/SIMAH_workplace/nhis/Restricted access data/Data/NHIS_Data.RData")

## load the function for RERI
load("0_Function Additive Interaction.R")


## Create functions and specify causes of death ------------------------------------------------------------------------------------------
# Function to run the Cox and Aalen models; also exports figures for the Cox PH assumption
    # Note: Two different versions of the model were ran identify the interaction effect (model with the interaction term) and the joint effect (model with interacting variable)

table4to9 <- function(data, design, deaths_list, SES, lifestyle, table_label){
  
  cat("Progress indicator:", "\n")  # progress indicator
  
  # Create labels, to be used later
  data_name  <- sub(".*_", "", enexpr(data))
  
  foreach (i = deaths_list) %do% {
    
    # 1) Data preparation 
    data <- mutate (data, 
                    cause_of_death = .data[[i]],
                    SES = {{SES}},
                    lifestyle = {{lifestyle}},
                    SES_lifestyle = interaction(SES, lifestyle)) # Create an 'interaction' variable, combining the SES and lifestyle
    
    design <- mutate (design,
                      cause_of_death = .data[[i]],
                      SES = {{SES}},
                      lifestyle = {{lifestyle}},
                      SES_lifestyle = interaction(SES, lifestyle))
    
    
    cat("    Compute n total and n with outcome by interaction variable", "\n")
    ns <- data %>%
      group_by(SES_lifestyle) %>%
      summarize(n_total = n()) %>%
      left_join(data %>%
                  filter(cause_of_death == 1) %>%
                  group_by(SES_lifestyle) %>%
                  summarize(n_case = n()) %>%
                  data.frame(), by = "SES_lifestyle") %>%
      data.frame() %>% 
      rename(variable = SES_lifestyle)
    cat("    Completed", "\n")
    
    
    # Create labels, to be used later
    death_name <- sub("_.*", "", enexpr(i))
    SES_name   <- enexpr(SES)
    lifestyle_name <- enexpr(lifestyle)
    
    cat(death_name, "\n") # progress indicator
    
    
    # 2) Run analyses     
    
    # Cox interaction model adjusted for survey weights
    cat("    Svy Cox Interaction model in progress", "\n")  # progress indicator
    if(data_name == "all"){
      cox_int <- svycoxph(Surv(bl_age, end_age, cause_of_death) ~ SES * lifestyle + female + married2 + race4 + srvy_yr22, design = design)
    } else if(data_name %in% c("female", "male")){
      cox_int <- svycoxph(Surv(bl_age, end_age, cause_of_death) ~ SES * lifestyle + married2 + race4 + srvy_yr22, design = design)
    }
    cat("    Completed", "\n")  # progress indicator
    
    # Cox joint effect model adjusted for survey weights
    cat("    Svy Cox Joint effects model in progress", "\n")  
    if(data_name == "all"){
      cox_joint <- svycoxph(Surv(bl_age, end_age, cause_of_death) ~ SES_lifestyle + female + married2 + race4 + srvy_yr22, design = design)
    } else if(data_name %in% c("female", "male")){
      cox_joint <- svycoxph(Surv(bl_age, end_age, cause_of_death) ~ SES_lifestyle + married2 + race4 + srvy_yr22, design = design)
    }
    cat("    Completed", "\n")    
    
    
    # Cox interaction model NOT adjusting for survey weights
    cat("    Cox Interaction model NOT adjusting for survey weights in progress", "\n")  # progress indicator
    cox_int_unwt <- coxph(Surv(bl_age, end_age, cause_of_death) ~ SES * lifestyle + married2 + race4 + srvy_yr22, data = data)
    cat("    Completed", "\n")  # progress indicator
    
    # Cox joint effect model NOT adjusting for survey weights
    cat("    Cox Joint effects model NOT adjusting for survey weights in progress", "\n")
    cox_joint_unwt <- coxph(Surv(bl_age, end_age, cause_of_death) ~ SES_lifestyle + married2 + race4 + srvy_yr22, data = data)
    cat("    Completed", "\n")
    
    
    # Aalen Interaction model
    cat("    Aalen Interaction model in progress", "\n")
    if(data_name == "all"){
      aalen_int <- aalen(Surv(bl_age, end_age, cause_of_death) ~ const(SES)*const(lifestyle) + const(female) + const(married2) + race4 + const(srvy_yr22), data = data)
    } else if(data_name %in% c("female", "male")){
      aalen_int <- aalen(Surv(bl_age, end_age, cause_of_death) ~ const(SES)*const(lifestyle) + const(married2) + race4 + const(srvy_yr22),  data = data)
    }
    cat("    Completed", "\n")
    
    # Aalen joint effects model
    cat("    Aalen Joint effects model in progress", "\n")
    if(data_name == "all"){
      aalen_joint <- aalen(Surv(bl_age, end_age, cause_of_death) ~ const(SES_lifestyle) + const(female) + const(married2) + race4 + const(srvy_yr22), data = data)
    } else if(data_name %in% c("female", "male")){
      aalen_joint <- aalen(Surv(bl_age, end_age, cause_of_death) ~ const(SES_lifestyle) + const(married2) + race4 + const(srvy_yr22), data = data) # robust = 0 to remove the 2 tests for age-varying effects
    }
    cat("    Completed", "\n")
    
    # Save model results 
    saveRDS(cox_int,    paste0(output_models, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, "_cox_int.rds"))
    saveRDS(cox_joint,  paste0(output_models, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, "_cox_joint.rds"))
    saveRDS(cox_int_unwt,    paste0(output_models, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, "_cox_int_unwt.rds"))
    saveRDS(cox_joint_unwt,  paste0(output_models, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, "_cox_joint_unwt.rds"))
    saveRDS(aalen_int,   paste0(output_models, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, "_aalen_int.rds"))
    saveRDS(aalen_joint, paste0(output_models, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, "_aalen_joint.rds"))
    
    # Save assumption plot for Cox model
    pdf(paste0(output_assump,  "CoxPH_", table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, ".pdf")); plot(cox.zph(cox_int), col = "red"); dev.off()   
    pdf(paste0(output_assump,  "CoxPH_", table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, ".pdf")); plot(cox.zph(cox_int_unwt), col = "red"); dev.off()
    
    
    # 3) Format and save results 
    cox_int_results <- cox_int %>% tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
      mutate(variable = term,
             HR = round(estimate, 2),
             conf.low = round(conf.low, 2),
             conf.high = round(conf.high, 2),
             p.value_HR = round(p.value, 3),
             p.value_HR = ifelse(p.value_HR <.001, "<.001", p.value_HR),
             CI = paste0("(",conf.low,", ", conf.high, ")")) %>%
      select(variable, HR, CI, p.value_HR) %>%
      filter(str_detect(variable, "SES|lifestyle")) %>%
      # mutate(variable = str_remove(variable, fixed("SES")),   # keep the name in order to calculate RERI
      #        variable = str_remove(variable, fixed("lifestyle"))) %>%
      add_row(variable = "INTERACTION MODELS", .before=1)
    
    
    cox_joint_results <- cox_joint %>% tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
      mutate(variable = term,
             HR = round(estimate, 2),
             conf.low = round(conf.low, 2),
             conf.high = round(conf.high, 2),
             p.value_HR = round(p.value, 3),
             p.value_HR = ifelse(p.value_HR <.001, "<.001", p.value_HR),
             CI = paste0("(",conf.low,", ", conf.high, ")")) %>%
      select(variable, HR, CI, p.value_HR) %>%
      filter(str_detect(variable, "SES")) %>%
      mutate(variable = str_remove(variable, fixed("SES_lifestyle"))) %>% 
      add_row(variable = "JOINT MODELS", .before=1)
    
    
    cox_int_unwt_results <- cox_int_unwt %>% tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
      mutate(variable = term,
             HR = round(estimate, 2),
             conf.low = round(conf.low, 2),
             conf.high = round(conf.high, 2),
             p.value_HR = round(p.value, 3),
             p.value_HR = ifelse(p.value_HR <.001, "<.001", p.value_HR),
             CI = paste0("(",conf.low,", ", conf.high, ")")) %>%
      select(variable, HR, CI, p.value_HR) %>%
      rename(HR_unwt = HR, CI_unwt = CI, p.value_HR_unwt = p.value_HR) %>%
      filter(str_detect(variable, "SES|lifestyle")) %>%
      # mutate(variable = str_remove(variable, fixed("SES")),   # keep the name in order to calculate RERI
      #        variable = str_remove(variable, fixed("lifestyle"))) %>%
      add_row(variable = "INTERACTION MODELS", .before=1)
    
    
    cox_joint_unwt_results <- cox_joint_unwt %>% tidy(exponentiate = TRUE, conf.int = TRUE) %>% 
      mutate(variable = term,
             HR = round(estimate, 2),
             conf.low = round(conf.low, 2),
             conf.high = round(conf.high, 2),
             p.value_HR = round(p.value, 3),
             p.value_HR = ifelse(p.value_HR <.001, "<.001", p.value_HR),
             CI = paste0("(",conf.low,", ", conf.high, ")")) %>%
      select(variable, HR, CI, p.value_HR) %>%
      rename(HR_unwt = HR, CI_unwt = CI, p.value_HR_unwt = p.value_HR) %>%
      filter(str_detect(variable, "SES")) %>%
      mutate(variable = str_remove(variable, fixed("SES_lifestyle"))) %>% 
      add_row(variable = "JOINT MODELS", .before=1)
    
    
    aalen_int_results <- as.data.frame(cbind(aalen_int$gamma, diag(aalen_int$robvar.gamma))) %>%
      mutate (variable = rownames(.),
              var = V2,
              p.value_Deaths = round(2*pnorm(-abs(estimate / sqrt(var))),3),
              p.value_Deaths = ifelse(p.value_Deaths <.001, "<.001", p.value_Deaths),
              lower.ci = round((estimate - (1.96 * sqrt(var)))*10000, 1),
              upper.ci = round((estimate + (1.96 * sqrt(var)))*10000, 1),
              estimate_10000py = round(estimate*10000, 1),
              Deaths_CI_10000py = paste0("(",lower.ci,", ", upper.ci, ")")) %>%
      select (variable, estimate_10000py, Deaths_CI_10000py, p.value_Deaths) %>%
      filter(str_detect(variable, "SES|lifestyle")) %>%
      mutate(variable = str_remove(variable, fixed("const(SES)")),
             variable = str_remove(variable, fixed("const(lifestyle)"))) %>%
      add_row(variable = "INTERACTION MODELS", .before=1)%>%
      remove_rownames()
    
    
    aalen_joint_results <- as.data.frame(cbind(aalen_joint$gamma, diag(aalen_joint$robvar.gamma))) %>%
      mutate (variable = rownames(.),
              var = V2,
              p.value_Deaths = round(2*pnorm(-abs(estimate / sqrt(var))),3),
              p.value_Deaths = ifelse(p.value_Deaths <.001, "<.001", p.value_Deaths),
              lower.ci = round((estimate - (1.96 * sqrt(var)))*10000, 1),
              upper.ci = round((estimate + (1.96 * sqrt(var)))*10000, 1),
              estimate_10000py = round(estimate*10000, 1),
              Deaths_CI_10000py = paste0("(",lower.ci,", ", upper.ci, ")")) %>%
      select (variable, estimate_10000py, Deaths_CI_10000py, p.value_Deaths) %>%
      filter(str_detect(variable, "SES")) %>%
      mutate(variable = str_remove(variable, fixed("const(SES_lifestyle)"))) %>%
      add_row(variable = "JOINT MODELS", .before=1) %>%
      remove_rownames()
    
    
    cox_results <- rbind(cox_int_results, cox_joint_results) %>%
      left_join(ns, by = "variable") %>%
      relocate(n_case, .after = variable) %>%
      relocate(n_total, .after = n_case)
    
    cox_unwt_results <- rbind(cox_int_unwt_results, cox_joint_unwt_results)
    
    
    
    
    ## compute RERI for the cox_int model
    add_int <- cox_int_results %>% 
      filter(str_detect(variable, ":")) %>% select(variable) %>%
      separate(variable, into = c("SES", "lifestyle"), sep = ":", remove = FALSE) %>%
      data.frame()
    
    foreach(i = 1:nrow(add_int))%do%{
      
      rs <- additive_interactions( cox_int, add_int[i, "SES"], add_int[i, "lifestyle"] )
      
      add_int[i, "RERI"] <- rs[1,2]
      add_int[i, "CI.lo"] <- rs[1,3]
      add_int[i, "CI.hi"] <- rs[1,4]
      add_int[i, "p.value"] <- rs[1,5]
      
    }
    
    ## merge the RERI output with the cox_int_results
    cox_results_RERI <- cox_results %>% 
      left_join(add_int %>%
                  mutate(RERI = round(RERI, 2),
                         CI.lo = round(CI.lo, 2),
                         CI.hi = round(CI.hi, 2),
                         CI_RERI = paste0("(", CI.lo,", ", CI.hi, ")"),
                         p.value_RERI = round(p.value, 3),
                         p.value_RERI = ifelse(p.value_RERI <.001, "<.001", p.value_RERI)
                  ) %>%
                  select(-SES, -lifestyle, -CI.lo, -CI.hi, -p.value), 
                by = "variable") %>%
      mutate(variable = str_remove(variable, fixed("SES")), 
             variable = str_remove(variable, fixed("lifestyle")))
    
    
    ## compute RERI for the cox_int_unwt model
    add_int_unwt <- cox_int_unwt_results %>% 
      filter(str_detect(variable, ":")) %>% select(variable) %>%
      separate(variable, into = c("SES", "lifestyle"), sep = ":", remove = FALSE) %>%
      data.frame()
    
    foreach(i = 1:nrow(add_int_unwt))%do%{
      
      rs <- additive_interactions( cox_int_unwt, add_int_unwt[i, "SES"], add_int_unwt[i, "lifestyle"] )
      
      add_int_unwt[i, "RERI"] <- rs[1,2]
      add_int_unwt[i, "CI.lo"] <- rs[1,3]
      add_int_unwt[i, "CI.hi"] <- rs[1,4]
      add_int_unwt[i, "p.value"] <- rs[1,5]
      
    }
    
    ## merge the RERI output with the cox_int_unwt_results
    cox_unwt_results_RERI <- cox_unwt_results %>% 
      left_join(add_int_unwt %>%
                  mutate(RERI = round(RERI, 2),
                         CI.lo = round(CI.lo, 2),
                         CI.hi = round(CI.hi, 2),
                         CI_RERI = paste0("(", CI.lo,", ", CI.hi, ")"),
                         p.value_RERI = round(p.value, 3),
                         p.value_RERI = ifelse(p.value_RERI <.001, "<.001", p.value_RERI)
                  ) %>%
                  select(-SES, -lifestyle, -CI.lo, -CI.hi, -p.value) %>%
                  rename(RERI_unwt = RERI, CI_RERI_unwt = CI_RERI, p.value_RERI_unwt = p.value_RERI), 
                by = "variable") %>%
      mutate(variable = str_remove(variable, fixed("SES")), 
             variable = str_remove(variable, fixed("lifestyle")))
    
    
    aalen_results <- rbind(aalen_int_results, aalen_joint_results)
    
    results <- full_join(cox_results_RERI, cox_unwt_results_RERI, by="variable") %>%
      full_join(aalen_results, by="variable") %>%
      add_row(variable = death_name, .before=1) 
    
    write_csv(results, paste0(output_tables, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, ".csv"), na="")
    cat("    Results were exported", "\n")  # progress indicator
  }   
}



# Test the function:
death_list <- "heart_death" # specify cause of death for testing

nhis_female <- sample_frac(nhis_female, 0.10) # select x% of sample for testing
nhis_female_svy <- nhis_female %>%
  as_survey_design(id=new_psu, strata=new_stratum, weights=new_weight, nest = TRUE)

table4to9(nhis_female, nhis_female_svy, death_list, edu3, alc5, "table4a") # run function for testing

table4to9(nhis_male, nhis_male_svy, death_list, edu3, alc5, "table4a")


# Specify the causes of death (to be used below)
death_list <- c("All9_death", "Alcohol_death", "Despair_death", "MVA_death", "OUI_death", "ISH_death",
                "AUD_death", "LDAC_death", "DM_death", "IHD_death", "IS_death", "HHD_death", "Poisoning_death")

# Table 4: Alcohol ----------------------------------------------------------------------------------------
## Edu x Alcohol
table4to9(nhis25_all, nhis25_all_svy, death_list, edu3, alc5, "table4a") # All participants
table4to9(nhis25_female, nhis25_female_svy, death_list, edu3, alc5, "table4a") # Females
table4to9(nhis25_male, nhis25_male_svy,   death_list, edu3, alc5, "table4a") # Males



##### compute RERI and merge with the output table based on previously saved model ----------------------------------------------------


## categories excluding the reference
educat <- c("edu3Highschool", "edu3Some college")
alccat <- c("alc5Former drinker", "alc5Lifetime abstainer", "alc5Category II", "alc5Category III")

edu3 <- rep(educat, 4)
alc5 <- rep(alccat, each = 2)
RERI <- CI.lo <- CI.hi <- p.value <- rep(NA, 8)

add_int <- data.frame(edu3, alc5, RERI, CI.lo, CI.hi, p.value)


foreach(i = 1:8)%do%{
  
  rs <- additive_interactions( cox_int, add_int[i, "edu3"], add_int[i, "alc5"] )
  
  add_int[i, "RERI"] <- rs[1,2]
  add_int[i, "CI.lo"] <- rs[1,3]
  add_int[i, "CI.hi"] <- rs[1,4]
  add_int[i, "p.value"] <- rs[1,5]
  
}

# load the cox_int_results for saved model first, and then merge with the add_int table for RERI and CI
# cox_int_results <- readRDS(cox_int, paste0(output_models, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, "_cox_int.rds"))

cox_int_results_RERI <- cox_int_results %>%
  left_join(  cox_int_results %>% 
                filter(str_detect(variable, ":")) %>% select(variable) %>%
                separate(variable, into = c("edu3", "alc5"), sep = ":", remove = FALSE) %>% 
                left_join(add_int %>%
                            mutate(RERI = round(RERI, 2),
                                   CI.lo = round(CI.lo, 2),
                                   CI.hi = round(CI.hi, 2),
                                   CI_RERI = paste0("(", CI.lo,", ", CI.hi, ")"),
                                   p.value_RERI = round(p.value, 3),
                                   p.value_RERI = ifelse(p.value_RERI <.001, "<.001", p.value_RERI)
                            ) %>%
                            select(-CI.lo, -CI.hi, -p.value) %>%
                            mutate(edu3 = str_remove(edu3, fixed("edu3")), 
                                   alc5 = str_remove(alc5, fixed("alc5"))), 
                          by = c("edu3", "alc5")),
              by = "variable" )





# **NOTE**: Need to change to code for this so that the start age and end age is specified
# table4to9(nhis_fem.age.gp1,  death_list, edu3, alc5, "table4a") # Females, age group 1 # NOTE: Need to change to code for this so that the start time and end time is specified
# table4to9(nhis_fem.age.gp2,  death_list, edu3, alc5, "table4a") # Females, age group 2
# table4to9(nhis_fem.age.gp3,  death_list, edu3, alc5, "table4a") # Females, age group 3
# table4to9(nhis_male.age.gp1, death_list, edu3, alc5, "table4a") # Males, age group 1
# table4to9(nhis_male.age.gp2, death_list, edu3, alc5, "table4a") # Males, age group 2
# table4to9(nhis_male.age.gp3, death_list, edu3, alc5, "table4a") # Males, age group 3

nhis_fem.white <- nhis25_female %>% filter(race4 == "White")

table4to9(nhis_fem.white, death_list, edu3, alc5, "table4a") # Females, white
table4to9(nhis_fem.black, death_list, edu3, alc5, "table4a") # Females, black
table4to9(nhis_fem.hisp,  death_list, edu3, alc5, "table4a") # Females, Hispanic
table4to9(nhis_fem.other, death_list, edu3, alc5, "table4a") # Females, Other

table4to9(nhis_male.white, death_list, edu3, alc5, "table4a") # Males, white
table4to9(nhis_male.black, death_list, edu3, alc5, "table4a") # Males, black
table4to9(nhis_male.hisp,  death_list, edu3, alc5, "table4a") # Males, Hispanic
table4to9(nhis_male.other, death_list, edu3, alc5, "table4a") # Males, Other



## Income x Alcohol
table4to9(nhis18_clean, nhis18_clean_svy, death_list, income5, alc5, "table4b") # All participants
table4to9(nhis18_female, nhis18_female_svy, death_list, income5, alc5, "table4b") # Females
table4to9(nhis18_male, nhis18_male_svy, death_list, income5, alc5, "table4b") # Males

# table4to9(nhis_fem.age.gp1,  death_list, income5, alc5, "table4b")  # Females, age group 1
# table4to9(nhis_fem.age.gp2,  death_list, income5, alc5, "table4b")  # Females, age group 2
# table4to9(nhis_fem.age.gp3,  death_list, income5, alc5, "table4b")  # Females, age group 3
# table4to9(nhis_male.age.gp1, death_list, income5, alc5, "table4b")  # Males, age group 1
# table4to9(nhis_male.age.gp2, death_list, income5, alc5, "table4b")  # Males, age group 2
# table4to9(nhis_male.age.gp3, death_list, income5, alc5, "table4b")  # Males, age group 3

table4to9(nhis_fem.white, death_list, income5, alc5, "table4b") # Females, white
table4to9(nhis_fem.black, death_list, income5, alc5, "table4b") # Females, black
table4to9(nhis_fem.hisp,  death_list, income5, alc5, "table4b") # Females, Hispanic
table4to9(nhis_fem.other, death_list, income5, alc5, "table4b") # Females, Other (non-Hispanic)

table4to9(nhis_male.white, death_list, income5, alc5, "table4b") # Males, white
table4to9(nhis_male.black, death_list, income5, alc5, "table4b") # Males, black
table4to9(nhis_male.hisp,  death_list, income5, alc5, "table4b") # Males, Hispanic
table4to9(nhis_male.other, death_list, income5, alc5, "table4b") # Males, Other (non-Hispanic)


## Race x Alcohol
table4to9(nhis18_clean, nhis18_clean_svy, death_list, race4, alc5, "table4c") # All participants
table4to9(nhis18_female, nhis18_female_svy, death_list, race4, alc5, "table4c") # Females
table4to9(nhis18_male, nhis18_male_svy, death_list, race4, alc5, "table4c") # Males

# table4to9(nhis_fem.age.gp1,  death_list, race4, alc5, "table4c")  # Females, age group 1
# table4to9(nhis_fem.age.gp2,  death_list, race4, alc5, "table4c")  # Females, age group 2
# table4to9(nhis_fem.age.gp3,  death_list, race4, alc5, "table4c")  # Females, age group 3
# table4to9(nhis_male.age.gp1, death_list, race4, alc5, "table4c")  # Males, age group 1
# table4to9(nhis_male.age.gp2, death_list, race4, alc5, "table4c")  # Males, age group 2
# table4to9(nhis_male.age.gp3, death_list, race4, alc5, "table4c")  # Males, age group 3

table4to9(nhis_fem.edu1, death_list, race4, alc5, "table4c") # Female, low edu
table4to9(nhis_fem.edu2, death_list, race4, alc5, "table4c") # Female, medium edu
table4to9(nhis_fen.edu3, death_list, race4, alc5, "table4c") # Female, high edu

table4to9(nhis_male.edu1, death_list, race4, alc5, "table4c") # Males, low edu
table4to9(nhis_male.edu2, death_list, race4, alc5, "table4c") # Males, medium edu
table4to9(nhis_male.edu3, death_list, race4, alc5, "table4c") # Males, high edu






# Table 5: Smoking ----------------------------------------------------------------------------------------
## Edu x Smoking
table4to9(nhis25_clean, nhis25_clean_svy, death_list, edu3, smk4, "table5a") # All participants
table4to9(nhis25_female, nhis25_female_svy, death_list, edu3, smk4, "table5a") # Females
table4to9(nhis25_male, nhis25_male_svy, death_list, edu3, smk4, "table5a") # Males

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
table4to9(nhis18_clean, nhis18_clean_svy, death_list, income5, smk4, "table5b") # All participants
table4to9(nhis18_female, nhis18_female_svy, death_list, income5, smk4, "table5b") # Females
table4to9(nhis18_male, nhis18_male_svy, death_list, income5, smk4, "table5b") # Males

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
table4to9(nhis18_clean, nhis18_clean_svy, death_list, race4, smk4, "table5c") # All participants
table4to9(nhis18_female, nhis18_female_svy, death_list, race4, smk4, "table5c") # Females
table4to9(nhis18_male, nhis18_male_svy, death_list, race4, smk4, "table5c") # Males

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
table4to9(nhis25_clean, nhis25_clean_svy, death_list, edu3, bmi4, "table6a") # All participants
table4to9(nhis25_female,nhis25_female_svy, death_list, edu3, bmi4, "table6a") # Females
table4to9(nhis25_male, nhis25_male_svy, death_list, edu3, bmi4, "table6a") # Males

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
table4to9(nhis18_clean, nhis18_clean_svy, death_list, income5, bmi4, "table6b") # All participants
table4to9(nhis18_female, nhis18_female_svy, death_list, income5, bmi4, "table6b") # Females
table4to9(nhis18_male, nhis18_male_svy, death_list, income5, bmi4, "table6b") # Males

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
table4to9(nhis18_clean, nhis18_clean_svy, death_list, race4, bmi4, "table6c") # All participants
table4to9(nhis18_female, nhis18_female_svy, death_list, race4, bmi4, "table6c") # Females
table4to9(nhis18_male, nhis18_male_svy, death_list, race4, bmi4, "table6c") # Males

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
table4to9(nhis25_clean, nhis25_clean_svy, death_list, edu3, phy3, "table7a") # All participants
table4to9(nhis25_female, nhis25_female_svy, death_list, edu3, phy3, "table7a") # Females
table4to9(nhis25_male, nhis25_male_svy, death_list, edu3, phy3, "table7a") # Males

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
table4to9(nhis18_clean, nhis18_clean_svy, death_list, income5, phy3, "table7b") # All participants
table4to9(nhis18_female, nhis18_female_svy, death_list, income5, phy3, "table7b") # Females
table4to9(nhis18_male, nhis18_male_svy, death_list, income5, phy3, "table7b") # Males

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
table4to9(nhis18_clean, nhis18_clean_svy, death_list, race4, phy3, "table7c") # All participants
table4to9(nhis18_female, nhis18_female_svy, death_list, race4, phy3, "table7c") # Females
table4to9(nhis18_male, nhis18_male_svy, death_list, race4, phy3, "table7c") # Males

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
table4to9(nhis25_clean, nhis25_clean_svy, death_list, edu3, PsyDistr3, "table8a") # All participants
table4to9(nhis25_female, nhis25_female_svy, death_list, edu3, PsyDistr3, "table8a") # Females
table4to9(nhis25_male, nhis25_male_svy, death_list, edu3, PsyDistr3, "table8a") # Males

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
table4to9(nhis18_clean, nhis18_clean_svy, death_list, income5, PsyDistr3, "table8b") # All participants
table4to9(nhis18_female, nhis18_female_svy, death_list, income5, PsyDistr3, "table8b") # Females
table4to9(nhis18_male, nhis18_male_svy, death_list, income5, PsyDistr3, "table8b") # Males

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
table4to9(nhis18_clean, nhis18_clean_svy, death_list, race4, PsyDistr3, "table8c") # All participants
table4to9(nhis18_female, nhis18_female_svy, death_list, race4, PsyDistr3, "table8c") # Females
table4to9(nhis18_male, nhis18_male_svy, death_list, race4, PsyDistr3, "table8c") # Males

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
table4to9(nhis18_clean, nhis18_clean_svy, death_list, alc5, PsyDistr3, "table9a") # All participants
table4to9(nhis18_female, nhis18_female_svy, death_list, alc5, PsyDistr3, "table9a") # Females
table4to9(nhis18_male, nhis18_male_svy, death_list, alc5, PsyDistr3, "table9a") # Males

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
table4to9(nhis18_clean, nhis18_clean_svy, death_list, hed4, PsyDistr3, "table9b") # All participants
table4to9(nhis18_female, nhis18_female_svy, death_list, hed4, PsyDistr3, "table9b") # Females
table4to9(nhis18_male, nhis18_male_svy, death_list, hed4, PsyDistr3, "table9b") # Males

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


