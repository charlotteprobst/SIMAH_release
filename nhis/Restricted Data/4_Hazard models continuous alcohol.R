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
memory.limit(size=1e+13)
options(scipen=999)


# Specify the data and output file locations
data_path    <- "C:/Users/yzhu/Desktop/SIMAH project/SIMAH/SIMAH_workplace/nhis/Restricted access data/Data/"
output_tables <- "C:/Users/yzhu/Desktop/SIMAH project/SIMAH/SIMAH_workplace/nhis/Restricted access data/Output/Hazard Models/"
output_models <- "C:/Users/yzhu/Desktop/SIMAH project/SIMAH/SIMAH_workplace/nhis/Restricted access data/Output/Hazard Models/Models/"
output_assump  <- "C:/Users/yzhu/Desktop/SIMAH project/SIMAH/SIMAH_workplace/nhis/Restricted access data/Output/Assumptions/"


# Load data
nhis_all    <- readRDS (paste0(data_path, "nhis_all.rds"))
nhis_male   <- readRDS (paste0(data_path, "nhis_male.rds"))
nhis_female <- readRDS (paste0(data_path, "nhis_female.rds"))

nhis_all_svy    <- readRDS (paste0(data_path, "nhis_all_svy.rds"))
nhis_male_svy   <- readRDS (paste0(data_path, "nhis_male_svy.rds"))
nhis_female_svy <- readRDS (paste0(data_path, "nhis_female_svy.rds"))


## Create functions and specify causes of death ------------------------------------------------------------------------------------------
# Function to run the Cox and Aalen models; also exports figures for the Cox PH assumption
# Note: Two different versions of the model were ran identify the interaction effect (model with the interaction term) and the joint effect (model with interacting variable)

table4to9 <- function(data, design, deaths_list, SES, lifestyle, table_label){
  
  cat("Progress indicator:", "\n")  # progress indicator
  
  # Create labels, to be used later
  data_name  <- sub(".*_", "", enexpr(data))
  
  foreach (i = deaths_list) %do% {
    
    # 1) Data preparation 
    data <- mutate(data, 
                   cause_of_death = .data[[i]],
                   SES = {{SES}},
                   lifestyle = {{lifestyle}}
                    ) # Create an 'interaction' variable, combining the SES and lifestyle
    
    design <- mutate(design,
                      cause_of_death = .data[[i]],
                      SES = {{SES}},
                      lifestyle = {{lifestyle}}
                      )
    
    
    cat("    Compute n total and n with outcome by interaction variable", "\n")
    ns <- data %>%
      group_by(SES) %>%
      summarize(n_total = n()) %>%
      left_join(data %>%
                filter(cause_of_death == 1) %>%
                group_by(SES) %>%
                summarize(n_case = n()) %>%
                data.frame(), by = "SES") %>%
      data.frame() %>% 
      rename(variable = SES) %>%
      add_row(variable = "All", n_total = nrow(data), n_case = sum(data$cause_of_death))
      
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
    
    
    # Cox interaction model NOT adjusting for survey weights
    cat("    Cox Interaction model NOT adjusting for survey weights in progress", "\n")  # progress indicator
    cox_int_unwt <- coxph(Surv(bl_age, end_age, cause_of_death) ~ SES * lifestyle + married2 + race4 + srvy_yr22, data = data)
    cat("    Completed", "\n")  # progress indicator
    
    
    # Aalen Interaction model
    cat("    Aalen Interaction model in progress", "\n")
    if(data_name == "all"){
      aalen_int <- aalen(Surv(bl_age, end_age, cause_of_death) ~ const(SES)*const(lifestyle) + const(female) + const(married2) + race4 + const(srvy_yr22), data = data)
    } else if(data_name %in% c("female", "male")){
      aalen_int <- aalen(Surv(bl_age, end_age, cause_of_death) ~ const(SES)*const(lifestyle) + const(married2) + race4 + const(srvy_yr22),  data = data)
    }
    cat("    Completed", "\n")
    
    
    # Save model results 
    saveRDS(cox_int,    paste0(output_models, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, "_cox_int.rds"))
    saveRDS(cox_int_unwt,    paste0(output_models, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, "_cox_int_unwt.rds"))
    saveRDS(aalen_int,   paste0(output_models, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, "_aalen_int.rds"))
 
    
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
      add_row(variable = "INTERACTION MODELS", .before=1)
    
    
    
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
      add_row(variable = "INTERACTION MODELS", .before=1)
    
    
    
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
             variable = str_replace(variable, fixed("const(lifestyle)"), "lifestyle")) %>%
      add_row(variable = "INTERACTION MODELS", .before=1)%>%
      remove_rownames()
    
    
    
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
    cox_results_RERI <- cox_int_results %>% 
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
      mutate(variable = str_remove(variable, fixed("SES")))

    
    
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
    cox_unwt_results_RERI <- cox_int_unwt_results %>% 
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
      mutate(variable = str_remove(variable, fixed("SES"))) 
    
    
    results <- full_join(cox_results_RERI, cox_unwt_results_RERI, by="variable") %>%
      full_join(aalen_int_results, by = "variable") %>%
      mutate(variable = str_replace(variable, "lifestyle", "Daily grams per day")) %>%
      add_row(variable = death_name, .before=1) %>%
      full_join(ns, by = "variable") %>%
      relocate(n_case, .after = variable) %>%
      relocate(n_total, .after = n_case)
    
    
    
    write_csv(results, paste0(output_tables, table_label, "_", death_name,"_", SES_name, "_", lifestyle_name, "_", data_name, ".csv"), na="")
    cat("    Results were exported", "\n")  # progress indicator
  }   
}




# Test the function:
death_list <- "allcause_death"

nhis_female <- sample_frac(nhis_female, 0.10) # select 10% of sample for testing
nhis_female_svy <- nhis_female %>% as_survey_design(id=new_psu, strata=new_stratum, weights=new_weight, nest = TRUE)

table4to9(nhis_female, nhis_female_svy, death_list, edu3, alc_daily_g, "table4c") # run function for testing



