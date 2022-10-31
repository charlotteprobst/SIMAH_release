
# SIMAH - NESARC Alcohol Use to Alcohol Use Disorder (AUD) Incidence

# Load libraries 
library(tidyverse)   # data management
library(tableone)    # descriptive characteristics 
library(knitr)       # table formatting 
library(survey)      # to work with survey data
library(srvyr)       # adds dplyr like syntax to the survey package
library(broom)       # model results


# Specify the data and output file locations
data    <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/2_Processed data/"  # Location of data
output  <- "C:/Users/klajd/OneDrive/SIMAH/SIMAH_workspace/nesarc/AlcUse and AUD/"  # Location of output



# Load and edit data -----------------------------------------------------------------------------------------------
nesarc <- readRDS(paste0(data, "nesarc_all.rds")) %>%
  
  # Remove those with data at one time point only (8,440 observations removed; n=69,306)
  group_by(idnum) %>% filter(n()>1) %>% ungroup() %>%             
  
  # Edit / create variables
  mutate(alc_daily_g = ifelse(alc_daily_g>200, 200, alc_daily_g), # Cap max g/day to 200
    alc5.factor_w1 = lag(alc5.factor, 1),                             # Create 'wave 1' version of some variables
    AUD_lifetime2_wave1 = lag(AUD_lifetime2, 1),
    alc_daily_g_wave1 = lag(alc_daily_g, 1)) %>% 
  
  # Remove baseline assessment (the variables needed from baseline were created above)
  filter(wave==2) %>%           
  
  # Create numeric version of outcome
  mutate (incident_AUD = ifelse(AUD_lifetime2=="No AUD", 0, 1),
          incident_AUD.factor = factor(ifelse(AUD_lifetime2=="No AUD", "No AUD", "Incident AUD"))) %>% 
  
  # Select variables of interest 
  select (idnum, wave, female, female.factor, psu, stratum, weight_wave2,  
          age, age7, alc_daily_g_wave1, alc5.factor_w1, alc5.factor, incident_AUD, incident_AUD.factor, AUD_lifetime2, AUD_lifetime2_wave1,
          race.factor, edu3) %>%
  rename(ethnicity4 = race.factor)


# Prepare dataframe, accounting for survey design
nesarc_srvyr <- nesarc %>%
  as_survey_design(id=psu, strata=stratum, weights=weight_wave2, nest = TRUE) %>%
  filter(AUD_lifetime2_wave1=="No AUD") %>%             # keep those with no AUD at wave 1
  filter(!is.na(alc5.factor_w1) & !is.na(incident_AUD)) # remove those with no baseline alcohol use
options(survey.lonely.psu="adjust")

nesarc_srvyr_female <- filter(nesarc_srvyr, female==1)
nesarc_srvyr_male   <- filter(nesarc_srvyr, female==0)



# Descriptives -----------------------------------------------------------------------------------------------

# Check prevalence of outcome; can OR be interpreted as RR ? 
nesarc_srvyr %>% 
  group_by(incident_AUD) %>% 
  summarize(proportion = survey_mean(),
            total = survey_total())

    nesarc_srvyr_female %>% 
      group_by(incident_AUD) %>% 
      summarize(proportion = survey_mean(),
        total = survey_total())
    
    
    nesarc_srvyr_male %>% 
      group_by(incident_AUD) %>% 
      summarize(proportion = survey_mean(),
        total = survey_total())
    # Prevalence of AUD is low (<10%); OR can be interpreted as RR
    

    
# Participant characteristics 
svyCreateTableOne(vars = c("female.factor", "age7", "edu3", "ethnicity4", "alc5.factor_w1", "alc5.factor"),
                  strata = "incident_AUD.factor", addOverall = TRUE, data = nesarc_srvyr) %>% 
  print(noSpaces = TRUE, catDigits = 0, contDigits = 1, printToggle = FALSE, test=FALSE, format="p") %>%
  write.csv(paste0(output, "Table 1 Demographic characteristics.csv"))
    
str(nesarc_srvyr)

# Alcohol use --> AUD Incidence Analyses --------------------------------------------------------------------

# Model 1: No adjustment ************************************************************************************

mod1_all <- svyglm(incident_AUD ~ alc5.factor_w1, family = quasibinomial, design = nesarc_srvyr)
results1_all <- tidy(mod1_all, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(group = "All", 
         adjustment = "None")
    


# Model 2: Age + Sex adjusted ************************************************************************************

# MEN and WOMEN
mod2_all <- svyglm(incident_AUD ~ alc5.factor_w1 + age + female, family = quasibinomial, design = nesarc_srvyr)
results2_all <- tidy(mod2_all, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(group = "All", 
         adjustment = "Age, sex")

# WOMEN
mod2_women <- svyglm(incident_AUD ~ alc5.factor_w1 + age, family = quasibinomial, design = nesarc_srvyr_female)
results2_women <- tidy(mod2_women, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(group = "Women", 
    adjustment = "Age, sex")

# MEN
mod2_men <- svyglm(incident_AUD ~ alc5.factor_w1 + age, family = quasibinomial, design = nesarc_srvyr_male)
results2_men <- tidy(mod2_men, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(group = "Men", 
    adjustment = "Age, sex")



# Model 3: Age + Sex + SES + Race adjusted *************************************************************************

# MEN and WOMEN
mod3_all <- svyglm(incident_AUD ~ alc5.factor_w1 + age + female + edu3 + ethnicity4, family = quasibinomial, design = nesarc_srvyr)
results3_all <- tidy(mod3_all, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(group = "All", 
    adjustment = "Age, sex, edu, ethnicity")

# WOMEN
mod3_women <- svyglm(incident_AUD ~ alc5.factor_w1 + age + edu3 + ethnicity4, family = quasibinomial, design = nesarc_srvyr_female)
results3_women <- tidy(mod3_women, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(group = "Women", 
    adjustment = "Age, sex, edu, ethnicity")

# MEN
mod3_men <- svyglm(incident_AUD ~ alc5.factor_w1 + age + edu3 + ethnicity4, family = quasibinomial, design = nesarc_srvyr_male)
results3_men <- tidy(mod3_men, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(group = "Men", 
    adjustment = "Age, sex, edu, ethnicity")



# Combine results **************************************************************************************************
rbind(results1_all, 
      results2_all, results2_men, results2_women,
      results3_all, results3_men, results3_women) %>%
  filter(str_detect(term, 'alc')) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)),
    CI = paste0("(", conf.low, ", ", conf.high, ")"),
    OR = estimate,
    AlcoholUse = str_remove(term, "alc5.factor_w1")) %>% 
  select(adjustment, group, AlcoholUse, OR, CI) %>% 
  pivot_wider(names_from = "group", values_from = c("OR", "CI"), names_vary="slowest") %>%
  write_csv(paste0(output,"Table 2 Results.csv", na="")) %>%
  kable()




