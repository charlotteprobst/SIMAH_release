#' Summarise mean alcohol consumption for price policy paper, Tables (Kilian et al.)
#'
#' @param
#' @keywords alcohol postprocessing
#' @export
#' @examples

# libraries
library(dplyr)
library(tidyr)
library(data.table)
library(openxlsx)

WorkingDirectory <- "/Users/carolinkilian/Desktop/SIMAH_workplace/microsim/2_output_data/"
OutputDirectory <- "/Users/carolinkilian/Desktop/SIMAH_workplace/pricing_policy/"
 
# load data
data_alccont <- read.csv(paste0(WorkingDirectory, "2025-01-29/output-policy_alcoholcont_2025-01-31.csv"))
data_alccat <- read.csv(paste0(WorkingDirectory, "2025-01-29/output-policy_alcoholcat_2025-01-31.csv"))
data_alccontcat <- read.csv(paste0(WorkingDirectory, "2025-01-29/output-policy_alcoholcontcat_2025-01-31.csv"))
  
# set standard format
options(digits = 8)

# --------------------------------------------
# TABLES
# --------------------------------------------
  
# Table S1) Average alcohol consumption levels in total population (continuous alcohol outcome)
  
alccont <- data_alccont %>%
  # set labels
  mutate(year = as.numeric(as.character(year)),
         sex = ifelse(sex=="m","Men","Women"),
         education = factor(education, 
                            levels = c("LEHS", "SomeC", "College"),
                            labels = c("High school or less", "Some college", "College +")),
         scenario = factor(case_when(
           policymodel == 1 ~ "Reference",
           policymodel != 1  ~ paste0("Scenario ", policymodel-1),
           TRUE ~ NA), levels = c("Reference", 
                                  "Scenario 1", "Scenario 2", 
                                  "Scenario 3", "Scenario 4")),
         setting = factor(setting,
                          levels = c("standard", "min", "max"),
                          labels = c("Standard", "Minimum", "Maximum"))) %>%
  # average across random number seeds
  group_by(scenario, setting, nunc, year, sex, education) %>% 
  summarise(meansimulation = mean(as.numeric(meansimulation)))
  
# get mean across multiple model runs 
mean <- alccont %>%
  group_by(scenario, setting, year, sex, education) %>% 
  summarise(meangpd = mean(as.numeric(meansimulation)))
    
# get uncertainty across multiple model runs (min/max)
var <- alccont %>% filter(year != "2000") %>%
  group_by(scenario, setting, year, sex, education) %>% 
  mutate(min = min(as.numeric(meansimulation)),
         max = max(as.numeric(meansimulation)),
         var = ifelse(meansimulation == min, "min",
                      ifelse(meansimulation == max, "max", NA))) %>% ungroup() %>%
  filter(meansimulation == min | meansimulation == max) %>%
  dplyr::select(c(scenario, setting, nunc, year, sex, education, var)) 

# get mean GPD with uncertainty    
output1A <- merge(alccont, var, all.y = T) %>% 
  rename("meangpd" = "meansimulation") %>% 
  dplyr::select(c(scenario, setting, year, sex, education, meangpd, var)) %>%
  pivot_wider(names_from = "var", values_from = "meangpd") %>%
  left_join(mean, .) %>% 
  # select just 2019 or 2000 in reference scenario
  filter(year == 2019 | (year == 2000 & scenario == "Reference")) 

# get change in mean GPD with uncertainty
reference <- output1A %>% filter(scenario == "Reference" & year == 2019) %>% ungroup() %>%
  rename("meangpdref" = "meangpd", "maxref" = "max", "minref" = "min") %>% 
  dplyr::select(-c(scenario))

output1B <- output1A %>% filter(scenario != "Reference") %>% 
  left_join(., reference) %>% 
  mutate(diffgpd = meangpd - meangpdref,
         diffmax = min - minref,
         diffmin = max - maxref,
         percgpd = (meangpd - meangpdref) / meangpdref,
         percmax = (min - minref) / minref,
         percmin = (max - maxref) / maxref) %>% 
  dplyr::select(c(scenario, setting, year, sex, education,
                  diffgpd, diffmin, diffmax,
                  percgpd, percmin, percmax)) 
 
# combine elements into Table 1 and formatting
output1 <- left_join(output1A, output1B) %>% 
  mutate(mean = case_when(
          !is.na(meangpd) & year != 2000 ~ paste0(format(round(meangpd, digits = 2), nsmall = 2), " (", format(round(min, digits = 2), nsmall = 2), ", ", format(round(max, digits = 2), nsmall = 2), ")"),
          !is.na(meangpd) & year == 2000 ~ paste0(format(round(meangpd, digits = 2), nsmall = 2)),
          TRUE ~ NA), 
         diff = case_when(
           !is.na(diffgpd) & year != 2000 ~ paste0(format(round(diffgpd, digits = 2), nsmall = 2), " (", format(round(diffmax, digits = 2), nsmall = 2), ", ", format(round(diffmin, digits = 2), nsmall = 2), ")"),
           !is.na(diffgpd) & year == 2000 ~ paste0(format(round(diffgpd, digits = 2), nsmall = 2)),
           TRUE ~ NA),
         perc = case_when(
           !is.na(percgpd) & year != 2000 ~ paste0(format(round(percgpd*100, digits = 2), nsmall = 2), " (", format(round(percmax*100, digits = 2), nsmall = 2), ", ", format(round(percmin*100, digits = 2), nsmall = 2), ")"),
           !is.na(percgpd) & year == 2000 ~ paste0(format(round(percgpd*100, digits = 2), nsmall = 2)),
           TRUE ~ NA)) %>% 
  dplyr::select(c(setting, scenario, year, sex, education, mean, diff, perc)) %>% 
  arrange(setting)
  

# Table S2) Prevalence of alcohol use categories (categorical alcohol outcome)

alccat <- data_alccat %>%
  # set labels
  mutate(year = as.numeric(as.character(year)),
         sex = ifelse(sex=="m","Men","Women"),
         education = factor(education, 
                            levels = c("LEHS", "SomeC", "College"),
                            labels = c("High school or less", "Some college", "College +")),
         scenario = factor(case_when(
           policymodel == 1 ~ "Reference",
           policymodel != 1  ~ paste0("Scenario ", policymodel-1),
           TRUE ~ NA), levels = c("Reference", 
                                  "Scenario 1", "Scenario 2", 
                                  "Scenario 3", "Scenario 4")),
         alc_cat = factor(alc_cat, 
                          levels = c("Non-drinker", "Low risk", "Medium risk", "High risk"),
                          labels = c("Abstinence", "Category I", "Category II", "Category III")),
         setting = factor(setting,
                          levels = c("standard", "min", "max"),
                          labels = c("Standard", "Minimum", "Maximum"))) %>%
  # average across random number seeds
  group_by(scenario, setting, nunc, year, sex, education, alc_cat) %>% 
  summarise(propsimulation = mean(as.numeric(propsimulation)))

# get mean across multiple model runs 
mean <- alccat %>%
  group_by(scenario, setting, year, sex, education, alc_cat) %>% 
  summarise(meanprop = mean(as.numeric(propsimulation)))
    
# get uncertainty across multiple model runs (min/max)
var <- alccat %>% filter(year != "2000") %>%
  group_by(scenario, setting, year, sex, education, alc_cat) %>% 
  mutate(min = min(as.numeric(propsimulation)),
         max = max(as.numeric(propsimulation)),
         var = ifelse(propsimulation == min, "min",
                      ifelse(propsimulation == max, "max", NA))) %>% ungroup() %>%
  filter(propsimulation == min | propsimulation == max) %>%
  dplyr::select(c(scenario, setting, nunc, year, sex, education, alc_cat, var)) 

# get prevalence of alcohol use categories
output2A <- merge(alccat, var, all.y = T) %>% 
  rename("meanprop" = "propsimulation") %>% 
  dplyr::select(c(scenario, setting, year, sex, education, alc_cat, meanprop, var)) %>%
  pivot_wider(names_from = "var", values_from = "meanprop") %>%
  left_join(mean, .) %>% 
  # select just 2019 or 2000 in reference scenario
  filter(year == 2019 | (year == 2000 & scenario == "Reference"))

# get change in mean GPD with uncertainty
reference <- output2A %>% filter(scenario == "Reference" & year == 2019) %>% ungroup() %>%
  rename("meanpropref" = "meanprop", "maxref" = "max", "minref" = "min") %>% 
  dplyr::select(-c(scenario))

output2B <- output2A %>% filter(scenario != "Reference") %>% 
  left_join(., reference) %>% 
  mutate(diffprop = meanprop - meanpropref,
         diffmax = min - minref,
         diffmin = max - maxref) %>% 
  dplyr::select(c(scenario, setting, year, sex, education, alc_cat,
                  diffprop, diffmin, diffmax)) 

# combine elements into Table 2 and formatting
output2 <- left_join(output2A, output2B) %>% 
  mutate(mean = case_when(
          !is.na(meanprop) & year != 2000 ~ paste0(format(round(meanprop*100, digits = 2), nsmall = 2), " (", format(round(min*100, digits = 2), nsmall = 2), ", ", format(round(max*100, digits = 2), nsmall = 2), ")"),
          !is.na(meanprop) & year == 2000 ~ paste0(format(round(meanprop*100, digits = 2), nsmall = 2)),
          TRUE ~ NA), 
         diff = case_when(
           !is.na(diffprop) & year != 2000 ~ paste0(format(round(diffprop*100, digits = 2), nsmall = 2), " (", format(round(diffmax*100, digits = 2), nsmall = 2), ", ", format(round(diffmin*100, digits = 2), nsmall = 2), ")"),
           !is.na(diffprop) & year == 2000 ~ paste0(format(round(diffprop*100, digits = 2), nsmall = 2)),
           TRUE ~ NA)) %>% 
  dplyr::select(c(setting, scenario, year, sex, education, mean, diff, alc_cat)) %>% 
  pivot_wider(names_from = alc_cat, values_from = c(mean, diff)) %>% 
  dplyr::select(c(setting, scenario, year, sex, education, 
                  mean_Abstinence, diff_Abstinence,
                  `mean_Category I`, `diff_Category I`,
                  `mean_Category II`, `diff_Category II`,
                  `mean_Category III`, `diff_Category III`)) %>% 
  arrange(setting)


# Table S3) Simulated average consumption levels by alcohol user category (continuous-categorical alcohol outcome)
    
alccontcat <- data_alccontcat %>%
  filter(!is.na(alc_cat_2018) & alc_cat_2018 != "Non-drinker") %>% 
  # set labels
  mutate(year = as.numeric(as.character(year)),
         sex = ifelse(sex=="m","Men","Women"),
         education = factor(education, 
                            levels = c("LEHS", "SomeC", "College"),
                            labels = c("High school or less", "Some college", "College +")),
         scenario = factor(case_when(
           policymodel == 1 ~ "Reference",
           policymodel != 1  ~ paste0("Scenario ", policymodel-1),
           TRUE ~ NA), levels = c("Reference", 
                                  "Scenario 1", "Scenario 2", 
                                  "Scenario 3", "Scenario 4")),
         alc_cat_2018 = factor(alc_cat_2018, 
                               levels = c("Low risk", "Medium risk", "High risk"),
                               labels = c("Category I", "Category II", "Category III")),
         setting = factor(setting,
                          levels = c("standard", "min", "max"),
                          labels = c("Standard", "Minimum", "Maximum"))) %>%
  # average across random number seeds
  group_by(scenario, setting, nunc, year, sex, education, alc_cat_2018) %>% 
  summarise(meansimulation = mean(as.numeric(meansimulation)),
            sesimulation = mean(as.numeric(sesimulation)))
  
# get mean across multiple model runs 
mean <- alccontcat %>%
  group_by(scenario, setting, year, sex, education, alc_cat_2018) %>% 
  summarise(meangpd = mean(as.numeric(meansimulation)))

# get uncertainty across multiple model runs (min/max)
var <- alccontcat %>% 
  group_by(scenario, setting, year, sex, education, alc_cat_2018) %>% 
  mutate(min = min(as.numeric(meansimulation)),
         max = max(as.numeric(meansimulation)),
         var = ifelse(meansimulation == min, "min",
                      ifelse(meansimulation == max, "max", NA))) %>% ungroup() %>%
  filter(meansimulation == min | meansimulation == max) %>%
  dplyr::select(c(scenario, setting, nunc, year, sex, education, alc_cat_2018, var)) 

# get mean GPD by alcohol use categories
output3A <- merge(alccontcat, var, all.y = T) %>% 
  rename("meangpd" = "meansimulation") %>% 
  dplyr::select(c(scenario, setting, year, sex, education, alc_cat_2018, meangpd, var)) %>%
  pivot_wider(names_from = "var", values_from = "meangpd") %>%
  left_join(mean, .) %>% 
  # select just 2019 or 2018 in reference scenario
  filter(year == 2019 | (year == 2018 & scenario == "Reference"))
    
# get change in mean GPD by alcohol use categories
reference <- output3A %>% filter(scenario == "Reference") %>% ungroup() %>%
      rename("meangpdref" = "meangpd", "maxref" = "max", "minref" = "min") %>% 
      dplyr::select(-scenario)
    
output3B <- output3A %>% filter(scenario != "Reference") %>% 
      left_join(., reference) %>% 
      mutate(diffgpd = meangpd - meangpdref,
             diffmax = min - minref,
             diffmin = max - maxref,
             percgpd = (meangpd - meangpdref) / meangpdref,
             percmax = (min - minref) / minref,
             percmin = (max - maxref) / maxref) %>% 
      dplyr::select(c(scenario, year, sex, education, alc_cat_2018,
                      diffgpd, diffmin, diffmax,
                      percgpd, percmin, percmax)) 

# combine elements into Table 3 and formatting
output3 <- left_join(output3A, output3B) %>% 
  mutate(mean = case_when(
          !is.na(meangpd) ~ paste0(format(round(meangpd, digits = 2), nsmall = 2), " (", format(round(min, digits = 2), nsmall = 2), ", ", format(round(max, digits = 2), nsmall = 2), ")"),
          TRUE ~ NA),
         diff = case_when(
           !is.na(diffgpd) ~ paste0(format(round(diffgpd, digits = 2), nsmall = 2), " (", format(round(diffmax, digits = 2), nsmall = 2), ", ", format(round(diffmin, digits = 2), nsmall = 2), ")"),
           TRUE ~ NA), 
         perc = case_when(
           !is.na(percgpd) ~ paste0(format(round(percgpd*100, digits = 2), nsmall = 2), " (", format(round(percmax*100, digits = 2), nsmall = 2), ", ", format(round(percmin*100, digits = 2), nsmall = 2), ")"),
           TRUE ~ NA)) %>% 
  dplyr::select(c(setting, alc_cat_2018, scenario, year, sex, education, mean, diff, perc)) %>% 
  arrange(setting, alc_cat_2018) 

# save all output as XLSX
write.xlsx(output1, file = paste0(OutputDirectory, Sys.Date(), "/Table_MeanGPD_", Sys.Date(), ".xlsx"), rowNames = F, keepNA = T, na.string = ".")
write.xlsx(output2, file = paste0(OutputDirectory, Sys.Date(), "/Table_PrevAlcUseCat_", Sys.Date(), ".xlsx"), rowNames = F, keepNA = T, na.string = ".")
write.xlsx(output3, file = paste0(OutputDirectory, Sys.Date(), "/Table_MeanGPDbyAlcCat_", Sys.Date(), ".xlsx"), rowNames = F, keepNA = T, na.string = ".")

