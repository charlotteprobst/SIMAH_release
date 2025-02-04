#' Summarise mean alcohol consumption for price policy paper, sensitivity analysis (Kilian et al.)
#'
#' @param
#' @keywords alcohol postprocessing
#' @export
#' @examples

# libraries
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)  
library(ggpubr)
library(ggh4x)

WorkingDirectory <- "/Users/carolinkilian/Desktop/SIMAH_workplace/microsim/2_output_data/"
OutputDirectory <- "/Users/carolinkilian/Desktop/SIMAH_workplace/pricing_policy/"
 
# load data
data_alccont <- read.csv(paste0(WorkingDirectory, "2025-01-29/output-policy_alcoholcont_2025-01-31.csv"))
data_alccat <- read.csv(paste0(WorkingDirectory, "2025-01-29/output-policy_alcoholcat_2025-01-31.csv"))
data_alccontcat <- read.csv(paste0(WorkingDirectory, "2025-01-29/output-policy_alcoholcontcat_2025-01-31.csv"))
  
# set ggplot layout
options(digits = 4)
  
col_green <- colorRampPalette(c("#74C67A", "#137751"))
col_blue <- colorRampPalette(c("#82A9B2", "#2D6D7A"))
col_red <- colorRampPalette(c("#FAAA74", "#9F4238"))
cb2 <- col_blue(2)
cr2 <- col_red(2)
c4 <- c(cb2, cr2)
c5 <- c("black", c4)
cg3 <- col_green(3)

ggtheme <- theme_bw() + theme(legend.position = "right",
                              strip.background = element_rect(fill = "white"),
                              panel.spacing = unit(1, "lines"),
                              text = element_text(size = 16, color = "black"),
                              strip.text = element_text(size = 16, color = "black"),
                              axis.text = element_text(size = 14, color = "black"))

# --------------------------------------------
# SENSITIVITY ANALYSIS
# --------------------------------------------
  
# Figure S1) Main alcohol consumption levels by different policy settings (continuous alcohol outcome)
  
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
                          labels = c("Standard", "SA1", "SA2"))) %>%
  # average across random number seeds
  group_by(scenario, setting, nunc, year, sex, education) %>% 
  summarise(meansimulation = mean(as.numeric(meansimulation)),
            sesimulation = mean(as.numeric(sesimulation)))
  
# get mean across multiple model runs 
mean <- alccont %>%
  group_by(scenario, setting, year, sex, education) %>% 
  summarise(meangpd = mean(as.numeric(meansimulation)),
            segpd = mean(as.numeric(sesimulation)))
    
# get uncertainty across multiple model runs (min/max)
var <- alccont %>% filter(year != "2000") %>%
  group_by(scenario, setting, year, sex, education) %>% 
  mutate(min = min(as.numeric(meansimulation)),
         max = max(as.numeric(meansimulation)),
         var = ifelse(meansimulation == min, "min",
                      ifelse(meansimulation == max, "max", NA))) %>% ungroup() %>%
  filter(meansimulation == min | meansimulation == max) %>%
  dplyr::select(c(scenario, setting, nunc, year, sex, education, var)) 
    
output1 <- merge(alccont, var, all.y = T) %>% 
  rename("meangpd" = "meansimulation", "segpd" = "sesimulation") %>% 
  dplyr::select(c(scenario, setting, year, sex, education, meangpd, segpd, var)) %>%
  pivot_wider(names_from = "var", values_from = c("meangpd", "segpd")) %>%
  left_join(mean, .) %>% 
  filter(year == 2019)
 
ggplot(data = output1) + 
  geom_point(aes(y = meangpd, x = setting, color = scenario, fill = scenario), shape = 23, size = 2) +
  geom_errorbar(aes(ymin = meangpd_min, ymax = meangpd_max, x = setting, colour = scenario), width = 0.1) +
  facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
  #ylim(0,NA) + 
  ylab("Mean grams of pure alcohol per day") +
  scale_color_manual(values = c5, name = "") + scale_fill_manual(values = c5, name = "") + 
  ggtheme + xlab("") 

ggsave(paste0(OutputDirectory, Sys.Date(), "/Supp_Fig1_MeanGPD_", Sys.Date(), ".png"), height = 7, width = 12, dpi = 300)

# Figure 2) Change in the prevalence of non-drinkers by different policy settings (categorial alcohol outcome)
# Might not be relevant for the Supplement as there are no additional non-drinkers in MIN
# and Standard + Max

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
                          levels = c("High risk", "Medium risk", "Low risk", "Non-drinker"),
                          labels = c("Category III", "Category II", "Category I", "Abstinence")),
         setting = factor(setting,
                          levels = c("standard", "min", "max"),
                          labels = c("Standard", "SA1", "SA2"))) %>%
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

output2 <- merge(alccat, var, all.y = T) %>% 
  rename("meanprop" = "propsimulation") %>% 
  dplyr::select(c(scenario, setting, year, sex, education, alc_cat, meanprop, var)) %>%
  pivot_wider(names_from = "var", values_from = "meanprop") %>%
  left_join(mean, .) %>% 
  # select just non-drinkers and year of policy
  filter(year == 2019 & alc_cat == "Abstinence") 

# re-arrange to show value of reference plus scenarios
reference <- output2 %>% filter(scenario == "Reference") %>% 
  mutate(condition = "reference") %>% 
  group_by(setting, year, sex, education, alc_cat, meanprop, min, max, condition) %>% 
  tidyr::expand(scenario = 1:4) %>% 
  mutate(scenario = paste0("Scenario ", scenario))

output2 <- output2 %>% filter(scenario != "Reference") %>% 
  mutate(condition = "model") %>% 
  rbind(., reference) %>% ungroup() %>%
  mutate(condition = factor(condition, levels = c("model", "reference")))
      
ggplot() + 
  geom_bar(data = output2[output2$condition == "model",], 
           aes(y = meanprop, x = setting, color = scenario, fill = scenario), 
           stat = "identity", linetype = "dashed", alpha = 0.5) +
  geom_bar(data = output2[output2$condition == "reference",], 
           aes(y = meanprop, x = setting), 
           stat = "identity", color = "black", fill = "black", alpha = 0.5) +
  #geom_errorbar(data = output2[output2$condition == "model",], 
  #              aes(ymin = min, ymax = max, x = setting, colour = scenario), width = 0.2) +
  facet_nested(sex + education ~ scenario, scales = "free") + 
  scale_x_discrete(guide = guide_axis_nested(delim = "#")) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,NA)) + ylab("Prevalence of abstinence (%)") +
  scale_color_manual(values = c4, name = "") + scale_fill_manual(values = c4, name = "") + 
  scale_linetype_manual(values = c("dashed", "solid"), name = "") +
  ggtheme + xlab("") + theme(legend.position = "none")

ggsave(paste0(OutputDirectory, Sys.Date(), "/Supp_Fig2_PrevNonDrinkers_", Sys.Date(), ".png"), height = 17, width = 14, dpi = 300)

# Figure 3) Change in mean consumption by alcohol use category by different policy settings (continuous-categorical alcohol outcome)
    
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
                          labels = c("Standard", "SA1", "SA2"))) %>%
  # average across random number seeds
  group_by(scenario, setting, nunc, year, sex, education, alc_cat_2018) %>% 
  summarise(meansimulation = mean(as.numeric(meansimulation)),
            sesimulation = mean(as.numeric(sesimulation)))
  
# get mean across multiple model runs 
mean <- alccontcat %>%
  group_by(scenario, setting, year, sex, education, alc_cat_2018) %>% 
  summarise(meangpd = mean(as.numeric(meansimulation)),
            segpd = mean(as.numeric(sesimulation)))

# get uncertainty across multiple model runs (min/max)
var <- alccontcat %>% 
  group_by(scenario, setting, year, sex, education, alc_cat_2018) %>% 
  mutate(min = min(as.numeric(meansimulation)),
         max = max(as.numeric(meansimulation)),
         var = ifelse(meansimulation == min, "min",
                      ifelse(meansimulation == max, "max", NA))) %>% ungroup() %>%
  filter(meansimulation == min | meansimulation == max) %>%
  dplyr::select(c(scenario, setting, nunc, year, sex, education, alc_cat_2018, var)) 

output3 <- merge(alccontcat, var, all.y = T) %>% 
  rename("meangpd" = "meansimulation", "segpd" = "sesimulation") %>% 
  dplyr::select(c(scenario, setting, year, sex, education, alc_cat_2018, meangpd, segpd, var)) %>%
  pivot_wider(names_from = "var", values_from = c("meangpd", "segpd")) %>%
  left_join(mean, .) %>% 
  # use just 2019
  filter(year == 2019)
    
reference <- output3 %>% filter(scenario == "Reference") %>% ungroup() %>%
      rename("meangpdref" = "meangpd", "maxref" = "meangpd_max", "minref" = "meangpd_min") %>% 
      dplyr::select(-c(scenario, segpd, segpd_max, segpd_min))
    
output3 <- output3 %>% filter(scenario != "Reference") %>% 
      left_join(., reference) %>% 
      mutate(diffgpd = meangpd - meangpdref,
             diffmax = meangpd_min - minref,
             diffmin = meangpd_max - maxref,
             percgpd = (meangpd - meangpdref) / meangpdref,
             percmax = (meangpd_min - minref) / minref,
             percmin = (meangpd_max - maxref) / maxref) %>% 
      dplyr::select(c(scenario, year, sex, education, alc_cat_2018,
                      diffgpd, diffmin, diffmax,
                      percgpd, percmin, percmax)) 
    
ggplot(data = output3) + 
  geom_point(aes(y = diffgpd, x = setting, color = alc_cat_2018, fill = alc_cat_2018), shape = 23, size = 2) +
  geom_errorbar(aes(ymin = diffmin, ymax = diffmax, x = setting, colour = alc_cat_2018), width = 0.1) +
  facet_nested(sex + education ~ scenario, scales = "free") + 
  ylim(NA,0) + ylab("Change in mean grams per day (reference: no policy)") +
  scale_color_manual(values = cg3, name = "Alcohol use") + scale_fill_manual(values = cg3, name = "Alcohol use") + 
  ggtheme + xlab("") 

ggsave(paste0(OutputDirectory, Sys.Date(), "/Supp_Fig3_GPDbyAlcCat_", Sys.Date(), ".png"), height = 17, width = 14, dpi = 300)

