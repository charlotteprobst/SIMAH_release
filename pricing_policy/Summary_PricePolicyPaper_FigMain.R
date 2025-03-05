#' Summarise mean alcohol consumption for price policy paper (Kilian et al.)
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

WorkingDirectory <- "/Users/julialemp/Desktop/SIMAH_workplace/microsim/2_output_data/"
OutputDirectory <- "/Users/julialemp/Desktop/SIMAH_workplace/pricing_policy/"

# load data
data_alccont <- read.csv(paste0(WorkingDirectory, "2025-02-27/output-policy_alcoholcont_2025-03-02.csv"))
data_alccat <- read.csv(paste0(WorkingDirectory, "2025-02-27/output-policy_alcoholcat_2025-03-02.csv"))
data_alccontcat <- read.csv(paste0(WorkingDirectory, "2025-02-27/output-policy_alcoholcontcat_2025-03-02.csv"))

# set ggplot layout
options(digits = 8)
  
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
# MAIN RESULTS
# --------------------------------------------
  
# Figure 1) Main alcohol consumption levels (continuous alcohol outcome)
  
alccont <- data_alccont %>%
  filter(setting == "standard") %>% 
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
                                  "Scenario 3", "Scenario 4"))) %>%
  # average across random number seeds
  group_by(scenario, nunc, year, sex, education) %>% 
  summarise(meansimulation = mean(as.numeric(meansimulation)),
            sesimulation = mean(as.numeric(sesimulation)))
  
# get mean across multiple model runs 
mean <- alccont %>%
  group_by(scenario, year, sex, education) %>% 
  summarise(meangpd = mean(as.numeric(meansimulation)),
            segpd = mean(as.numeric(sesimulation)))
    
# get uncertainty across multiple model runs (min/max)
var <- alccont %>% filter(year != "2000") %>%
  group_by(scenario, year, sex, education) %>% 
  mutate(min = min(as.numeric(meansimulation)),
         max = max(as.numeric(meansimulation)),
         var = ifelse(meansimulation == min, "min",
                      ifelse(meansimulation == max, "max", NA))) %>% ungroup() %>%
  filter(meansimulation == min | meansimulation == max) %>%
  dplyr::select(c(scenario, nunc, year, sex, education, var)) 
    
output1 <- merge(alccont, var, all.y = T) %>% 
  rename("meangpd" = "meansimulation", "segpd" = "sesimulation") %>% 
  dplyr::select(c(scenario, year, sex, education, meangpd, segpd, var)) %>%
  pivot_wider(names_from = "var", values_from = c("meangpd", "segpd")) %>%
  left_join(mean, .)
    
ggplot() + 
  geom_vline(xintercept = 2019, linetype = "dashed", linewidth = 0.5, colour = "grey") + 
  geom_line(data = output1[output1$scenario == "Reference" & output1$year != 2019,], 
            aes(y = meangpd_min, x = year, colour = scenario), linewidth = 0.5, alpha = 0.5) + 
  geom_line(data = output1[output1$scenario == "Reference" & output1$year != 2019,], 
            aes(y = meangpd_max, x = year, colour = scenario), linewidth = 0.5, alpha = 0.5) + 
  geom_line(data = output1[output1$scenario == "Reference" & output1$year != 2019,], 
            aes(y = meangpd, x = year, colour = scenario), linewidth = 0.5) +
  geom_point(data = output1[output1$year == 2019,], 
             aes(y = meangpd, x = year, colour = scenario, fill = scenario), shape = 23, size = 2) +
  geom_errorbar(data = output1[output1$year == 2019,], 
                aes(ymin = meangpd_min, ymax = meangpd_max, x = year, colour = scenario), width = 0.2) + 
  facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
  #ylim(0,NA) + 
  ylab("Mean grams of pure alcohol per day") +
  scale_x_continuous(breaks = seq(2000, 2019, 3), limits = c(2000, 2019+0.1)) + 
  scale_color_manual(values = c5, name = "") + scale_fill_manual(values = c5, name = "") + 
  ggtheme + xlab("") 

ggsave(paste0(OutputDirectory, Sys.Date(), "/Fig1_MeanGPD_V2_", Sys.Date(), ".png"), height = 7, width = 14, dpi = 300)

# Figure 2) Change in the prevalence of non-drinkers (categorial alcohol outcome)

# generate output for categorical alcohol use

alccat <- data_alccat %>%
  filter(setting == "standard") %>% 
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
                          labels = c("Category III", "Category II", "Category I", "Abstinence"))) %>%
  # average across random number seeds
  group_by(scenario, nunc, year, sex, education, alc_cat) %>% 
  summarise(propsimulation = mean(as.numeric(propsimulation)))

# get mean across multiple model runs 
mean <- alccat %>%
  group_by(scenario, year, sex, education, alc_cat) %>% 
  summarise(meanprop = mean(as.numeric(propsimulation)))
    
# get uncertainty across multiple model runs (min/max)
var <- alccat %>% filter(year != "2000") %>%
  group_by(scenario, year, sex, education, alc_cat) %>% 
  mutate(min = min(as.numeric(propsimulation)),
         max = max(as.numeric(propsimulation)),
         var = ifelse(propsimulation == min, "min",
                      ifelse(propsimulation == max, "max", NA))) %>% ungroup() %>%
  filter(propsimulation == min | propsimulation == max) %>%
  dplyr::select(c(scenario, nunc, year, sex, education, alc_cat, var)) 

output2 <- merge(alccat, var, all.y = T) %>% 
  rename("meanprop" = "propsimulation") %>% 
  dplyr::select(c(scenario, year, sex, education, alc_cat, meanprop, var)) %>%
  pivot_wider(names_from = "var", values_from = "meanprop") %>%
  left_join(mean, .) %>% 
  # select just non-drinkers and year of policy
  filter(year == 2019 & alc_cat == "Abstinence") 

# re-arrange to show value of reference plus scenarios
reference <- output2 %>% filter(scenario == "Reference") %>% 
  mutate(condition = "reference") %>% 
  group_by(year, sex, education, alc_cat, meanprop, min, max, condition) %>% 
  tidyr::expand(scenario = 1:4) %>% 
  mutate(scenario = paste0("Scenario ", scenario))

output2 <- output2 %>% filter(scenario != "Reference") %>% 
  mutate(condition = "model") %>% 
  rbind(., reference) %>% 
  mutate(condition = factor(condition, levels = c("model", "reference"))) 
      
ggplot() + 
  geom_bar(data = output2[output2$condition == "model",], 
           aes(y = meanprop, x = scenario, color = scenario, fill = scenario), 
           stat = "identity", linetype = "dashed", alpha = 0.5) +
  geom_bar(data = output2[output2$condition == "reference",], 
           aes(y = meanprop, x = scenario), 
           stat = "identity", color = "black", fill = "black", alpha = 0.5) +
  #geom_errorbar(data = output2[output2$condition == "model",], 
  #              aes(ymin = min, ymax = max, x = scenario, colour = scenario), width = 0.2) +
  facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
  scale_y_continuous(labels = scales::percent, limits = c(0,NA)) + ylab("Prevalence of abstinence (%)") +
  scale_color_manual(values = c4, name = "") + scale_fill_manual(values = c4, name = "") + 
  scale_linetype_manual(values = c("dashed", "solid"), name = "") +
  ggtheme + xlab("") + theme(legend.position = "none")

ggsave(paste0(OutputDirectory, Sys.Date(), "/Fig2_PrevNonDrinkers_", Sys.Date(), ".png"), height = 7, width = 14, dpi = 300)

# Figure 3) Change in mean consumption by alcohol use category (continuous-categorical alcohol outcome)
    
alccontcat <- data_alccontcat %>%
  filter(!is.na(alc_cat_2018) & alc_cat_2018 != "Non-drinker") %>% 
  filter(setting == "standard") %>% 
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
                               labels = c("Category I", "Category II", "Category III"))) %>%
  # average across random number seeds
  group_by(scenario, nunc, year, sex, education, alc_cat_2018) %>% 
  summarise(meansimulation = mean(as.numeric(meansimulation)),
            sesimulation = mean(as.numeric(sesimulation)))
  
# get mean across multiple model runs 
mean <- alccontcat %>%
  group_by(scenario, year, sex, education, alc_cat_2018) %>% 
  summarise(meangpd = mean(as.numeric(meansimulation)),
            segpd = mean(as.numeric(sesimulation)))

# get uncertainty across multiple model runs (min/max)
var <- alccontcat %>% 
  group_by(scenario, year, sex, education, alc_cat_2018) %>% 
  mutate(min = min(as.numeric(meansimulation)),
         max = max(as.numeric(meansimulation)),
         var = ifelse(meansimulation == min, "min",
                      ifelse(meansimulation == max, "max", NA))) %>% ungroup() %>%
  filter(meansimulation == min | meansimulation == max) %>%
  dplyr::select(c(scenario, nunc, year, sex, education, alc_cat_2018, var)) 

output3 <- merge(alccontcat, var, all.y = T) %>% 
  rename("meangpd" = "meansimulation", "segpd" = "sesimulation") %>% 
  dplyr::select(c(scenario, year, sex, education, alc_cat_2018, meangpd, segpd, var)) %>% 
  # output3 %>% janitor::get_dupes()
  # issue: for men, college, cat III, two nuncs happen to result in the same (min & max) values in 2018 and 2019: nunc 48 and nunc 52
  # made sure this does not affect results; resolved by using distinct() 
  distinct() %>% 
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
  geom_point(aes(y = diffgpd, x = scenario, color = alc_cat_2018, fill = alc_cat_2018), shape = 23, size = 2) +
  geom_errorbar(aes(ymin = diffmin, ymax = diffmax, x = scenario, colour = alc_cat_2018), width = 0.1) +
  facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
  ylim(NA,0) + ylab("Change in mean grams per day (reference: no policy)") +
  scale_color_manual(values = cg3, name = "Alcohol use") + scale_fill_manual(values = cg3, name = "Alcohol use") + 
  ggtheme + xlab("") 

ggsave(paste0(OutputDirectory, Sys.Date(), "/Fig3_GPDbyAlcCat_", Sys.Date(), ".png"), height = 8, width = 16, dpi = 300)


# Edit Julia Feb 7, 2025: get mean of change with uncertainty
diff_2019 <- alccontcat %>% filter(year == "2019") %>%
  dplyr::select(-sesimulation) %>%
  tidyr::pivot_wider(names_from = "scenario", values_from = "meansimulation") %>%
  mutate(across(starts_with("Scenario"), ~ . - Reference, .names = "{.col}_diff")) %>% # compute the difference between the reference scenario and each scenario (1-4) in 2019
  mutate(across(ends_with("_diff"), ~ (. / Reference), .names = "{.col}_percent"))

# summarise across nunc (mean, min, max)
output3C <- diff_2019 %>%
  group_by(year, sex, education, alc_cat_2018) %>%
  summarise(across(ends_with("_diff"), ~ mean(.x, na.rm = TRUE), .names = "{.col}mean_new"),
            across(ends_with("_diff"), ~ min(.x, na.rm = TRUE), .names = "{.col}min_new"),
            across(ends_with("_diff"), ~ max(.x, na.rm = TRUE), .names = "{.col}max_new"),
            across(ends_with("_percent"), ~ mean(.x, na.rm = TRUE), .names = "{.col}mean_new"),
            across(ends_with("_percent"), ~ min(.x, na.rm = TRUE), .names = "{.col}min_new"),
            across(ends_with("_percent"), ~ max(.x, na.rm = TRUE), .names = "{.col}max_new")) %>%
  ungroup()

output3C = output3C %>%
  pivot_longer(
    cols = starts_with("Scenario"), 
    names_to = c("Scenario", ".value"), 
    names_pattern = "Scenario (\\d+)_diff(.*)") %>%
  mutate(Scenario = paste0("Scenario ", Scenario))

output3C = output3C %>%
  rename_with(~ stringr::str_replace(.x, "(mean|min|max)_new", "diff\\1_new")) %>%
  rename_with(~ stringr::str_replace(.x, "_percentdiff(mean|min|max)_new", "perc\\1_new")) %>%
  rename(diffgpd_new = diffmean_new,
         percgpd_new = percmean_new,
         scenario = Scenario)

ggplot(data = output3C) + 
  geom_point(aes(y = diffgpd_new, x = scenario, color = alc_cat_2018, fill = alc_cat_2018), shape = 23, size = 2) +
  geom_errorbar(aes(ymin = diffmin_new, ymax = diffmax_new, x = scenario, colour = alc_cat_2018), width = 0.1) +
  facet_grid(cols = vars(education), rows = vars(sex), scales = "free") + 
  ylim(NA,0) + ylab("Change in mean grams per day (reference: no policy)") +
  scale_color_manual(values = cg3, name = "Alcohol use") + scale_fill_manual(values = cg3, name = "Alcohol use") + 
  ggtheme + xlab("") 

ggsave(paste0(OutputDirectory, Sys.Date(), "/Fig3_GPDbyAlcCat_", Sys.Date(), "_new.png"), height = 8, width = 16, dpi = 300)
