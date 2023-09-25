# ----------------------------------------------------------------
# ----------------------------------------------------------------
## Project: SIMAH  
## Title: Sunday sales ban and BRFSS
## State: all US states
## Author: Carolin Kilian
## Start Date: 07/05/2023
# ----------------------------------------------------------------
# ----------------------------------------------------------------

# --------------------------------------------------------------------------------------

rm(list = ls())

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# LIBARIES
# ----------------------------------------------------------------
# ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(urbnmapr)
library(ggpattern)
library(data.table)
library(Hmisc)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# FUNCTIONS
# ----------------------------------------------------------------

wtd.lci <- function(x, w) {
  n    <- length(x)
  mean <- wtd.mean(x, w)
  se   <- wtd.var(x, w)/sqrt(length(x))
  lci   <- mean + (qt(0.025,n-1)*se)
  return(lci)
}

wtd.uci <- function(x, w) {
  n    <- length(x)
  mean <- wtd.mean(x, w)
  se   <- wtd.var(x, w)/sqrt(length(x))
  uci   <- mean - (qt(0.025,n-1)*se)
  return(uci)
}

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230922

# prepared/cleaned data
data <- data.table(readRDS("acp_brfss/20230922_brfss_clean.RDS"))

# ----------------------------------------------------------------
# DESCRIPTIVES BRFSS
# ----------------------------------------------------------------

# ALCOHOL POLICY -> SHOW IN A MAP

out1 <- data %>% 
  group_by(State, YEAR, controlstate, drinkculture, sunsalesban, sunsalesban_di, DatePolicy) %>% summarise() %>%
  mutate(ControlState = ifelse(controlstate == 1, "yes", "no"),
         DrinkCulture = drinkculture) %>% group_by(State) %>% 
  mutate(policy.total = sum(sunsalesban_di),
         SunSalesPolicy = ifelse(policy.total == 0, "Sunday sales were never banned",
                                 ifelse(policy.total == 19, "Sunday sales were always banned", 
                                        ifelse(policy.total > 0 & policy.total < 19, "Sunday sales ban was repealed", NA)))) %>%
  filter(YEAR == 2018) %>% # use 2018 data as 2019 data for New Jersey is missing (https://www.cdc.gov/brfss/annual_data/2019/pdf/overview-2019-508.pdf)
  mutate(DatePolicy = ifelse(State %like% "Georgia|Kansas|Kentucky", NA, DatePolicy), # minor policy changes not impacting their overall Sunday sales ban (allowing local options/beer sales)
         SunSalesPolicy = factor(SunSalesPolicy, levels = c("Sunday sales were never banned", 
                                                            "Sunday sales ban was repealed",
                                                            "Sunday sales were always banned"))) %>% 
  select(c("State", "ControlState", "DrinkCulture", "SunSalesPolicy", "DatePolicy"))

# SOCIODEMOGRAPHICS WEIGHTED

out2 <- data %>% group_by(SunSalesPolicy) %>%
  
  # calculate statistics 
  summarise(n = n(),
            sex.prev = wtd.mean(sex_num, final_sample_weight),
            sex.lci = wtd.lci(sex_num, final_sample_weight),
            sex.uci = wtd.uci(sex_num, final_sample_weight),
            age.mean = wtd.mean(age_var),
            age.sd = sqrt(wtd.var(age_var, final_sample_weight)),
            LEHS.prev = wtd.mean(LEHS, final_sample_weight),
            LEHS.lci = wtd.lci(LEHS, final_sample_weight),
            LEHS.uci = wtd.uci(LEHS, final_sample_weight),
            SomeC.prev = wtd.mean(SomeC, final_sample_weight),
            SomeC.lci = wtd.lci(SomeC, final_sample_weight),
            SomeC.uci = wtd.uci(SomeC, final_sample_weight),
            College.prev = wtd.mean(College, final_sample_weight),
            College.lci = wtd.lci(College, final_sample_weight),
            College.uci = wtd.uci(College, final_sample_weight),
            ms.prev = wtd.mean(marital_status, final_sample_weight),
            ms.lci = wtd.lci(marital_status, final_sample_weight),
            ms.uci = wtd.uci(marital_status, final_sample_weight),
            White.prev = wtd.mean(White, final_sample_weight),
            White.lci = wtd.lci(White, final_sample_weight),
            White.uci = wtd.uci(White, final_sample_weight),
            Black.prev = wtd.mean(Black, final_sample_weight),
            Black.lci = wtd.lci(Black, final_sample_weight),
            Black.uci = wtd.uci(Black, final_sample_weight),
            Hisp.prev = wtd.mean(Hisp, final_sample_weight),
            Hisp.lci = wtd.lci(Hisp, final_sample_weight),
            Hisp.uci = wtd.uci(Hisp, final_sample_weight),
            ROth.prev = wtd.mean(ROth, final_sample_weight),
            ROth.lci = wtd.lci(ROth, final_sample_weight),
            ROth.uci = wtd.uci(ROth, final_sample_weight)) %>%
  
  # summary format for table
  mutate(N = as.character(n),
         Sex = paste0(round(sex.prev*100, 1), " (", round(sex.lci*100, 1), "-", round(sex.uci*100, 1), ")"),
         Age = paste0(round(age.mean, 2), " (", round(age.sd, 2), ")"),
         Edu_LEHS = paste0(round(LEHS.prev*100, 1), " (", round(LEHS.lci*100, 1), "-", round(LEHS.uci*100, 1), ")"),
         Edu_SomeC = paste0(round(SomeC.prev*100, 1), " (", round(SomeC.lci*100, 1), "-", round(SomeC.uci*100, 1), ")"),
         Edu_College = paste0(round(College.prev*100, 1), " (", round(College.lci*100, 1), "-", round(College.uci*100, 1), ")"),
         MaritalStatus = paste0(round(ms.prev*100, 1), " (", round(ms.lci*100, 1), "-", round(ms.uci*100, 1), ")"),
         RacEth_White = paste0(round(White.prev*100, 1), " (", round(White.lci*100, 1), "-", round(White.uci*100, 1), ")"),
         RacEth_Black = paste0(round(Black.prev*100, 1), " (", round(Black.lci*100, 1), "-", round(Black.uci*100, 1), ")"),
         RacEth_Hisp = paste0(round(Hisp.prev*100, 1), " (", round(Hisp.lci*100, 1), "-", round(Hisp.uci*100, 1), ")"),
         RacEth_Other = paste0(round(ROth.prev*100, 1), " (", round(ROth.lci*100, 1), "-", round(ROth.uci*100, 1), ")")) %>%
  select(c("SunSalesPolicy", "N", "Sex", "Age", "Edu_LEHS", "Edu_SomeC", "Edu_College", 
           "MaritalStatus", "RacEth_White", "RacEth_Black", "RacEth_Hisp", "RacEth_Other")) %>%
  
  # rearrange to table format
  pivot_longer(cols = !SunSalesPolicy, names_to = "variables", values_to = "stat") %>%
  pivot_wider(names_from = "SunSalesPolicy", values_from = "stat") 

# ALCOHOL USE BY SALE POLICY AND OVER TIME

# alcohol use prevalence full sample  
out3a <- data %>% group_by(YEAR, SunSalesPolicy) %>%
  summarise(alc.prev = wtd.mean(drinkingstatus, final_sample_weight),
            alc.lbi = wtd.lci(drinkingstatus, final_sample_weight),
            alc.ubi = wtd.uci(drinkingstatus, final_sample_weight))

# alcohol use past-year alcohol users
out3b <- data %>% filter(gramsperday > 0) %>%
  mutate(alccat3 = ifelse(sex_recode == "Men" & gramsperday <= 60, 0,
                          ifelse(sex_recode == "Men" & gramsperday > 60, 1,
                                 ifelse(sex_recode == "Women" & gramsperday <= 40, 0,
                                        ifelse(sex_recode == "Women" & gramsperday > 40, 1, NA))))) %>%
  group_by(YEAR, SunSalesPolicy) %>%
  summarise(gpd.mean = wtd.mean(gramsperday, final_sample_weight),
            gpd.sd = sqrt(wtd.var(gramsperday, final_sample_weight)),
            alccat3.prev = wtd.mean(alccat3, final_sample_weight),
            alccat3.lbi = wtd.lci(alccat3, final_sample_weight),
            alccat3.ubi = wtd.uci(alccat3, final_sample_weight))

out3 <- left_join(out3a, out3b) %>%
  
  # summary format for table
  mutate(AlcPrev = paste0(round(alc.prev*100, 1), " (", round(alc.lbi*100, 1), "-", round(alc.ubi*100, 1), ")"),
         GPD = paste0(round(gpd.mean, 2), " (", round(gpd.sd, 2), ")"),
         AlcCat3 = paste0(round(alccat3.prev*100, 1), " (", round(alccat3.lbi*100, 1), "-", round(alccat3.ubi*100, 1), ")")) %>%
  select(c("SunSalesPolicy", "AlcPrev", "GPD", "AlcCat3")) %>%
  
  # rearrange to table format
  pivot_longer(cols = !c("SunSalesPolicy", "YEAR"), names_to = "variables", values_to = "stat") %>%
  pivot_wider(names_from = "variables", values_from = "stat") %>%
  pivot_wider(names_from = "SunSalesPolicy", values_from = c("AlcPrev", "GPD", "AlcCat3"))
  
# ----------------------------------------------------------------
# POLICY CHANGE MAP
# ----------------------------------------------------------------

col.fill <- c("#1D9A6C", "#FFE154")
state.fill <- c("#2E2E2E", "#2E2E2E", "#1D9A6C")

usa <- urbnmapr::states
usa <- left_join(usa, out1, by = c('state_name' = 'State')) %>% filter(!is.na(SunSalesPolicy)) %>%
  mutate(Minnesota = as.factor(ifelse(state_name %like% "Minnesota", 1, 0)), 
         PolicyChange = as.factor(ifelse(SunSalesPolicy == "Sunday sales ban was repealed", 1, 0)))

ggplot() + 
  geom_polygon_pattern(data = usa, color = "white", size = 0.1,
                       aes(x = long, y = lat, group = group, pattern = PolicyChange, 
                           pattern_colour = Minnesota, fill = SunSalesPolicy), 
                       pattern_density = 0.05, pattern_spacing = 0.01,
                       pattern_key_scale_factor = 0.6) + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_pattern_colour_manual(values = col.fill) + 
  scale_pattern_discrete(choices = c("none", "stripe")) +
  scale_fill_manual(values = state.fill) +
  ggthemes::theme_map() + theme(legend.position="none", 
                                panel.background = element_rect(fill = "white", color = "white"))

# ----------------------------------------------------------------
# EXPORT
# ----------------------------------------------------------------

ggsave(paste0("acp_brfss/outputs/figures/", DATE, "_map_SunSalesPolicies.jpg"), dpi = 300, width = 8, height = 5)

write.xlsx(out.missings, file = paste0("acp_brfss/outputs/", DATE, "_BRFSS_Missings.xlsx"), rowNames = FALSE)
write.xlsx(out1, file = paste0("acp_brfss/outputs/", DATE, "_StateSummary.xlsx"), rowNames = FALSE)
write.xlsx(out2, file = paste0("acp_brfss/outputs/", DATE, "_BRFSS_SampleDesc_output.xlsx"), rowNames = FALSE)
write.xlsx(out3, file = paste0("acp_brfss/outputs/", DATE, "_BRFSS_AlcDesc_output.xlsx"), rowNames = FALSE)
