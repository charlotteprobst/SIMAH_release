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
library(data.table)
library(urbnmapr)
library(ggpattern)
library(survey)
library(jtools)
#install.packages('multicore',,'http://www.rforge.net/')
#library(multicore)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230926

data <- as.data.frame(readRDS("acp_brfss/20230925_brfss_clean.RDS"))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE DATA
# ----------------------------------------------------------------

# POLICY DATA AND GROUPING -> US MAP (below)

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

# GET GROUPING FOR DESCRIPTIVE STATISTICS 

sub <- out1 %>% select(State, SunSalesPolicy)
pdat <- left_join(data, sub) %>%
  
  # select random subsample (for testing)
  # sample_frac(0.001) %>% 
  
  # calculate ALCCAT3 variables
  mutate(alccat3 = ifelse(sex_recode == "Men" & gramsperday > 0 & gramsperday <= 60, 0,
                          ifelse(sex_recode == "Men" & gramsperday > 60, 1,
                                 ifelse(sex_recode == "Women" & gramsperday > 0 & gramsperday <= 40, 0,
                                        ifelse(sex_recode == "Women" & gramsperday > 40, 1, 
                                               ifelse(gramsperday == 0, NA, NA))))))

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# SET SURVEY DESIGN
# ----------------------------------------------------------------

options(survey.lonely.psu = "adjust")
svydat <- svydesign(ids = ~X_PSU, strata = ~interaction(X_STSTR, YEAR), 
                    weights = ~final_sample_weight_adj, nest = T, data = pdat)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# SOCIODEMOGRAPHICS
# ----------------------------------------------------------------

# SAMPLE SIZE

nrow(pdat)
nrow(pdat[pdat$gramsperday > 0,])

# WEIGHTED STAT

stat <- svyby(~sex_num + age_var + LEHS + SomeC + College + 
                marital_status + White + Black + Hisp + ROth, ~SunSalesPolicy, svydat, svymean, multicore=TRUE) 

out2a <- stat %>% rename_all(~str_replace_all(.,"statistic.","")) %>%
  select(c("SunSalesPolicy", "sex_num", "age_var", "LEHS", "SomeC", "College", 
           "marital_status", "White", "Black", "Hisp", "ROth")) %>%
  pivot_longer(cols = !SunSalesPolicy, names_to = "variables", values_to = c("stat")) %>%
  pivot_wider(names_from = "SunSalesPolicy", values_from = "stat") 

out2b <- confint(stat) %>% as.data.frame %>% rownames_to_column(var = "Var") %>%
  separate(Var, c("SunSalesPolicy", "variables"), ":") %>%
  pivot_wider(names_from = "SunSalesPolicy", values_from = c("2.5 %", "97.5 %")) 

out2c <- svyby(~age_var, ~SunSalesPolicy, svydat, svysd, keep.var = F, multicore=TRUE) %>%
  pivot_wider(names_from = "SunSalesPolicy", values_from = "statistic") %>%
  rename("SE_Sunday sales were never banned" = "Sunday sales were never banned",
         "SE_Sunday sales ban was repealed" = "Sunday sales ban was repealed",
         "SE_Sunday sales were always banned" = "Sunday sales were always banned") %>%
  mutate(variables = "age_var")

out2 <- left_join(out2a, out2b) %>% left_join(., out2c) 

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# ALCOHOL USE (WEIGHTED)
# ----------------------------------------------------------------

# ANY ALCOHOL USE

stat <- svyby(~drinkingstatus, ~SunSalesPolicy + YEAR, svydat, svymean, multicore=TRUE) 

out3a <- stat %>% 
  select(c("SunSalesPolicy", "YEAR", "drinkingstatus")) %>%
  pivot_wider(names_from = "SunSalesPolicy", values_from = "drinkingstatus")

out3b <- confint(stat) %>% as.data.frame %>% rownames_to_column(var = "Var") %>%
  separate_wider_delim(Var, ".", names = c("SunSalesPolicy", "YEAR")) %>%
  pivot_wider(names_from = "SunSalesPolicy", values_from = c("2.5 %", "97.5 %")) %>%
  mutate(YEAR = as.numeric(YEAR))

out3 <- left_join(out3a, out3b)

# GRAMS PER DAY, past-month alcohol users

stat <- svyby(~gramsperday, ~SunSalesPolicy + YEAR, subset(svydat, gramsperday > 0), svymean, multicore=TRUE) 

out4a <- stat %>% 
  select(c("SunSalesPolicy", "YEAR", "gramsperday")) %>%
  pivot_wider(names_from = "SunSalesPolicy", values_from = "gramsperday") 

out4b <- svyby(~gramsperday, ~SunSalesPolicy + YEAR, subset(svydat, gramsperday > 0), svysd, keep.var = F, multicore=TRUE) %>%
  mutate(SunSalesPolicy = paste0("SD_", SunSalesPolicy)) %>%
  pivot_wider(names_from = "SunSalesPolicy", values_from = "statistic") %>%
  mutate(YEAR = as.numeric(YEAR))

out4 <- left_join(out4a, out4b)

# ANY HAZARDOUS USE, past-month alcohol users

stat <- svyby(~alccat3, ~SunSalesPolicy + YEAR, subset(svydat, gramsperday > 0), svymean, multicore=TRUE) 

out5a <- stat %>% 
  select(c("SunSalesPolicy", "YEAR", "alccat3")) %>%
  pivot_wider(names_from = "SunSalesPolicy", values_from = "alccat3") 

out5b <- confint(stat) %>% as.data.frame %>% rownames_to_column(var = "Var") %>%
  separate_wider_delim(Var, ".", names = c("SunSalesPolicy", "YEAR")) %>%
  pivot_wider(names_from = "SunSalesPolicy", values_from = c("2.5 %", "97.5 %")) %>%
  mutate(YEAR = as.numeric(YEAR))

out5 <- left_join(out5a, out5b)  

# --------------------------------------------------------------------------------------

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

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# EXPORT
# ----------------------------------------------------------------

ggsave(paste0("acp_brfss/outputs/figures/", DATE, "_map_SunSalesPolicies.jpg"), dpi = 300, width = 8, height = 5)

write.xlsx(out1, file = paste0("acp_brfss/outputs/", DATE, "_StateSummary.xlsx"), rowNames = FALSE)
write.xlsx(out2, file = paste0("acp_brfss/outputs/", DATE, "_BRFSS_SVY_SampleDesc_output.xlsx"), rowNames = FALSE)

list_out <- list("Drinkingstatus" = out3, "GPD" = out4, "AlcCat3" = out5)
write.xlsx(list_out, file = paste0("acp_brfss/outputs/", DATE, "_BRFSS_SVY_AlcDesc_output.xlsx"), rowNames = FALSE)
