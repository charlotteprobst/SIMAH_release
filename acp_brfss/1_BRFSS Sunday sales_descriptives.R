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

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# LOAD DATA
# ----------------------------------------------------------------

setwd("/Users/carolinkilian/Desktop/SIMAH_workplace/")
DATE <- 20230808

# BRFSS 
datBRFSS <- data.table(readRDS("brfss/processed_data/BRFSS_upshifted_2000_2020_final.RDS"))

# POLICIES
datAP <- read_csv("acp_brfss/data/20230808_ALCPOLICY_2019.csv")

# UNEMPLOYMENT RATE
datUNEMP <- read.xlsx("acp_brfss/data/20230706_state_unemployment.xlsx", sheet = 1, startRow = 7) %>%
  select(c("X2", "X3", "rate")) %>% rename("State" = "X2", "YEAR" = "X3", "unemp.rate" = "rate") %>%
  mutate(YEAR = as.numeric(YEAR))

# MERGE DATA
data <- merge(datBRFSS, datAP, by.x = c("State", "YEAR"), by.y = c("state", "year"), all.x = T)
data <- merge(data, datUNEMP, by = c("State", "YEAR"), all.x = T, all.y = F)

# --------------------------------------------------------------------------------------

# ----------------------------------------------------------------
# PREPARE DATA
# ----------------------------------------------------------------

# define sunday sales bans and filter for states
data <- 
  data %>% mutate(sunsalesban_di = ifelse(sunsalesban >= 0.5, 1, ifelse(sunsalesban < 0.5, 0, NA)),
                  sunsalesban = factor(ifelse(sunsalesban == 0, "no ban", ifelse(sunsalesban == 0.5, "partial ban", 
                                       ifelse(sunsalesban == 1, "full ban", NA))), levels = c("no ban", "partial ban", "full ban"))) %>%
  filter(State != "USA", State != "DC") %>%
  filter(YEAR < 2020)

ggplot(data, aes(sunsalesban)) + geom_histogram(stat = "count")
ggplot(data, aes(sunsalesban_di)) + geom_histogram(stat = "count")

# --------------------------------------------------------------------------------------

# select random subsample (for now)
# pdat <- data %>% sample_frac(0.05)

# define age groups and factor variables
pdat <- data %>% 
  mutate(sex_num = ifelse(sex_recode == "Male", 0, ifelse(sex_recode == "Female", 1, NA)),
         age_gr = as.factor(ifelse(age_var < 35, "18-34", ifelse(age_var >= 35 & age_var < 50, "35-49", 
                                   ifelse(age_var >= 50 & age_var < 65, "50-64", ifelse(age_var >= 65, "65+", NA))))),
         education_summary = factor(education_summary, levels = c("College", "SomeC", "LEHS")),
         White = ifelse(race_eth %like% "White", 1, 0), 
         Black = ifelse(race_eth %like% "Black", 1, 0), 
         Hisp = ifelse(race_eth %like% "Hisp", 1, 0),
         ROth = ifelse(race_eth %like% "Other", 1, 0),
         LEHS = ifelse(education_summary %like% "LEHS", 1, 0),
         SomeC = ifelse(education_summary %like% "SomeC", 1, 0),
         College = ifelse(education_summary %like% "College", 1, 0))
  
# ----------------------------------------------------------------
# DESCRIPTIVES BRFSS
# ----------------------------------------------------------------

# ALCOHOL POLICY -> SHOW IN A MAP

sub <- pdat %>% 
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
                                                            "Sunday sales were always banned")))

# include grouping variable SunSalesPolicy into pdat 
sub <- sub %>% select(c("State", "SunSalesPolicy"))
pdat <- left_join(pdat, sub, by = c("State" = "State")) 

# select output for summary table
out1 <- sub %>% select(c("State", "ControlState", "DrinkCulture", "SunSalesPolicy", "DatePolicy"))

# SOCIODEMOGRAPHICS !! UNWEIGHTED

out2 <- pdat %>% group_by(SunSalesPolicy) %>%
  
  # calculate statistics 
  summarise(n = n(),
            sex.prev = mean(sex_num),
            sex.lci = mean(sex_num) - (1.96*(sd(sex_num)/sqrt(length(sex_num)))),
            sex.uci = mean(sex_num) + (1.96*(sd(sex_num)/sqrt(length(sex_num)))),
            age.mean = mean(age_var),
            age.sd = sd(age_var),
            LEHS.prev = mean(LEHS),
            LEHS.lci = mean(LEHS) - (1.96*(sd(LEHS)/sqrt(length(LEHS)))),
            LEHS.uci = mean(LEHS) + (1.96*(sd(LEHS)/sqrt(length(LEHS)))),
            SomeC.prev = mean(SomeC),
            SomeC.lci = mean(SomeC) - (1.96*(sd(SomeC)/sqrt(length(SomeC)))),
            SomeC.uci = mean(SomeC) + (1.96*(sd(SomeC)/sqrt(length(SomeC)))),
            College.prev = mean(College),
            College.lci = mean(College) - (1.96*(sd(College)/sqrt(length(College)))),
            College.uci = mean(College) + (1.96*(sd(College)/sqrt(length(College)))),
            ms.prev = mean(marital_status),
            ms.lci = mean(marital_status) - (1.96*(sd(marital_status)/sqrt(length(marital_status)))),
            ms.uci = mean(marital_status) - (1.96*(sd(marital_status)/sqrt(length(marital_status)))),
            White.prev = mean(White),
            White.lci = mean(White) - (1.96*(sd(White)/sqrt(length(White)))),
            White.uci = mean(White) - (1.96*(sd(White)/sqrt(length(White)))),
            Black.prev = mean(Black),
            Black.lci = mean(Black) - (1.96*(sd(Black)/sqrt(length(Black)))),
            Black.uci = mean(Black) - (1.96*(sd(Black)/sqrt(length(Black)))),
            Hisp.prev = mean(Hisp),
            Hisp.lci = mean(Hisp) - (1.96*(sd(Hisp)/sqrt(length(Hisp)))),
            Hisp.uci = mean(Hisp) - (1.96*(sd(Hisp)/sqrt(length(Hisp)))),
            ROth.prev = mean(ROth),
            ROth.lci = mean(ROth) - (1.96*(sd(ROth)/sqrt(length(ROth)))),
            ROth.uci = mean(ROth) - (1.96*(sd(ROth)/sqrt(length(ROth))))) %>% 
  
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
out3a <- pdat %>% group_by(YEAR, SunSalesPolicy) %>%
  summarise(alc.prev = mean(drinkingstatus),
            alc.lbi = mean(drinkingstatus) - (1.96*(sd(drinkingstatus)/sqrt(length(drinkingstatus)))),
            alc.ubi = mean(drinkingstatus) + (1.96*(sd(drinkingstatus)/sqrt(length(drinkingstatus)))))

# alcohol use past-year alcohol users
out3b <- pdat %>% filter(gramsperday_raw > 0) %>%
  mutate(alccat3 = ifelse(sex_recode == "Male" & gramsperday_raw <= 60, 0,
                          ifelse(sex_recode == "Male" & gramsperday_raw > 60, 1,
                                 ifelse(sex_recode == "Female" & gramsperday_raw <= 40, 0,
                                        ifelse(sex_recode == "Female" & gramsperday_raw > 40, 1, NA))))) %>%
  group_by(YEAR, SunSalesPolicy) %>%
  summarise(gpd.mean = mean(gramsperday_raw),
            gpd.sd = sd(gramsperday_raw),
            alccat3.prev = mean(alccat3),
            alccat3.lbi = mean(alccat3) - (1.96*(sd(alccat3)/sqrt(length(alccat3)))),
            alccat3.ubi = mean(alccat3) + (1.96*(sd(alccat3)/sqrt(length(alccat3)))))

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

usa <- urbnmapr::states
usa <- left_join(usa, out1, by = c('state_name' = 'State')) %>% filter(!is.na(SunSalesPolicy)) %>%
  mutate(Minnesota = as.factor(ifelse(state_name %like% "Minnesota", 1, 0)))

ggplot() + 
  geom_polygon_pattern(data = usa, color = "white", size = 0.1,
                       aes(x = long, y = lat, group = group, pattern = SunSalesPolicy, 
                           pattern_fill = ControlState, pattern_colour = Minnesota), 
                       pattern_density = 0.05, pattern_spacing = 0.01,
                       pattern_key_scale_factor = 0.6) + 
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_pattern_colour_manual(values = col.fill) + 
  scale_pattern_discrete(choices = c("none", "stripe", "crosshatch")) + 
  ggthemes::theme_map() + theme(legend.position="none", 
                                panel.background = element_rect(fill = "white", color = "white"))

# ----------------------------------------------------------------
# EXPORT
# ----------------------------------------------------------------

ggsave(paste0("acp_brfss/outputs/figures/", DATE, "_map_SunSalesPolicies.jpg"), dpi = 300, width = 8, height = 5)

write.xlsx(out1, file = paste0("acp_brfss/outputs/", DATE, "_StateSummary.xlsx"), rowNames = FALSE)
write.xlsx(out2, file = paste0("acp_brfss/outputs/", DATE, "_BRFSS_SampleDesc_output.xlsx"), rowNames = FALSE)
write.xlsx(out3, file = paste0("acp_brfss/outputs/", DATE, "_BRFSS_AlcDesc_output.xlsx"), rowNames = FALSE)
