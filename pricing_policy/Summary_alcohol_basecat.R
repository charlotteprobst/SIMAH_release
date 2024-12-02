# Libaries
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggthemes)

# Load data 
Output <- read.csv(paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/2024-09-12/output-policy_alcohol_detail_2024-09-15.csv"))

# Set ggplot layout
options(digits = 4)

c4 <- c("#A9D8B6", "#487B79", "#1F4328", "#1482AB")
c6 <- c("#FFD679", "#BFBFBF", "#A9D8B6", "#487B79", "#1F4328", "#1482AB")

ggtheme <- theme_bw() + theme(legend.position="right",
                              strip.background = element_rect(fill="white"),
                              panel.spacing = unit(1,"lines"),
                              text = element_text(size=20))

# generate output, use 2014 as reference

base_alc <- Output %>% filter(year == 2014) %>% dplyr::select(c(ID, base_alc = alc_cat))

output1 <- Output %>%
  merge(., base_alc, by = "ID", all.x = T) %>%
  mutate(sex = ifelse(sex=="m","Men","Women"),
         scenario = case_when(
           #setting == "counterfactual" ~ "counterfactual",
           scenario != 0 & setting == "mup" ~ "mup",
           TRUE ~ NA)) %>%
  filter(base_alc != "Non-drinker") %>%
  group_by(scenario, year, base_alc, sex, education) %>% 
  summarise(meangpd = mean(alc_gpd)) %>%
  mutate(education = factor(education, 
                            levels = c("LEHS", "SomeC", "College"),
                            labels = c("High school or less", "Some college", "College +")),
         #scenario = factor(scenario, 
         #                  levels = c("counterfactual", "price 5%", 
         #                             "price 10%", "price 20%", "mup"),
         #                  labels = c("No policy", "5% price increase", 
         #                             "10% price increase", "20% price increase", "Minimum unit price")),
         base_alc = factor(base_alc,
                           levels = c("Low risk","Medium risk", "High risk")))

plot1 <- ggplot(data = output1, aes(x = year, y = meangpd, colour = education)) + 
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
  facet_grid(cols = vars(base_alc), rows = vars(sex), scales = "free") + 
  ylim(0,NA) + ylab("Mean grams of alcohol per day") +
  scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
  ggtheme + xlab("") + 
  scale_color_manual(values = c4, name="") + 
  ggtitle("Simulated reduction in alcohol use - by alcohol category in 2014")
ggsave(paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/2024-09-12/plot-policy_meangpd_BaseCat2014_", Sys.Date(), ".png"), plot1, width = 14, height = 8)

# difference / percentage change

base_alc <- output1 %>% filter(year == 2014) %>% 
  group_by(base_alc, sex, education) %>% 
  mutate(base_gpd = meangpd) %>%
  dplyr::select(c(base_alc, sex, education, base_gpd)) 
  
output2 <- output1 %>% 
  left_join(., base_alc) %>%
  mutate(diff = meangpd - base_gpd,
         perc = (meangpd - base_gpd) / base_gpd) 

plot2 <- ggplot(data = output2, aes(x = year, y = diff, colour = education)) + 
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
  facet_grid(cols = vars(base_alc), rows = vars(sex)) + 
  scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
  ggtheme + xlab("") + ylab("") +
  scale_color_manual(values = c4, name="") + 
  ggtitle("Simulated reduction in alcohol use — by alcohol category in 2014", 
          "Change in mean grams per day, reference: alcohol use in 2014")
ggsave(paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/2024-09-12/plot-policy_diffgpd_BaseCat2014_", Sys.Date(), ".png"), plot2, width = 14, height = 8)

plot3 <- ggplot(data = output2, aes(x = year, y = perc, colour = education)) + 
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
  facet_grid(cols = vars(base_alc), rows = vars(sex)) + 
  scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
  ggtheme + xlab("") + ylab("") +
  scale_color_manual(values = c4, name="") + 
  ggtitle("Simulated reduction in alcohol use — by alcohol category in 2014", 
          "% change in mean grams per day, reference: alcohol use in 2014")
ggsave(paste0(WorkingDirectory, "SIMAH_workplace/microsim/2_output_data/2024-09-12/plot-policy_percgpd_BaseCat2014_", Sys.Date(), ".png"), plot3, width = 14, height = 8)

# ----------------------------------

output2 <- output1 %>% ungroup() %>%
  #dplyr::select(-c(samplenum, seed)) %>%
  pivot_wider(names_from = scenario, values_from = meangpd) %>%
  mutate(diffP05 = `5% price increase` - `No policy`,
         diffP10 = `10% price increase` - `No policy`,
         diffP20 = `20% price increase` - `No policy`,
         diffMUP = `Minimum unit price` - `No policy`,
         percP05 = (`5% price increase` - `No policy`) / `No policy`,
         percP10 = (`10% price increase` - `No policy`) / `No policy`,
         percP20 = (`20% price increase` - `No policy`) / `No policy`,
         percMUP = (`Minimum unit price` - `No policy`) / `No policy`) %>% 
  dplyr::select(c(year, base_alc, sex, education,
                  diffP05, diffP10, diffP20, diffMUP, 
                  percP05, percP10, percP20, percMUP)) %>% 
  pivot_longer(cols = c(diffP05, diffP10, diffP20, diffMUP, 
                        percP05, percP10, percP20, percMUP),
               names_to = "scenario", values_to = "value") %>%
  mutate(out = substr(scenario, start = 1, stop = 4),
         scenario = substr(scenario, start = 5, stop = 7)) %>% 
  pivot_wider(names_from = out, values_from = value) %>%
  mutate(scenario = factor(scenario, 
                           levels = c("P05", "P10", "P20", "MUP"),
                           labels = c("5% price increase", "10% price increase", 
                                      "20% price increase", "Minimum unit price")))

plot2A <- ggplot(data = output2[output2$base_alc == "High risk",], aes(x = year, y = diff, colour = scenario)) + 
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
  facet_grid(cols = vars(education), rows = vars(sex)) + 
  scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
  ggtheme + xlab("") + ylab("") +
  scale_color_manual(values = c4, name="") + 
  ggtitle("Simulated reduction in alcohol use — High risk alcohol user", 
          "Change in mean grams per day, reference: no policy")

plot2B <- ggplot(data = output2[output2$base_alc == "Medium risk",], aes(x = year, y = diff, colour = scenario)) + 
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
  facet_grid(cols = vars(education), rows = vars(sex)) + 
  scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
  ggtheme + xlab("") + ylab("") +
  scale_color_manual(values = c4, name="") + 
  ggtitle("Simulated reduction in alcohol use — Medium risk alcohol user", 
          "Change in mean grams per day, reference: no policy")

plot2C <- ggplot(data = output2[output2$base_alc == "Low risk",], aes(x = year, y = diff, colour = scenario)) + 
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
  facet_grid(cols = vars(education), rows = vars(sex)) + 
  scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
  ggtheme + xlab("") + ylab("") +
  scale_color_manual(values = c4, name="") + 
  ggtitle("Simulated reduction in alcohol use — Low risk alcohol user", 
          "Change in mean grams per day, reference: no policy")

plot3A <- ggplot(data = output2[output2$base_alc == "High risk",], aes(x = year, y = perc, colour = scenario)) + 
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
  facet_grid(cols = vars(education), rows = vars(sex)) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
  ggtheme + xlab("") + ylab("") +
  scale_color_manual(values = c4, name="") + 
  ggtitle("Simulated reduction in alcohol use — High risk alcohol user", 
          "% change in mean grams per day, reference: no policy")

plot3B <- ggplot(data = output2[output2$base_alc == "Medium risk",], aes(x = year, y = perc, colour = scenario)) + 
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
  facet_grid(cols = vars(education), rows = vars(sex)) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
  ggtheme + xlab("") + ylab("") +
  scale_color_manual(values = c4, name="") + 
  ggtitle("Simulated reduction in alcohol use — Medium risk alcohol user", 
          "% change in mean grams per day, reference: no policy")

plot3C <- ggplot(data = output2[output2$base_alc == "Low risk",], aes(x = year, y = perc, colour = scenario)) + 
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 2015, color = "#FF0000", linetype = "dashed") +
  facet_grid(cols = vars(education), rows = vars(sex)) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks = seq(2010, 2019, 3), limits = c(2010,2020)) + 
  ggtheme + xlab("") + ylab("") +
  scale_color_manual(values = c4, name="") + 
  ggtitle("Simulated reduction in alcohol use — Low risk alcohol user", 
          "% change in mean grams per day, reference: no policy")
