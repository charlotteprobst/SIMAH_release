library(tidyr)

dle_results <- read.csv("SIMAH_workplace/life_expectancy/2_out_data/2021_decomp/LifeExpectancy_1821_detail_ACS.csv")

dle_results_1 <- dle_results %>% filter(Race != "Other") %>%
  mutate(SES = replace(SES, SES == "College", "High"),
         SES = replace(SES, SES == "SomeC", "Middle"),
         SES = replace(SES, SES == "LEHS", "Low"),
         Sex = replace(Sex, Sex == 1, "Men"),
         Sex = replace(Sex, Sex == 2, "Women")) %>%
  spread(Race, Life_expectancy) %>%
  mutate(`White-Black` = White-Black,
         `White-Hispanic` = White-Hispanic) %>%
  gather(key = "Racial_Disparity", value = "Diff_Life_Expectancy", 7:8)
  
# White-Black disparities during 2019-2020
dle_results_1 %>% filter(Year %in% 2019:2020 & Racial_Disparity == "White-Black") %>% 
  select(Year, Sex, SES, Racial_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2020` - `2019`)

# White-Hispanic disparities during 2019-2020
dle_results_1 %>% filter(Year %in% 2019:2020 & Racial_Disparity == "White-Hispanic") %>% 
  select(Year, Sex, SES, Racial_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2020` - `2019`)


# White-Black disparities during 2020-2021
dle_results_1 %>% filter(Year %in% 2020:2021 & Racial_Disparity == "White-Black") %>% 
  select(Year, Sex, SES, Racial_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2021` - `2020`)

# White-Hispanic disparities during 2020-2021
dle_results_1 %>% filter(Year %in% 2020:2021 & Racial_Disparity == "White-Hispanic") %>% 
  select(Year, Sex, SES, Racial_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2021` - `2020`)


color.vec <- c("#90be6d", "#f9c74f", "#f94144")

# Plot on racial/ethnic disparity in life expectancy over time
le_graph <- ggplot(data = dle_results_1, aes(x = Year, y = Diff_Life_Expectancy, colour = SES)) + 
  facet_grid(rows = vars(Sex), cols = vars(Racial_Disparity)) +
  ylab("Difference in life expectancy at age 18") +
  theme_light()+
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text=element_text(size = 12), 
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "bottom") +
  scale_color_manual(name = "Educational attainment", 
                     breaks = c("High", "Middle", "Low"), values = color.vec, 
                     labels = c("High", "Middle", "Low")) +   
  geom_line(aes(color = SES), linewidth = .9, alpha = .6) +
  geom_point(size = 2, aes(color = SES)) 
le_graph
ggsave(paste0("SIMAH_workplace/life_expectancy/3_graphs/2021_decomp/LE_racial_disparity_", k.run, k.pop_type, ".jpg"), 
       dpi=600, width=20, height=15, units="cm")




dle_results_2 <- dle_results %>% filter(Race != "Other") %>%
  mutate(SES = replace(SES, SES == "College", "High"),
         SES = replace(SES, SES == "SomeC", "Middle"),
         SES = replace(SES, SES == "LEHS", "Low"),
         Sex = replace(Sex, Sex == 1, "Men"),
         Sex = replace(Sex, Sex == 2, "Women")) %>%
  spread(SES, Life_expectancy) %>%
  mutate(`High-Low` = High-Low,
         `High-Middle` = High-Middle) %>%
  gather(key = "SES_Disparity", value = "Diff_Life_Expectancy", 7:8)

# High-Low disparities during 2019-2020
dle_results_2 %>% filter(Year %in% 2019:2020 & SES_Disparity == "High-Low") %>% 
  select(Year, Sex, Race, SES_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2020` - `2019`)

# High-Middle disparities during 2019-2020
dle_results_2 %>% filter(Year %in% 2019:2020 & SES_Disparity == "High-Middle") %>% 
  select(Year, Sex, Race, SES_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2020` - `2019`)


# High-Low disparities during 2020-2021
dle_results_2 %>% filter(Year %in% 2020:2021 & SES_Disparity == "High-Low") %>% 
  select(Year, Sex, Race, SES_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2021` - `2020`)

# High-Middle disparities during 2020-2021
dle_results_2 %>% filter(Year %in% 2020:2021 & SES_Disparity == "High-Middle") %>% 
  select(Year, Sex, Race, SES_Disparity, Diff_Life_Expectancy) %>%
  spread(Year, Diff_Life_Expectancy) %>% 
  mutate(Change_in_Disparity = `2021` - `2020`)


le_graph <- ggplot(data = dle_results_2, aes(x = Year, y = Diff_Life_Expectancy, colour = Race)) + 
  facet_grid(rows = vars(Sex), cols = vars(SES_Disparity)) +
  ylab("Difference in life expectancy at age 18") +
  theme_light()+
  theme(strip.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = 'black'), 
        text=element_text(size = 12), 
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 12), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "bottom") +
  scale_color_manual(name = "Race and ethnicity",
                     breaks = c("White", "Hispanic", "Black"), values = color.vec,
                     labels = c("White", "Hispanic", "Black")) +
  geom_line(aes(color = Race), linewidth = .9, alpha = .6) +
  geom_point(size = 2, aes(color = Race)) 
le_graph
ggsave(paste0("SIMAH_workplace/life_expectancy/3_graphs/2021_decomp/LE_SES_disparity_", k.run, k.pop_type, ".jpg"), 
       dpi=600, width=20, height=15, units="cm")
