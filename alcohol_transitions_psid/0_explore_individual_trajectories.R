# Plot individual trajectories
# Load necessary libraries
library(ggplot2)
library(tidyverse)

datat4 <- readRDS("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/datat4.RDS")
 
# Plot individual trajectories 
ggplot(datat4, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  labs(x = "Year", y = "GPD", title = "Individual Trajectories") +
  theme_minimal() +
  theme(legend.position = "none")

# Cap alcohol at 60 grams
datat4_capped <- datat4 %>% mutate(gpd = ifelse(gpd>60, 60, gpd))

# Define the desired order of education levels
education_order <- c("Less than or equal to high school", "Some college", "College +")
datat4_capped <- datat4_capped %>%
  mutate(education = factor(education, levels = education_order))

# Plot individual trajectories using capped data
ggplot(datat4_capped, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  labs(x = "Year", y = "GPD", title = "Individual Trajectories") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot individual trajectories by groups
male_data <- datat4_capped %>% ungroup() %>% filter(sex==0)
male_data %>% summarize(mean(gpd)) 
female_data <- datat4_capped %>% ungroup() %>% filter(sex==1)
female_data %>% summarize(mean(gpd))

male_18_24 <- male_data %>% filter(age_cat=="18-24")
male_25_64 <- male_data %>% filter(age_cat=="25-64")
male_65 <- male_data %>% filter(age_cat=="65+")
female_18_24 <- female_data %>% filter(age_cat=="18-24")
female_25_64 <- female_data %>% filter(age_cat=="25-64")
female_65 <- female_data %>% filter(age_cat=="65+")

ggplot(male_18_24, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  facet_grid(race ~ education, scales = "free_y") +
  labs(x = "Year", y = "GPD", title = "Individual trajectories: Men, 18-24") +
  theme(legend.position = "none")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/Men_18_24.png", dpi=300, width=33, height=19, units="cm")

ggplot(male_25_64, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  facet_grid(race ~ education, scales = "free_y") +
  labs(x = "Year", y = "GPD", title = "Individual trajectories: Men, 25-64") +
  theme(legend.position = "none")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/Men_25_64.png", dpi=300, width=33, height=19, units="cm")

ggplot(male_65, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  facet_grid(race ~ education, scales = "free_y") +
  labs(x = "Year", y = "GPD", title = "Individual trajectories: Men, 65+") +
  theme(legend.position = "none")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/Men_65_plus.png", dpi=300, width=33, height=19, units="cm")

# Women
ggplot(female_18_24, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  facet_grid(race ~ education, scales = "free_y") +
  labs(x = "Year", y = "GPD", title = "Individual trajectories: Women, 18-24") +
  theme(legend.position = "none")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/Women_18_24.png", dpi=300, width=33, height=19, units="cm")

ggplot(female_25_64, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  facet_grid(race ~ education, scales = "free_y") +
  labs(x = "Year", y = "GPD", title = "Individual trajectories: Women, 25-64") +
  theme(legend.position = "none")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/Women_25_64.png", dpi=300, width=33, height=19, units="cm")

ggplot(female_65, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  facet_grid(race ~ education, scales = "free_y") +
  labs(x = "Year", y = "GPD", title = "Individual trajectories: Women, 65+") +
  theme(legend.position = "none")
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/Women_65_plus.png", dpi=300, width=33, height=19, units="cm")

## Same plots, COVID period only
ggplot(male_18_24, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  facet_grid(race ~ education, scales = "free_y") +
  labs(x = "Year", y = "GPD", title = "Individual trajectories: Men, 18-24") +
  theme(legend.position = "none") +
  xlim(2019,2021)
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/Men_18_24_covid.png", dpi=300, width=33, height=19, units="cm")

ggplot(male_25_64, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  facet_grid(race ~ education, scales = "free_y") +
  labs(x = "Year", y = "GPD", title = "Individual trajectories: Men, 25-64") +
  theme(legend.position = "none") +
  xlim(2019,2021)
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/Men_25_64_covid.png", dpi=300, width=33, height=19, units="cm")

ggplot(male_65, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  facet_grid(race ~ education, scales = "free_y") +
  labs(x = "Year", y = "GPD", title = "Individual trajectories: Men, 65+") +
  theme(legend.position = "none") +
  xlim(2019,2021)
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/Men_65_plus_covid.png", dpi=300, width=33, height=19, units="cm")

# Women
ggplot(female_18_24, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  facet_grid(race ~ education, scales = "free_y") +
  labs(x = "Year", y = "GPD", title = "Individual trajectories: Women, 18-24") +
  theme(legend.position = "none") +
  xlim(2019,2021)
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/Women_18_24_covid.png", dpi=300, width=33, height=19, units="cm")

ggplot(female_25_64, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  facet_grid(race ~ education, scales = "free_y") +
  labs(x = "Year", y = "GPD", title = "Individual trajectories: Women, 25-64") +
  theme(legend.position = "none")+
  xlim(2019,2021)
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/Women_25_64_covid.png", dpi=300, width=33, height=19, units="cm")

ggplot(female_65, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  facet_grid(race ~ education, scales = "free_y") +
  labs(x = "Year", y = "GPD", title = "Individual trajectories: Women, 65+") +
  theme(legend.position = "none")+
  xlim(2019,2021)
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/Women_65_plus_covid.png", dpi=300, width=33, height=19, units="cm")

# Plot trajectories of specific individuals for demonstration purposes
filtered_df <- datat4_capped %>%
  group_by(uniqueID) %>%
  filter(any(gpd >= 60) & any(gpd==0))

filtered_df_men <- filtered_df %>%
  filter(sex==0)
filtered_df_women <- datat4_capped %>%
  group_by(uniqueID) %>%
  filter(any(gpd >= 60) & any(gpd==0)) %>%
  filter(sex==1)

ggplot(filtered_df_men, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  facet_grid(race ~ education, scales = "free_y") +
  labs(x = "Year", y = "GPD", title = "Male extreme switchers, n = 859") +
  theme(legend.position = "none")+
  xlim(2005,2021)
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/frequent_switchers_men.png", dpi=300, width=33, height=19, units="cm")

ggplot(filtered_df_women, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  facet_grid(race ~ education, scales = "free_y") +
  labs(x = "Year", y = "GPD", title = "Female extreme switchers, n = 286") +
  theme(legend.position = "none")+
  xlim(2005,2021)
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/frequent_switchers_women.png", dpi=300, width=33, height=19, units="cm")

temp_1 <- filtered_df_women %>% 
  filter(uniqueID==114173)
temp_2 <- filtered_df_women %>% 
  filter(uniqueID==1100004)
temp_3 <- filtered_df_women %>% 
  filter(uniqueID==2478032)
temp_4 <- filtered_df_women %>% 
  filter(uniqueID==5493007)
temp_5 <- filtered_df_women %>% 
  filter(uniqueID==173035)

ggplot(temp_1, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  labs(x = "Year", y = "GPD") +
  theme(legend.position = "none", text = element_text(size = 20))+
  geom_hline(yintercept = c(1, 20, 40, 60), linetype = "dashed", color = "black")+
  xlim(2005,2021)
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/person_1.png", dpi=300, width=33, height=19, units="cm")

ggplot(temp_2, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  labs(x = "Year", y = "GPD") +
  theme(legend.position = "none", text = element_text(size = 20))+
  geom_hline(yintercept = c(1, 20, 40, 60), linetype = "dashed", color = "black")+
  xlim(2005,2021)
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/person_2.png", dpi=300, width=33, height=19, units="cm")

ggplot(temp_3, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  labs(x = "Year", y = "GPD") +
  theme(legend.position = "none", text = element_text(size = 20))+
  geom_hline(yintercept = c(1, 20, 40, 60), linetype = "dashed", color = "black")+
  xlim(2005,2021)
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/person_3.png", dpi=300, width=33, height=19, units="cm")

ggplot(temp_4, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  labs(x = "Year", y = "GPD") +
  theme(legend.position = "none", text = element_text(size = 20))+
  geom_hline(yintercept = c(1, 20, 40, 60), linetype = "dashed", color = "black")+
  xlim(2005,2021)
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/person_4.png", dpi=300, width=33, height=19, units="cm")

ggplot(temp_5, aes(x = year, y = gpd, group = uniqueID, color = as.factor(uniqueID))) +
  geom_line() +
  labs(x = "Year", y = "GPD") +
  theme(legend.position = "none", text = element_text(size = 20))+
  geom_hline(yintercept = c(1, 20, 40, 60), linetype = "dashed", color = "black")+
  xlim(2005,2021)
ggsave("C:/Users/cmp21seb/Documents/SIMAH/SIMAH_workplace/alcohol_transitions_psid/trajectory_plots/person_5.png", dpi=300, width=33, height=19, units="cm")

