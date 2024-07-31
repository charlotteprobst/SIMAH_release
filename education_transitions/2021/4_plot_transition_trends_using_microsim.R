# Read in the microsim outputs

# Switching between standard and covid TPs
Output <- readRDS(paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/education_output_2000_2022_switching_tps.rds"))
# Using standard TPs only
Output_standard <- readRDS(paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/education_output_2000_2022_not_switching_tps.rds"))

# Generate a colour blind friendly pallete:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


################ PLOTS USING STANDARD TPS ONLY

# Filter for white men and women
white_data_standard <- Output_standard %>%
  filter(microsim.init.race == "WHI")

# Plot for White Men
education_at_26_white_men_standard <- white_data_standard %>%
  filter(microsim.init.age == "26", microsim.init.sex == "m") %>%
  group_by(year) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(education_at_26_white_men_standard, aes(x = year, y = Percentage, fill = microsim.init.education)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cbPalette) +
  labs(title = "Education Levels at Age 26 Over Time, White Men\nStandard TPs t/o",
       x = "Year",
       y = "Percentage",
       fill = "Education Level") +
  theme_minimal()

ggsave("SIMAH_workplace/education_transitions/2021/plots/edu_cat_age26_stacked_white_men_microsim_standard_tps.png", dpi=300, width = 12, height = 7)

# Plot for White Women
education_at_26_white_women_standard <- white_data_standard %>%
  filter(microsim.init.age == "26", microsim.init.sex == "f") %>%
  group_by(year) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(education_at_26_white_women_standard, aes(x = year, y = Percentage, fill = microsim.init.education)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cbPalette) +
  labs(title = "Education Levels at Age 26 Over Time, White Women\nStandard TPs t/o",
       x = "Year",
       y = "Percentage",
       fill = "Education Level") +
  theme_minimal()

ggsave("SIMAH_workplace/education_transitions/2021/plots/edu_cat_age26_stacked_white_women_microsim_standard_tps.png", dpi=300, width = 12, height = 7)

# Filter for black men and women
black_data_standard <- Output_standard %>%
  filter(microsim.init.race == "BLA")

# Plot for Black Men
education_at_26_black_men_standard <- black_data_standard %>%
  filter(microsim.init.age == "26", microsim.init.sex == "m") %>%
  group_by(year) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(education_at_26_black_men_standard, aes(x = year, y = Percentage, fill = microsim.init.education)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cbPalette) +
  labs(title = "Education Levels at Age 26 Over Time, Black Men\nStandard TPs t/o",
       x = "Year",
       y = "Percentage",
       fill = "Education Level") +
  theme_minimal()

ggsave("SIMAH_workplace/education_transitions/2021/plots/edu_cat_age26_stacked_black_men_microsim_standard_tps.png", dpi=300, width = 12, height = 7)

# Plot for Black Women
education_at_26_black_women_standard <- black_data_standard %>%
  filter(microsim.init.age == "26", microsim.init.sex == "f") %>%
  group_by(year) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(education_at_26_black_women_standard, aes(x = year, y = Percentage, fill = microsim.init.education)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cbPalette) +
  labs(title = "Education Levels at Age 26 Over Time, Black Women\nStandard TPs t/o",
       x = "Year",
       y = "Percentage",
       fill = "Education Level") +
  theme_minimal()

ggsave("SIMAH_workplace/education_transitions/2021/plots/edu_cat_age26_stacked_black_women_microsim_standard_tps.png", dpi=300, width = 12, height = 7)


############# SWITCHING

# Filter for white men and women
white_data <- Output %>%
  filter(microsim.init.race == "WHI")

# Plot for White Men
education_at_26_white_men <- white_data %>%
  filter(microsim.init.age == "26", microsim.init.sex == "m") %>%
  group_by(year) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(education_at_26_white_men, aes(x = year, y = Percentage, fill = microsim.init.education)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cbPalette) +
  labs(title = "Education Levels at Age 26 Over Time, White Men\nSwitching between standard and COVID TPs",
       x = "Year",
       y = "Percentage",
       fill = "Education Level") +
  theme_minimal()

ggsave("SIMAH_workplace/education_transitions/2021/plots/edu_cat_age26_stacked_white_men_microsim_switching.png", dpi=300, width = 12, height = 7)

# Plot for White Women
education_at_26_white_women <- white_data %>%
  filter(microsim.init.age == "26", microsim.init.sex == "f") %>%
  group_by(year) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(education_at_26_white_women, aes(x = year, y = Percentage, fill = microsim.init.education)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cbPalette) +
  labs(title = "Education Levels at Age 26 Over Time, White Women\nSwitching between standard and COVID TPs",
       x = "Year",
       y = "Percentage",
       fill = "Education Level") +
  theme_minimal()

ggsave("SIMAH_workplace/education_transitions/2021/plots/edu_cat_age26_stacked_white_women_microsim_switching.png", dpi=300, width = 12, height = 7)

# Filter for black men and women
black_data <- Output %>%
  filter(microsim.init.race == "BLA")

# Plot for Black Men
education_at_26_black_men <- black_data %>%
  filter(microsim.init.age == "26", microsim.init.sex == "m") %>%
  group_by(year) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(education_at_26_black_men, aes(x = year, y = Percentage, fill = microsim.init.education)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cbPalette) +
  labs(title = "Education Levels at Age 26 Over Time, Black Men\nSwitching between standard and COVID TPs",
       x = "Year",
       y = "Percentage",
       fill = "Education Level") +
  theme_minimal()

ggsave("SIMAH_workplace/education_transitions/2021/plots/edu_cat_age26_stacked_black_men_microsim_switching.png", dpi=300, width = 12, height = 7)

# Plot for Black Women
education_at_26_black_women <- black_data %>%
  filter(microsim.init.age == "26", microsim.init.sex == "f") %>%
  group_by(year) %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(education_at_26_black_women, aes(x = year, y = Percentage, fill = microsim.init.education)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = cbPalette) +
  labs(title = "Education Levels at Age 26 Over Time, Black Women\nSwitching between standard and COVID TPs",
       x = "Year",
       y = "Percentage",
       fill = "Education Level") +
  theme_minimal()

ggsave("SIMAH_workplace/education_transitions/2021/plots/edu_cat_age26_stacked_black_women_microsim_switching.png", dpi=300, width = 12, height = 7)
