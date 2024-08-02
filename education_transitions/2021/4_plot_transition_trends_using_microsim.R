# Read in the microsim outputs

# Switching between standard and covid TPs
Output_switching <- readRDS(paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/education_output_2000_2022_switching_tps.rds"))
# Using standard TPs only
Output_standard <- readRDS(paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/education_output_2000_2022_not_switching_tps.rds"))

# Generate a colour blind friendly pallete:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


Output_switching$microsim.init.education <- factor(Output_switching$microsim.init.education, levels = c("LEHS", "SomeC", "College"))
Output_standard$microsim.init.education <- factor(Output_standard$microsim.init.education, levels = c("LEHS", "SomeC", "College"))



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
white_data <- Output_switching %>%
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
black_data <- Output_switching %>%
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

# DIRECTLY COMPARE THE NON-SWITCHING AND SWITCHING TPS FOR THE YEARS 2020,2021 AND 2022 AND PLOT THE DIFFERENCE

Output_switching <- Output_switching %>% 
  mutate(type="switching") %>%
  group_by(microsim.init.race, microsim.init.sex, agecat, year) %>%
  mutate(Percentage = n / sum(n) * 100) 

Output_standard <- Output_standard %>% mutate(type="standard") %>%
  group_by(microsim.init.race, microsim.init.sex, agecat, year) %>%
  mutate(Percentage = n / sum(n) * 100)

combined <- inner_join(Output_standard, Output_switching, by = join_by(year, microsim.init.sex, microsim.init.race, microsim.init.age, microsim.init.education, agecat, samplenum, seed)) 
combined_2020 <- combined %>% filter(year>=2020)
combined_2020 <- combined_2020 %>% 
  mutate(difference_n=n.y-n.x,
         percent_dif=Percentage.y-Percentage.x) 

combined_2020_30 <- combined_2020 %>%
  filter(microsim.init.race!="OTH") %>%
  filter(microsim.init.age=="30")

# Plot the % differences
# As a line plot
ggplot(combined_2020_30, aes(x = year, y = percent_dif, color = microsim.init.education)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  
  facet_grid(microsim.init.sex ~ microsim.init.race) +
  labs(
    title = "Percent Difference at age 30 by Year and Education Category",
    x = "Year",
    y = "Percent Difference",
    color = "Education"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 2)) +  # Adjust x-axis breaks to show years as integers
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# As a bar chart
ggplot(combined_2020_30, aes(x = factor(year), y = percent_dif, fill = microsim.init.education)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add horizontal line at y = 0
  facet_wrap(microsim.init.sex ~ microsim.init.race) +
  labs(
    title = "Percent Difference by Year and Education Category",
    x = "Year",
    y = "Percent Difference",
    fill = "Education"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing.x = unit(2, "lines"))
  