# Read in the microsim output
Output <- readRDS(paste0(WorkingDirectory, "/SIMAH_workplace/microsim/2_output_data/output_first_run_education_2000-2022.rds"))

# Filter for white people
white_data <- Output %>%
  filter(microsim.init.race == "WHI")

summary_data_white <- white_data %>%
  group_by(year, microsim.init.sex, agecat) %>%
  mutate(percent = n / sum(n) * 100)

# Plot the data for white people
ggplot(summary_data_white, aes(x = year, y = percent, fill = microsim.init.education)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(microsim.init.sex ~ agecat) +
  labs(title = "Percentage of White People by Education within Each Year",
       x = "Year",
       y = "Percentage",
       fill = "Education Category") +
  theme_minimal()

# by age 30 where have people ended up (overall)
education_at_30 <- summary_data_white %>%
  filter(microsim.init.age == "30") %>%
  group_by(year, microsim.init.sex) %>%
  mutate(Percentage = n/sum(n) * 100) %>%
  dplyr::select(-percent)

# Define the order of education levels
education_at_30$microsim.init.education <- factor(education_at_30$microsim.init.education,
                                    levels = c("LEHS", "SomeC", "College"))

# Generate a colour blind friendly pallete:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Plot the data for people aged 30
ggplot(education_at_30, aes(x = year, y = Percentage, fill = microsim.init.education)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ microsim.init.sex) +
  scale_fill_manual(values = cbPalette) +
  labs(title = "Education Levels at Age 30 Over Time",
       x = "Year",
       y = "Percentage",
       fill = "Education Level") +
  theme_minimal()

ggsave("SIMAH_workplace/education_transitions/2021/plots/edu_cat_age30_stacked_white.png", dpi=300, width = 12, height = 7)

# Filter for Black people
black_data <- Output %>%
  filter(microsim.init.race == "BLA")

summary_data_black <- black_data %>%
  group_by(year, microsim.init.sex, agecat) %>%
  mutate(percent = n / sum(n) * 100)

# Plot the data for Black people
ggplot(summary_data_black, aes(x = year, y = percent, fill = microsim.init.education)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(microsim.init.sex ~ agecat) +
  labs(title = "Percentage of Black People by Education within Each Year, White people (using COVID TPs)",
       x = "Year",
       y = "Percentage",
       fill = "Education Category") +
  theme_minimal()

# by age 30 where have people ended up (overall)
education_at_30_black <- summary_data_black %>%
  filter(microsim.init.age == "30") %>%
  group_by(year, microsim.init.sex) %>%
  mutate(Percentage = n/sum(n) * 100) %>%
  dplyr::select(-percent)

# Define the order of education levels
education_at_30_black$microsim.init.education <- factor(education_at_30$microsim.init.education,
                                                  levels = c("LEHS", "SomeC", "College"))

# Plot the data for people aged 30
ggplot(education_at_30_black, aes(x = year, y = Percentage, fill = microsim.init.education)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ microsim.init.sex) +
  scale_fill_manual(values = cbPalette) +
  labs(title = "Education Levels at Age 30 Over Time, Black people (using COVID TPs)",
       x = "Year",
       y = "Percentage",
       fill = "Education Level") +
  theme_minimal()