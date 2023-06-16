# creating plots for RSA based on alcohol policy experiments 
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(tidyverse)
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)

data <- read.csv("SIMAH_workplace/microsim/2_output_data/policy_experiments/processed_calibration_data.csv")

# first draw a plot of the calibrated model

calibratedmodel <- data %>% filter(percentreduction=="0%") %>% 
  pivot_longer(cols=observed_rate:simulated_rate) %>% 
  mutate(education = factor(education, levels=c("High school or less", "Some college","College +")),
         name = recode(name, "observed_rate"="Observed","simulated_rate"="Simulated"))

ggplot(data=calibratedmodel, aes(x=year, y=value, colour=sex, linetype=name)) + 
  geom_line(linewidth=1) + 
  facet_grid(cols=vars(education), rows=vars(cause), scales="free") + ylim(0,NA) + 
  scale_colour_brewer(palette="Dark2", name="") + 
  scale_linetype_manual(values=c("solid","dashed"), name="") + 
  theme_bw() + 
  theme(legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=20)) +
  ylab("Mortality rate per 100,000 population") +
  xlab("") + 
  ggtitle("Best calibrated model, observed and simulated mortality")
ggsave("SIMAH_workplace/microsim/2_output_data/policy_experiments/calibrated_model.png", dpi=500, width=33, height=21, units="cm")

reporting <- calibratedmodel %>% pivot_wider(names_from=name, values_from=value) %>% 
  mutate(pct_difference = abs(observed_count-simulated_count)/observed_count)

# now read in the policy effects
data <- read.csv("SIMAH_workplace/microsim/2_output_data/policy_experiments/processed_policy_data.csv")

# summarise as change in mortality 
policymodel <- data %>%
  group_by(year, percentreduction, sex, education) %>% 
  filter(year>=2014) %>% 
  summarise(simulated_count = sum(simulated_count)) %>% 
  ungroup() %>% group_by(percentreduction, sex, education) %>% 
  mutate(change_score = simulated_count-lag(simulated_count),
         change_pct = (simulated_count-lag(simulated_count))/lag(simulated_count),
         change_score = ifelse(is.na(change_score), 0, change_score),
         change_pct = ifelse(is.na(change_pct), 0, change_pct))

ggplot(data=policymodel, aes(x=year, y=change_pct, colour=as.factor(percentreduction))) + 
  geom_line(linewidth=1) + 
  facet_grid(rows=vars(education), cols=vars(sex), scales="free") + 
  scale_colour_brewer(palette="Dark2", name="Percentage reduction in alcohol use") + 
  # scale_linetype_manual(values=c("solid","dashed"), name="") + 
  theme_bw() + 
  theme(legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=15),
        plot.title=element_text(hjust=0.5, size=15)) +
  ylab("Percentage change in mortality") +
  xlab("") + 
  geom_vline(xintercept=2015, linetype="dashed") + 
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Percentage change in mortality from liver cirrhosis, suicide and alcohol use disorders following taxation policy") + 
  xlim(2014, 2016)

ggsave("SIMAH_workplace/microsim/2_output_data/policy_experiments/policy_change.png", dpi=500, width=25, height=19, units="cm")




