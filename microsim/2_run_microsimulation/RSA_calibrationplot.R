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
         name = recode(name, "observed_rate"="Observed","simulated_rate"="Simulated")) %>% 
  filter(education!="Some college")

ggplot(data=calibratedmodel, aes(x=year, y=value, colour=sex, linetype=name)) + 
  geom_line(linewidth=1.5) + 
  facet_grid(cols=vars(education), rows=vars(cause), scales="free") + ylim(0,NA) + 
  scale_colour_brewer(palette="Dark2", name="") + 
  scale_linetype_manual(values=c("solid","dashed"), name="") + 
  theme_bw() + 
  theme(legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=20),
        legend.key.width=unit(2,"cm")) +
  ylab("Mortality rate per 100,000 population") +
  xlab("") + 
  ggtitle("Calibrated model, observed and simulated mortality") +
  geom_vline(xintercept=2015, linetype="dashed") 
ggsave("SIMAH_workplace/microsim/2_output_data/policy_experiments/calibrated_model.png", dpi=500, width=33, height=21, units="cm")

reporting <- calibratedmodel %>% pivot_wider(names_from=name, values_from=value) %>% 
  mutate(pct_difference = abs(observed_count-simulated_count)/observed_count)




