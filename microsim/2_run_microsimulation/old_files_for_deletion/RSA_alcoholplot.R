# plotting RSA results for policy experiments 
# creating plots for RSA based on alcohol policy experiments 
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(tidyverse)
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
setwd(WorkingDirectory)
# now read in the policy effects
data <- read.csv("SIMAH_workplace/microsim/2_output_data/policy_experiments/processed_alcoholchange_data.csv") %>% 
  mutate(education = factor(education, levels=c("High school or less","Some college","College +")),
         percentreduction = factor(percentreduction, levels=c("0%","7.1%","10.8%","14.5%")))

ggplot(data=data, aes(x=year, y=mean, colour=as.factor(percentreduction))) + 
  geom_line(linewidth=1) + 
  facet_grid(rows=vars(sex), cols=vars(education), scales="fixed") + 
  scale_colour_brewer(palette="Dark2", name="") + 
  theme_bw() + 
  theme(legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=15),
        axis.text = element_text(size=15),
        plot.title=element_text(hjust=0.5, size=18)) +
  ylab("Mean alcohol use (grams per day)") +
  xlab("") + xlim(2010,2019)

ggsave("SIMAH_workplace/microsim/2_output_data/policy_experiments/alcohol_change.png", dpi=500, width=25, height=19, units="cm")
