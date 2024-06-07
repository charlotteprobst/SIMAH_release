# Setup
setwd("C:/Users/cmp21seb/Documents/SIMAH/")
library(tidyverse)

# Read in the model outputs for each alternative method

# Poisson regression, NESARC
output_poissonregression <- read_csv("SIMAH_workplace/NESARC/output-1_poissonregression.csv")

# GLMM, NESARC
output_GLMM_poisson <- read_csv("SIMAH_workplace/NESARC/output-1_GLMM_poisson.csv")

# Multinomial regression, NESARC (see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6842126/)
output_multinom <- read_csv("SIMAH_workplace/NESARC/output-NESARCmultinom.csv")

# Markov model with uninformed priors and 'optimiser'
# nb. this method doesn't use NESARC data but the model structure is informed by the NESARC Markov model
output_uninformedpriors <- read_csv("SIMAH_workplace/NESARC/output-1_optimisedoutput.csv")

# Deterministic population, BRFSS
output_deterministicpop <- read_csv("SIMAH_workplace/brfss/processed_data/output-1_deterministicpop.csv") # pre-modelling
### add model results


# Generate plots for each model

# Poisson regression, NESARC

ggplot(subset(output_poissonregression, AlcCAT=="Non-drinker" & microsim.init.race=="WHI"), aes(x=year, y=propsimulation, colour=as.factor(samplenum))) + 
  geom_line() + 
  # geom_point() + 
  geom_line(aes(x=year, y=proptarget), colour="black", linewidth=1) + 
  geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
  theme(legend.position="none") + ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle("Poisson - Non-drinkers - White")
ggsave("SIMAH_workplace/alcohol_models_comparison/poisson_regression_nesarc/White_nondrinkers.png", dpi=300, width=33, height=19, units='cm')

ggplot(subset(output_poissonregression, AlcCAT=="Low risk" & microsim.init.race=="WHI"), aes(x=year, y=propsimulation, colour=as.factor(samplenum))) + 
  geom_line() + 
  # geom_point() + 
  geom_line(aes(x=year, y=proptarget), colour="black", linewidth=1) + 
  geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
  theme(legend.position="none") + ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle("Poisson - Low risk - White")
ggsave("SIMAH_workplace/alcohol_models_comparison/poisson_regression_nesarc/White_lowrisk.png", dpi=300, width=33, height=19, units='cm')

ggplot(subset(output_poissonregression, AlcCAT=="Medium risk" & microsim.init.race=="WHI"), aes(x=year, y=propsimulation, colour=as.factor(samplenum))) + 
  geom_line() + 
  # geom_point() + 
  geom_line(aes(x=year, y=proptarget), colour="black", linewidth=1) + 
  geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
  theme(legend.position="none") + ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle("Poisson - Medium risk - White")
ggsave("SIMAH_workplace/alcohol_models_comparison/poisson_regression_nesarc/White_mediumrisk.png", dpi=300, width=33, height=19, units='cm')


ggplot(subset(output_poissonregression, AlcCAT=="High risk" & microsim.init.race=="WHI"), aes(x=year, y=propsimulation, colour=as.factor(samplenum))) + 
  geom_line() + 
  # geom_point() + 
  geom_line(aes(x=year, y=proptarget), colour="black", linewidth=1) + 
  geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
  theme(legend.position="none") + ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle("Poisson - High risk - White")
ggsave("SIMAH_workplace/alcohol_models_comparison/poisson_regression_nesarc/White_highrisk.png", dpi=300, width=33, height=19, units='cm')


## GLMM, NESARC
output_GLMM_poisson$year <- round(output_GLMM_poisson$year, 0)
ggplot(subset(output_GLMM_poisson, AlcCAT=="Non-drinker" & microsim.init.race=="WHI"), aes(x=year, y=propsimulation, colour="red")) + # , colour=as.factor(samplenum))) + 
  geom_line() + 
  # geom_point() + 
  geom_line(aes(x=year, y=proptarget), colour="black", linewidth=1) + 
  geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle("GLMM - Non-drinkers - White")
ggsave("SIMAH_workplace/alcohol_models_comparison/GLMM_nesarc/White_nondrinkers.png", dpi=300, width=33, height=19, units='cm')

ggplot(subset(output_GLMM_poisson, AlcCAT=="Low risk" & microsim.init.race=="WHI"), aes(x=year, y=propsimulation, colour="red")) + # , colour=as.factor(samplenum))) + 
  geom_line() + 
  # geom_point() + 
  geom_line(aes(x=year, y=proptarget), colour="black", linewidth=1) + 
  geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle("GLMM - Low risk - White")
ggsave("SIMAH_workplace/alcohol_models_comparison/GLMM_nesarc/White_lowrisk.png", dpi=300, width=33, height=19, units='cm')

ggplot(subset(output_GLMM_poisson, AlcCAT=="Medium risk" & microsim.init.race=="WHI"), aes(x=year, y=propsimulation, colour="red")) + # , colour=as.factor(samplenum))) + 
  geom_line() + 
  # geom_point() + 
  geom_line(aes(x=year, y=proptarget), colour="black", linewidth=1) + 
  geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle("GLMM - Medium risk - White")
ggsave("SIMAH_workplace/alcohol_models_comparison/GLMM_nesarc/White_mediumrisk.png", dpi=300, width=33, height=19, units='cm')

ggplot(subset(output_GLMM_poisson, AlcCAT=="High risk" & microsim.init.race=="WHI"), aes(x=year, y=propsimulation, colour="red")) + # , colour=as.factor(samplenum))) + 
  geom_line() + 
  geom_line(aes(x=year, y=proptarget), colour="black", linewidth=1) + 
  geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle("GLMM - High risk - White")
ggsave("SIMAH_workplace/alcohol_models_comparison/GLMM_nesarc/White_highrisk.png", dpi=300, width=33, height=19, units='cm')

## Multinomial logistic regression (Barbosa method), NESARC
ggplot(subset(output_multinom, AlcCAT=="Non-drinker" & microsim.init.race=="WHI"), aes(x=year, y=propsimulation, colour="red")) + # , colour=as.factor(samplenum))) + 
  geom_line() + 
  # geom_point() + 
  geom_line(aes(x=year, y=proptarget), colour="black", linewidth=1) + 
  geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle("Multinom - Non-drinkers - White")
ggsave("SIMAH_workplace/alcohol_models_comparison/multinom/White_nondrinkers.png", dpi=300, width=33, height=19, units='cm')

ggplot(subset(output_multinom, AlcCAT=="Low risk" & microsim.init.race=="WHI"), aes(x=year, y=propsimulation, colour="red")) + # , colour=as.factor(samplenum))) + 
  geom_line() + 
  # geom_point() + 
  geom_line(aes(x=year, y=proptarget), colour="black", linewidth=1) + 
  geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle("Multinom - Low risk - White")
ggsave("SIMAH_workplace/alcohol_models_comparison/multinom/White_lowrisk.png", dpi=300, width=33, height=19, units='cm')

ggplot(subset(output_multinom, AlcCAT=="Medium risk" & microsim.init.race=="WHI"), aes(x=year, y=propsimulation, colour="red")) + # , colour=as.factor(samplenum))) + 
  geom_line() + 
  # geom_point() + 
  geom_line(aes(x=year, y=proptarget), colour="black", linewidth=1) + 
  geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle("Multinom - Medium risk - White")
ggsave("SIMAH_workplace/alcohol_models_comparison/multinom/White_mediumrisk.png", dpi=300, width=33, height=19, units='cm')

ggplot(subset(output_multinom, AlcCAT=="High risk" & microsim.init.race=="WHI"), aes(x=year, y=propsimulation, colour="red")) + # , colour=as.factor(samplenum))) + 
  geom_line() + 
  # geom_point() + 
  geom_line(aes(x=year, y=proptarget), colour="black", linewidth=1) + 
  geom_ribbon(aes(ymin=proptarget-1.96*se, ymax=proptarget+1.96*se), fill="grey", colour=NA, alpha=0.6) + 
  theme_bw() + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylim(0,NA) +
  facet_grid(cols=vars(microsim.init.sex, agecat), rows=vars(microsim.init.education)) +
  ggtitle("Multinom - High risk - White")
ggsave("SIMAH_workplace/alcohol_models_comparison/multinom/White_highrisk.png", dpi=300, width=33, height=19, units='cm')
