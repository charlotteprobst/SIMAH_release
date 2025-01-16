# Illustrations for Pricing Policy Paper Supplement

library(ggthemes)

# Load relevant data from 1_run_microsimulation_policy.R

library(devtools)
library(roxygen2)
library(gatbxr)
library(faux)
library(dplyr)
library(tidyverse)
library(fitdistrplus)
library(lhs)
library(truncnorm)
library(data.table)
library(gridExtra)
library(doParallel)
options(dplyr.summarise.inform = FALSE)

WorkingDirectory <- "/Users/carolinkilian/Desktop/"
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)
install("SIMAH_code/calibrationpackage", dep=T)

library(microsimpackage)
library(calibrationpackage)

source("SIMAH_code/microsim/0_model_settings.R")
source("SIMAH_code/microsim/0_policy_settings.R")
source("SIMAH_code/microsim/0_load_microsim_files.R")

data <- basepop

# Example for beer 

# Population distribution
dist <- rnorm(100000, -0.52, 0.0808)
hist(dist, xlab = "Own-price elasticity", main = "", col = "white", breaks = 100)
abline(v = -0.52, col = "red", lwd = 2)
abline(v = -0.48, col = "blue", lwd = 2)

# Distribution for one microsimulation run
dist <- rnorm(100000, -0.48, 0.07)
hist(dist, xlab = "Own-price elasticity", main = "", col = "white", breaks = 100)
abline(v = -0.48, col = "blue", lwd = 2)

# Inter-individual variability
newGPD <- data %>% filter(alc_cat != "Non-drinker") %>% 
  mutate(beer_percentreduction = rnorm_pre(log(beergpd)^2, mu = -0.48, sd = 0.07, r = .6, empirical = T),
         beer_decile = ntile(log(beergpd),10)) %>% 
  dplyr::select(beer_percentreduction, beergpd, beer_decile) 

ggplot(newGPD) + 
  geom_boxplot(aes(x = as.factor(beer_decile), y = beer_percentreduction)) + 
  geom_hline(yintercept = -0.48, col = "blue", linewidth=1) + 
  geom_hline(yintercept = 0, col = "black", linetype = "dashed") + 
  xlab("\nDeciles of beer consumption\n") + 
  ylab("Percent reduction in beer consumption\n") + 
  theme_base() + theme(axis.text = element_text(size = 12), 
                       axis.title = element_text(size = 12))

# Inter-individual variability by different strength of correlation
newGPDlong <- newGPD %>% 
  mutate(beer_pr_min = rnorm_pre(log(beergpd)^2, mu = -0.48, sd = 0.07, r = .4, empirical = T), 
         beer_pr_main = beer_percentreduction, 
         beer_pr_max = rnorm_pre(log(beergpd)^2, mu = -0.48, sd = 0.07, r = .8, empirical = T))
  dplyr::select(-beer_percentreduction) %>% 
  pivot_longer(cols = c("beer_pr_min", "beer_pr_main", "beer_pr_max"), 
               values_to = "pr", names_to = "corr")

labs <- c("r = 0.4", "r = 0.6", "r = 0.8")
names(labs) <- c("beer_pr_min", "beer_pr_main", "beer_pr_max")

ggplot(newGPDlong) + 
  geom_boxplot(aes(x = as.factor(beer_decile), y = pr)) + 
  geom_hline(yintercept = -0.48, col = "blue", linewidth=1) + 
  geom_hline(yintercept = 0, col = "black", linetype = "dashed") + 
  xlab("\nDeciles of beer consumption\n") + 
  ylab("Percent reduction in beer consumption\n") + 
  facet_grid(cols = vars(corr), labeller = labeller(corr = labs)) + 
  theme_base() + theme(axis.text = element_text(size = 12), 
                       axis.title = element_text(size = 12), 
                       plot.background = element_rect(color = "white"))
ggsave("/Users/carolinkilian/Desktop/SIMAH/Pricing policies microsimulation/FigS3_Individual-variability-cons-elasticity_Beer.png", width = 16, height = 6)


