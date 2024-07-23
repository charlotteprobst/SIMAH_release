#####SIMAH project 2022 - script for running SIMAH microsimulation model
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(devtools)
library(roxygen2)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(fitdistrplus)
library(lhs)
library(truncnorm)
library(data.table)
library(gridExtra)
library(foreach)
library(doParallel)
options(dplyr.summarise.inform = FALSE)

###set working directory to the main "SIMAH" folder in your directory 
# WorkingDirectory <- "U:/SIMAH/"
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"

DataDirectory <- paste0(WorkingDirectory, "SIMAH_workplace/microsim/1_input_data/")

# load in microsim R package
setwd(paste(WorkingDirectory))

install("SIMAH_code/microsimpackage", dep=T)

source("SIMAH_code/microsim/2_run_microsimulation/0_model_settings.R")

Output <- read_rds("SIMAH_workplace/microsim/2_output_data/policy_experiments/Alcohol_output_policyexperiments.RDS")

outputs <- do.call(rbind,Output)

CatAlcohol <- outputs %>% group_by(samplenum, year, microsim.init.sex, microsim.init.education, AlcCAT) %>% 
  summarise(n=sum(n)) %>% 
  drop_na()

tojoin <- data.frame(samplenum=1:length(percentreductions), percentreduction=percentreductions)


MeanAlcohol <- outputs %>% group_by(samplenum, year, microsim.init.sex, microsim.init.education) %>% 
  summarise(mean = mean(meangpd, na.rm=T)) %>% 
  mutate(year = as.numeric(as.character(year)),
         samplenum = as.numeric(as.character(samplenum)),
         microsim.init.sex = recode(microsim.init.sex, "m"="Men","f"="Women"),
         microsim.init.education = recode(microsim.init.education,
                                          "LEHS"="High school or less",
                                          "SomeC"="Some college",
                                          "College"="College +"),
         microsim.init.education = factor(microsim.init.education,
                                          levels=c("High school or less","Some college","College +"))) %>% 
  left_join(., tojoin) %>% 
  filter(samplenum<=4) %>% 
  mutate(percentreduction = paste0(percentreduction*100, "%"),
         percentreduction = factor(percentreduction, levels=c("0%","7.1%","10.8%","14.5%"))) %>% 
  rename(sex=microsim.init.sex, education=microsim.init.education)


write.csv(MeanAlcohol, "SIMAH_workplace/microsim/2_output_data/policy_experiments/processed_alcoholchange_data.csv",
          row.names=F)


ggplot(data=MeanAlcohol, aes(x=year, y=mean, colour=as.factor(percentreduction))) + 
  geom_line(linewidth=1) +
  facet_grid(cols=vars(microsim.init.education), rows=vars(microsim.init.sex)) + xlim(2014,2019) + ylim(0,NA) +
  ylab("Mean grams of alcohol per day") +
  theme_bw() + 
  theme(legend.position="bottom",
        strip.background = element_rect(fill="white"),
        panel.spacing = unit(1,"lines"),
        text = element_text(size=20)) + xlab("") + 
  scale_colour_brewer(palette="Dark2", name="Percentage reduction in alcohol use") + 
  ggtitle("Simulated reduction in alcohol use following taxation policy")
ggsave("SIMAH_workplace/microsim/2_output_data/policy_experiments/alcohol_change_education.png", dpi=500, width=25, height=19, units="cm")

# or not split by education 
MeanAlcoholSex <- MeanAlcohol %>% 
  group_by(year, microsim.init.sex, percentreduction) %>% 
  summarise(mean=mean(mean))

ggplot(data=MeanAlcoholSex, aes(x=year, y=mean, colour=as.factor(percentreduction))) + 
  geom_line(linewidth=1) +
  facet_grid(rows=vars(microsim.init.sex)) + xlim(2014,2019) + ylim(0,NA) +
  ylab("Mean grams of alcohol per day") +
  theme_bw() + 
  theme(legend.position="bottom",
        strip.background = element_rect(fill="white"),
        text = element_text(size=20)) + xlab("") + 
  scale_colour_brewer(palette="Dark2", name="Percentage reduction in alcohol use") + 
  ggtitle("Reductions in alcohol use following taxation policy")

getwd()

ggsave("SIMAH_workplace/microsim/2_output_data/policy_experiments/alcohol_change.png", dpi=500, width=25, height=19, units="cm")
