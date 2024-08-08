#####Wrapper code for dynamic microsimulation
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
options(scipen=999)


###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee
SelectedState <- "USA"

####Size of population 
PopulationSize <- 200000
WholePopSize <- read.csv("SIMAH_workplace/microsim/1_generating_population/constraintsUSA.csv") %>% 
  dplyr::select(marriedF:unmarriedM) %>% mutate(total=marriedF+unmarriedF+marriedM+unmarriedM)
proportion <- PopulationSize/WholePopSize$total

allpathways <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agest_PE_all.RDS")[[1]] %>% 
  group_by(year, microsim.init.sex, agegroup) %>% 
  summarise(microsim = mean(rateper100000),
            microsim=ifelse(is.na(microsim),0,microsim),
            populationtotal = mean(populationtotal)) %>% 
  rename(sex = microsim.init.sex) %>% 
  mutate(type = "All pathways")

heavyonly <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agest_HeavyOnly.RDS")[[1]] %>% 
  group_by(year, microsim.init.sex, agegroup) %>% 
  summarise(microsim = mean(rateper100000),
            microsim=ifelse(is.na(microsim),0,microsim),
            populationtotal = mean(populationtotal)) %>% 
  rename(sex = microsim.init.sex) %>% 
  mutate(type = "Heavy alcohol use")
metaboliconly <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agest_MetabolicOnly.RDS")[[1]] %>% 
  group_by(year, microsim.init.sex, agegroup) %>% 
  summarise(microsim = mean(rateper100000),
            microsim=ifelse(is.na(microsim),0,microsim),
            populationtotal = mean(populationtotal)) %>% 
  rename(sex = microsim.init.sex) %>% 
  mutate(type = "Metabolic interaction")
heponly <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agest_HepatitisOnly.RDS")[[1]] %>% 
  group_by(year, microsim.init.sex, agegroup) %>% 
  summarise(microsim = mean(rateper100000),
            microsim=ifelse(is.na(microsim),0,microsim),
            populationtotal = mean(populationtotal)) %>% 
  rename(sex = microsim.init.sex) %>% 
  mutate(type = "Hepatitis")

files <- rbind(allpathways, heavyonly, metaboliconly, heponly)
rm(allpathways, heavyonly, metaboliconly, heponly)
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/process_cirrhosis_1984_2016.R")

cirrhosismortality <- cirrhosismortality %>% 
  rename(year=Year) %>% 
  dplyr::select(year, sex, agegroup, rate) %>% 
  rename(target=rate)

meansim <- left_join(files, cirrhosismortality) %>% 
  mutate(sex=ifelse(sex=="m","Men","Women")) %>% 
  group_by(year, type, sex,agegroup) %>% 
  summarise(min=min(microsim),
            max=max(microsim),
            Simulated =mean(microsim),
            Observed=mean(target)) %>% 
  pivot_longer(cols=c(Simulated,Observed)) %>% 
  filter(agegroup=="35-44" | agegroup=="45-54" | agegroup=="55-64" | agegroup=="65-74") %>% 
  mutate(type = factor(type, levels=c("Heavy alcohol use",
                                "Metabolic interaction",
                                "Hepatitis",
                                "All pathways")))

men <- ggplot(data=subset(meansim, sex=="Men"), aes(x=year, y=value, colour=name)) + 
  geom_line(size=2) + 
  scale_colour_manual(values=c("black","grey70")) + 
  geom_ribbon(aes(ymin=min, ymax=max), colour=NA, alpha=0.3) + 
  facet_grid(rows=vars(agegroup), cols=vars(type), scales="fixed") + 
  theme_bw() +
  theme(strip.background = element_rect(fill="white"),
        text = element_text(size=18),
        legend.title=element_blank(),
        legend.position="bottom") + 
  ylab("Mortality rate per 100,000 population") +
  geom_vline(xintercept=2010, linetype="dashed") + xlab("") + ylim(0,NA) + 
  ggtitle("Men")
women <- ggplot(data=subset(meansim, sex=="Women"), aes(x=year, y=value, colour=name)) + 
  geom_line(size=2) + 
  scale_colour_manual(values=c("black","grey70")) + 
  geom_ribbon(aes(ymin=min, ymax=max), colour=NA, alpha=0.3) + 
  facet_grid(rows=vars(agegroup), cols=vars(type), scales="fixed") + 
  theme_bw() +
  theme(strip.background = element_rect(fill="white"),
        text = element_text(size=18),
        legend.title=element_blank(),
        legend.position="bottom") + 
  ylab("Mortality rate per 100,000 population") +
  geom_vline(xintercept=2010, linetype="dashed") + xlab("") + ylim(0,NA) + 
  ggtitle("Women")

library(gridExtra)
library(ggpubr)

combined <- ggarrange(men, women, ncol=1, nrow=2, common.legend = TRUE, legend="bottom")

ggsave("SIMAH_workplace/microsim/2_output_data/publication/Fig_pathways_separate_agesp.png",
       dpi=500, width=38, height=30, units="cm")





