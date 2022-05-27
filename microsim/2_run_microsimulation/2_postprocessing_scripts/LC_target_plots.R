# simple plots of liver cirrhosis mortality over time 
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
WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

####which geography -  needs to be written as USA, California, Minnesota, New York, Texas, Tennessee
SelectedState <- "USA"

####Size of population 
PopulationSize <- 200000
WholePopSize <- read.csv("SIMAH_workplace/microsim/1_generating_population/constraintsUSA.csv") %>% 
  dplyr::select(marriedF:unmarriedM) %>% mutate(total=marriedF+unmarriedF+marriedM+unmarriedM)
proportion <- PopulationSize/WholePopSize$total

source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/process_cirrhosis_1984_2016.R")

cirrhosismortality_agest <- cirrhosismortality_agest %>% 
  mutate(sex = ifelse(sex=="f","Women","Men"))


ggplot(data=cirrhosismortality_agest, aes(x=Year, y=agestrate)) + 
  # scale_colour_manual(values=c("black","grey50")) + 
  # scale_linetype_manual(values=c("solid","dotdash")) + 
  # geom_ribbon(aes(ymin=min, ymax=max), fill='grey90', colour=NA) + 
  geom_line(size=1.5) + 
  # scale_linetype_manual(values=c("dashed","solid","dotted")) + 
  facet_grid(cols=vars(sex), scales="fixed") + 
  theme_bw() +
  theme(strip.background = element_rect(fill="white"),
        text = element_text(size=18),
        legend.position="bottom",
        legend.title=element_blank()) + 
  ylab("Mortality rate per 100,000 population") +
  # geom_vline(xintercept=2010, linetype="dashed") + 
  xlab("") +
  ylim(0,NA) + xlim(1984, 2019)

ggsave("SIMAH_workplace/microsim/2_output_data/publication/Target_agest.png",
       dpi=1000, width=33, height=19, units="cm")
