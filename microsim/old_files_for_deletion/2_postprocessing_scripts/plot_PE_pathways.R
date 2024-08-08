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
  summarise(cirrhosistotal = mean(cirrhosistotal),
            cirrhosistotal=ifelse(is.na(cirrhosistotal),0,cirrhosistotal),
            populationtotal = mean(populationtotal)) %>% 
  rename(sex = microsim.init.sex) %>% 
  mutate(type = "All pathways")
heavyonly <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agest_HeavyOnly.RDS")[[1]] %>% 
  group_by(year, microsim.init.sex, agegroup) %>% 
  summarise(cirrhosistotal = mean(cirrhosistotal),
            cirrhosistotal=ifelse(is.na(cirrhosistotal),0,cirrhosistotal),
            populationtotal = mean(populationtotal)) %>% 
  rename(sex = microsim.init.sex) %>% 
  mutate(type = "Heavy alcohol use")
metaboliconly <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agest_MetabolicOnly.RDS")[[1]] %>% 
  group_by(year, microsim.init.sex, agegroup) %>% 
  summarise(cirrhosistotal = mean(cirrhosistotal),
            cirrhosistotal=ifelse(is.na(cirrhosistotal),0,cirrhosistotal),
            populationtotal = mean(populationtotal)) %>% 
  rename(sex = microsim.init.sex) %>% 
  mutate(type = "Metabolic interaction")
heponly <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agest_HepatitisOnly.RDS")[[1]] %>% 
  group_by(year, microsim.init.sex, agegroup) %>% 
  summarise(cirrhosistotal = mean(cirrhosistotal),
            cirrhosistotal=ifelse(is.na(cirrhosistotal),0,cirrhosistotal),
            populationtotal = mean(populationtotal)) %>% 
  rename(sex = microsim.init.sex) %>% 
  mutate(type = "Hepatitis")

files <- rbind(allpathways, heavyonly, metaboliconly, heponly)
rm(allpathways, heavyonly, metaboliconly, heponly)
source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/process_cirrhosis_1984_2016.R")

age2010 <- files %>% filter(year==2010) %>% 
  ungroup() %>% 
  group_by(year, sex, agegroup) %>% 
  summarise(poptotal = mean(populationtotal)) %>% ungroup() %>% 
  group_by(year, sex) %>% 
  mutate(percent = poptotal / sum(poptotal)) %>% ungroup() %>% dplyr::select(sex, agegroup, percent)

sim <- left_join(files, age2010) %>% 
  mutate(cirrhosistotal = ifelse(is.na(cirrhosistotal),0, cirrhosistotal)) %>% 
  group_by(year, type, sex, agegroup) %>% 
  mutate(weightedrate = (cirrhosistotal/populationtotal*100000)*percent) %>% ungroup() %>% 
  group_by(year, type, sex) %>% 
  summarise(microsim = sum(weightedrate))

meansim <- sim %>% group_by(year, sex, type) %>% summarise(microsim=mean(microsim)) %>% rename(Year=year)

meansim <- left_join(meansim, cirrhosismortality_agest) %>% 
  rename(target=agestrate) %>% 
  mutate(sex=ifelse(sex=="m","Men","Women")) %>% 
  group_by(Year, sex, type) %>% 
  summarise(min=min(microsim),
         max=max(microsim),
         Simulated = mean(microsim),
         Observed=mean(target)) %>% 
  pivot_longer(cols=c(Simulated,Observed)) %>% 
  mutate(type= factor(type, levels=c("Heavy alcohol use",
                                     "Metabolic interaction",
                                     "Hepatitis",
                                     "All pathways")))


quant <- meansim %>% dplyr::select(Year, sex, type, name, value) %>% 
  filter(Year>1984) %>% 
  pivot_wider(names_from=name, values_from=value) %>% 
  mutate(percentdiff = ((abs(Observed-Simulated)) / mean(Observed+Simulated))*100) %>% 
  group_by(type, sex) %>% 
  summarise(mean = mean(percentdiff))

ggplot(data=meansim, aes(x=Year, y=value, colour=name)) + 
  geom_line(size=2) + 
  scale_colour_manual(values=c("black","grey70")) + 
  geom_ribbon(aes(ymin=min, ymax=max), colour=NA, alpha=0.3) + 
  facet_grid(rows=vars(sex), cols=vars(type), scales="fixed") + 
  theme_bw() +
  theme(strip.background = element_rect(fill="white"),
        text = element_text(size=18),
        legend.title=element_blank(),
        legend.position="bottom") + 
  ylab("Age-standardized mortality rate per 100,000 population") + 
  xlab("") + ylim(0,NA) + 
  xlim(1984,2010)

ggsave("SIMAH_workplace/microsim/2_output_data/publication/Fig_pathways_separate_agest.png",
       dpi=500, width=30, height=30, units="cm")
