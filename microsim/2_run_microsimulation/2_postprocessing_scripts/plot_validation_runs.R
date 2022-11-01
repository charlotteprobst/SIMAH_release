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

# PE <- read_rds("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_PE.RDS") %>% 
#   do.call(rbind,.) %>% 
#   group_by(year, microsim.init.sex, agegroup) %>% 
#   summarise(microsim = mean(rateper100000),
#             microsim = ifelse(is.na(microsim),0,microsim)) %>% 
#   mutate(samplenum="PE") %>% 
#   rename(sex=microsim.init.sex)

files <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agesp_2019-1.RDS") %>% 
  do.call(rbind,.) %>% group_by(year, samplenum, microsim.init.sex, agegroup) %>% 
  summarise(microsim = mean(rateper100000),
            microsim = ifelse(is.na(microsim),0,microsim)) %>% 
  rename(sex=microsim.init.sex) %>% 
  mutate(samplenum=as.character(samplenum))

source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/process_cirrhosis_1984_2016.R")

cirrhosismortality <- cirrhosismortality %>% 
  rename(year=Year) %>% 
  dplyr::select(year, sex, agegroup, rate) %>% 
  rename(target=rate)

# files <- left_join(files,cirrhosismortality) %>% 
#   group_by(year, sex, agegroup) %>% 
#   mutate(min=min(microsim), max=max(microsim),
#          sex = ifelse(sex=="m","Men","Women"),
#          agegroup = ifelse(agegroup=="75.","75+", agegroup)) %>% 
#   filter(agegroup!="15-19") %>% filter(agegroup!="20-24") %>% 
#   mutate(PE = ifelse(samplenum=="PE",microsim, NA)) %>% 
#   group_by(year, sex,agegroup) %>% 
#   fill(PE, .direction="downup")

meansim <- left_join(files, cirrhosismortality) %>% 
  mutate(sex=ifelse(sex=="m","Men","Women")) %>% 
  group_by(year, sex,agegroup) %>% 
  summarise(min=min(microsim),
            max=max(microsim),
            Simulated = mean(microsim),
            Observed=mean(target)) %>% 
  pivot_longer(cols=c(Simulated,Observed)) %>% 
  filter(agegroup=="35-44" | agegroup=="45-54" | agegroup=="55-64" | agegroup=="65-74")

quant <- meansim %>% dplyr::select(-c(min,max)) %>% 
  pivot_wider(names_from=sex, values_from=value) %>% 
  filter(name=="Simulated") %>% ungroup() %>% 
  mutate(pct_change_men = (Men / lead(Men)-1)*100,
         pct_change_women = (Women / lead(Women)-1)*100) %>% 
  filter(agegroup=="65-74")

ggplot(data=meansim, aes(x=year, y=value, colour=name)) + 
  geom_line(size=2) + 
  scale_colour_manual(values=c("black","grey70")) + 
  geom_ribbon(aes(ymin=min, ymax=max), colour=NA, alpha=0.3) + 
  facet_grid(rows=vars(sex), cols=vars(agegroup), scales="fixed") + 
  theme_bw() +
  theme(strip.background = element_rect(fill="white"),
        text = element_text(size=18),
        legend.title=element_blank(),
        legend.position="bottom") + 
  ylab("Mortality rate per 100,000 population") +
  geom_vline(xintercept=2010, linetype="dashed") + xlab("") + ylim(0,NA)

ggsave("SIMAH_workplace/microsim/2_output_data/publication/Fig2_agesp.png",
       dpi=500, width=30, height=30, units="cm")

files <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agest_2019-1.RDS") %>% 
  do.call(rbind,.)

age2010 <- files %>% filter(year==2010) %>% 
  ungroup() %>% 
  group_by(year, microsim.init.sex, agegroup) %>% 
  summarise(poptotal = mean(populationtotal)) %>% ungroup() %>% 
  group_by(year, microsim.init.sex) %>% 
  mutate(percent = poptotal / sum(poptotal)) %>% ungroup() %>% dplyr::select(microsim.init.sex, agegroup, percent)

sim <- left_join(files, age2010) %>% 
  mutate(cirrhosistotal = ifelse(is.na(cirrhosistotal),0, cirrhosistotal)) %>% 
  group_by(year, samplenum, seed, microsim.init.sex, agegroup) %>% 
  mutate(weightedrate = (cirrhosistotal/populationtotal*100000)*percent) %>% ungroup() %>% 
  group_by(year, samplenum, seed, microsim.init.sex) %>% 
  summarise(microsim = sum(weightedrate)) %>% rename(sex=microsim.init.sex)

meansim <- sim %>% group_by(year, sex, samplenum) %>% summarise(microsim=mean(microsim)) %>% rename(Year=year)

meansim <- left_join(meansim, cirrhosismortality_agest) %>% 
  rename(target=agestrate) %>% 
  mutate(sex=ifelse(sex=="m","Men","Women")) %>% 
  group_by(Year, sex) %>% 
  summarise(min=min(microsim),
         max=max(microsim),
         Simulated = mean(microsim),
         Observed=mean(target)) %>% 
  pivot_longer(cols=c(Simulated,Observed))

quant <- meansim %>% dplyr::select(-c(min,max)) %>% 
  pivot_wider(names_from=sex, values_from=value) %>% 
  filter(name=="Simulated") %>% ungroup() %>% 
  mutate(pct_change_men = (Men / lead(Men)-1)*100,
         pct_change_women = (Women / lead(Women)-1)*100)

quant <- meansim %>% dplyr::select(Year, sex, name, value) %>% 
  filter(Year>=1985) %>% 
  pivot_wider(names_from=name, values_from=value) %>% 
  mutate(percentdiff = ((abs(Observed-Simulated)) / mean(Observed+Simulated))*100) %>% 
  group_by(sex) %>% 
  summarise(mean = mean(percentdiff))

ggplot(data=meansim, aes(x=Year, y=value, colour=name)) + 
  geom_line(size=2) + 
  scale_colour_manual(values=c("black","grey70")) + 
  geom_ribbon(aes(ymin=min, ymax=max), colour=NA, alpha=0.3) + 
  facet_grid(rows=vars(sex), scales="fixed") + 
  theme_bw() +
  theme(strip.background = element_rect(fill="white"),
        text = element_text(size=18),
        legend.title=element_blank(),
        legend.position="bottom") + 
  ylab("Age-standardized mortality rate per 100,000 population") +
  geom_vline(xintercept=2010, linetype="dashed") + xlab("") + ylim(0,NA)

ggsave("SIMAH_workplace/microsim/2_output_data/publication/Fig1_agest.png",
       dpi=500, width=30, height=30, units="cm")
