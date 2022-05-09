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

files <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agesp.RDS") %>% 
  do.call(rbind,.) %>% group_by(year, samplenum, microsim.init.sex, agegroup) %>% 
  summarise(microsim = mean(rateper100000),
            microsim = ifelse(is.na(microsim),0,microsim)) %>% 
  rename(sex=microsim.init.sex)

source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/process_cirrhosis_1984_2016.R")

cirrhosismortality <- cirrhosismortality %>% 
  rename(year=Year) %>% 
  dplyr::select(year, sex, agegroup, rate) %>% 
  rename(target=rate)

files <- left_join(files,cirrhosismortality) %>% 
  mutate(agegroup = ifelse(agegroup=="75.","75+", agegroup),
         sex = ifelse(sex=="f","Women","Men")) %>% 
  filter(agegroup!="15-19") %>% filter(agegroup!="20-24") %>% 
  filter(agegroup!="25-34") %>% 
  filter(agegroup!="75+") %>% 
  group_by(year, sex, agegroup) %>% 
  summarise(min=min(microsim),
            max=max(microsim),
            mean=mean(microsim),
            target=mean(target)) %>% 
  pivot_longer(mean:target) %>% 
  mutate(name = ifelse(name=="target","Observed","Simulated"))

ggplot(data=files, aes(x=year, y=value, colour=name, linetype=name)) + 
  scale_colour_manual(values=c("black","grey50")) + 
  scale_linetype_manual(values=c("solid","dotdash")) + 
  geom_ribbon(aes(ymin=min, ymax=max), fill='grey90', colour=NA) + 
  geom_line(size=1.5) + 
  # scale_linetype_manual(values=c("dashed","solid","dotted")) + 
  facet_grid(rows=vars(sex), cols=vars(agegroup), scales="fixed") + 
  theme_bw() +
  theme(strip.background = element_rect(fill="white"),
        text = element_text(size=18),
        legend.position="bottom",
        legend.title=element_blank()) + 
  ylab("Mortality rate per 100,000 population") +
  geom_vline(xintercept=2010, linetype="dashed") + xlab("") +
  ylim(0,NA)

ggsave("SIMAH_workplace/microsim/2_output_data/publication/Fig2_agesp.png",
       dpi=1000, width=25, height=21, units="cm")

# files <- (Sys.glob(paste("SIMAH_workplace/microsim/2_output_data/validation/outputfiles/*.csv", sep="")))
# 
# files <- lapply(files, function(x) read.csv(x)) %>% 
#   do.call(rbind,.)

# 
files <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agest_test-1.RDS") %>%
  do.call(rbind,.)

age2010 <- files %>% filter(year==2010) %>% 
  ungroup() %>% 
  group_by(year, microsim.init.sex, agegroup) %>% 
  summarise(poptotal = mean(populationtotal)) %>% ungroup() %>% 
  group_by(year, microsim.init.sex) %>% 
  mutate(percent = poptotal / sum(poptotal)) %>% ungroup() %>% dplyr::select(microsim.init.sex, agegroup, percent)

files <- files %>% group_by(year, microsim.init.sex, agegroup)

sim <- left_join(files, age2010) %>% 
  mutate(cirrhosistotal = ifelse(is.na(cirrhosistotal),0, cirrhosistotal)) %>% 
  group_by(year, samplenum, seed, microsim.init.sex, agegroup) %>% 
  mutate(weightedrate = (cirrhosistotal/populationtotal*100000)*percent) %>% ungroup() %>% 
  group_by(year, samplenum, seed, microsim.init.sex) %>% 
  summarise(microsim = sum(weightedrate)) %>% rename(sex=microsim.init.sex)

meansim <- sim %>% group_by(year, sex) %>% 
  summarise(min = min(microsim),
            max = max(microsim),
            microsim=mean(microsim)) %>% rename(Year=year)

meansim <- left_join(meansim, cirrhosismortality_agest) %>% 
  rename(target=agestrate) %>% 
  mutate(sex=ifelse(sex=="m","Men","Women")) %>% 
  group_by(Year, sex) %>% 
  pivot_longer(c(microsim,target)) %>% 
  mutate(name = ifelse(name=="microsim","Simulated",
                       ifelse(name=="target","Observed",name)),
         min = ifelse(name=="Observed",NA,min),
         max = ifelse(name=="Observed",NA,max))

ggplot(data=meansim, aes(x=Year, y=value, colour=name, linetype=name)) + 
  scale_colour_manual(values=c("black","grey50")) + 
  scale_linetype_manual(values=c("solid","dotdash")) + 
  geom_ribbon(aes(ymin=min, ymax=max), fill='grey90', colour=NA) + 
  geom_line(size=1.5) + 
  # scale_linetype_manual(values=c("dashed","solid","dotted")) + 
  facet_grid(rows=vars(sex), scales="fixed") + 
  theme_bw() +
  theme(strip.background = element_rect(fill="white"),
        text = element_text(size=18),
        legend.position="bottom",
        legend.title=element_blank()) + 
  ylab("Age-standardized mortality rate per 100,000 population") +
  geom_vline(xintercept=2010, linetype="dashed") + xlab("") +
  ylim(0,NA)

ggsave("SIMAH_workplace/microsim/2_output_data/publication/Fig1_agest.png",
       dpi=1000, width=21, height=21, units="cm")
