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

files <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agesp_2019.RDS") %>% 
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
  filter(agegroup!="75+")

means2 <- files %>% 
  mutate(percentdifference = abs(microsim - target) / mean(microsim+target),
         absdifference = abs(microsim - target),
         yeargroups = ifelse(year >=1985 & year<=2010, 1,
                             ifelse(year>=2011, 2, 0))) %>% filter(yeargroups!=0) %>% 
  group_by(sex, agegroup, year) %>% 
  summarise(meanpercent = mean(percentdifference),
            minpercent = min(percentdifference),
            maxpercent = max(percentdifference)) %>% filter(sex=="Women") %>% 
  filter(agegroup=="55-64")

diff <- means %>% group_by(sex,agegroup,yeargroups) %>% 
  summarise(meanpercentdiff = mean(meanpercent),
            minpercent = min(meanpercent),
            maxpercent = max(meanpercent))

files <- files %>% 
  group_by(year, sex, agegroup) %>% 
  summarise(min=min(microsim),
            max=max(microsim),
            mean=mean(microsim),
            target=mean(target)) %>% 
  pivot_longer(mean:target) %>% 
  mutate(name = ifelse(name=="target","Observed","Simulated"))

ggplot(data=files, aes(x=year, y=value, colour=name, linetype=name)) + 
  scale_colour_manual(values=c("black","grey50")) + 
  scale_linetype_manual(values=c("solid","solid")) + 
  scale_fill_manual(values=c("white","grey50")) + 
  geom_ribbon(aes(ymin=min, ymax=max, fill=name), alpha=0.5, colour=NA) + 
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

ggplot(data=subset(files, name=="Observed"), aes(x=year, y=value, colour=name, linetype=name)) + 
  # scale_colour_manual(values=c("blue","orange")) +
  # scale_colour_brewer(palette="Dark2") +
  scale_colour_manual(values=c("#D95F02")) + 
  scale_linetype_manual(values=c("solid","solid")) + 
  scale_fill_manual(values=c("#D95F02")) + 
  # geom_ribbon(aes(ymin=min, ymax=max, fill=name), colour=NA, alpha=0.5) + 
  geom_line(size=2) + 
  # scale_linetype_manual(values=c("dashed","solid","dotted")) + 
  facet_grid(rows=vars(sex), cols=vars(agegroup), scales="fixed") + 
  theme_bw() +
  theme(strip.background = element_rect(fill="white"),
        text = element_text(size=18),
        legend.position="bottom",
        legend.title=element_blank()) + 
  scale_x_continuous(breaks=c(1990, 2000, 2010, 2019)) + 
  ylab("Mortality rate per 100,000 population") +
  geom_vline(xintercept=2010, linetype="dashed", size=2) + xlab("") +
  ylim(0,NA)
  
ggsave("SIMAH_workplace/microsim/2_output_data/publication/Fig2_presentation_V1.png",
       dpi=1000, width=33, height=18, units="cm")

ggplot(data=files, aes(x=year, y=value, colour=name, linetype=name)) + 
  # scale_colour_manual(values=c("blue","orange")) +
  # scale_colour_brewer(palette="Dark2") +
  scale_colour_manual(values=c("#D95F02", "darkgreen")) +
  scale_linetype_manual(values=c("solid","solid")) + 
  scale_fill_manual(values=c("white","darkgreen")) + 
  geom_ribbon(aes(ymin=min, ymax=max, fill=name), colour=NA, alpha=0.5) + 
  geom_line(size=2) + 
  # scale_linetype_manual(values=c("dashed","solid","dotted")) + 
  facet_grid(rows=vars(sex), cols=vars(agegroup), scales="fixed") + 
  theme_bw() +
  theme(strip.background = element_rect(fill="white"),
        text = element_text(size=18),
        legend.position="bottom",
        legend.title=element_blank()) + 
  scale_x_continuous(breaks=c(1990, 2000, 2010, 2019)) + 
  ylab("Mortality rate per 100,000 population") +
  geom_vline(xintercept=2010, linetype="dashed", size=2) + xlab("") +
  ylim(0,NA)

ggsave("SIMAH_workplace/microsim/2_output_data/publication/Fig2_presentation_V2.png",
       dpi=1000, width=33, height=18, units="cm")



files <- readRDS("SIMAH_workplace/microsim/2_output_data/validation/Cirrhosis_validation_agest_2019.RDS") %>%
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

differences <- sim %>% rename(Year=year) %>% left_join(.,cirrhosismortality_agest) %>% 
  group_by(Year, samplenum, sex) %>% 
  summarise(microsim=mean(microsim),
            target = mean(agestrate))

meandifferences <- differences %>% 
  mutate(percentdifference = abs(microsim - target) / (microsim+target/2),
         absdifference = abs(microsim - target),
         yeargroups = ifelse(Year >=1985 & Year<=2010, 1,
                             ifelse(Year>=2011, 2, 0))) %>% 
  group_by(Year, sex) %>% 
  summarise(meanrate = mean(microsim),
            minrate = min(microsim),
            maxrate = max(microsim),
            target = mean(target),
    meandifference = mean(percentdifference),
            mindifference = min(percentdifference),
            maxdifference = max(percentdifference))

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
  scale_linetype_manual(values=c("solid","solid")) + 
  scale_fill_manual(values=c("white","grey50")) + 
  geom_ribbon(aes(ymin=min, ymax=max, fill=name), alpha=0.5, colour=NA) + 
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

ggsave("SIMAH_workplace/microsim/2_output_data/publication/Fig1_agest_experiment25percent.png",
       dpi=1000, width=21, height=21, units="cm")


# for presentation
ggplot(data=subset(meansim, name=="Observed"), aes(x=Year, y=value, colour=name, linetype=name)) + 
  scale_colour_manual(values=c("#D95F02","grey50")) +
  # scale_colour_brewer(palette="Dark2") + 
  scale_linetype_manual(values=c("solid","solid")) + 
  # geom_ribbon(aes(ymin=min, ymax=max, fill=name), alpha=0.5, colour=NA) + 
  geom_line(size=2) + 
  # scale_linetype_manual(values=c("dashed","solid","dotted")) + 
  facet_grid(cols=vars(sex), scales="fixed") + 
  scale_x_continuous(breaks=c(1990, 2000, 2010, 2019)) + 
  theme_bw() +
  theme(strip.background = element_rect(fill="white"),
        text = element_text(size=19.5),
        legend.position="bottom",
        legend.title=element_blank()) + 
  ylab("Age-standardized mortality rate per 100,000 population") +
  geom_vline(xintercept=2010, linetype="dashed", size=2) + xlab("") +
  ylim(0,NA)

ggsave("SIMAH_workplace/microsim/2_output_data/publication/Fig1_presentation1.png",
       dpi=500, width=34, height=18, units="cm")

ggplot(data=meansim, aes(x=Year, y=value, colour=name, linetype=name)) + 
  scale_colour_manual(values=c("#D95F02","darkgreen")) +
  # scale_colour_brewer(palette="Dark2") +
  scale_linetype_manual(values=c("solid","solid")) + 
  scale_fill_manual(values=c("white","darkgreen")) + 
  geom_ribbon(aes(ymin=min, ymax=max, fill=name), alpha=0.5, colour=NA) + 
  geom_line(size=2) + 
  # scale_linetype_manual(values=c("dashed","solid","dotted")) + 
  facet_grid(cols=vars(sex), scales="fixed") + 
  scale_x_continuous(breaks=c(1990, 2000, 2010, 2019)) + 
  theme_bw() +
  theme(strip.background = element_rect(fill="white"),
        text = element_text(size=19.5),
        legend.position="bottom",
        legend.title=element_blank()) + 
  ylab("Age-standardized mortality rate per 100,000 population") +
  geom_vline(xintercept=2010, linetype="dashed", size=2) + xlab("") +
  ylim(0,NA)

ggsave("SIMAH_workplace/microsim/2_output_data/publication/Fig1_presentation2.png",
       dpi=500, width=34, height=18, units="cm")

meandifferences <- meansim %>% dplyr::select(Year, sex, name, value) %>% 
  pivot_wider(names_from=name, values_from=value) %>% 
  mutate(percentdifference = abs(Simulated - Observed) / Observed,
         absdifference = abs(Simulated - Observed),
         yeargroups = ifelse(Year >=1985 & Year<=2010, 1,
                             ifelse(Year>=2011, 2, 0)))
diff <- meandifferences %>% group_by(sex,yeargroups) %>% 
  summarise(meanpercent = mean(percentdifference),
            meanabsdifference = mean(absdifference))
