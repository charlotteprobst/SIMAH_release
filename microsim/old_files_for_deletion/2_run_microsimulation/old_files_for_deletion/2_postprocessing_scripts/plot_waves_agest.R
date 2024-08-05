#####Wrapper code for dynamic microsimulation
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc()
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
options(scipen=999)

####EDIT ONLY BELOW HERE ### 
###set working directory to the main "Microsimulation" folder in your directory 
WorkingDirectory <- "~/Google Drive/SIMAH Sheffield/"
# WorkingDirectory <- "/home/cbuckley/"
setwd(paste(WorkingDirectory))

# now plot cirrhosis output over waves 
files <- (Sys.glob(paste("SIMAH_workplace/microsim/2_output_data/calibration_output_decay/Cirrhosis*.RDS", sep="")))
index <- c(1,10,11,12,13,14,15,2,3,4,5,6,7,8,9)
files
files <- files[order(index)]
list <- lapply(files, function(x) readRDS(x)) 

for(i in 1:length(list)){
  list[[i]] <- as.data.frame(do.call(rbind,list[[i]]))
  list[[i]]$wave <- i
}

files <- do.call(rbind, list)
WholePopSize <- read.csv("SIMAH_workplace/microsim/1_generating_population/constraintsUSA.csv") %>% 
  dplyr::select(marriedF:unmarriedM) %>% mutate(total=marriedF+unmarriedF+marriedM+unmarriedM)
proportion <- 200000/WholePopSize$total

source("SIMAH_code/microsim/2_run_microsimulation/1_preprocessing_scripts/process_cirrhosis_1984_2016.R")

age2010 <- files %>% filter(year==2010) %>% 
  ungroup() %>% 
  group_by(year, microsim.init.sex, agegroup, samplenum) %>% 
  summarise(poptotal = mean(populationtotal)) %>% ungroup() %>% 
  group_by(year, microsim.init.sex, agegroup) %>% 
  summarise(poptotal = mean(poptotal)) %>% ungroup() %>% 
  group_by(year, microsim.init.sex) %>% 
  mutate(percent = poptotal / sum(poptotal)) %>% ungroup() %>% dplyr::select(microsim.init.sex, agegroup, percent)

sim <- left_join(files, age2010) %>% 
  mutate(cirrhosistotal = ifelse(is.na(cirrhosistotal),0, cirrhosistotal)) %>% 
  group_by(wave, year, samplenum, seed, microsim.init.sex, agegroup) %>% 
  mutate(weightedrate = (cirrhosistotal/populationtotal*100000)*percent) %>% ungroup() %>% 
  group_by(wave, year, samplenum, seed, microsim.init.sex) %>% 
  summarise(microsim = sum(weightedrate)) %>% rename(sex=microsim.init.sex)

meansim <- sim %>% group_by(wave, year, sex, samplenum) %>% summarise(microsim=mean(microsim)) %>% rename(Year=year)

meansim <- left_join(meansim, cirrhosismortality_agest) %>% 
  rename(target=agestrate) %>% 
  mutate(sex = ifelse(sex=="f","Women","Men"),
        microsim = ifelse(microsim>20 & Year==1984 & sex=="Women",NA,
                          ifelse(microsim>30 & Year==1984 & sex=="Men",NA, microsim)))
  # pivot_longer(microsim:target)

which <- unique(meansim[is.na(meansim$microsim),]$samplenum)

meansim <- meansim %>% filter(!samplenum %in% which)


ggplot(data=meansim, aes(x=Year, y=microsim, colour=as.factor(samplenum))) + geom_line() + 
  geom_line(aes(x=Year, y=target),colour="black", size=1) + 
  facet_grid(cols=vars(wave), rows=vars(sex)) + theme_bw() + 
  xlab("") +
  ylab("Age standardized rate per 100,000 population") +
  scale_x_continuous(breaks=c(1984, 2010), labels=c(1984,2010)) + 
  theme(legend.position="none",
        text = element_text(size=12),
        panel.spacing.x = unit(6,"mm")) 
ggsave("SIMAH_workplace/microsim/2_output_data/publication/SuppFig5.png", dpi=300, width=33, height=19, units="cm")
