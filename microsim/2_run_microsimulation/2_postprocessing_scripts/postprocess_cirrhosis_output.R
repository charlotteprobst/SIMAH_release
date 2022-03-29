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

# first plot how implausibility changes over waves
files <- (Sys.glob(paste("SIMAH_workplace/microsim/2_output_data/calibration_output/implausibility*.csv", sep="")))

index <- c(1,10,11,12,13,14,15,2,3,4,5,6,7,8,9)
files <- files[order(index)]
# 
files
list <- lapply(files, function(x) read.csv(x, header = TRUE)) 
for(i in 1:length(list)){
  list[[i]]$wave <- i
}

imp <- do.call(rbind, list)
ggplot(data=imp, aes(x=as.factor(wave), y=maximplausibility)) + geom_boxplot() + theme_bw() + 
  xlab("wave") + ylab("implausibility")
ggsave("SIMAH_workplace/microsim/2_output_data/calibration_output/plots/implausibility.png",
       dpi=300, width=33, height=19, units="cm")

# now plot cirrhosis output over waves 
files <- (Sys.glob(paste("SIMAH_workplace/microsim/2_output_data/calibration_output/Cirrhosis*.RDS", sep="")))
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

meansim <- files %>% group_by(wave, year, samplenum, seed, microsim.init.sex) %>% 
  mutate(microsim=ifelse(is.na(rateper100000),0,rateper100000)) %>% rename(sex=microsim.init.sex) %>% ungroup() %>% 
  group_by(wave, year, sex, agegroup, samplenum) %>% summarise(microsim=mean(microsim))
target <- cirrhosismortality %>% filter(Year<=2010 & Year>=1984) %>% rename(target=rate, year=Year) %>% 
  dplyr::select(year, sex, agegroup, target) %>% mutate(target=ifelse(is.na(target),0,target))
meansim <- left_join(meansim, target) %>% pivot_longer(cols=microsim:target)

agegroups <- c("35-44","45-54","55-64")
for(i in agegroups){
ggplot(data=subset(meansim, agegroup==i), aes(x=year, y=value, colour=name, linetype=as.factor(samplenum))) + 
  geom_line(size=1) + 
  facet_grid(rows=vars(sex), cols=vars(wave)) +
  scale_linetype_discrete(guide="none") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text = element_text(size=12),
        strip.background = element_rect(fill="white")) +
  ylab("rate per 100,000") + xlab("") + ggtitle(paste(i))
ggsave(paste0("SIMAH_workplace/microsim/2_output_data/calibration_output/plots/cirrhosis_waves_",i,".png"),
       dpi=300, width=33, height=19, units="cm")
}

# now plot the final wave for all age groups 
finalwave <- meansim %>% filter(wave==max(meansim$wave))
ggplot(data=finalwave, aes(x=year, y=value, colour=name, linetype=as.factor(samplenum))) + 
  geom_line(size=1) + 
  facet_grid(rows=vars(sex), cols=vars(agegroup)) +
  scale_linetype_discrete(guide="none") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        text = element_text(size=12),
        strip.background = element_rect(fill="white")) +
  ylab("rate per 100,000") + xlab("")
ggsave(paste0("SIMAH_workplace/microsim/2_output_data/calibration_output/plots/cirrhosis_finalwave.png"),
       dpi=300, width=33, height=19, units="cm")

# now plot the lhs samples 
files <- (Sys.glob(paste("SIMAH_workplace/microsim/2_output_data/calibration_output/lhs*.csv", sep="")))
index <- c(1,10,11,12,13,14,15,2,3,4,5,6,7,8,9)
# files
files <- files[order(index)]
list <- lapply(files, function(x) read.csv(x)) 
for(i in 1:length(list)){
  list[[i]]$wave <- i
}

files <- do.call(rbind, list) %>% pivot_longer(cols=BETA_MALE_MORTALITY:DECAY_SPEED) %>% 
  mutate(wave=as.factor(wave))

ggplot(data=files, aes(x=value, group=wave, fill=wave, colour=wave)) + 
  geom_density(aes(x=value, y=..scaled..,), alpha=0.4, inherit.aes = TRUE) + 
  facet_wrap(~name, scales="free") + theme_bw()
ggsave(paste0("SIMAH_workplace/microsim/2_output_data/calibration_output/plots/lhs_density.png"),
       dpi=300, width=33, height=19, units="cm")

# save the top 3 LH samples for the best run from the final wave 
topruns <- as.numeric(unlist(imp %>% filter(wave==max(imp$wave)) %>% 
  mutate(ntile = ntile(maximplausibility, nrow(.))) %>% 
  filter(ntile<=3) %>% dplyr::select(samplenum)))
toplhs <- do.call(rbind,list) %>% filter(wave==max(imp$wave)) %>% 
  filter(SampleNum %in% topruns)
